{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances, TupleSections, OverloadedStrings #-}
module Language.Go.Parser (parsePackage
                          ,parseFile
                          ,parseText
                          ,defaultPackageLoader
                          ,SourceRange(..)
                          ,SourcePos(..)
                          ,Parser
                          ,ParserAnnotation
                          ) where

import Prelude hiding (readFile)
import Language.Go.Parser.Ambiguous
import Language.Go.AST
import Language.Go.Parser.Lexer
import Language.Go.Parser.State
import Language.Go.Parser.Util
import Language.Go.Bindings
import Language.Go.Bindings.Types
import Language.Go.Types

import System.IO (FilePath)
import AlexTools (SourceRange(..), initialInput)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.IO
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, fromJust, isNothing, maybeToList)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad (liftM, unless)
import Lens.Simple
import Data.Default.Class
import Data.Semigroup
import System.Environment (lookupEnv)
import System.Directory
import System.FilePath
import Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Traversable
import Data.Bifunctor
import Control.Arrow
import Data.List (foldl', or)
import Control.Applicative
import Data.Char (generalCategory, GeneralCategory (..))
import Data.Generics.Uniplate.Data


-- | We don't resolve types for now, so all bindings are typed with this type
noType = Nil

runParser :: Parser a -> IO (Either (SourceRange, String) a)
runParser = (`evalStateT` def) . runExceptT

parseText :: Text -- ^ Text of a file/module
          -> (Text -> Parser (Package SourceRange))
          -- ^ A resolver/loader function for imported
          -- modules. Implementations are encouraged to use
          -- `parse`. For an example implementation see
          -- `defaultPackageLoader` and `parsePackage`.
          -> IO (Either (SourceRange, String) (File SourceRange))
parseText txt loader = runParser $ parse txt loader

parse :: Text -- ^ Text of the program/module
      -> (Text -> Parser (Package SourceRange))
      -- ^ A resolver/loader function for imported modules 
      -> Parser (File SourceRange)
parse txt loader = (liftHappy $ file $ lexer $ initialInput txt)
               >>= (\f@(File rng _ pname _ _) -> return $ Package rng pname (f :| []))
               >>= transformBiM loadImport
               >>= postprocess
               >>= (\(Package _ _ (f :| [])) -> return f)
  where loadImport :: ImportSpec SourceRange -> Parser (ImportSpec SourceRange)
        loadImport is@(Import _ _ path) = resolveImport path loader >> return is

liftHappy :: Either (SourceRange, String) (a SourceRange)
          -> Parser (a SourceRange)
liftHappy = either throwError return

-- | 1) Collect standard library identifiers
--   2) Collect binding information from imports
--   3) Collect local top-level bindings
--   4) Traverse scopes (function bodies, blocks) gathering local
--   binding information and resolve program identifiers as types or
--   variables/constants/functions.
--   6) Check that all names are in the right contexts: e.g. no type
--   names are used as expressions and vice-versa.
--
--   Top-down rewriting as we need to see the context.

-- | Infer identifier bindings, types of variables and expressions and
-- disambiguate expressions
-- postprocess :: Package SourceRange -> Parser (Package SourceRange)
-- postprocess = annotateBindings >=> disambiguate

--   Based on the semantics of identifiers (in annotations) rewrite expressions:
--      * FieldSelector with the object part that is a Name. Check if
--        that Name is a package name. If so, this expression can
--        be a QualifiedName.
--      * CallExpr with either:
--        - the length of the arguments list is 1 and the first
--          argument is not a type. Check if the function part is a
--          name which is a named type, or a Name with a qualifier that
--          referse to a type. If so, this is a type conversion.
--        - the first argument is a Name
--          (disambiguated per rule 1). If so, that argument can be a
--          named type instead
--   Note: this just looks at the top level node, so should be called
--   from a function that does a recursive traversal.
disambiguate :: Expression SourceRange -> Parser (Expression SourceRange)
disambiguate e = case e of
  FieldSelector a (Name na Nothing ident) fident@(Id _ _ fname) ->
    case ident of
      Id _ bind pname -> case bind^.bindingKind of
        PackageB _ pexports ->
          if HM.member fname pexports then return $ Name a (Just ident) fident
          else unexpected fident $ "Identifier " ++ (T.unpack fname) ++ " not exported from package " ++ (T.unpack pname)
        _                   -> return e
      BlankId {}  -> unexpected ident "Reading from a blank identifier"
      _ -> undefined
  CallExpr a fn mtype args mspread ->
       do -- 1. disambiguate arguments to see if the first argument is
          -- actually a type
          (mtype', args') <- case args of
            (a:as) -> case a of
              Name _ mqual ident ->
                do (mtype'', mexp) <- disambiguateFirstTypeParam mtype mqual ident
                   return (mtype'', (maybeToList mexp) ++ as)
              _                  -> return (mtype, args)
            [] -> return (mtype, args)
          let call' = CallExpr a fn mtype' args' mspread
          -- 2. Disambiguate the call expression from a type conversion.
          case fn of
            Name rng mqual ident ->
              if isType ident && -- the function part is actually a type
                 isNothing mtype && -- the first argument is not a type
                 length args == 1 && -- there is exactly one argument
                 isNothing mspread -- there is no spread argument
              then return $ pos (rng, head args) Conversion 
                   (NamedType rng $ TypeName rng mqual ident)
                   (head args)
              else return call'
            _ -> return call'
  _ -> return e

disambiguateFirstTypeParam :: Maybe (Type SourceRange)
                           -> Maybe (Id SourceRange)
                           -> Id (SourceRange)
                           -> Parser (Maybe (Type SourceRange), Maybe (Expression SourceRange))
disambiguateFirstTypeParam mtype mqual ident =
  if isType ident then
    case mtype of
      Just _ -> unexpected ident "More than one type parameter to a function call"
      Nothing -> return (Just $ pos (mqual, ident) NamedType $ pos (mqual, ident) TypeName mqual ident, Nothing)
  else return (mtype, Just $ pos (mqual, ident) Name mqual ident)

isType :: Id a -> Bool
isType (Id _ bind _) = case bind^.bindingKind of
  TypeB _ -> True
  _       -> False
    
-- 1) The scope of a predeclared identifier is the universe block.
-- 2) The scope of an identifier denoting a constant, type, variable,
-- or function (but not method) declared at top level (outside any
-- function) is the package block.
-- 3) The scope of the package name of an imported package is the file
-- block of the file containing the import declaration.
-- 4) The scope of an identifier denoting a method receiver, function
-- parameter, or result variable is the function body.
-- 5) The scope of a constant or variable identifier declared inside a
-- function begins at the end of the ConstSpec or VarSpec
-- (ShortVarDecl for short variable declarations) and ends at the end
-- of the innermost containing block.
-- 6) The scope of a type identifier declared inside a function begins
-- at the identifier in the TypeSpec and ends at the end of the
-- innermost containing block.

class Postprocess a where
  postprocess :: a -> Parser a
  -- ^ Postprocess the AST:
  -- 1. Collect the bindings from `a` in the current scope and modify
  -- binding annotations on identifiers to reflect the kind of the
  -- binding and the relevant type.
  -- 2. Disambiguate expressions.
  -- 3. Infer/check types

-- | This instance will record only the bindings exported from the
-- package. 
instance Postprocess (Package SourceRange) where
  postprocess (Package a pname files) =
    do -- 1. Initialize scope with the predefined type/var bindings
       identifiers .= defaultBindings
       -- 2. with a new scope, for each file
       newScope $ do 
         -- 3. Gather all the top level bindings in the current package
         imports <- liftM (HM.fromList) $ mapM (\f -> collectTopLevelBindings f >>= return . (fileName f,)) $ NE.toList files
         -- 4. Traverse and postprocess each file
         (Package a pname) <$> mapM (postprocessFile imports) files

postprocessFile :: HashMap Text (HashMap Text Binding) -> File SourceRange -> Parser (File SourceRange)
postprocessFile imports f = bracketScope $
  modifyInnerScope (`HM.union` (HM.lookupDefault (HM.empty) (fileName f) imports))
  >> postprocess f

-- | Adds bindings for the top-level declarations to the
-- state. Returns the imported declarations. The latter are not part
-- of the state to reflect that the scope of imports is the file
-- scope, and the scope of top level declarations is the package
-- scope.
collectTopLevelBindings :: File SourceRange
                        -> Parser (HashMap Text Binding)
collectTopLevelBindings (File _ _ _ imports tops)  =
  do oldTopBinds <- getCurrentBindings
     -- 1. Add import bindings,
     mapM postprocess imports
     -- remembering the mapping separately
     newTopBinds <- getCurrentBindings
     let importbinds = HM.difference newTopBinds oldTopBinds
     -- 2. Scan and add top level bindings
     mapM postprocess tops
     -- 3. Remove bindings for package names (difference between the
     -- state and the remembered mapping)
     modifyInnerScope (`HM.difference` importbinds)
     return importbinds

instance Postprocess (File SourceRange) where
  postprocess (File a fname pname imports topLevels) =
    (File a fname pname imports) <$> mapM postprocessFunctions topLevels

postprocessFunctions :: TopLevel SourceRange
                     -> Parser (TopLevel SourceRange)
postprocessFunctions tl = case tl of
  FunctionDecl a name params returns mbody -> newScope $
    do params' <- postprocess params
       returns' <- postprocess returns
       liftM (FunctionDecl a name params' returns') $
         sequence $ fmap (mapM postprocess) mbody
  MethodDecl a recv name params returns mbody -> newScope $
    do recv' <- postprocess recv
       params' <- postprocess params
       returns' <- postprocess returns
       liftM (MethodDecl a recv' name params' returns') $
         sequence $ fmap (mapM postprocess) mbody
       -- we have already analysed and recorded bindings from
       -- declarations in `collectTopLevelBindings` and there are no
       -- nested binding scopes in them, so we skip top declarations.
  TopDecl {} -> return tl

instance Postprocess (ParameterList SourceRange) where
  postprocess pl = case pl of
    NamedParameterList a nps mrestp -> NamedParameterList a <$> postprocess nps <*> postprocess mrestp
    AnonymousParameterList a aps mrestp -> AnonymousParameterList a <$> postprocess aps <*> postprocess mrestp

instance Postprocess (NamedParameter SourceRange) where
  postprocess (NamedParameter a ident ty) =
    do ty' <- postprocess ty
       tyt <- getType ty'
       i' <- declareBindingIdM ident (VarB tyt)
       return $ NamedParameter a i' ty'

instance Postprocess (ReturnList SourceRange) where
  postprocess rl = case rl of
    NamedReturnList a nps  -> NamedReturnList a <$> postprocess nps
    AnonymousReturnList a aps -> AnonymousReturnList a <$> postprocess aps

-- An identifier is exported iff:
-- 1) the first character of the identifier's name is a Unicode upper
-- case letter (Unicode class "Lu"); and
-- 2) the identifier is declared in the package block or it is a field
-- name or method name.
isExported :: Text -> Binding -> Bool
isExported n bind | not (bind^.bindingImported) = generalCategory (T.head n) == UppercaseLetter
isExported _ _                                  = False
  
topLevelNames :: Package SourceRange -> Parser Bindings
topLevelNames (Package _ _ _) = undefined
  -- do mapM postprocess decls
  --    getCurrentBindings

instance Postprocess (ImportDecl SourceRange) where
  postprocess (ImportDecl a ispecs) = ImportDecl a <$> mapM postprocess ispecs

instance Postprocess (TopLevel SourceRange) where
  postprocess tl = case tl of
    FunctionDecl a fname params returns body ->
      do (params', returns') <- newScope $ liftM2 (,) (postprocess params) (postprocess returns)
         bk <- VarB <$> (Function Nothing <$> getParamTypes params' <*> getSpreadType params' <*> getReturnTypes returns')
         fname' <- declareBindingIdM fname bk
         return (FunctionDecl a fname' params' returns' body)
    MethodDecl  a recv mname params returns body ->
      do (params', returns', recv') <- newScope $ liftM3 (,,) (postprocess params) (postprocess returns) (postprocess recv)
         bk <- VarB <$> (Function <$> (Just <$> getType recv') <*> getParamTypes params' <*> getSpreadType params' <*> getReturnTypes returns')
         mname' <- declareBindingIdM mname bk
         return (MethodDecl a recv' mname' params' returns' body)
    TopDecl a decl -> TopDecl a <$> postprocess decl

instance Postprocess (Declaration SourceRange) where
  postprocess decl = case decl of
    TypeDecl a tspecs -> TypeDecl a <$> mapM postprocess tspecs
    VarDecl a vspecs -> VarDecl a <$> mapM postprocess vspecs
    ConstDecl a cspecs -> ConstDecl a <$> mapM postprocess cspecs

instance Postprocess (TypeSpec SourceRange) where
  postprocess (TypeSpec a tid typ) = TypeSpec a <$>
   (TypeB <$> getType typ >>= declareBindingIdM tid) <*> return typ

instance Postprocess (VarSpec SourceRange) where
  postprocess vs = case vs of
    TypedVarSpec a idents ty inits ->
      do idents' <- mapM (\ident -> (VarB <$> getType ty) >>= declareBindingIdM ident) idents
         return $ TypedVarSpec a idents' ty inits
         -- ^ TODO check that the inferred types of inits are compatible with ty
    UntypedVarSpec a idents inits ->
      if NE.length idents == NE.length inits then
        do inits' <- mapM postprocess inits
           idents' <- mapM (\(ident, init) ->
                              VarB <$> getType init >>= declareBindingIdM ident) $ NE.zip idents inits'
           return (UntypedVarSpec a idents' inits')
      else unexpected a "The number of initializers does not match the number of variables being declared"

instance Postprocess (ConstSpec SourceRange) where
  -- TODO: we need to know the context (type) of the previous const specs here
  postprocess (ConstSpec a idents mrhs) =
    case mrhs of
      Nothing -> undefined
      Just (mtype, inits) -> undefined

instance Postprocess (FieldDecl SourceRange) where
  postprocess decl = undefined -- case decl of
    -- NamedFieldDecl _ ids _ _ -> sequence (NE.map (declareBinding Field) ids)
    --                          >> return ()
    -- _                        -> return ()

instance Postprocess (ImportSpec SourceRange) where
  postprocess i@(Import _ itype path) =
    lookupImport path >>=
    \case Nothing -> unexpected (i^.ann) $ "Could not find the source for the package \"" ++ show path ++ "\""
          Just prg@(Package _ _ _) ->
            do exports <- newScope $ postprocess prg >> getCurrentBindings
               let (package, global) = undefined --HM.foldlWithKey' makeImportBindings (emptyBindings, emptyBindings) exports
               undefined

instance Postprocess (Statement SourceRange) where
  postprocess s = case s of
    ShortVarDeclStmt a idents inits ->
      do mbks <- mapM lookupBindingIdM $ NE.filter nonBlankId idents
         let atLeastOneNew = any isNothing mbks
         unless atLeastOneNew $ unexpected s "Short variable declaration that doesn't declare new variables"
         unless (NE.length idents == NE.length inits) $ unexpected s "The number of initializers doesn't match the number of variable declared"
         inits' <- mapM postprocess inits
         idents' <- mapM (\(ident, init) ->
                            VarB <$> getType init >>= declareBindingIdM ident) $ NE.zip idents inits'
         return (ShortVarDeclStmt a idents' inits')
    _ -> descendBiM (postprocess :: Declaration SourceRange -> Parser (Declaration SourceRange)) s >>=
         descendBiM (postprocess :: Expression SourceRange -> Parser (Expression SourceRange)) >>=
         descendBiM (postprocess :: ExprClause SourceRange -> Parser (ExprClause SourceRange)) >>=
         descendBiM (postprocess :: TypeClause SourceRange -> Parser (TypeClause SourceRange)) >>=
         descendBiM (postprocess :: CommClause SourceRange -> Parser (CommClause SourceRange)) >>=
         descendM postprocess

nonBlankId i = case i of
  Id {} -> True
  BlankId _ -> False

instance Postprocess (Expression SourceRange) where
  postprocess e =
    let mpp = case e of
          FunctionLit a params returns body -> error "unsupported expression"
          CompositeLit a ty els -> CompositeLit a <$> postprocess ty <*> mapM postprocess els
          MethodExpr a recv ident -> MethodExpr a <$> postprocess recv <*> postprocess ident
          CallExpr a fne mty args mvariadic -> CallExpr a <$> postprocess fne <*> postprocess mty <*> mapM postprocess args <*> sequence (postprocess <$> mvariadic)
          Name a mqualifier ident -> Name a <$> postprocess mqualifier <*> postprocess ident
          -- ^ TODO Check if ident is bound to a var or const. Check
          -- that qualifier is bound to a package name and ident is
          -- bound to a var or const exported from it
          BinaryExpr a op left right -> BinaryExpr a op <$> postprocess left <*> postprocess right
          UnaryExpr a op e -> UnaryExpr a op <$> postprocess e
          Conversion a ty e -> Conversion a <$> postprocess ty <*> postprocess e
          FieldSelector a e fname -> FieldSelector a <$> postprocess e <*> postprocess fname
          IndexExpr a base index -> IndexExpr a <$> postprocess base <*> postprocess index
          SliceExpr a base me1 me2 me3 -> SliceExpr a <$> postprocess base <*> postprocess me1 <*> postprocess me2 <*> postprocess me3
          TypeAssertion a e ty -> TypeAssertion a <$> postprocess e <*> postprocess ty
    in  mpp >>= disambiguate

instance Postprocess a => Postprocess (Maybe a) where
  postprocess = sequence . fmap postprocess

instance Postprocess a => Postprocess [a] where
  postprocess = mapM postprocess

instance Postprocess (ExprClause SourceRange) where
  postprocess = error "Switch statements are not supported"

instance Postprocess (TypeClause SourceRange) where
  postprocess = error "Switch statements are not supported"

instance Postprocess (CommClause SourceRange) where
  postprocess = error "Select statements are not supported"

instance Postprocess (Type SourceRange) where
  postprocess = transformBiM (postprocess :: Id SourceRange -> Parser (Id SourceRange))

instance Postprocess (Id SourceRange) where
  postprocess i = case i of
    Id rng _ name -> Id rng <$> (getBinding name rng) <*> pure name
    BlankId _     -> return i

instance Postprocess (Receiver SourceRange) where
  postprocess = undefined

instance Postprocess (Element SourceRange) where
  postprocess = undefined

makeImportBindings :: (Bindings, Bindings) -> Text -> Binding -> (Bindings, Bindings)
makeImportBindings = undefined

-- exportBindingTransform :: (Text, Binding) -> (Text, Binding)
-- exportBindingTransform (n, bind) =
--                      case (itype, bind) of
--                        (ImportAll, _) -> (n, bind.bindingImported .~ True)
--                        -- Field names are never imported with qualifiers
--                        (ImportQualified mpiname, Binding {_bindingKind = FieldOrMethodB _}) -> (n, bind.bindingImported .~ True)
--                        (ImportQualified mpiname, _)->
--                         (ImportedQualifiedB (fromMaybe pname mpiname) n, kr)

unId :: Id a -> Text
unId (Id _ _ t) = t 
            
-- filterExported
--  An identifier may be exported to permit access to it from another package. An identifier is exported if both:


-- All other identifiers are not exported.
-- Each package has a package block containing all Go source text for that package.

-- | Declare a binding, checking whether it was already declared in
-- this scope, and also recording it in the identifire. If the check
-- fails, an appropriate error with a provided source range will be
-- thrown.
declareBindingIdM :: Id SourceRange -> BindingKind -> Parser (Id SourceRange)
declareBindingIdM (Id rng _ ident) bk =
  let bind = mkBinding ident rng bk in
  declareBindingM ident bind >> return (Id rng bind ident)
declareBindingIdM ident@(BlankId _)    _  = return ident


declareBindingM :: Text -> Binding -> Parser ()
declareBindingM name b =
  do mbind <- uses identifiers (HM.lookup name . fst . NE.uncons)
     case mbind of
       Just prev -> if prev^.bindingThisScope then
                      unexpected (b^.bindingDeclLoc) $ "Duplicate declaration for " ++
                      show name ++ ". Previous declaration at " ++
                      show (prev^.bindingDeclLoc)
                    else identifiers %= declareBinding name b
       Nothing -> identifiers %= declareBinding name b

-- | Look up the binding kind of an identifier. If the identifier is
-- not found, fail with an error.
getBindingKind :: Text -> SourceRange -> Parser BindingKind
getBindingKind name rng = view bindingKind <$> getBinding name rng

getBinding :: Text -> SourceRange -> Parser Binding
getBinding name rng =
  uses identifiers (lookupBinding name) >>=
  \case Just b -> return b
        Nothing -> unexpected rng $ "Unknown identifier " ++ show name

        

lookupBindingIdM :: Id a -> Parser (Maybe BindingKind)
lookupBindingIdM (Id _ _ name) = do b <- uses identifiers $ lookupBinding name
                                    return $ fmap (view bindingKind) b
lookupBindingIdM (BlankId _) = return Nothing
        
-- | Push a fresh binding context and evaluate the parser in it. Pops
-- the context afterwards.
newScope :: Parser a -> Parser a
newScope p = do identifiers %= pushScope
                rp <- p
                identifiers %= fromJust . popScope
                return rp

-- | remember the state of the top level scope, execute a parser, and
-- restore the remembered state
bracketScope :: Parser a -> Parser a
bracketScope p = do lexenv <- use identifiers
                    rp <- p
                    identifiers .= lexenv
                    return rp

-- | Returns the tip of bindings stack
getCurrentBindings :: Parser (HashMap Text Binding)
getCurrentBindings = uses identifiers NE.head

-- | Modify just the inner scope
modifyInnerScope :: (Scope -> Scope) -> Parser ()
modifyInnerScope f = do (top :| rest) <- use identifiers
                        identifiers .= (f top :| rest)

-- | Deletes all information about current bindings
-- clearBindings :: Parser ()
-- clearBindings = identifiers .= []

resolveImport :: Text -> (Text -> Parser (Package SourceRange)) -> Parser (Package SourceRange)
resolveImport path loader = uses modules (HM.lookup path) >>=
  \case (Just p) -> return p
        Nothing  -> do pgm <- loader path
                       lift $ zoom modules $ modify (HM.insert path pgm)
                       return pgm

lookupImport :: Text -> Parser (Maybe (Package SourceRange))
lookupImport path = liftM (HM.lookup path) $ use modules

-- Default module loading (mimicking the behaviour of the official
-- implementation):
-- 1) The package to be loaded is located in a directory $GOPATH/<importpath>/
-- 2) Parse all the go source files in the package directory
--    (e.g. with file names of the format *.go)
-- 3) Make sure these files have the same package name (throw an error otherwise)
-- 4) Parse each file in sequence and catenate them 
defaultPackageLoader :: FilePath -> Parser (Package SourceRange)
defaultPackageLoader path = ExceptT $ lift $ loader
  where loader :: IO (Either (SourceRange, String) (Package SourceRange))
        loader = lookupEnv "GOPATH" >>=
          \case Nothing     -> return $ Left (fakeRange, "GOPATH environment variable is not declared. Can't load modules without it.")
                Just gopath -> let modulePath = gopath </> path
                               in  liftM (bimap (fakeRange,) id) $ parsePackage modulePath
                 
-- | Parse a file using the default imported module loader.
parseFile :: FilePath -> IO (Either String (File SourceRange))
parseFile fp = do moduleText <- liftIO $ readFile fp
                  liftM (bimap (\err -> "Parse error at " ++ show (fst err) ++ ": " ++ snd err) id) $ parseText moduleText (defaultPackageLoader . unpack)

-- | Parse a module using the default module loader. The first
-- argument is the directory path that contains the module source
-- files. It is an absolute path, e.g. not relative to the 'GOPATH'
-- environment variable.
parsePackage :: FilePath -> IO (Either String (Package SourceRange))
parsePackage path = -- TODO: catch the exceptions from IO functions
                    -- and convert them to ExceptT error messages for
                    -- uniform interface
  listDirectory path >>= \files ->
  let mgoFiles = nonEmpty $ filter ((==) ".go" . takeExtensions) files
  in runExceptT $ case mgoFiles of
       Nothing -> throwError $ "Can't load the module in " ++ path ++ " as it does not have any Go source files."
       Just goFiles -> do (file :| rest) <- mapM (ExceptT . parseFile) goFiles
                          let (File _ _ pname _ _) = file
                          return $ Package fakeRange pname (file :| rest)
