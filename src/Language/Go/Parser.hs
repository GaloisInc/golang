{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances, TupleSections #-}
module Language.Go.Parser (parsePackage
                          ,parseFile
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
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad (liftM)
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
import Data.List (foldl')
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
          -> IO (Either (SourceRange, String) (Package SourceRange))
parseText txt loader = runParser $ parse txt loader

parse :: Text -- ^ Text of the program/module
      -> (Text -> Parser (Package SourceRange))
      -- ^ A resolver/loader function for imported modules 
      -> Parser (Package SourceRange)
parse txt loader = (liftHappy $ file $ lexer $ initialInput txt)
               >>= (\f@(File rng _ pname _ _) -> return $ Package rng pname (f :| []))
               >>= transformBiM loadImport
               >>= postprocess
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
postprocess :: Package SourceRange -> Parser (Package SourceRange)
postprocess = annotateBindings >=> disambiguate

--   Based on the semantics of identifiers (in annotations) rewrite expressions:
--      * FieldSelector with the object part that is a Name. Check if
--        that Name is a package name. If so, this expression it can
--        be a QualifiedName.
--      * CallExpr with either:
--        - the length of the arguments list is 1. Check if the
--          function part is a name which is a named type, or a field
--          selector which is a a QualifiedTypeName. If so, this is a
--          type conversion.
--        - the first argument is a Name or a field selector. If so,
--          that argument can be a named type instead
disambiguate :: Package SourceRange -> Parser (Package SourceRange)
disambiguate = transformBiM disambiguateExpression
  where disambiguateExpression :: Expression SourceRange
                               -> Parser (Expression SourceRange)
        disambiguateExpression e = case e of
          FieldSelector a (Name na ident) fident@(Id _ _ fname) ->
            case ident of
              Id _ bind pname -> case bind^.bindingKind of
                PackageB _ pexports ->
                  if HM.member fname pexports then return $ Qualified a ident fident
                  else unexpected fident $ "Identifier " ++ (T.unpack fname) ++ " not exported from package " ++ (T.unpack pname)
                _                   -> return e
              BlankId {}  -> unexpected ident "Reading from a blank identifier"
          _ -> undefined

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

class HasBindings a where
  annotateBindings :: a -> Parser a
  -- ^ Collect the bindings from `a` in the current scope and modify
  -- binding annotations on identifiers to reflect the kind of the
  -- binding and the relevant type.

-- | This instance will record only the bindings exported from the
-- package. 
instance HasBindings (Package SourceRange) where
  annotateBindings (Package a pname files) =
    do -- 1. Initialize scope with the predefined type/var bindings
       identifiers .= defaultBindings
       -- 2. with a new scope, for each file
       newScope $ do 
         -- 3. Gather all the top level bindings in the current package
         imports <- liftM (HM.fromList) $ mapM (\f -> collectTopLevelBindings f >>= return . (fileName f,)) $ NE.toList files
         -- 4. Traverse each file, recording and annotating bindings
         (Package a pname) <$> mapM (annotateFileBindings imports) files

annotateFileBindings :: HashMap Text (HashMap Text Binding) -> File SourceRange -> Parser (File SourceRange)
annotateFileBindings imports f = bracketScope $
  modifyInnerScope (`HM.union` (HM.lookupDefault (HM.empty) (fileName f) imports))
  >> annotateBindings f

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
     mapM annotateBindings imports
     -- remembering the mapping separately
     newTopBinds <- getCurrentBindings
     let importbinds = HM.difference newTopBinds oldTopBinds
     -- 2. Scan and add top level bindings
     mapM annotateBindings tops
     -- 3. Remove bindings for package names (difference between the
     -- state and the remembered mapping)
     modifyInnerScope (`HM.difference` importbinds)
     return importbinds

instance HasBindings (File SourceRange) where
  annotateBindings (File a fname pname imports topLevels) =
    (File a fname pname imports) <$> mapM annotateFunctions topLevels
    where annotateFunctions :: TopLevel SourceRange
                            -> Parser (TopLevel SourceRange)
          annotateFunctions tl = case tl of
            FunctionDecl a name params returns mbody ->
              fmap (FunctionDecl a name params returns) $ newScope $
              do annotateBindings params
                 annotateBindings returns
                 sequence $ fmap (mapM annotateBindings) mbody
            MethodDecl a recv name params returns mbody ->
              fmap (MethodDecl a recv name params returns) $ newScope $
              do annotateBindings params
                 annotateBindings returns
                 sequence $ fmap (mapM annotateBindings) mbody
            -- we have already analysed and recorded bindings from
            -- declarations in `collectTopLevelBindings` and there are
            -- no nested binding scopes in them, so we skip top
            -- declarations.
            TopDecl {} -> return tl

instance HasBindings (ParameterList SourceRange) where
  annotateBindings pl = case pl of
    NamedParameterList a nps mrestp -> NamedParameterList a <$> mapM annotateBindings nps <*> (sequence $ annotateBindings <$> mrestp)
    AnonymousParameterList {} -> return pl

instance HasBindings (NamedParameter SourceRange) where
  annotateBindings np@(NamedParameter a ident ty) =
    liftM (\i -> NamedParameter a i ty) $ declareBindingIdM ident (VarB noType) 

instance HasBindings (ReturnList SourceRange) where
  annotateBindings rl = case rl of
    NamedReturnList a nps  -> NamedReturnList a <$> mapM annotateBindings nps
    AnonymousReturnList {} -> return rl

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
  -- do mapM annotateBindings decls
  --    getCurrentBindings

instance HasBindings (ImportDecl SourceRange) where
  annotateBindings (ImportDecl a ispecs) = ImportDecl a <$> mapM annotateBindings ispecs

instance HasBindings (TopLevel SourceRange) where
  annotateBindings tl = case tl of
    FunctionDecl a fname params returns body ->
      do bk <- VarB <$> (Function Nothing <$> getParamTypes params <*> getSpreadType params <*> getReturnTypes returns)
         fname' <- declareBindingIdM fname bk
         return (FunctionDecl a fname' params returns body)
    MethodDecl  a recv mname params returns body ->
      do bk <- VarB <$> (Function <$> (Just <$> getType recv) <*> getParamTypes params <*> getSpreadType params <*> getReturnTypes returns)
         mname' <- declareBindingIdM mname bk
         return (MethodDecl a recv mname' params returns body)
    TopDecl a decl -> TopDecl a <$> annotateBindings decl

instance HasBindings (Declaration SourceRange) where
  annotateBindings decl = case decl of
    TypeDecl a tspecs -> TypeDecl a <$> mapM annotateBindings tspecs
    VarDecl a vspecs -> VarDecl a <$> mapM annotateBindings vspecs
    ConstDecl a cspecs -> ConstDecl a <$> mapM annotateBindings cspecs

instance HasBindings (TypeSpec SourceRange) where
  annotateBindings (TypeSpec a tid typ) = TypeSpec a <$>
   (TypeB <$> getType typ >>= declareBindingIdM tid) <*> return typ

instance HasBindings (VarSpec SourceRange) where
  annotateBindings vs = case vs of
    TypedVarSpec a idents ty inits ->
      do idents' <- mapM (\ident -> (VarB <$> getType ty) >>= declareBindingIdM ident) idents
         return $ TypedVarSpec a idents' ty inits
         -- ^ TODO check that the inferred types of inits are compatible with ty
    UntypedVarSpec a idents inits ->
      if NE.length idents == NE.length inits then
        do inits' <- mapM annotateBindings inits
           idents' <- mapM (\(ident, init) ->
                              VarB <$> getType init >>= declareBindingIdM ident) $ NE.zip idents inits'
           return (UntypedVarSpec a idents' inits')
      else unexpected a "The number of initializers does not match the number of variables being declared"

instance HasBindings (ConstSpec SourceRange) where
  annotateBindings (ConstSpec a idents mrhs) =
    case mrhs of
      Nothing -> undefined
      Just (mtype, inits) -> undefined

instance HasBindings (FieldDecl SourceRange) where
  annotateBindings decl = undefined -- case decl of
    -- NamedFieldDecl _ ids _ _ -> sequence (NE.map (declareBinding Field) ids)
    --                          >> return ()
    -- _                        -> return ()

instance HasBindings (ImportSpec SourceRange) where
  annotateBindings i@(Import _ itype path) =
    lookupImport path >>=
    \case Nothing -> unexpected (i^.ann) $ "Could not find the source for the package \"" ++ show path ++ "\""
          Just prg@(Package _ _ _) ->
            do exports <- newScope $ annotateBindings prg >> getCurrentBindings
               let (package, global) = undefined --HM.foldlWithKey' makeImportBindings (emptyBindings, emptyBindings) exports
               undefined

instance HasBindings (Statement SourceRange) where
  annotateBindings = undefined

instance HasBindings (Expression SourceRange) where
  annotateBindings = undefined

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
getBinding :: Text -> SourceRange -> Parser BindingKind
getBinding name rng =
  uses identifiers (lookupBinding name) >>=
  \case Just bk -> return bk
        Nothing -> unexpected rng $ "Unknown identifier " ++ show name
        
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
parseFile :: FilePath -> IO (Either String (Package SourceRange))
parseFile fp = do moduleText <- liftIO $ readFile fp
                  liftM (bimap (\err -> "Parse error at " ++ show (fst err) ++ ": " ++ snd err) id) $ parseText moduleText (defaultPackageLoader . unpack)

-- | Parse a module using the default module loader. The first
-- argument is the directory path that contains the module source
-- files. It is an absolute path, e.g. not relative to the 'GOPATH'
-- environment variable.
parsePackage :: FilePath -> IO (Either String (Package SourceRange))
parsePackage path = -- TODO: catch the exceptions from IO functions and
              -- convert them to ExceptT error messages for uniform
              -- interface
  listDirectory path >>= \files ->
  let mgoFiles = nonEmpty $ filter ((==) ".go" . takeExtensions) files
  in runExceptT $ case mgoFiles of
       Nothing -> throwError $ "Can't load the module in " ++ path ++ " as it does not have any Go source files."
       Just goFiles -> undefined -- liftM sconcat $ sequence $ NE.map (ExceptT . parseFile) goFiles
