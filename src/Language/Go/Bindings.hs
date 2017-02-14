{-# LANGUAGE OverloadedStrings, ExistentialQuantification, RankNTypes, FlexibleContexts #-}

-- | Identifier bindings and operations on them
module Language.Go.Bindings where

import Language.Go.Types
import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Strict as HM
import AlexTools hiding (range)
import Lens.Simple hiding ((&))
import Control.Arrow
import Language.Go.Parser.Util
import Language.Go.AST (Id(..), Type(..), TypeName(..))
import Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Default.Class
import Control.Applicative
import Data.Graph.Inductive hiding (mkNode)
import Data.Graph.Inductive.Query.DFS
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data
import Data.List (nub)
import Control.Monad.State
import Language.Go.Bindings.Types
import Data.Tree
import Data.Graph.Inductive.Basic
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe (fromJust)

defaultBindings = predeclaredBindings :| []

declareBindingId :: Id SourceRange -> BindingKind -> Bindings -> Bindings
declareBindingId (BlankId _) _ = id
declareBindingId id@(Id a ident) bk = declareBinding ident id bk

declareBinding :: Ranged r => Text -> r -> BindingKind -> Bindings -> Bindings
declareBinding ident r bk (b :| bs) = HM.insert ident (Binding (range r) bk False True) b :| bs

lookupBinding :: Text -> Bindings -> Maybe BindingKind
lookupBinding id (b :| _) = _bindingKind <$> HM.lookup id b

pushScope :: Bindings -> Bindings
pushScope (b :| rest) = (HM.map notThisContext b) :| b:rest
  where notThisContext bind = bind {_bindingThisScope = False}

popScope :: Bindings -> Maybe Bindings
popScope = snd . NE.uncons
                         
predeclaredBindings :: HashMap Text Binding
predeclaredBindings = HM.fromList $ map (second $ \k -> Binding fakeRange k True True) $  [("bool", TypeB Boolean)
  ,("uint8", TypeB $ Int (Just 8) False)
  ,("uint16", TypeB $ Int (Just 16) False)
  ,("uint32", TypeB $ Int (Just 32) False)
  ,("uint64", TypeB $ Int (Just 64) False)
  ,("int8", TypeB $ Int (Just 8) True)
  ,("int16", TypeB $ Int (Just 16) True)
  ,("int32", TypeB $ Int (Just 32) True)
  ,("int64", TypeB $ Int (Just 64) True)
  ,("float32", TypeB $ Float 32)
  ,("float64", TypeB $ Float 64)
  ,("complex64", TypeB $ Complex 64)
  ,("complex128", TypeB $ Complex 128)
  ,("byte", TypeB $ Int (Just 8) False)
  ,("rune", TypeB $ Int (Just 32) True)
  ,("uint", TypeB $ Int (Just 32) False)
  ,("int", TypeB $ Int Nothing True)
  ,("uintptr", TypeB $ Int (Just 64) False)
  ,("string", TypeB String)
  ,("iota", ConstB Iota)
  ,("nil", ConstB Nil)
  ,("true", ConstB Boolean)
  ,("false", ConstB Boolean)
  ]
  ++ map (id &&& (ConstB . BuiltIn))
  ["append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make",
   "new", "panic", "print", "println", "real", "recover"
  ]

type TBGBuilder a r = State (TypeBindingGraph a) r

tbgBuild :: TBGBuilder a r -> TypeBindingGraph a
tbgBuild = (`execState` def)

--          * initialize it with mu-of the syntactic type,
--          * rewrite the value by successively inlining all the type
--            name references that occur in the SCC, save for the
--            reference for this root node which is replaced with
--            MuRef (maybe rename it as Tie?). The type names to
--            rewrite are either in the SCC or in the current output
resolveTypes :: (Data a, Default a, MonadError String m) => HashMap Text (Type a) -> Scope -> m (HashMap Text ValueType)
resolveTypes tyDecls outerScope =
  -- 1. Construct a type binding graph
  let tbg  = constructTypeBindingGraph tyDecls
      gr   = tbg^.tbGraph
  -- 2. Compute the condensation of the graph (CG)
      cgr  = condensation gr
  -- 3. Traverse the CG in postorder
      postorderSCCs = postorderF $ dffWith' lab' cgr
      resolveSCC :: (MonadError String m, MonadWriter (HashMap Text ValueType) m)
                 => [Node] -> m ()
      resolveSCC scc = case scc of
                         []  -> return ()
                         [n] -> resolveNode n
                         _   -> mapM_ resolveRecursiveNode scc
      resolveNode :: (MonadError String m, MonadWriter (HashMap Text ValueType) m)
                  => Node -> m ()
      resolveNode n = case lab gr n of
        Just ((NamedType _ (TypeName _ Nothing (Id _ tn))), True, resolved) ->
          if resolved then
            liftM (HM.singleton tn) (resolveType $ fromJust $ HM.lookup tn tyDecls) >>= tell
          else undefined
      resolveRecursiveNode :: (MonadError String m, MonadWriter (HashMap Text ValueType) m)
                  => Node -> m ()
      resolveRecursiveNode n =
        case lab gr n of
          Just ((NamedType _ (TypeName _ Nothing (Id _ tn))), True, resolved) ->
            -- for each root node in the SCC:
            if resolved then
              -- add a new entry in the output map,
              liftM (HM.singleton tn) (mkResolvedType n) >>= tell
            else return ()
          _                         ->
            throwError $ "Bindings.resolveTypes internal error: can't find TBG node " ++ (show n)
      mkResolvedType :: (MonadError String m, MonadWriter (HashMap Text ValueType) m) => Node -> m ValueType
      mkResolvedType = undefined
      resolveType :: (MonadError String m, MonadWriter (HashMap Text ValueType) m) => Type a -> m ValueType
      resolveType = undefined
  --  For each CG node (SCC):
  in  execWriterT $ mapM_ resolveSCC postorderSCCs

constructTypeBindingGraph :: (Data a, Default a) => HashMap Text (Type a) -> TypeBindingGraph a
constructTypeBindingGraph = tbgBuild . mapM_ (uncurry addTypeBinding) . HM.toList

addTypeBinding :: (Data a, Default a) => Text -> Type a -> TBGBuilder a ()
addTypeBinding tyName ty =
  let tyInsLink from =
        do newTyNode <- mkNode ty False False
           -- link the type declaration node with the type node
           tbGraph %= insEdge (from, newTyNode, False)
           -- mark the type name as resolved
           markResolved from
           -- link the type node with the type names referenced from it
           mapM_ (linkToTypeName newTyNode) $ nub $ referencedTypeNames ty
  in  do roots <- use tbgRoots
         case HM.lookup tyName roots of
           Just n  -> tyInsLink n
           Nothing -> do newBNode <- mkRootNode tyName
                         tyInsLink newBNode

-- | Produces a list of typenames referenced from a type. If a
-- typename is referenced via a Pointer, then it is paired with a
-- True. False otherwise. Invariant: we expect all imported
-- (qualified) type names to be substituted away.
referencedTypeNames :: (Data a, Typeable a) => Type a -> [(Text, Bool)]
referencedTypeNames t = case t of
  NamedType _ (TypeName _ _ (Id _ tn)) -> [(tn, False)]
  PointerType _ (NamedType _ (TypeName _ _ (Id _ tn))) -> [(tn, True)]
    --- TODO: children won't go into FieldDecl or parameterList/returnList or MethodSpec. Need something with a biplate flavor. Maybe descendBi?
  _ -> concatMap referencedTypeNames $ children t

-- | Create a node for the type name, if it doesn't exist, and make an
-- edge from a given node. Label the edge with the boolean associated
-- with the type name
linkToTypeName :: Default a => Node -> (Text, Bool) -> TBGBuilder a ()
linkToTypeName from (tyn, pointer) =
  do roots <- use tbgRoots 
     to <- case HM.lookup tyn roots of
             -- the node exists, so just use that
             Just n -> return n
             -- otherwise create a new node
             Nothing -> mkRootNode tyn
     -- insert a new labeled edge
     tbGraph %= insEdge (from, to, pointer)

-- | Creates a new node with the label, returns the node id and the modified TBG
mkNode :: Type a -> Bool -> Bool -> TBGBuilder a Node
mkNode ty root resolved = do newNodeId <- uses tbGraph (head . newNodes 1)
                             tbGraph %= insNode (newNodeId, (ty, root, resolved))
                             return newNodeId

mkRootNode :: Default a => Text -> TBGBuilder a Node
mkRootNode tyName = do nd <- mkNode (NamedType def $ TypeName def Nothing (Id def tyName)) True False
                       tbgRoots %= HM.insert tyName nd
                       return nd

markResolved :: Node -> TBGBuilder a ()
markResolved n = do (mctx, rest) <-uses tbGraph (match n)
                    case mctx of
                      Just (to, nd , (ty, root, _), from) ->
                        tbGraph .= (to, nd , (ty, root, True), from) & rest
                      Nothing -> return ()
