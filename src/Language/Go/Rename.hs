{-|
Module      : Language.Go.Rename
Description : Golang renamer
Maintainer  : abagnall@galois.com
Stability   : experimental

The renamer fills in missing qualifiers for global identifiers so that
every global becomes fully qualified.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Go.Rename (rename) where

import           Control.Monad (forM_)
import           Control.Monad.Identity (Identity(..))
import           Control.Monad.State (StateT, evalStateT, gets, modify)

import           Data.Default.Class
import           Data.Functor.Product
import           Data.List (nub)
import           Data.Text (Text)

import           Data.Parameterized.TraversableFC

import           Language.Go.AST
import           Language.Go.Rec
import           Language.Go.Types

-- | Entry point.
rename :: Show a => Node a tp -> Node a tp
rename = runRM def . para rename_alg

data RState =
  RState { rs_package :: Text -- ^ Name of current package
         , rs_locals :: [Text] -- ^ Locals in scope
         }

instance Default RState where
  def = RState { rs_package = "INITIAL_RENAMER_PACKAGE_NAME"
               , rs_locals = [] }

-- We could use ReaderT instead and scan blocks for assign statements
-- before entering them. Either way seems reasonable.
type RM' a = StateT RState Identity a

newtype RM a (tp :: NodeType) =
  RM { unRM :: RM' (Node a tp) }

runRM :: RState -> RM a tp -> Node a tp
runRM st = runIdentity . (`evalStateT` st) . unRM

-- | Helper for running subterm renamer actions.
run :: Product (Node a) (RM a) tp -> RM' (Node a tp)
run (Pair _node (RM m)) = m


-- | The renamer algebra.
rename_alg :: Show a => NodeF a (Product (Node a) (RM a)) tp -> RM a tp

rename_alg (MainNode nm pkg imports) = RM $ do
  imports' <- mapM run imports
  modify $ \rs -> rs { rs_package = nm }
  pkg' <- run pkg
  return $ In $ MainNode nm pkg' imports'

-- | Store package name in reader state.
rename_alg (PackageNode name path imports file_paths files inits) = RM $ do
  modify $ \rs -> rs { rs_package = name }
  files' <- mapM run files
  inits' <- mapM run inits
  return $ In $ PackageNode name path imports file_paths files' inits'

-- | So far this is the only place where we need access to original
-- nodes so we can register the LHS variables as locals before
-- renaming them (as they would be considered to be globals otherwise
-- and have qualifiers erroneously filled in).
rename_alg (AssignStmt x assign_type op lhs rhs) = RM $ do
  case assign_type of
    -- Only add variables when the assignment is a "short declaration"
    -- statement (':=' syntax). If a variable already exists as a
    -- global, it will be shadowed by a new local.
    Define -> do
      forM_ lhs $ \(Pair node _rm) -> case node of
        In (IdentExpr _x _tp _q (Ident _k name)) -> addLocal name
        _node -> error "rename_alg AssignStmt: expected ident in LHS"
    _ -> return ()
  In <$> (AssignStmt x assign_type op <$> mapM run lhs <*> mapM run rhs)

rename_alg (DeclStmt x decl) = RM $ do
  decl' <- run decl
  case decl' of
    In (GenDecl _x specs) -> forM_ specs $ \spec -> case spec of
      In (ConstSpec _y names _ty _values) -> forM_ names $ addLocal . identName
      In (VarSpec _y names _ty _values) -> forM_ names $ addLocal . identName
      _spec -> return ()
    _decl -> return ()
  return $ In $ DeclStmt x decl'

-- | Save and restore locals.
rename_alg (BlockNode stmts) = RM $ local $ In . BlockNode <$> mapM run stmts

-- | Also need to save and restore the locals around a for loop, even
-- though the body is already a block, because the initialization
-- statement can bind new locals.
rename_alg (ForStmt x ini cond post body) = RM $
  local $ In <$> (ForStmt x <$> mapM run ini <*> mapM run cond <*>
                   mapM run post <*> run body)

-- | Same thing for if statements.
rename_alg (IfStmt x ini cond body els) = RM $
  local $ In <$> (IfStmt x <$> mapM run ini <*> run cond <*>
                   run body <*> mapM run els)

-- | Same thing for switch statements.
rename_alg (SwitchStmt x ini tag body) = RM $
  local $ In <$> (SwitchStmt x <$> mapM run ini <*> mapM run tag <*> run body)

-- | Same thing for typeswitch statements.
rename_alg (TypeSwitchStmt x ini stmt body) = RM $
  local $ In <$> (TypeSwitchStmt x <$> mapM run ini <*> run stmt <*> run body)

-- | Add bindings for params and named returns.
rename_alg (FuncDecl x recv name params variadic results body) = RM $
  local $ do
    params' <- mapM run params
    results' <- mapM run results
    forM_ (field_names $ params' ++ results') addLocal
    recv' <- mapM run recv
    variadic' <- mapM run variadic -- Should be Nothing if we've run
                                   -- the desugarer
    body' <- mapM run body
    return $ In $ FuncDecl x recv' name params' variadic' results' body'

-- | Add bindings for params and named returns
rename_alg (FuncLitExpr x tp params results body) = RM $
  local $ do
    params' <- mapM run params
    results' <- mapM run results
    forM_ (field_names $ params' ++ results') addLocal
    body' <- run body
    return $ In $ FuncLitExpr x tp params' results' body'

-- When an unqualified identifier isn't in the local context, set its
-- qualifier to the current package.
rename_alg (IdentExpr x tp Nothing ident@(Ident k name)) = RM $ do
  locals <- gets rs_locals
  if name `elem` locals then return $ In $ IdentExpr x tp Nothing ident
    else do
    pkgName <- gets rs_package
    return $ In $ IdentExpr x tp (Just (Ident IdentPkgName pkgName)) $
      Ident k name

-- Do nothing for all other nodes.
rename_alg node = RM $ In <$> traverseFC run node


addLocal :: Text -> RM' ()
addLocal nm = modify $ \rs -> rs { rs_locals = nub $ nm : rs_locals rs }

-- | Save and restore the locals around a renamer computation.
local :: RM' a -> RM' a
local m = do
  locals <- gets rs_locals
  m' <- m
  modify $ \rs -> rs { rs_locals = locals }
  return m'

field_names :: [Node a Field] -> [Text]
field_names [] = []
field_names (In (FieldNode nms _ty _tag) : fields) =
  (identName <$> nms) ++ field_names fields
