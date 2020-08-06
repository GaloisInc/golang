{-|
Module      : Lang.Crucible.Go.Desugar
Description : Go syntax desugarer
Maintainer  : abagnall@galois.com
Stability   : experimental

"Desugaring" phase of the go frontend. Applies the following syntactic
transformations:
* replace nil idents with NilExprs and type idents with NamedTypeExprs.
* introduce/eliminate tuples where "multi-values" appear.
* convert variadic arguments to slice literals.
* insert missing return statements and fill in "naked returns".
* replace increment and decrement statements with assign statements.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Go.Desugar (desugar) where

import           Control.Monad.Except
import           Control.Monad.Identity

import           Language.Go.AST
import           Language.Go.Rec
import           Language.Go.Types

-- | Entry point of the module.
desugar :: Show a => Node a tp -> Node a tp
desugar node = case runDesugarM $ cataM desugar_alg node of
  Left msg -> error msg
  Right node' -> node'

type DesugarM a = ExceptT String Identity a

runDesugarM :: DesugarM a -> Either String a
runDesugarM = runIdentity . runExceptT

-- | Desugar Go programs so they are more directly translatable to Crucible.
desugar_alg :: Show a => NodeF a (Node a) tp -> DesugarM (Node a tp)

-- | Convert nil and type identifiers to their own different syntactic
-- forms so all remaining identifiers are term-level identifiers
-- (variables, functions, etc.).
desugar_alg (IdentExpr x tp qual (Ident kind name)) =
  return $ In $ case kind of
  IdentNil -> NilExpr x NoType
  IdentTypeName -> NamedTypeExpr x tp (Ident kind name)
  -- TODO: IOTA?
  _ -> IdentExpr x tp qual (Ident kind name)

-- | Generate projections for the RHS when it's a tuple.
desugar_alg (AssignStmt x assign_tp op lhs rhs) =
  case (assign_tp, op, lhs, rhs) of
    (AssignOperator, Just binop, [l], [r]) ->
      return $ In $ AssignStmt x Assign Nothing [l]
      [In $ BinaryExpr x (typeOf' l) l binop r]
    (_tp, _op, _l, _r) ->
      return $ In $ AssignStmt x assign_tp op lhs $ unpack_tuple rhs

desugar_alg (ConstSpec x names ty values) =
  return $ In $ ConstSpec x names ty $ unpack_tuple values
desugar_alg (VarSpec x names ty values) =
  return $ In $ VarSpec x names ty $ unpack_tuple values
desugar_alg (InitializerStmt vars values) =
  return $ In $ InitializerStmt vars $ unpack_tuple values

-- | Pack results into a tuple, unless there is exactly one return value.
desugar_alg (ReturnStmt x results) =
  return $ In $ ReturnStmt x $
  if length results == 1 then results else
    [In $
     TupleExpr x (TupleType $ typeToNameType . typeOf' <$> results) results]

-- | Generate projections for arguments in the case of a single tuple
-- argument, and deal with variadic arguments.
desugar_alg (CallExpr x tp fun args) =
  case typeOf' fun of
    FuncType _recv params _result variadic ->
      if variadic then
        let regular_params = init params
            args' = unpack_tuple args
            n = length regular_params
            regular_args = take n args'
            variadic_arg = case drop n args' of
              -- It could be an actual slice value with the ellipsis syntax.
              [In (EllipsisExpr _ _ (Just e))] -> e
              -- Otherwise pack all of the variadic arguments into a
              -- slice literal. Use Nothing for the syntactic type
              -- field since we don't need it. Use 'last params'
              -- (always a slice type) for the semantic type.
              args'' -> In $ CompositeLitExpr x (last params) Nothing args''
        in
          return $ In $ CallExpr x tp fun $ regular_args ++ [variadic_arg]
      else
        return $ In $ CallExpr x tp fun $ unpack_tuple args
    _ -> throwError $ "desugar_alg: expected FuncType, got " ++ show (typeOf' fun)

-- | Insert missing return statements and convert variadic to slice.
desugar_alg (FuncDecl x recv name params variadic results
             (Just (In (BlockNode body)))) = do
  let params' = params ++ case variadic of
        Nothing -> []
        Just (In (FieldNode names tp tag)) ->
          [In $ FieldNode names (In $ ArrayTypeExpr x
                                 (SliceType $ typeOf' tp) Nothing tp) tag]
  return $ In $ FuncDecl x recv name params' Nothing results $ Just $ In $
    BlockNode $ insert_returns x (mkTuple x $ field_names x results) body

-- | Desugar increment and decrement statements to assign statements.
desugar_alg (IncDecStmt x expr is_incr) = do
  return $ In $ AssignStmt x AssignOperator
    (Just $ if is_incr then BPlus else BMinus) [expr] $
    [In $ BasicLitExpr x (BasicType $ BasicInt Nothing) $
     BasicLit { basiclit_type = LiteralInt
              , basiclit_value = "1" }]

-- | Do nothing for all other nodes.
desugar_alg n = return $ In n

-- | When the input list consists of a single expression of tuple
-- type, generate a list of its projected elements. This is used to
-- allow a tuple value to appear in a context that expects multiple
-- values.
unpack_tuple :: [Node a Expr] -> [Node a Expr]
unpack_tuple [arg] = case typeOf' arg of
  -- Elements of 1-tuple type shall be unchanged.
  TupleType [_t] -> [arg]
  TupleType ts ->
    (\(i, t) -> In $ ProjExpr (annotOf' arg) (typeOfNameType t) arg i)
    <$> zip [0..] ts
  _t -> [arg]
unpack_tuple args = args

-- | Given a default return expression, ensure that a list of
-- statements terminates with a return statement.
insert_returns :: a -> Node a Expr -> [Node a Stmt] -> [Node a Stmt]
-- Replace empty body with a return.
insert_returns x e [] = [In $ ReturnStmt x [e]]
-- Fill in empty return. There are two possibilities:
-- 1) our function has no returns so es = [] and this has no effect.
-- 2) our function has named returns and this is "naked return", so we
-- fill in the return identifiers.
insert_returns _x e [In (ReturnStmt y [])] = [In $ ReturnStmt y [e]]
-- If there's already a nonempty return statement, leave it alone.
insert_returns _x _e stmts@[In (ReturnStmt _ _)] = stmts
-- If the last statement is not a return, insert one after.
insert_returns x e [stmt] = [stmt, In $ ReturnStmt x [e]]
-- Recurse to get to the last statement.
insert_returns x e (stmt:stmts) = stmt : insert_returns x e stmts

field_names :: a -> [Node a Field] -> [Node a Expr]
field_names x = map $ \(In (FieldNode [nm] tp _)) ->
  In $ IdentExpr x (typeOf' tp) Nothing nm

mkTuple :: a -> [Node a Expr] -> Node a Expr
mkTuple x es = In $ TupleExpr x (TupleType $ typeToNameType . typeOf' <$> es) es