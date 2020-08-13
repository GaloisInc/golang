{-|
Module      : Language.Go.AST
Description : Go language abstract syntax
Maintainer  : abagnall@galois.com
Stability   : experimental

Go syntax in open recursion style, similar to the 'App' type of
crucible expressions.

The constructors are designed to closely match the standard 'go/ast'
type definitions, with some adjustments to account for liberties taken
by goblin as well as additional "internal language" forms such as
tuples.

We also include semantic type information in expression nodes, and
have a BasicConstExpr form for representing the results of evaluated
constant expressions (most literals end up as BasicConstExprs).
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Go.AST where

import           Data.Text hiding (inits)

import           Data.Parameterized.TraversableFC

import           Language.Go.Rec
import           Language.Go.Types

-- TODO: make fields strict?

-- | The type of AST nodes with annotations of type 'a' is the least
-- fixed point of 'NodeF a'.
type Node a = Fix (NodeF a)

-- | Go AST node functor indexed by NodeType.
data NodeF (a :: *) (f :: NodeType -> *) (i :: NodeType) where

  -- | The main package.
  MainNode :: Text -- ^ main package name
           -> f Package -- ^ the main package itself
           -> [f Package] -- ^ flat list containing the transitive
                          -- closure of dependencies, ordered such
                          -- that if package A depends on B, then B
                          -- appears before A in the list (guaranteed
                          -- by goblin).
           -> NodeF a f Main

  -- | A package contains one or more files.
  PackageNode :: Text -- ^ package name
              -> Text -- ^ package path
              -> [Text] -- ^ paths of imports
              -> [Text] -- ^ source file absolute paths
              -> [f File] -- ^ source files
              -> [f Stmt] -- ^ initializers in execution order
              -> NodeF a f Package

  -- | A Go source file.
  FileNode :: Text -- ^ file path
           -> Ident -- ^ package name
           -> [f Decl] -- ^ top-level declarations
           -> [f Decl] -- ^ imports in this file
           -> NodeF a f File

  ----------------------------------------------------------------------
  -- Statements

  -- | An assignment or a short variable declaration.
  AssignStmt :: a
             -> AssignType
             -> Maybe BinaryOp
             -> [f Expr] -- ^ lhs
             -> [f Expr] -- ^ rhs
             -> NodeF a f Stmt

  BlockStmt :: a
            -> f Block
            -> NodeF a f Stmt

  -- | A break, continue, goto, or fallthrough statement.
  BranchStmt :: a
             -> BranchType
             -> Maybe Ident -- ^ label
             -> NodeF a f Stmt

  -- | A declaration in a statement list.
  DeclStmt :: a
           -> f Decl
           -> NodeF a f Stmt

  DeferStmt :: a
            -> f Expr -- ^ call expression
            -> NodeF a f Stmt

  EmptyStmt :: a -> NodeF a f Stmt

  -- | A (stand-alone) expression in a statement list.
  ExprStmt :: a
           -> f Expr
           -> NodeF a f Stmt

  ForStmt :: a
          -> Maybe (f Stmt) -- ^ initialization statement; or nil
          -> Maybe (f Expr) -- ^ condition; or nil
          -> Maybe (f Stmt) -- ^ post iteration statement; or nil
          -> f Block -- ^ loop body
          -> NodeF a f Stmt

  GoStmt :: a
         -> f Expr -- ^ call expression
         -> NodeF a f Stmt

  IfStmt :: a
         -> Maybe (f Stmt) -- ^ initialization statement; or nil
         -> f Expr -- ^ condition
         -> f Block -- ^ body
         -> Maybe (f Stmt) -- ^ else branch; or nil
         -> NodeF a f Stmt

  -- | An increment or decrement statement.
  IncDecStmt :: a
             -> f Expr -- ^ inner expression
             -> Bool -- ^ true if increment, false if decrement
             -> NodeF a f Stmt

  LabeledStmt :: a
              -> Ident -- ^ label
              -> f Stmt
              -> NodeF a f Stmt

  -- | A for statement with a range clause.
  RangeStmt :: a
            -> Maybe (f Expr) -- ^ key
            -> Maybe (f Expr) -- ^ value
            -> f Expr -- ^ value to range over
            -> f Block -- ^ body
            -> Bool -- ^ is assign?
            -> NodeF a f Stmt

  ReturnStmt :: a
             -> [f Expr] -- ^ result expressions
             -> NodeF a f Stmt

  SelectStmt :: a
             -> f Block -- ^ body (CommClauseStmts only)
             -> NodeF a f Stmt

  SendStmt :: a
           -> f Expr -- ^ channel
           -> f Expr -- ^ value
           -> NodeF a f Stmt

  -- | An expression switch statement.
  SwitchStmt :: a
             -> Maybe (f Stmt) -- ^ initialization statement; or nil
             -> Maybe (f Expr) -- ^ tag expression; or nil
             -> f Block -- ^ body (CaseClauseStmts only)
             -> NodeF a f Stmt

  TypeSwitchStmt :: a
                 -> Maybe (f Stmt) -- ^ initialization statement; or nil
                 -> f Stmt -- ^ x := y.(type) or y.(type)
                 -> f Block -- ^ body (CaseClauseStmts only)
                 -> NodeF a f Stmt

  -- | A case of an expression or type switch statement.
  CaseClauseStmt :: a
                 -> [f Expr] -- ^ list of expressions or types; nil
                             -- means default case
                 -> [f Stmt] -- ^ statement list
                 -> NodeF a f Stmt

  -- | A case of a select statement.
  CommClauseStmt :: a
                 -> Maybe (f Stmt) -- ^ send or receive statement; nil
                                   -- means default case
                 -> [f Stmt] -- ^ body
                 -> NodeF a f Stmt

  -- | A top-level initializer (never appears in source code,
  -- generated by Go's typechecker).
  InitializerStmt :: [f Expr] -- ^ list of constant/variable identifiers
                  -> [f Expr] -- ^ initial values
                  -> NodeF a f Stmt

  ----------------------------------------------------------------------
  -- Expressions

  -- | A literal of basic type. 
  BasicLitExpr :: a -> Type -> BasicLit -> NodeF a f Expr

  -- | A constant value produced by the Go typechecker's constant
  -- evaluator (most literals end up as one of these rather than a
  -- BasicLitExpr).
  BasicConstExpr :: a -> Type -> BasicConst -> NodeF a f Expr

  BinaryExpr :: a -> Type
             -> f Expr -- ^ left operand
             -> BinaryOp -- ^ operator
             -> f Expr -- ^ right operand
             -> NodeF a f Expr

  -- | An expression followed by an argument list.
  CallExpr :: a -> Type
           -> Bool -- ^ true if the last argument is an ellipsis
           -> f Expr -- ^ function expression
           -> [f Expr] -- ^ function arguments
           -> NodeF a f Expr

  CastExpr :: a -> Type
           -> f Expr -- ^ expression being cast
           -> f Expr -- ^ type to cast to
           -> NodeF a f Expr

  -- | A composite literal.
  CompositeLitExpr :: a -> Type
                   -> Maybe (f Expr) -- ^ literal type; or nil
                   -> [f Expr] -- ^ list of composite elements
                   -> NodeF a f Expr

  IdentExpr :: a -> Type
            -> Maybe Ident -- ^ Optional qualifier
            -> Ident -- ^ name
            -> NodeF a f Expr

  -- | An Ellipsis node stands for the "..." type in a parameter list
  -- or the "..." length in an array type.
  EllipsisExpr :: a -> Type
               -> Maybe (f Expr) -- ^ ellipsis element type (parameter
                                 -- lists only); or nil
               -> NodeF a f Expr

  -- | A function literal.
  FuncLitExpr :: a -> Type
              -> [f Field] -- ^ function parameter types
              -> [f Field] -- ^ function return types
              -> f Block -- ^ function body
              -> NodeF a f Expr

  -- | An expression followed by an index.
  IndexExpr :: a -> Type
            -> f Expr -- ^ expression
            -> f Expr -- ^ index
            -> NodeF a f Expr

  -- | (key : value) pairs in composite literals.
  KeyValueExpr :: a -> Type
               -> f Expr -- ^ key
               -> f Expr -- ^ value
               -> NodeF a f Expr

  -- | A parenthesized expression.
  ParenExpr :: a -> Type
            -> f Expr -- ^ parenthesized expression
            -> NodeF a f Expr

  -- | An expression followed by a selector.
  SelectorExpr :: a -> Type
               -> f Expr -- ^ expression
               -> Ident -- ^ field selector
               -> NodeF a f Expr

  -- | An expression followed by slice indices.
  SliceExpr :: a -> Type
            -> f Expr -- ^ expression
            -> Maybe (f Expr) -- ^ begin of slice range; or nil
            -> Maybe (f Expr) -- ^ end of slice range; or nil
            -> Maybe (f Expr) -- ^ maximum capacity of slice; or nil
            -> Bool -- ^ true if 3-index slice (2 colons present)
            -> NodeF a f Expr

  -- | An expression of the form "*" Expression.
  StarExpr :: a -> Type
           -> f Expr -- ^ operand
           -> NodeF a f Expr

  -- | An expression followed by a type assertion.
  TypeAssertExpr :: a -> Type
                 -> f Expr -- ^ expression
                 -> Maybe (f Expr) -- ^ asserted type; nil means type
                                   -- switch X.(type)
                 -> NodeF a f Expr

  -- | A unary expression. Unary "*" expressions are represented via
  -- StarExpr nodes.
  UnaryExpr :: a -> Type
            -> UnaryOp -- ^ operator
            -> f Expr -- ^ operand
            -> NodeF a f Expr

  ----------------------------------------------------------------------
  -- Type expressions

  -- | Named type (includes basic types like bool, int32, etc.)
  NamedTypeExpr :: a -> Type -> Ident -> NodeF a f Expr

  PointerTypeExpr :: a -> Type
                  -> f Expr
                  -> NodeF a f Expr

  -- | An array or slice type.
  ArrayTypeExpr :: a -> Type
                -> Maybe (f Expr) -- ^ length. Ellipsis for [...]T
                                  -- array types, nil for slice types
                -> f Expr -- ^ element type
                -> NodeF a f Expr

  FuncTypeExpr :: a -> Type
               -> [f Field] -- ^ (incoming) parameters; non-nil
               -> Maybe (f Field) -- ^ variadic parameter
               -> [f Field] -- ^ (outgoing) results
               -> NodeF a f Expr

  InterfaceTypeExpr :: a -> Type
                    -> [f Field] -- ^ list of methods
                    -> Bool -- ^ true if (source) methods are missing
                            -- in the methods list
                    -> NodeF a f Expr

  MapTypeExpr :: a -> Type
              -> f Expr -- ^ key type
              -> f Expr -- ^ value type
              -> NodeF a f Expr

  StructTypeExpr :: a -> Type
                 -> [f Field] -- ^ list of field declarations
                 -> NodeF a f Expr

  ChanTypeExpr :: a -> Type
               -> ChanDir -- ^ channel direction
               -> f Expr -- ^ value type
               -> NodeF a f Expr

  -- | Internal language only
  TupleExpr :: a -> Type
            -> [f Expr]
            -> NodeF a f Expr

  -- | Internal language only
  ProjExpr :: a -> Type
           -> f Expr
           -> Int
           -> NodeF a f Expr

  -- | Internal language only
  NilExpr :: a -> Type -> NodeF a f Expr

  ----------------------------------------------------------------------
  -- Declarations

  FuncDecl :: a
           -> Maybe (f Field) -- ^ receiver (methods); or nil (functions)
           -> Ident -- ^ function/method name
           -> [f Field] -- ^ parameters
           -> Maybe (f Field) -- ^ variadic parameter
           -> [f Field] -- ^ results
           -> Maybe (f Block) -- ^ function body; or nil for external
                              -- (non-Go) function
           -> NodeF a f Decl

  -- | (generic declaration node) An import, constant, type or
  -- variable declaration.
  GenDecl :: a
          -> [f Spec] -- ^ specs
          -> NodeF a f Decl

  TypeAliasDecl :: a
                -> [f Bind]
                -> NodeF a f Decl

  ----------------------------------------------------------------------
  -- Variable type binding

  Binding :: Ident
          -> f Expr
          -> NodeF a f Bind

  ----------------------------------------------------------------------
  -- Specifications

  -- | A single package import.
  ImportSpec :: a
             -> Maybe Ident -- ^ local package name (including "."); or nil
             -> Text -- ^ import path
             -> NodeF a f Spec

  -- | A constant declaration.
  ConstSpec :: a
            -> [Ident] -- ^ value names (nonempty)
            -> Maybe (f Expr) -- ^ value type; or nil
            -> [f Expr] -- ^ initial values
            -> NodeF a f Spec

  -- | A variable declaration.
  VarSpec :: a
          -> [Ident] -- ^ value names (nonempty)
          -> Maybe (f Expr) -- ^ value type; or nil
          -> [f Expr] -- ^ initial values
          -> NodeF a f Spec

  -- | A type declaration (TypeSpec production).
  TypeSpec :: a
           -> Ident -- ^ type name
           -> f Expr -- ^ IdentExpr, ParenExpr, SelectorExpr,
                     -- StarExpr, or any xxxTypeExpr
           -> NodeF a f Spec

  ----------------------------------------------------------------------
  -- Misc.

  -- | A Field represents a Field declaration list in a struct type, a
  -- method list in an interface type, or a parameter/result
  -- declaration in a signature. Field.Names is nil for unnamed
  -- parameters (parameter lists which only contain types) and
  -- embedded struct fields. In the latter case, the field name is the
  -- type name.
  FieldNode :: [Ident] -- ^ field/method/parameter names; or nil
            -> f Expr -- ^ field/method/parameter type
            -> Maybe BasicLit -- ^ field tag; or nil
            -> NodeF a f Field

  -- | A braced statement list.
  BlockNode :: [f Stmt]
            -> NodeF a f Block

data AssignType =
  Assign
  | Define
  | AssignOperator
  deriving (Eq, Show)

data BranchType =
  Break
  | Continue
  | Goto
  | Fallthrough
  deriving (Eq, Show)

data BasicLitType =
  LiteralBool
  | LiteralInt
  | LiteralFloat
  | LiteralComplex
  | LiteralImag
  | LiteralChar
  | LiteralString
  deriving (Eq, Show)

-- | A literal of basic type. 
data BasicLit =
  BasicLit
  { basiclit_type :: BasicLitType -- ^ "kind" of the literal
  , basiclit_value :: Text -- ^ literal value as a string
  }
  deriving (Eq, Show)

-- | Float constants are represented by rationals by Go's constant evaluator.
data BasicConst =
  BasicConstBool Bool
  | BasicConstString Text
  | BasicConstInt Integer
  | BasicConstFloat BasicConst BasicConst -- ^ Numerator and denominator ints
  | BasicConstComplex BasicConst BasicConst -- ^ Real and imaginary floats
  deriving (Eq, Show)

data BinaryOp =
  BPlus -- ^ +
  | BMinus -- ^ -
  | BMult -- ^ *
  | BDiv -- ^ /
  | BMod -- ^ %
  | BAnd -- ^ &
  | BOr -- ^ |
  | BXor -- ^ ^
  | BShiftL -- ^ <<
  | BShiftR -- ^ >>
  | BAndNot -- ^ &^
  | BLAnd -- ^ logical AND
  | BLOr -- ^ logical OR
  | BEq -- ^ ==
  | BLt -- ^ <
  | BGt -- ^ >
  | BNeq -- ^ !=
  | BLeq -- ^ <=
  | BGeq -- ^ >=
  deriving (Eq, Show)

data UnaryOp =
  UPlus -- ^ +
  | UMinus -- ^ -
  | UNot -- ^ !
  | UBitwiseNot -- ^ ^
  | UStar -- ^ *
  | UAddress -- ^ &
  | UArrow -- ^ <-
  deriving (Eq, Show)

annotOf :: NodeF a f Expr -> a
annotOf (BasicLitExpr x _ _) = x
annotOf (BasicConstExpr x _ _) = x
annotOf (BinaryExpr x _ _ _ _) = x
annotOf (CallExpr x _ _ _ _) = x
annotOf (CastExpr x _ _ _) = x
annotOf (CompositeLitExpr x _ _ _) = x
annotOf (IdentExpr x _ _ _) = x
annotOf (EllipsisExpr x _ _) = x
annotOf (FuncLitExpr x _ _ _ _) = x
annotOf (IndexExpr x _ _ _) = x
annotOf (KeyValueExpr x _ _ _) = x
annotOf (ParenExpr x _ _) = x
annotOf (SelectorExpr x _ _ _) = x
annotOf (SliceExpr x _ _ _ _ _ _) = x
annotOf (StarExpr x _ _) = x
annotOf (TypeAssertExpr x _ _ _) = x
annotOf (UnaryExpr x _ _ _) = x
annotOf (NamedTypeExpr x _ _) = x
annotOf (PointerTypeExpr x _ _) = x
annotOf (ArrayTypeExpr x _ _ _) = x
annotOf (FuncTypeExpr x _ _ _ _) = x
annotOf (InterfaceTypeExpr x _ _ _) = x
annotOf (MapTypeExpr x _ _ _) = x
annotOf (StructTypeExpr x _ _) = x
annotOf (ChanTypeExpr x _ _ _) = x
annotOf (TupleExpr x _ _) = x
annotOf (ProjExpr x _ _ _) = x
annotOf (NilExpr x _) = x

annotOf' :: Node a Expr -> a
annotOf' = annotOf . out

typeOf :: NodeF a f Expr -> Type
typeOf (BasicLitExpr _ tp _) = tp
typeOf (BasicConstExpr _ tp _) = tp
typeOf (BinaryExpr _ tp _ _ _) = tp
typeOf (CallExpr _ tp _ _ _) = tp
typeOf (CastExpr _ tp _ _) = tp
typeOf (CompositeLitExpr _ tp _ _) = tp
typeOf (IdentExpr _ tp _ _) = tp
typeOf (EllipsisExpr _ tp _) = tp
typeOf (FuncLitExpr _ tp _ _ _) = tp
typeOf (IndexExpr _ tp _ _) = tp
typeOf (KeyValueExpr _ tp _ _) = tp
typeOf (ParenExpr _ tp _) = tp
typeOf (SelectorExpr _ tp _ _) = tp
typeOf (SliceExpr _ tp _ _ _ _ _) = tp
typeOf (StarExpr _ tp _) = tp
typeOf (TypeAssertExpr _ tp _ _) = tp
typeOf (UnaryExpr _ tp _ _) = tp
typeOf (NamedTypeExpr _ tp _) = tp
typeOf (PointerTypeExpr _ tp _) = tp
typeOf (ArrayTypeExpr _ tp _ _) = tp
typeOf (FuncTypeExpr _ tp _ _ _) = tp
typeOf (InterfaceTypeExpr _ tp _ _) = tp
typeOf (MapTypeExpr _ tp _ _) = tp
typeOf (StructTypeExpr _ tp _) = tp
typeOf (ChanTypeExpr _ tp _ _) = tp
typeOf (TupleExpr _ tp _) = tp
typeOf (ProjExpr _ tp _ _) = tp
typeOf (NilExpr _ tp) = tp

typeOf' :: Node a Expr -> Type
typeOf' = typeOf . out

deriving instance (Eq a, forall n. Eq (f n)) => Eq (NodeF a f i)
deriving instance (Show a, forall n. Show (f n)) => Show (NodeF a f i)
deriving instance Eq a => Eq (Fix (NodeF a) i)
deriving instance Show a => Show (Fix (NodeF a) i)

-- $(return [])

instance FunctorFC (NodeF a) where
  fmapFC = fmapFCDefault

instance FoldableFC (NodeF a) where
  foldMapFC = foldMapFCDefault

-- It would be nice to derive this automatically using
-- structuralTraversal.
instance TraversableFC (NodeF a) where
  traverseFC f (MainNode nm pkg pkgs) =
    MainNode nm <$> f pkg <*> traverse f pkgs
  traverseFC f (PackageNode name path imports file_paths files inits) =
    PackageNode name path imports file_paths <$>
    traverse f files <*> traverse f inits
  traverseFC f (FileNode path name decls imports) =
    FileNode path name <$> traverse f decls <*> traverse f imports
  traverseFC f (BlockNode stmts) = BlockNode <$> traverse f stmts
  traverseFC f (AssignStmt x tp op lhs rhs) =
    AssignStmt x tp op <$> traverse f lhs <*> traverse f rhs
  traverseFC f (BlockStmt x stmt) = BlockStmt x <$> f stmt
  traverseFC _f (BranchStmt x tp lbl) = pure $ BranchStmt x tp lbl
  traverseFC f (DeclStmt x decl) = DeclStmt x <$> f decl
  traverseFC f (DeferStmt x e) = DeferStmt x <$> f e
  traverseFC _f (EmptyStmt x) = pure $ EmptyStmt x
  traverseFC f (ExprStmt x e) = ExprStmt x <$> f e
  traverseFC f (ForStmt x ini cond post body) =
    ForStmt x <$> traverse f ini <*>
    traverse f cond <*> traverse f post <*> f body
  traverseFC f (GoStmt x e) = GoStmt x <$> f e
  traverseFC f (IfStmt x ini cond body els) =
    IfStmt x <$> traverse f ini <*> f cond <*> f body <*> traverse f els
  traverseFC f (IncDecStmt x e b) = IncDecStmt x <$> f e <*> pure b
  traverseFC f (LabeledStmt x lbl stmt) = LabeledStmt x lbl <$> f stmt
  traverseFC f (RangeStmt x key value range body assign) =
    RangeStmt x <$> traverse f key <*>
    traverse f value <*> f range <*> f body <*> pure assign
  traverseFC f (ReturnStmt x es) = ReturnStmt x <$> traverse f es
  traverseFC f (SelectStmt x body) = SelectStmt x <$> f body
  traverseFC f (SendStmt x chan value) = SendStmt x <$> f chan <*> f value
  traverseFC f (SwitchStmt x ini tag body) =
    SwitchStmt x <$> traverse f ini <*> traverse f tag <*> f body
  traverseFC f (TypeSwitchStmt x ini assign body) =
    TypeSwitchStmt x <$> traverse f ini <*> f assign <*> f body
  traverseFC f (CaseClauseStmt x es stmts) =
    CaseClauseStmt x <$> traverse f es <*> traverse f stmts
  traverseFC f (CommClauseStmt x stmt stmts) =
    CommClauseStmt x <$> traverse f stmt <*> traverse f stmts
  traverseFC f (InitializerStmt vars values) =
    InitializerStmt <$> traverse f vars <*> traverse f values
  traverseFC _f (BasicLitExpr x tp lit) = pure $ BasicLitExpr x tp lit
  traverseFC _f (BasicConstExpr x tp c) = pure $ BasicConstExpr x tp c
  traverseFC f (BinaryExpr x tp left op right) =
    BinaryExpr x tp <$> f left <*> pure op <*> f right
  traverseFC f (CallExpr x tp b fun args) =
    CallExpr x tp b <$> f fun <*> traverse f args
  traverseFC f (CastExpr x tp e ty) = CastExpr x tp <$> f e <*> f ty
  traverseFC f (CompositeLitExpr x tp ty es) =
    CompositeLitExpr x tp <$> traverse f ty <*> traverse f es
  traverseFC _f (IdentExpr x tp qual ident) = pure $ IdentExpr x tp qual ident
  traverseFC f (EllipsisExpr x tp ty) = EllipsisExpr x tp <$> traverse f ty
  traverseFC f (FuncLitExpr x tp params results body) =
    FuncLitExpr x tp <$> traverse f params <*> traverse f results <*> f body
  traverseFC f (IndexExpr x tp e ix) = IndexExpr x tp <$> f e <*> f ix
  traverseFC f (KeyValueExpr x tp key value) =
    KeyValueExpr x tp <$> f key <*> f value
  traverseFC f (ParenExpr x tp e) = ParenExpr x tp <$> f e
  traverseFC f (SelectorExpr x tp e ident) =
    SelectorExpr x tp <$> f e <*> pure ident
  traverseFC f (SliceExpr x tp e begin end m b) =
    SliceExpr x tp <$> f e <*> traverse f begin <*>
    traverse f end <*> traverse f m <*> pure b
  traverseFC f (StarExpr x tp e) = StarExpr x tp <$> f e
  traverseFC f (TypeAssertExpr x tp e ty) =
    TypeAssertExpr x tp <$> f e <*> traverse f ty
  traverseFC f (UnaryExpr x tp op e) = UnaryExpr x tp op <$> f e
  traverseFC _f (NamedTypeExpr x tp nm) = pure $ NamedTypeExpr x tp nm
  traverseFC f (PointerTypeExpr x tp ty) = PointerTypeExpr x tp <$> f ty
  traverseFC f (ArrayTypeExpr x tp len ty) =
    ArrayTypeExpr x tp <$> traverse f len <*> f ty
  traverseFC f (FuncTypeExpr x tp params variadic results) =
    FuncTypeExpr x tp <$> traverse f params <*>
    traverse f variadic <*> traverse f results
  traverseFC f (InterfaceTypeExpr x tp methods b) =
    InterfaceTypeExpr x tp <$> traverse f methods <*> pure b
  traverseFC f (MapTypeExpr x tp key value) =
    MapTypeExpr x tp <$> f key <*> f value
  traverseFC f (StructTypeExpr x tp fields) =
    StructTypeExpr x tp <$> traverse f fields
  traverseFC f (ChanTypeExpr x tp dir ty) = ChanTypeExpr x tp dir <$> f ty
  traverseFC f (TupleExpr x tp es) = TupleExpr x tp <$> traverse f es
  traverseFC f (ProjExpr x tp e i) = ProjExpr x tp <$> f e <*> pure i
  traverseFC _f (NilExpr x tp) = pure $ NilExpr x tp
  traverseFC f (FuncDecl x recv nm params variadic results body) =
    FuncDecl x <$> traverse f recv <*> pure nm <*> traverse f params <*>
    traverse f variadic <*> traverse f results <*> traverse f body
  traverseFC f (GenDecl x specs) = GenDecl x <$> traverse f specs
  traverseFC f (TypeAliasDecl x binds) = TypeAliasDecl x <$> traverse f binds
  traverseFC f (Binding ident e) = Binding ident <$> f e
  traverseFC _f (ImportSpec x name path) = pure $ ImportSpec x name path
  traverseFC f (ConstSpec x names ty es) =
    ConstSpec x names <$> traverse f ty <*> traverse f es
  traverseFC f (VarSpec x names ty es) =
    VarSpec x names <$> traverse f ty <*> traverse f es
  traverseFC f (TypeSpec x name e) = TypeSpec x name <$> f e
  traverseFC f (FieldNode names ty tag) = FieldNode names <$> f ty <*> pure tag

-- TODO
-- traverseNodeF :: forall m a f g tp. Applicative m =>
--                  (forall u. f u -> m (g u)) ->
--                  NodeF a f tp -> m (NodeF a g tp)
-- -- traverseNodeF = $(U.structuralTraversal [t|NodeF|] [])
-- traverseNodeF = $(U.structuralTraversal [t|NodeF|]
--                   -- [(U.ConType [t|[]|] `U.TypeApp` U.AnyType, [|traverse|])
--                   -- , (U.ConType [t|Maybe|] `U.TypeApp` U.AnyType, [|traverse|])]
--                    -- [(U.ConType [t|[]|] `U.TypeApp` U.AnyType `U.TypeApp` U.AnyType, [|traverse|]),
--                    [
--                      -- (U.ConType [t|Maybe|] `U.TypeApp` (U.AnyType `U.TypeApp` U.AnyType), [|traverse|])
--                    ]
--                  )

-- -- traverseNodeF :: forall ext m f g tp. Applicative m
-- --               => (forall u . f u -> m (g u))
-- --               -> NodeF ext f tp -> m (NodeF ext g tp)
-- -- traverseNodeF = $(U.structuralTraversal [t|NodeF|] [])

-- instance TraversableFC (NodeF a) where
--   -- traverseFC = $(U.structuralTraversal [t|NodeF|] [])
--   traverseFC = traverseNodeF

packageName :: Node a Package -> Text
packageName (In (PackageNode nm _path _imports _file_paths _files _inits)) = nm

fileName :: Node a File -> Text
fileName (In (FileNode _path (Ident _k nm) _decls _imports)) = nm

fieldType :: Node a Field -> Type
fieldType (In (FieldNode _names tp_expr _tag)) = typeOf' tp_expr

declSpecs :: Node a Decl -> [Node a Spec]
declSpecs (In (GenDecl _x specs)) = specs
declSpecs _decl = error "declSpecs: expected GenDecl"

readBool :: Text -> Bool
readBool s = case s of
  "true" -> True
  "false" -> False
  _ -> error $ "readBool: not a bool: " ++ show s

isLoop :: Node a tp -> Bool
isLoop (In (ForStmt _x _ini _cond _post _body)) = True
isLoop (In (RangeStmt _x _k _v _e _body _assign)) = True
isLoop _node = False
