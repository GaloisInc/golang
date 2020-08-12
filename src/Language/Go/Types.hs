{-|
Module      : Language.Go.Types
Description : Golang type semantics
Maintainer  : abagnall@galois.com
Stability   : experimental

The definitions here mirror those from the "go/types" package. They
represent semantic type information, not syntactic type expressions as
may appear in the syntax of a program.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
module Language.Go.Types where

import           Data.Text

-- | Type indices for Go AST nodes.
data NodeType = Main | Package | File | Stmt | Expr
              | Decl | Field | Spec | Bind | Block

-- | An identifier.
data Ident =
  Ident IdentKind -- ^ The kind of object the identifier denotes
  Text -- ^ Identifier text
  deriving (Eq, Show)

identName :: Ident -> Text
identName (Ident _k name) = name

data IdentKind =
  IdentNoKind
  | IdentBuiltin
  | IdentConst
  | IdentFunc
  | IdentLabel
  | IdentNil
  | IdentPkgName
  | IdentTypeName
  | IdentVar
  deriving (Eq, Show)

data UntypedKind =
  UntypedBool
  | UntypedInt
  | UntypedRune
  | UntypedFloat
  | UntypedComplex
  | UntypedString
  | UntypedNil
  deriving (Eq, Show)

data BasicKind =
  BasicInvalid
  | BasicBool
  | BasicInt (Maybe Int) -- ^ 8, 16, 32, 64
  | BasicUInt (Maybe Int) -- ^ 8, 16, 32, 64
  | BasicUIntptr
  | BasicFloat Int -- ^ 32, 64
  | BasicComplex Int -- ^ 64, 128
  | BasicString
  | BasicUnsafePointer
  | BasicUntyped UntypedKind
  deriving (Eq, Show)

data ChanDir =
  ChanDirSend
  | ChanDirRecv
  | ChanDirBoth
  deriving (Eq, Show)

data NameType = NameType Text Type
  deriving (Eq, Show)

typeOfNameType :: NameType -> Type
typeOfNameType (NameType _nm tp) = tp

typeToNameType :: Type -> NameType
typeToNameType tp = NameType "" tp

data Type =
  NoType
  | ArrayType Int Type
  | BasicType BasicKind
  | ChanType ChanDir Type
  | InterfaceType [NameType]
  | MapType Type Type
  | NamedType Type
  | PointerType Type
  -- | receiver, params, result (always tuple), variadic?
  | FuncType (Maybe (Ident, Type)) [Type] Type Bool
  | SliceType Type
  | StructType [NameType]
  | TupleType [NameType]
  deriving (Eq, Show)

isUntyped :: Type -> Bool
isUntyped (BasicType (BasicUntyped _)) = True
isUntyped _ = False

mkReturnType :: [Type] -> Type
mkReturnType [tp] = tp
mkReturnType tps = TupleType $ typeToNameType <$> tps

arrayTypeLen :: Type -> Int
arrayTypeLen (ArrayType len _t) = len
arrayTypeLen tp = error $ "arrayTypeLen: expected ArrayType, got" ++ show tp

arrayTypeTy :: Type -> Type
arrayTypeTy (ArrayType _len ty) = ty
arrayTypeTy tp = error $ "arrayTypeTy: expected ArrayType, got" ++ show tp

sliceTypeTy :: Type -> Type
sliceTypeTy (SliceType ty) = ty
sliceTypeTy tp = error $ "sliceTypeTy: expected SliceType, got" ++ show tp

-- | The type of the built-in 'new' function, given the element
-- type. Use empty list for argument types.
newType :: Type -> Type
newType tp = FuncType Nothing [] (PointerType tp) False

-- | The type of the built-in 'make' function, given the element
-- type. Use empty list for argument types.
makeType :: Type -> Type
makeType tp = FuncType Nothing [] tp False
