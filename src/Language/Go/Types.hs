{-|
Module      : Lang.Crucible.Go.Types
Description : TODO: short description
Maintainer  : abagnall@galois.com
Stability   : experimental

TODO: long description
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
