{-# LANGUAGE DeriveDataTypeable #-}
-- | 'Resolved' type representations and inference/checking
module Language.Go.Types (SemanticType(..), Binding(..), BindingKind(..), Typed, defaultType) where

import Data.Int
import Language.Go.AST
import Data.HashMap.Strict (HashMap)
import AlexTools
import Data.Text (Text, unpack)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.Go.Bindings.Types
import Data.Generics.Uniplate.Data
import Language.Go.Bindings
import Lens.Simple

class Typed a where
  getType :: a -> SemanticType

instance Typed (Expression a) where
  getType e = case e of
    IntLit {} -> Int Nothing True
    FloatLit {} -> Float Nothing
    ImaginaryLit {} -> Complex Nothing
    RuneLit {} -> runeType
    StringLit {} -> String
    Name _ (Id _ bind _) -> case bind^.bindingKind of
                              VarB st -> st
                              ConstB st -> st
                              _       -> error "An identifier is used as a variable, but not bound to a value"
    Qualified _ _ (Id _ bind _) ->
      case bind^.bindingKind of
        VarB st -> st
        ConstB st -> st
        _       -> error "An identifier is used as a variable, but not bound to a value"
    _ -> error "Expression not supported"

instance Typed (Type a) where
  getType t = case t of
    _ -> undefined

-- | Coerce a possible type to a storable type; see spec "Constants":
-- "The default type of an untyped constant is bool, rune, int,
-- float64, complex128 or string respectively, depending on whether it
-- is a boolean, rune, integer, floating-point, complex, or string
-- constant."
defaultType :: SemanticType -> SemanticType
defaultType = transform concretize
  where concretize ct = case ct of
          Float Nothing -> Float (Just 64)
          Complex Nothing -> Complex (Just 128)
          _ -> ct
