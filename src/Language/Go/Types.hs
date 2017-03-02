{-# LANGUAGE DeriveDataTypeable #-}
-- | 'Resolved' type representations and inference/checking
module Language.Go.Types where

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

instance Typed (NamedParameter a) where
  getType (NamedParameter _ _ ty) = getType ty

instance Typed (Receiver a) where
  getType (Receiver _ _ pointed tn) =
    let tnt = Alias (reannotate (const ()) tn) in if pointed then Pointer tnt else tnt

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

getParamTypes :: ParameterList SourceRange -> [SemanticType]
getParamTypes pl = case pl of
  NamedParameterList _ nps _ -> map getType nps
  AnonymousParameterList _ aps _ -> map getType aps

getSpreadType :: ParameterList SourceRange -> Maybe SemanticType
getSpreadType pl = case pl of
  NamedParameterList _ _ mnp -> getType <$> mnp
  AnonymousParameterList _ _ manp -> getType <$> manp

getReturnTypes :: ReturnList SourceRange -> [SemanticType]
getReturnTypes rl = case rl of
  NamedReturnList _ nps -> map getType nps
  AnonymousReturnList _ aps -> map getType aps

getReceiverType :: Receiver SourceRange -> SemanticType
getReceiverType = getType
                      
