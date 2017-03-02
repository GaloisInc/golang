{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
-- | 'Resolved' type representations and inference/checking
module Language.Go.Types where

import Language.Go.AST
import Data.Generics.Uniplate.Data
import Language.Go.Bindings
import Lens.Simple
import Control.Monad.Error.Class
import Language.Go.Parser.Util (unexpected)

class Typed a where
  getType :: MonadError (SourceRange, String) m => a -> m SemanticType
                        
instance Typed (Expression SourceRange) where
  getType e = case e of
    IntLit {} -> return $ Int Nothing True
    FloatLit {} -> return $ Float Nothing
    ImaginaryLit {} -> return $ Complex Nothing
    RuneLit {} -> return runeType
    StringLit {} -> return String
    Name _ (Id rng bind _) -> case bind^.bindingKind of
                              VarB st -> return st
                              ConstB st -> return st
                              _       -> unexpected rng "An identifier is used as a variable, but not bound to a value"
    Qualified _ _ (Id rng bind _) ->
      case bind^.bindingKind of
        VarB st -> return st
        ConstB st -> return st
        _       -> unexpected rng "An identifier is used as a variable, but not bound to a value"
    _ -> unexpected e "Expression not supported"

instance Typed (Type a) where
  getType t = case t of
    _ -> undefined

instance Typed (NamedParameter a) where
  getType (NamedParameter _ _ ty) = getType ty

instance Typed (Receiver a) where
  getType (Receiver _ _ pointed tn) =
    let tnt = Alias (reannotate (const ()) tn) in return $ if pointed then Pointer tnt else tnt

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

getParamTypes :: MonadError (SourceRange, String) m
              => ParameterList SourceRange -> m [SemanticType]
getParamTypes pl = case pl of
  NamedParameterList _ nps _ -> mapM getType nps
  AnonymousParameterList _ aps _ -> mapM getType aps

getSpreadType :: MonadError (SourceRange, String) m
              => ParameterList SourceRange -> m (Maybe SemanticType)
getSpreadType pl = case pl of
  NamedParameterList _ _ mnp -> sequence $ getType <$> mnp
  AnonymousParameterList _ _ manp -> sequence $ getType <$> manp

getReturnTypes :: MonadError (SourceRange, String) m
               => ReturnList SourceRange -> m [SemanticType]
getReturnTypes rl = case rl of
  NamedReturnList _ nps -> mapM getType nps
  AnonymousReturnList _ aps -> mapM getType aps

getReceiverType :: MonadError (SourceRange, String) m
                => Receiver SourceRange -> m SemanticType
getReceiverType = getType
                      
