{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
-- | 'Resolved' type representations and inference/checking
module Language.Go.Types where

import Language.Go.AST
import Data.Generics.Uniplate.Data
import Language.Go.Bindings
import Lens.Simple
import Control.Monad.Error.Class
import Language.Go.Parser.Util (unexpected, Ranged(..))
import Control.Monad (unless)

class Typed a where
  getType :: MonadError (SourceRange, String) m => a -> m SemanticType
                        
instance Typed (Expression SourceRange) where
  getType e = case e of
    IntLit {} -> return $ Int Nothing True
    FloatLit {} -> return $ Float Nothing
    ImaginaryLit {} -> return $ Complex Nothing
    RuneLit {} -> return runeType
    StringLit {} -> return String
    Name _ mqual (Id rng bind _) -> case bind^.bindingKind of
                                      VarB st -> return st
                                      ConstB st -> return st
                                      _       -> unexpected rng "An identifier is used as a variable, but not bound to a value"
    -- For binary operators, other than comparisons, the operand types
    -- must be identical unless the operation involves shifts or
    -- untyped constants. Arithmetic operators apply to numeric values
    -- and yield a result of the same type as the first
    -- operand. Except for shift operations, if one operand is an
    -- untyped constant and the other operand is not, the constant is
    -- converted to the type of the other operand.
    BinaryExpr _ op left right ->
      do lt <- getType left
         rt <- getType right
         case op of
           Add -> do assertTypeIdentity (left, right) lt rt
                     return lt
    UnaryExpr  _ op operand    -> undefined
    _ -> unexpected e "Expression not supported"

isInt :: SemanticType -> Bool
isInt (Int {}) = True
isInt _ = False

assertTypeIdentity :: (MonadError (SourceRange, String) m, Ranged r)
                   => r -> SemanticType -> SemanticType -> m ()
assertTypeIdentity rng st1 st2 = unless (st1 == st2) $ unexpected rng "Expecting operands to have identical types"

instance Typed (Type SourceRange) where
  getType t = case t of
    NamedType _ (TypeName _ _ (Id rng bind _)) ->
                 case bind^.bindingKind of
                   TypeB st -> return st
                   _ -> unexpected rng "An identifier is used as a type name, but not bound to a type"
    _ -> undefined

instance Typed (NamedParameter SourceRange) where
  getType (NamedParameter _ _ ty) = getType ty

instance Typed (Receiver SourceRange) where
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
                      
-- | Type assignability (see Spec). Something of type `r` is assignable to something of type `l` if this predicate holds
assignable :: SemanticType -> SemanticType -> Bool
assignable l r = undefined


-- | Type identity. From the Spec:
-- Two types are either identical or different.  Two named types are
-- identical if their type names originate in the same TypeSpec.  A
-- named and an unnamed type are always different. Two unnamed types
-- are identical if the corresponding type literals are identical,
-- that is, if they have the same literal structure and corresponding
-- components have identical types. In detail:
--   * Two array types are identical if they have identical element types and the
--     same array length.
--   * Two slice types are identical if they have identical element types.
--   * Two struct types are identical if they have the same sequence of fields, and
--     if corresponding fields have the same names, and identical types, and
--     identical tags. Two anonymous fields are considered to have the same name.
--     Lower-case field names from different packages are always different.
--   * Two pointer types are identical if they have identical base types.
--   * Two function types are identical if they have the same number of parameters
--     and result values, corresponding parameter and result types are identical,
--     and either both functions are variadic or neither is. Parameter and result
--     names are not required to match.
--   * Two interface types are identical if they have the same set of methods with
--     the same names and identical function types. Lower-case method names from
--     different packages are always different. The order of the methods is
--     irrelevant.
--   * Two map types are identical if they have identical key and value types.
--   * Two channel types are identical if they have identical value types and the
--     same direction.
instance Eq SemanticType where
  (Alias tyn1) == (Alias tyn2) = tyn1 `typeNamesIdentical` tyn2
  (Alias _)    == _            = False
  _            == (Alias _)     = False
  Int width1 signed1 == Int width2 signed2 = width1 == width2 && signed1 == signed2
  et1            == et2             = error $ "Unmatched semantic type comparison: " ++ show et1 ++ " and " ++ show et2
  
-- | "Two named types are identical if their type names originate in
-- the same TypeSpec." Which means both names and declaration
-- locations should be the same.
typeNamesIdentical :: TypeName a -> TypeName a -> Bool
typeNamesIdentical (TypeName _ _ (Id _ bind1 name1)) (TypeName _ _ (Id _ bind2 name2)) = name1 == name2 && (bind1^.bindingDeclLoc == bind2^.bindingDeclLoc)

-- | Remove all the unknown bitwidths from types
specializeType :: Int {- ^ Machine word width in bits -} ->  SemanticType -> SemanticType
specializeType machineWordSize = transform (specializeInt machineWordSize . defaultType) 

specializeInt :: Int -> SemanticType -> SemanticType
specializeInt machineWordSize ty = case ty of
  Int Nothing signed -> Int (Just machineWordSize) signed
  _ -> ty
