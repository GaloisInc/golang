{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, LambdaCase, TupleSections #-}

-- | 'Resolved' type representations and inference/checking
module Language.Go.Types where

import Language.Go.AST
import Data.Generics.Uniplate.Data
import Language.Go.Bindings
import Lens.Simple
import Control.Monad.Error.Class
import Language.Go.Parser.Util (unexpected, Ranged(..))
import Control.Monad (unless, liftM)
import Control.Applicative
import Data.Map (Map)
import Data.Text (Text)
import Control.Arrow

class Typed a where
  getType :: MonadError (SourceRange, String) m => a -> m VarType
               
-- The following is a gross simplification that will only give correct
-- results for (a subset of?) correct Go code. Actual Go type checking
-- rules are more intricate and are not implemented fully right now.
getExprType :: MonadError (SourceRange, String) m => Expression SourceRange -> m ExprType
getExprType e = case e of
  CallExpr _ f _t params spread -> do
    f_ty <- getExprType f
    case f_ty of
      VarType (Function _ _ _ [rty]) -> return $ VarType rty
      VarType (Function _ _ _ rtys) ->
        error $ "Types.getExprType: expected exactly one return type, found " ++ show rtys
      ConstType cty ->
        error $ "Types.getExprType: expected VarType, found ConstType " ++ show cty
    
  IntLit _ i -> return $ ConstType $ CInt i
  FloatLit _ d -> return $ ConstType $ CFloat d
  ImaginaryLit _ im -> return $ ConstType $ CComplex 0 im
  RuneLit _ c -> return $ ConstType $ CRune c
  StringLit _ s -> return $ ConstType $ CString s
  Name _ mqual (Id rng bind _) ->
    case bind^.bindingKind of
      VarB st -> return $ VarType st
      ConstB st -> return $ VarType st
      _ -> unexpected rng "An identifier is used as a variable, but not bound to a value"
    -- For binary operators, other than comparisons, the operand types
    -- must be identical unless the operation involves shifts or
    -- untyped constants. Arithmetic operators apply to numeric values
    -- and yield a result of the same type as the first
    -- operand. Except for shift operations, if one operand is an
    -- untyped constant and the other operand is not, the constant is
    -- converted to the type of the other operand.
  BinaryExpr _ op left right ->
    do lt <- getExprType left
       rt <- getExprType right
       case op of
         Add -> do assertTypeIdentity (left, right) lt rt
                   return lt
         Subtract -> do assertTypeIdentity (left, right) lt rt
                        return lt
         Multiply -> do assertTypeIdentity (left, right) lt rt
                        return lt
         Divide -> do assertTypeIdentity (left, right) lt rt
                      return lt
           -- FIXME: rhs of shifts is actually required to be uint or
           -- a constant representable by uint
         LeftShift  -> if isInteger lt && isInteger rt then return lt
                       else unexpected e $ "Shift expressions require integer operands"
         RightShift -> if isInteger lt && isInteger rt then return lt
                       else unexpected e $ "Shift expressions require integer operands"
         BitwiseXOr -> if isInteger lt && isInteger rt then return lt
                       else unexpected e $ "Shift expressions require integer operands"
         BitwiseOr  -> if isInteger lt && isInteger rt then return lt
                       else unexpected e $ "Shift expressions require integer operands"
         BitwiseAnd  -> if isInteger lt && isInteger rt then return lt
                        else unexpected e $ "Shift expressions require integer operands"
         _ -> unexpected e $ "Type analysis has not yet been implemented for the binary operator " ++ show op
  UnaryExpr  _ op operand -> do ot <- getExprType operand
                                -- assertTypeP (isInteger .||. isFloat) ot
                                return ot
                                     
  IndexExpr _ base index ->
    do btype <- getExprType base
       itype <- getExprType index
       liftM VarType $ getIndexedElementType e btype itype
                                  
  CompositeLit _ litType elements ->
    getType litType >>= underlyingType litType >>= (return . VarType)
         -- The LiteralType's underlying type must be a struct, array,
         -- slice, or map type (the grammar enforces this constraint
         -- except when the type is given as a TypeName)

         -- FIXME: typecheck the types of elements
         -- case lty of
         --   -- For struct literals the following rules apply:
         --   -- 1. A key must be a field name declared in the struct type.
         --   -- 2. An element list that does not contain any keys must
         --   -- list an element for each struct field in the order in
         --   -- which the fields are declared.
         --   -- 3. If any element has a key, every element must have a key.
         --   -- 4. An element list that contains keys does not need to
         --   -- have an element for each struct field. Omitted fields
         --   -- get the zero value for that field.
         --   -- 5. A literal may omit the element list; such a literal
         --   -- evaluates to the zero value for its type.
         --   -- 6. It is an error to specify an element for a
         --   -- non-exported field of a struct belonging to a different
         --   -- package.
         --   Struct fields -> unexpected litType $ "Struct type analysis is not implemented"
         --   -- An element with a key uses the key as its index. The key
         --   -- must be a non-negative constant representable by a value
         --   -- of type int; and if it is typed it must be of integer
         --   -- type.
         --   -- FIXME: support for array literals with types that don't
         --   -- specify size: inferring size from the maximum index (can
         --   -- be different from the lenght of the litral)
         --   Array (Just (IntLit _ len)) eltype -> undefined
  _ -> unexpected e $ "in Types.getExprType: Expression not supported " ++ show e

getIndexedElementType :: (Ranged r, MonadError (SourceRange, String) m) => r -> ExprType -> ExprType -> m VarType
getIndexedElementType rng btype itype =
  let baseTypeErr = unexpected rng $ "Indexing on a non-array value"
      indexTypeErr = unexpected rng $ "Incompatible index type"
  in case btype of
    ConstType cty -> case cty of
      CString t -> if isInteger itype then return runeType else indexTypeErr
      _ -> baseTypeErr
    VarType vty -> case vty of
      Array _ elt -> if isInteger itype then return elt else indexTypeErr
      Slice elt ->   if isInteger itype then return elt else indexTypeErr
      String ->      if isInteger itype then return runeType else indexTypeErr
      Map ktype vtype ->
        if itype `assignableTo` ktype then return vtype else indexTypeErr
      Pointer ptype -> getIndexedElementType rng (VarType ptype) itype
      _ -> baseTypeErr

getLiteralElementTypes :: (MonadError (SourceRange, String) m)
                       => Element SourceRange -> m (Maybe ExprType, ExprType)
getLiteralElementTypes elt = case elt of
  Element _ e -> do et <- getExprType e
                    return (Nothing, et)
  -- KeyedEl _ k v -> (,) <$> (Just <$> getKeyType k) <*> getExprType v


(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) = liftA2 (||)

-- -- | Assert a predicate on a type
-- assertTypeP :: (MonadError (SourceRange, String) m) => Bool -> m ()
-- assertTypeP b = if b then return () else throwError ""

instance Typed (Expression SourceRange) where
  getType = liftM defaultType . getExprType

-- | Returns 
-- isInteger :: 

assertTypeIdentity :: (MonadError (SourceRange, String) m, Ranged r)
                   => r -> ExprType -> ExprType -> m ()
assertTypeIdentity rng st1 st2 = unless (st1 == st2) $ unexpected rng "Expecting operands to have identical types"

deriving instance Eq ExprType

instance Typed (Type SourceRange) where
  getType t = case t of
    NamedType _ (TypeName _ _ (Id rng bind _)) ->
                 case bind^.bindingKind of
                   TypeB st -> return st
                   _ -> unexpected rng "An identifier is used as a type name, but not bound to a type"
    -- | Types without a constant integer length speficied aren't
    -- supported right now. Also, types without a specified length are
    -- only valid in composite literals and should be handled there.
    ArrayType _ mlen elt ->
      do elty <- getType elt
         return $ Array (fmap (reannotate (const ())) mlen) elty
    FunctionType _ params rets ->
      pure (uncurry $ Function Nothing) <*> paramTys params <*> retTys rets
    _ -> unexpected t $ "Unsupported type " ++ show t
    where
      paramTys :: MonadError (SourceRange, String) m =>
                  ParameterList SourceRange -> m ([VarType], Maybe VarType)
      paramTys (NamedParameterList _ params spread) =
        pure (,) <*> (mapM getType params) <*> (mapM getType spread)
      paramTys (AnonymousParameterList _ params spread) =
        pure (,) <*> (mapM getType params) <*> (mapM getType spread)
      retTys :: MonadError (SourceRange, String) m => ReturnList SourceRange -> m [VarType]
      retTys (NamedReturnList _ returns) = mapM getType returns
      retTys (AnonymousReturnList _ returns) = mapM getType returns

instance Typed (NamedParameter SourceRange) where
  getType (NamedParameter _ _ ty) = getType ty

instance Typed (Receiver SourceRange) where
  getType (Receiver _ _ pointed tn) =
    let tnt = Alias (reannotate (const ()) tn)
    in return $ if pointed then Pointer tnt else tnt

-- | Coerce a possible type to a storable type; see spec "Constants":
-- "The default type of an untyped constant is bool, rune, int,
-- float64, complex128 or string respectively, depending on whether it
-- is a boolean, rune, integer, floating-point, complex, or string
-- constant."
defaultType :: ExprType -> VarType
defaultType et = case et of
  VarType vt -> vt
  ConstType ct -> case ct of
    CBool _  -> Boolean
    CRune _  -> runeType
    CInt _   -> Int Nothing True
    CFloat _ -> Float 64
    CComplex _ _ -> Complex 128
    CString _ -> String

-- | Whether a value of an `ExprType` is assignable to a location of a `VarType`.
assignableTo :: ExprType -> VarType -> Bool
assignableTo from to =
  -- Spec: A value x is assignable to a variable of type T ("x is
  -- assignable to T") in any of these cases:
  case from of
    VarType fromVT -> case (fromVT, to) of
      --   * x is a bidirectional channel value, T is a channel type, x's
      --     type V and T have identical element types, and at least one of
      --     V or T is not a named type.
      (Channel (Duplex ()) fromElT, Channel _ toElT) -> fromElT == toElT
      -- * x's type is identical to T (part 1)
      (Alias tn1, Alias tn2) -> typeNamesIdentical tn1 tn2
      (Alias (TypeName _ _ (Id _ fromBind _)), Channel _ toElT) -> fromBind^.bindingKind == TypeB (Channel (Duplex ()) toElT)
      (Channel (Duplex ()) fromElT, Alias (TypeName _ _ (Id _ tobind _))) ->
        (\case (TypeB (Channel _ toElT)) -> fromElT == toElT
               _ -> False) (tobind^.bindingKind)
      
      -- * x's type V and T have identical underlying types and at
      --   least one of V or T is not a named type.
      (Alias (TypeName _ _ (Id _ fromBind _)), _) -> fromBind^.bindingKind == TypeB to      
      (_, Alias (TypeName _ _ (Id _ tobind _))) -> tobind^.bindingKind == TypeB fromVT
      --   * T is an interface type and x implements T.
      (_, Interface ifields) -> fromVT `implements` ifields        
      --   * x is the predeclared identifier nil and T is a pointer,
      --     function, slice, map, channel, or interface type.
      (Nil, Pointer _) -> True
      (Nil, Function _ _ _ _) -> True
      (Nil, Map _ _) -> True
      (Nil, Channel _ _) -> True
      (Nil, Interface _) -> True
      -- * x's type is identical to T (part 2)
      _ | fromVT == to -> True
      _ | otherwise -> False
    --   * x is an untyped constant representable by a value of type T.
    ConstType ct -> representableBy ct to


-- | Whether a constant can be represented by a particular variable
-- type. FIXME: fill in the remaining cases for const types. Also
-- check that the constant values fit in the bitwidths.
representableBy :: ConstType -> VarType -> Bool
representableBy (CInt _) vt = isInteger (VarType vt)
representableBy (CFloat f) vt =
  if isFloat (VarType vt) then True
  else if isInteger (VarType vt) then
          let (whole, fraction) = properFraction f
          in  fraction == 0 && representableBy (CInt whole) vt
       else False

isInteger :: ExprType -> Bool
isInteger etype = case etype of
  VarType (Int _ _) -> True
  ConstType (CInt _) -> True
  _ -> False

isFloat :: ExprType -> Bool
isFloat etype = case etype of
  VarType (Float _) -> True
  ConstType (CFloat _) -> True
  _ -> False

getParamTypes :: MonadError (SourceRange, String) m
              => ParameterList SourceRange -> m [VarType]
getParamTypes pl = case pl of
  NamedParameterList _ nps _ -> mapM getType nps
  AnonymousParameterList _ aps _ -> mapM getType aps

getSpreadType :: MonadError (SourceRange, String) m
              => ParameterList SourceRange -> m (Maybe VarType)
getSpreadType pl = case pl of
  NamedParameterList _ _ mnp -> sequence $ getType <$> mnp
  AnonymousParameterList _ _ manp -> sequence $ getType <$> manp

getReturnTypes :: MonadError (SourceRange, String) m
               => ReturnList SourceRange -> m [VarType]
getReturnTypes rl = case rl of
  NamedReturnList _ nps -> mapM getType nps
  AnonymousReturnList _ aps -> mapM getType aps

getReceiverType :: MonadError (SourceRange, String) m
                => Receiver SourceRange -> m VarType
getReceiverType = getType
                      
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
instance Eq VarType where
  (Alias tyn1) == (Alias tyn2) = tyn1 `typeNamesIdentical` tyn2
  (Alias _)    == _            = False
  _            == (Alias _)     = False
  Int width1 signed1 == Int width2 signed2 = width1 == width2 && signed1 == signed2
  et1            == et2             = error $ "Unmatched semantic type comparison: " ++ show et1 ++ " and " ++ show et2

-- Except for shift operations, if the operands of a binary operation
-- are different kinds of untyped constants, the operation and, for
-- non-boolean operations, the result use the kind that appears later
-- in this list: integer, rune, floating-point, complex. For example,
-- an untyped integer constant divided by an untyped complex constant
-- yields an untyped complex constant.
ctypeOrdering :: ConstType -> Maybe Int
ctypeOrdering ct = case ct of
  CInt _       -> Just 1
  CRune _      -> Just 2
  CFloat _     -> Just 3
  CComplex _ _ -> Just 4
  _            -> Nothing
  
-- | Subtyping/ordering for constant types
instance Ord ConstType where 
  ct1 <= ct2 = if ct1 == ct2 then True
               else case (ctypeOrdering ct1, ctypeOrdering ct2) of
                      (Just o1, Just o2) -> o1 <= o2
                      _                  -> False

deriving instance Eq BindingKind
deriving instance Eq Binding
  
-- | "Two named types are identical if their type names originate in
-- the same TypeSpec." Which means both names and declaration
-- locations should be the same.
typeNamesIdentical :: TypeName a -> TypeName a -> Bool
typeNamesIdentical (TypeName _ _ (Id _ bind1 name1)) (TypeName _ _ (Id _ bind2 name2)) = name1 == name2 && (bind1^.bindingDeclLoc == bind2^.bindingDeclLoc)
typeNamesIdentical _ _ = False

-- | Remove all the unknown bitwidths from types
specializeType :: Int {- ^ Machine word width in bits -} -> VarType -> VarType
specializeType machineWordSize = transform (specializeInt machineWordSize) 

specializeInt :: Int -> VarType -> VarType
specializeInt machineWordSize ty = case ty of
  Int Nothing signed -> Int (Just machineWordSize) signed
  _ -> ty

-- | FIXME: implement this. Returns True when a given var type implements an interface specification (given by a hashmap of fields and types)
implements :: VarType -> Map Text VarType -> Bool
implements _ _ = error "interface type analysis is not implemented yet"

-- | For type aliases, returns the type it aliases to. Otherwise, returns the type
underlyingType :: (Ranged r, MonadError (SourceRange, String) m)
               => r -> VarType -> m VarType
underlyingType r (Alias (TypeName _ _ (Id _ bk ident))) =
  case bk^.bindingKind of
    TypeB vt -> underlyingType r vt
    _        -> unexpected r $ "Type alias " ++ show ident ++ " is not bound to a type"
underlyingType _ t = return t

varType :: MonadError a m => ExprType -> m VarType
varType (VarType t) = return t
varType (ConstType t) = error $ "expected VarType, got " ++ show t
