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

data ValueType = Int (Maybe Int32) {- ^ Bitwidth. Architecture-dependent if `Nothing` -} Bool {- ^ Signed? -}
               | Boolean
               | Float Int32 {- ^ Bitwidth -}
               | Complex Int32 {- ^ Bitwidth -}
               | Iota
               | Nil
               | String
               | Function (Maybe ValueType) -- ^ Method receiver type
                 [ValueType] -- ^ Parameter types
                 (Maybe  ValueType) -- ^ Spread parameter
                 [ValueType] -- ^ Return types
               | Array (Maybe (Expression SourceRange))
               | Struct (HashMap Text (ValueType, Maybe Text))
               | Pointer ValueType
               | Interface (HashMap Text ValueType)
               | Map ValueType ValueType
               | Slice ValueType
               | Channel (ChannelDirection SourceRange) ValueType
               | BuiltIn Text -- ^ built-in function
               | Alias (TypeName SourceRange)
  deriving (Data, Typeable, Show)
