{-# LANGUAGE DeriveDataTypeable #-}
-- | 'Resolved' type representations and inference/checking
module Language.Go.Types (ValueType(..), Binding(..), BindingKind(..)) where

import Data.Int
import Language.Go.AST
import Data.HashMap.Strict (HashMap)
import AlexTools
import Data.Text (Text, unpack)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.Go.Bindings.Types


