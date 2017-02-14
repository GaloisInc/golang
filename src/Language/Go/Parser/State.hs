{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Language.Go.Parser.State where

import Data.HashMap.Strict (HashMap)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)
import Language.Go.AST
import AlexTools
import Lens.Simple
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow
import Language.Go.Parser.Util
import Data.Int
import Language.Go.Bindings
import Language.Go.Bindings.Types


data ParserState = ParserState {_modules :: HashMap Text (Package (SourceRange, Maybe BindingKind))
                               ,_identifiers :: Bindings
                               }

makeLenses ''ParserState

instance Default ParserState where
  def = ParserState { _modules = HM.empty, _identifiers = defaultBindings }


