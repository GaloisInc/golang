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
import Control.Monad.State.Strict
import Control.Monad.Except

data ParserState = ParserState {_modules :: HashMap Text (Package (SourceRange, Maybe BindingKind))
                               ,_identifiers :: Bindings
                               }

makeLenses ''ParserState

instance Default ParserState where
  def = ParserState { _modules = HM.empty, _identifiers = defaultBindings }

type Parser a = ExceptT (SourceRange, String) (StateT ParserState IO) a
type ParserAnnotation = (SourceRange, Maybe BindingKind)
