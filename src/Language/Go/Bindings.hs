{-# LANGUAGE OverloadedStrings, ExistentialQuantification, RankNTypes, FlexibleContexts #-}

-- | Identifier bindings and operations on them
module Language.Go.Bindings where

import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Strict as HM
import AlexTools hiding (range)
import Lens.Simple hiding ((&))
import Control.Arrow
import Language.Go.Parser.Util
import Language.Go.AST (Id(..), Type(..), TypeName(..), BindingKind (..), Binding (..), VarType (..))
import Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Default.Class
import Control.Applicative
import Data.Graph.Inductive hiding (mkNode)
import Data.Graph.Inductive.Query.DFS
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data
import Data.List (nub)
import Control.Monad.State
import Language.Go.Bindings.Types
import Data.Tree
import Data.Graph.Inductive.Basic
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe (fromJust)

defaultBindings = predeclaredBindings :| []

-- declareBindingId :: Id SourceRange -> BindingKind -> Bindings -> Bindings
-- declareBindingId (BlankId _) _ = id
-- declareBindingId id@(Id _ _ ident) bk = declareBinding ident 

mkBinding :: Ranged r => Text -> r -> BindingKind -> Binding
mkBinding n r bk = Binding {_bindingDeclLoc = range r
                           ,_bindingKind = bk
                           ,_bindingImported = False
                           ,_bindingThisScope = True
                           }

declareBinding :: Text -> Binding -> Bindings -> Bindings
declareBinding ident bind (b :| bs) = HM.insert ident bind b :| bs

-- | lookup the value of the binding recursively in the scopes
lookupBinding :: Text -> Bindings -> Maybe Binding
lookupBinding name (b :| bs) =
  case HM.lookup name b of
    Just bind -> Just bind
    Nothing -> case bs of
      [] -> Nothing
      b':bs' -> lookupBinding name (b':|bs')

pushScope :: Bindings -> Bindings
pushScope (b :| rest) = (HM.map notThisContext b) :| b:rest
  where notThisContext bind = bind {_bindingThisScope = False}

popScope :: Bindings -> Maybe Bindings
popScope = snd . NE.uncons

emptyScope = HM.empty
                         
predeclaredBindings :: HashMap Text Binding
predeclaredBindings = HM.fromList $ map (second $ \k -> Binding fakeRange k True True) $  [("bool", TypeB Boolean)
  ,("uint8", TypeB $ Int (Just 8) False)
  ,("uint16", TypeB $ Int (Just 16) False)
  ,("uint32", TypeB $ Int (Just 32) False)
  ,("uint64", TypeB $ Int (Just 64) False)
  ,("int8", TypeB $ Int (Just 8) True)
  ,("int16", TypeB $ Int (Just 16) True)
  ,("int32", TypeB $ Int (Just 32) True)
  ,("int64", TypeB $ Int (Just 64) True)
  ,("float32", TypeB $ Float 32)
  ,("float64", TypeB $ Float 64)
  ,("complex64", TypeB $ Complex 64)
  ,("complex128", TypeB $ Complex 128)
  ,("byte", TypeB $ Int (Just 8) False)
  ,("bool", TypeB $ Boolean)
  ,("rune", TypeB runeType)
  ,("uint", TypeB $ Int Nothing False)
  ,("int", TypeB $ Int Nothing True)
  ,("uintptr", TypeB $ Int (Just 64) False)
  ,("string", TypeB String)
  ,("iota", ConstB Iota)
  ,("nil", ConstB Nil)
  ,("true", ConstB  Boolean)
  ,("false", ConstB Boolean)
  ]
  ++ map (id &&& (ConstB . BuiltIn))
  ["append", "cap", "close", "complex", "copy", "delete", "imag", "len", "make",
   "new", "panic", "print", "println", "real", "recover"
  ]

runeType = Int (Just 32) True
