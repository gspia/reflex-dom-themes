{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Reflex.Dom.Component.NaiveI18n where

import           Data.Text (Text)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type LangMap a = Map.Map a Text

mtLabel :: Ord a => a -> LangMap a -> Maybe Text
mtLabel = Map.lookup


--------------------------------------------------------------------------------

