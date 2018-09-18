{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction, UnicodeSyntax, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.Component.MenuCommon
Description : Menuitems, e.g., for a navbar and other menu-like structures.
Copyright   : (c) gspia 2017 -
License     : BSD3
Maintainer  : gspia

= Menu-structures

This module contains functions that can be used to construct menus.


== Note

-}

module Reflex.Dom.Component.MenuCommon where

-- import           Control.Applicative
-- import Control.Arrow -- ((&&&), arr)
-- import           Control.Lens
-- import           Control.Monad (mapM)
-- import Control.Monad.Fix (MonadFix)
-- import Control.Monad.Zip (mzipWith)
-- import           Control.Monad.State.Lazy

-- import qualified Data.Map as Map
-- import qualified Data.ChunkedZip as Z
-- import           Data.Maybe
-- import qualified Data.Monoid as Mon
import           Data.List                      (nub)
import qualified Data.Map                       as Map
import           Data.Map                       (Map)
import           Data.Maybe
-- import           Data.Semigroup
-- import           Data.Set                        (Set)
-- import qualified Data.Set                        as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tree

-- import           Language.Javascript.JSaddle
-- import           Reflex
-- import           Reflex.Dom                             hiding (mainWidget)
import           Reflex.Dom.Core

--------------------------------------------------------------------------------

import qualified Reflex.Dom.HTML5.Attrs as A
-- import qualified Reflex.Dom.HTML5.Elements as E
import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Tree
-- import           Reflex.Dom.HTML5.Component.Common.RfpUtils

--------------------------------------------------------------------------------

-- import           Reflex.Dom.Component.NaiveI18n

--------------------------------------------------------------------------------

-- A simple way to define menu items.


data MenuItemConfig t a b = MenuItemConfig
    { _menuItemConfigMenuTag ∷ a
    , _menuItemConfigIcons ∷ [A.ClassName]
    , _menuItemConfigSetLabel ∷ Event t Text
    -- , _menuItemConfigDD ∷ Bool
    -- ^ True, if it is a drop down
    , _menuItemConfigOneOfGroup ∷ Maybe b
    -- ^ Only one of the items in the same group can be active at a time.
    }
    | MICSeparator
      { _MICSeparatorIcons ∷ [A.ClassName]
      }

instance Eq a ⇒ Eq (MenuItemConfig t a b) where
  (==) (MenuItemConfig t1 _ _ _) (MenuItemConfig t2 _ _ _) = t1 == t2
  (==) (MICSeparator _) (MICSeparator _) = True
  (==) _ _ = False

instance Show a ⇒ Show (MenuItemConfig t a b) where
  show (MenuItemConfig t1 _ _ _) = show t1
  show (MICSeparator _) = "mic sep"

instance (Reflex t, ActSretval a) ⇒ ActSretval (MenuItemConfig t a b) where
    defRetval = defMenuItemConfig defRetval

-- | Default value with input: no icons, events, is leaf, belongs to no groups.
defMenuItemConfig ∷ (Reflex t) ⇒ a → MenuItemConfig t a b
defMenuItemConfig a = MenuItemConfig a [] never Nothing

--------------------------------------------------------------------------------

data MenuItem t a = MenuItem
    { _menuItemMenuTag ∷ a
    , _menuItemIcons ∷ [A.ClassName]
    , _menuItemDD ∷ Bool -- True, if it is a drop down
    , _menuItemLabel ∷ Dynamic t Text
    }
    | MenuItemSep
      { _menuItemSepIcons ∷ [A.ClassName]
      }

instance Eq a ⇒ Eq (MenuItem t a) where
    (==) (MenuItem t1 _ _ _ ) (MenuItem t2 _ _ _) = t1 == t2
    (==) (MenuItemSep _) (MenuItemSep _) = True
    (==) _ _ = False
instance Show a ⇒ Show (MenuItem t a) where
  show (MenuItem t1 _ _ _) = show t1
  show (MenuItemSep _) = "mi sep"

instance (Reflex t, ActSretval a) ⇒ ActSretval (MenuItem t a) where
    defRetval = defMenuItem defRetval

-- | Default 'MenuItem' for tag @a@.
defMenuItem ∷ (Reflex t) ⇒ a → MenuItem t a
defMenuItem a = MenuItem a [] False (constDyn "")

--------------------------------------------------------------------------------

mic2mi ∷ (Reflex t, MonadHold t m, Show a)
       ⇒ MenuItemConfig t a b
       → m (MenuItem t a)
mic2mi mic2@MenuItemConfig{} = do
    dLbl ← holdDyn (T.pack . show $ _menuItemConfigMenuTag mic2) $
            _menuItemConfigSetLabel mic2
    pure $
        MenuItem
            (_menuItemConfigMenuTag mic2)
            (_menuItemConfigIcons mic2)
            False -- (_menuItemConfigDD mic2)
            dLbl
mic2mi (MICSeparator ico) = pure $ MenuItemSep ico

-- | This builds up a MenuItem tree from configs.
micTr2miTr ∷ (Reflex t, MonadHold t m, Show a)
                ⇒ Tree (MenuItemConfig t a b)
                → m (Tree (MenuItem t a))
micTr2miTr tr = do
    tr2 ← mapM mic2mi $ tr
    let ntr = mapTree (\r s → if null s
                                  then r
                                  else case r of
                                        MenuItem t i _d l → MenuItem t i True l
                                        MenuItemSep _     → r
                       ) tr2
        rl = rootLabel ntr
        sf = subForest ntr
        ret = case rl of
                MenuItem t i _d l → MenuItem t i False l
                MenuItemSep _     → rl
    pure $ Node ret sf -- don't mark the root node

mapTree ∷ (a → [Tree a] → b) → Tree a → Tree b
mapTree f tr = Node (f rl sf) (fmap (mapTree f) sf)
  where
    rl = rootLabel tr
    sf = subForest tr


-- | Collect from a configuration tree information about
-- groups, that is, each indivual group as a map element where
-- the key is the group name (user given tag @b@) and
-- value is a list of menutag (also user given @a@).
findOneOfGroups ∷ forall t a b. (Eq b, Ord b)
                ⇒ Tree (MenuItemConfig t a b) → Map b [a]
findOneOfGroups tr = Map.fromList $ zip uniqBs bGrps
  where
    -- bWithGroup = zip uniqBs bGrps
    bGrps = fmap (\b →
                    fmap _menuItemConfigMenuTag
                    $ filter ((==b) . fromJust
                                     . _menuItemConfigOneOfGroup
                              ) grps ) uniqBs
    uniqBs ∷ [b] = nub $ fromJust . _menuItemConfigOneOfGroup <$> grps
    grps = filter (isJust . _menuItemConfigOneOfGroup) . flatten $ tr


-- | Gives a Map where each node (except root) is a key
-- with parent node (menutag,path) as a value.
findParents ∷ forall a. (Eq a, Ord a)
            ⇒ Tree (a, [Int]) → Map a (a,[Int])
findParents tr = Map.fromList . mkList $ tr
  where
    -- r = rootLabel tr
    mkList ∷ Tree (a,[Int]) → [(a,(a,[Int]))]
    mkList tr2 = prLst
        ++ if null sf
            then []
            else concatMap mkList sf
      where
        rl = rootLabel tr2
        sf = subForest tr2
        sfNds = fmap rootLabel sf
        prLst = fmap (\n → (fst n,rl)) sfNds

-- | Gives a Map where each OneOfGroup is a key
-- with a parent node (menutag, path) as a value.
findParents4OneOfs ∷ forall t a b. (Eq a, Ord a, Eq b, Ord b)
                ⇒ Tree (MenuItemConfig t a b)
                → Map a (a,[Int])
                → Map b (a,[Int])
findParents4OneOfs tr mp = Map.fromList lst2
  where
    lst2 = fmap (\(k,v) →
                 let mv = lookup k uniqABs
                  in (fromJust mv, v)
                ) lst
    lst ∷ [(a,(a,[Int]))] = filter (\(k,_v) → k `elem` uniqAs) $ Map.toList mp
    uniqAs ∷ [a] = fmap fst uniqABs
    uniqABs ∷ [(a,b)] = nub $
        (\i →
          (_menuItemConfigMenuTag i
          , fromJust . _menuItemConfigOneOfGroup $ i
          )
        ) <$> grps
    grps = filter (isJust . _menuItemConfigOneOfGroup) . flatten $ tr



--------------------------------------------------------------------------------

data LRMenu2 a = LRMenu2
  { _lrMenu2Name ∷ Text
  , _lrMenu2Tree ∷ Tree a
  }

--------------------------------------------------------------------------------



evs4OneOfs2 ∷ forall a b t. (Reflex t, Show a, Eq a, Ord a, Show b, Eq b, Ord b)
             ⇒ Map b [a]
             → Event t (a, [Int])
             → [Event t (a, [Int])]
evs4OneOfs2 ogs eTgPth = eList
  where
    ks = Map.keys ogs
    eList ∷ [Event t (a, [Int])] =
            fmap (\k → 
                    let mVal = Map.lookup k ogs
                     in case mVal of
                         Just v →
                            -- traceEventWith (const "evs4OneOfs2") $
                            ffilter
                                (\n →
                                    let pg = fst n
                                     in pg `elem` v
                                ) eTgPth
                         Nothing → never
                 ) ks

parentEvents ∷ (Reflex t, Show a, Eq a, Ord a, Show b, Eq b, Ord b)
             ⇒ Map b (a,[Int]) → Event t (a, [Int])
             → [Event t (a, [Int])]
parentEvents mp eTgPth =
    if null elms
        then []
        else fmap (\e → ffilter ( (== e) . fst) eTgPth ) elms
  where
    elms = fmap fst . Map.elems $ mp

--------------------------------------------------------------------------------

compStateEv2TagEv ∷ forall a b t m. (Reflex t, MonadHold t m)
                  ⇒ Event t (CompState t ActNode (MenuItem t a))
                  → Either [MenuItemConfig t a b] (Tree (MenuItemConfig t a b))
                  → m (Event t a)
compStateEv2TagEv eCsAnMi eLstTr = do
    let eCe ∷ Event t (Event t [Int]) =
              (coincidence . fmap (fmap (getPath . _activeStateElemId)))
              . fmap _ceMUp . _csCompEvent <$> eCsAnMi
    eUpd ∷ Event t [Int] ← switchHold never eCe
    let fmd =
            case eLstTr of
                Left lst →
                    fmapMaybe (\iLst →
                                let i = head iLst
                                 in if i < length lst && i>=0
                                    then Just (lst Prelude.!! i)
                                    else Nothing
                            ) (fmap (drop 1) eUpd)
                Right tr →  fmapMaybe (getNodeAtPath tr) eUpd
    let eMt ∷ Event t a =
            -- traceEventWith (\a → "body, eMt:" ++ show a) $
            fmap _menuItemConfigMenuTag fmd
    pure eMt

--------------------------------------------------------------------------------

