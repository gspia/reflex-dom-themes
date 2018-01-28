{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.Component.Menu
Description : Menuitems, e.g., for a navbar and other menu-like structures.
Copyright   : (c) gspia 2017
License     : BSD3
Maintainer  : gspia

= Menu-structures

This module contains functions that can be used to construct menus.

* Define "tags" to be used (to be associated) with menuitems.
  Like data MIs = Mi1 | Mi2 ...
* Define the menuitems with MIDefRec.
* Depending on component, collect them to a list, tree or a forest.
* Also, each component needs a language-map mapping tags to the
  languages that are used.
* And feed them to a mk-function.
* Function returns an event telling what menuitem was used.

== Note

TODO! TODO! TODO! Namings inconsistency (draw vs mk).

-}

module Reflex.Dom.Component.Menu where

import Control.Applicative
-- import Control.Arrow ((&&&), arr)
-- import Control.Lens
import Control.Monad (mapM)
-- import Control.Monad.Fix (MonadFix)
-- import Control.Monad.Zip (mzipWith)
import Control.Monad.State.Lazy

-- import qualified Data.Map as Map
import qualified Data.ChunkedZip as Z
import Data.Maybe
import qualified Data.Monoid as Mon
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree

import Reflex.Dom.Core

--------------------------------------------------------------------------------

import Reflex.Dom.HTML5.Attrs as A
import Reflex.Dom.HTML5.Elements as E

--------------------------------------------------------------------------------

import Reflex.Dom.Component.NaiveI18n
import Reflex.Dom.Theme.Raw.BS4 as B

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- A simple way to define menu items.
--
data MIDefRec a = MIDefRec
  { _menuTag :: a
  , _icons :: [ClassName]
  , _uri :: Text
  , _selectable :: Bool
  } deriving (Eq, Show)


defRec2Cfg :: forall a t. (Show a, Eq a, Ord a, Reflex t)
           => MIDefRec a -> MenuItemConfig t a
defRec2Cfg (MIDefRec mt ico u s) = mkMenuItemCfg mt ico u (constDyn s)

mkMenuItemCfg :: forall a t. (Reflex t, Show a, Eq a, Ord a)
              => a -> [ClassName] -> Text -> Dynamic t Bool -> MenuItemConfig t a
mkMenuItemCfg mt ico url activable =
  MenuItemConfig mt ico never never
    activable (pure $ href (URL url) def)
    -- (constDyn activable) (pure $ href (URL url)  $ def)

--------------------------------------------------------------------------------


data MenuItemConfig t a = MenuItemConfig
  { _menuItemConfigMenuTag :: a
  , _menuItemConfigIcons :: [ClassName]
  , _menuItemConfigSetLabel :: Event t Text
  , _menuItemConfigSetActive :: Event t Bool
  , _menuItemConfigSetActivable :: Dynamic t Bool
  , _menuItemConfigMenuItemAttrs :: Dynamic t EA
  }
-- makeLenses ''MenuItemConfig

instance Eq a => Eq (MenuItemConfig t a) where
  (==) (MenuItemConfig t1 _ _ _ _ _) (MenuItemConfig t2 _ _ _ _ _) = t1 == t2
instance Show a => Show (MenuItemConfig t a) where
  show (MenuItemConfig t1 _ _ _ _ _) = show t1

--------------------------------------------------------------------------------

data MenuItem t a = MenuItem
  { _menuItemMenuTag :: a
  , _menuItemIcons :: [ClassName]
  , _menuItemdLabel :: Dynamic t Text
  , _menuItemdActive :: Dynamic t Bool
  , _menuItemdActivable :: Dynamic t Bool
  , _menuItemdEa :: Dynamic t EA
  , _menuItemeMouseC :: Event t a
  }
-- makeLenses ''MenuItem

instance Eq a => Eq (MenuItem t a) where
  (==) (MenuItem t1 _ _ _ _ _ _) (MenuItem t2 _ _ _ _ _ _) = t1 == t2
instance Show a => Show (MenuItem t a) where
  show (MenuItem t1 _ _ _ _ _ _) = show t1


-- newtype FForest a = FForest (Forest a)
newtype FForest a = FForest {unFF :: Forest a}
  deriving (Eq, Functor, Traversable, Foldable, Show)

instance Z.Zip FForest where
  zipWith :: forall a b c. (a -> b -> c) -> FForest a -> FForest b -> FForest c
  zipWith f (FForest a) (FForest b) = FForest $ go a b
    where
      go :: [Tree a] -> [Tree b] -> [Tree c]
      go [] _ = []
      go _ [] = []
      go (a2:as) (b2:bs) = Z.zipWith f a2 b2 : go as bs

-- instance Applicative FForest where
-- pure a = FForest $ [Node a []]
-- liftA2 (FForest as) (FForest bs) = undefined  -- base 4.10

instance Semigroup (FForest a) where
  (<>) (FForest a) (FForest b) = FForest $ a ++ b

-- | The top-level menu items are trees, where the root nodes contain
-- the label to be shown in the top-level menu bar.
-- If node has a sub-tree, it is a drop down menu. Each sub-tree
-- means further drop down menus.
type MenuForestConfig t a = FForest (MenuItemConfig t a)
type MenuForest t a = FForest (MenuItem t a)

mfc2lst :: MenuForestConfig t a -> [Tree (MenuItemConfig t a)]
-- mfc2lst (FForest mfc) = mfc
mfc2lst = unFF

--------------------------------------------------------------------------------

class Functor f => StructMethods f a where
  toList :: f a -> [a]


--------------------------------------------------------------------------------

-- |
-- We devide UI-components to the structural and visual parts.
-- Structural parts are collected into the '*Struct'-data. These take care of
-- reading the element-configurations and drawing the component and giving
-- back an event. The element configurations are given to the 'Struct' when
-- initializing the component.
--
-- The '*Conf'-data contains configuration options for the whole component.
--
data LRMenuStruct a = LRMenuStruct
  { _lrMenuStructLmenu :: FForest a
  , _lrMenuStructRmenu :: FForest a
  }
-- makeLenses ''LRMenuStruct

instance Eq a => Eq (LRMenuStruct a) where
  (==) (LRMenuStruct l1 r1) (LRMenuStruct l2 r2) = l1 == l2 && r1 == r2
instance Show a => Show (LRMenuStruct a) where
  show (LRMenuStruct l1 r1) = show l1 ++ "\n" ++ show r1

instance Functor LRMenuStruct where
  fmap :: forall a b. (a -> b) -> LRMenuStruct a -> LRMenuStruct b
  fmap f (LRMenuStruct lmenu rmenu)
    = LRMenuStruct (fmap f lmenu) (fmap f rmenu)

instance Z.Zip LRMenuStruct where
  zipWith f (LRMenuStruct almn armn) (LRMenuStruct blmn brmn)
    = LRMenuStruct (Z.zipWith f almn blmn) (Z.zipWith f armn brmn)

-- asList :: LRMenuStruct a -> [a]
-- asList (LRMenuStruct l r) = f l ++ f r
--   where
--     f = concat . fmap flatten . unFF

instance StructMethods LRMenuStruct a where
  toList :: LRMenuStruct a -> [a]
  toList (LRMenuStruct l r) = f l ++ f r
    where
      f = concatMap flatten . unFF


data LRMenuConf a = LRMenuConf
  { _lrMenuConfName :: Text
  , _lrMenuConfStruct :: LRMenuStruct a
  }
-- makeLenses ''LRMenuConf

instance Eq a => Eq (LRMenuConf a) where
  (==) (LRMenuConf n1 s1) (LRMenuConf n2 s2) = n1 == n2 && s1 == s2
instance Show a => Show (LRMenuConf a) where
  show (LRMenuConf n1 _) = show n1


--------------------------------------------------------------------------------

newtype LMenuStruct a = LMenuStruct
  { _lMenuStructLmenu :: FForest a
  }
-- makeLenses ''LMenuStruct

instance Eq a => Eq (LMenuStruct a) where
  (==) (LMenuStruct l1) (LMenuStruct l2) = l1 == l2
instance Show a => Show (LMenuStruct a) where
  show (LMenuStruct l1) = show l1

instance Functor LMenuStruct where
  fmap :: forall a b. (a -> b) -> LMenuStruct a -> LMenuStruct b
  fmap f (LMenuStruct lmenu)
    = LMenuStruct (fmap f lmenu)

instance Z.Zip LMenuStruct where
  zipWith f (LMenuStruct almn) (LMenuStruct blmn)
    = LMenuStruct (Z.zipWith f almn blmn)

instance StructMethods LMenuStruct a where
  toList :: LMenuStruct a -> [a]
  toList (LMenuStruct l) = (concatMap flatten . unFF) l


data LMenuConf a = LMenuConf
  { _lMenuConfName :: Text
  , _lMenuConfStruct :: LMenuStruct a
  }
-- makeLenses ''LMenuConf

instance Eq a => Eq (LMenuConf a) where
  (==) (LMenuConf n1 s1) (LMenuConf n2 s2) = n1 == n2 && s1 == s2
instance Show a => Show (LMenuConf a) where
  show (LMenuConf n1 _) = show n1

--------------------------------------------------------------------------------

newtype LstMenuStruct a = LstMenuStruct
  { _lstMenuStructMenu :: [a]
  }
-- makeLenses ''LstMenuStruct

instance Eq a => Eq (LstMenuStruct a) where
  (==) (LstMenuStruct l1) (LstMenuStruct l2) = l1 == l2
instance Show a => Show (LstMenuStruct a) where
  show (LstMenuStruct l1) = show l1

instance Functor LstMenuStruct where
  fmap :: forall a b. (a -> b) -> LstMenuStruct a -> LstMenuStruct b
  fmap f (LstMenuStruct mn)
    = LstMenuStruct (fmap f mn)

instance Z.Zip LstMenuStruct where
  zipWith f (LstMenuStruct almn) (LstMenuStruct blmn)
    = LstMenuStruct (Z.zipWith f almn blmn)

instance StructMethods LstMenuStruct a where
  toList :: LstMenuStruct a -> [a]
  toList (LstMenuStruct l) = l


data LstMenuConf a = LstMenuConf
  { _lstMenuConfName :: Text
  , _lstMenuConfStruct :: LstMenuStruct a
  }
-- makeLenses ''LstMenuConf

instance Eq a => Eq (LstMenuConf a) where
  (==) (LstMenuConf n1 s1) (LstMenuConf n2 s2) = n1 == n2 && s1 == s2
instance Show a => Show (LstMenuConf a) where
  show (LstMenuConf n1 _) = show n1


--------------------------------------------------------------------------------

data LstDropdownStruct a = LstDropdownStruct
  { _lstDropdownStructRoot :: a
  , _lstDropdownStructElems :: [a]
  }
-- makeLenses ''LstDropdownStruct

instance Eq a => Eq (LstDropdownStruct a) where
  (==) (LstDropdownStruct r1 els1) (LstDropdownStruct r2 els2)
    = r1 == r2 && els1 == els2
instance Show a => Show (LstDropdownStruct a) where
  show (LstDropdownStruct r els) = show r ++ "\n" ++ show els

instance Functor LstDropdownStruct where
  fmap :: forall a b. (a -> b) -> LstDropdownStruct a -> LstDropdownStruct b
  fmap f (LstDropdownStruct r els)
    = LstDropdownStruct (f r) (fmap f els)

instance Z.Zip LstDropdownStruct where
  zipWith f (LstDropdownStruct r1 els1) (LstDropdownStruct r2 els2)
    = LstDropdownStruct (f r1 r2) (Z.zipWith f els1 els2)

instance StructMethods LstDropdownStruct a where
  toList :: LstDropdownStruct a -> [a]
  toList (LstDropdownStruct _r els) = els


data LstDropdownConf a = LstDropdownConf
  { _lstDropdownConfName :: Text
  , _lstDropdownConfStruct :: LstDropdownStruct a
  }
-- makeLenses ''LstDropdownConf

instance Eq a => Eq (LstDropdownConf a) where
  (==) (LstDropdownConf n1 s1) (LstDropdownConf n2 s2) = n1 == n2 && s1 == s2
instance Show a => Show (LstDropdownConf a) where
  show (LstDropdownConf n1 _) = show n1


--------------------------------------------------------------------------------
  -- Helpers
  --

-- liftA2 will produce all combinations if given two lists,
-- which is the reason it doesn't work here.
-- We want to maintain both the type and structure (say, length
-- or tree form).
biTreeMap :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
biTreeMap f (Node r1 fo1) (Node r2 fo2) =
    Node (f r1 r2) $ uncurry (biTreeMap f) <$> zip fo1 fo2
  -- Node (f r1 r2) $ (\(a,b) -> biTreeMap f a b) <$> zip fo1 fo2

numberTree :: Eq a => Tree a -> State Int (Tree Int)
numberTree (Node _ []) = do
  i <- get
  put (i+1)
  return (Node i [])
numberTree (Node _ forest) = do
  i <- get
  put (i+1)
  subf <- mapM numberTree forest
  return (Node i subf)

numTree :: (Eq a) => Tree a -> Tree Int
numTree t = evalState (numberTree t) 0

-- numTree2 :: (Eq a) => Tree a -> (Tree Int, Int)
-- numTree2 t = runState (numberTree t) 0


numberForest :: Eq a => Forest a -> State Int (Forest Int)
numberForest = mapM numberTree

numForest :: (Eq a) => Forest a -> Forest Int
numForest t = evalState (numberForest t) 0

-- numForest2 :: (Eq a) => Forest a -> (Forest Int, Int)
-- numForest2 t = runState (numberForest t) 0


numberFForest  :: (Eq a) => FForest a -> State Int (FForest Int)
numberFForest (FForest ft) = FForest <$> numberForest ft

numFForest :: (Eq a) => FForest a -> FForest Int
numFForest (FForest t) = FForest $ evalState (numberForest t) 0

-- numFForest2 :: (Eq a) => FForest a -> (FForest Int, Int)
-- numFForest2 t = runState (numberFForest t) 0

numberLRConf :: Eq a => LRMenuStruct a-> State Int (LRMenuStruct Int)
numberLRConf (LRMenuStruct lFF rFF) = do
  [f1,f2] <- mapM numberFForest [lFF,rFF]
  pure $ LRMenuStruct f1 f2

numLRConf :: (Eq a) => LRMenuStruct a -> LRMenuStruct Int
numLRConf t = evalState (numberLRConf t) 0

numberLConf :: Eq a => LMenuStruct a-> State Int (LMenuStruct Int)
numberLConf (LMenuStruct lFF) = do
  [f1] <- mapM numberFForest [lFF]
  pure $ LMenuStruct f1

numLConf :: (Eq a) => LMenuStruct a -> LMenuStruct Int
numLConf t = evalState (numberLConf t) 0



getNodeByInt  :: Tree (a,Int) -> Int -> Maybe (a,Int)
getNodeByInt tr i =
  if null x
     then Nothing
     else Just $ head x
  where
    x = getNodeI tr i
    getNodeI :: Tree (a,Int) -> Int -> [(a,Int)]
    getNodeI (Node xx@(_,j) forest) ii =
      if ii == j
         then [xx]
         else getFo i forest
      where
        getFo :: Int -> [Tree (a,Int)] -> [(a,Int)]
        getFo _ [] =  []
        getFo iii (tr2:lst) =
          if null y
             then getFo iii lst
             else y
          where
            y = getNodeI tr2 i

findElemLR :: LRMenuStruct (a,Int) -> Int -> Maybe (a,Int)
findElemLR (LRMenuStruct l r) i =
  case ml of
    Just _  -> ml
    Nothing -> mr
  where
    ml = findNodeByIntFF2 l i
    mr = findNodeByIntFF2 r i

findElemL :: LMenuStruct (a,Int) -> Int -> Maybe (a,Int)
findElemL (LMenuStruct l) = findNodeByIntFF2 l

findNodeByIntFF2 :: FForest (a,Int) -> Int -> Maybe (a,Int)
findNodeByIntFF2 (FForest a) = findNodeByIntFF a

findNodeByIntFF  :: [Tree (a,Int)] -> Int -> Maybe (a,Int)
findNodeByIntFF [] _ = Nothing
findNodeByIntFF (tr:trs) i =
  if null x
     then findNodeByIntFF trs i
     else Just $ head x
  where
    x = getNodeI tr i
    getNodeI :: Tree (a,Int) -> Int -> [(a,Int)]
    getNodeI (Node x2@(_,j) forest) i2 =
      if i2 == j
         then [x2]
         else getFo i forest
      where
        getFo :: Int -> [Tree (a,Int)] -> [(a,Int)]
        getFo _ [] =  []
        getFo i3 (tr2:lst) =
          if null y
             then getFo i3 lst
             else y
          where
            y = getNodeI tr2 i3

--------------------------------------------------------------------------------

-- Lang-support.
updMenuItemConfig :: (Reflex t, Show a, Eq a, Ord a)
                  => Event t (LangMap a)
                  -> MenuItemConfig t a
                  -> Event t Bool
                  -> MenuItemConfig t a
updMenuItemConfig eLangMap mic eB2 =
    mic { _menuItemConfigSetActive = eB2
        , _menuItemConfigSetLabel
            = fmapMaybe (mtLabel (_menuItemConfigMenuTag mic)) eLangMap
        }
  -- mic & menuItemConfig_setActive .~ eB2
  --     & menuItemConfig_setLabel .~
  --       (fmapMaybe (mtLabel (_menuItemConfigMenuTag mic)) eLangMap )

--------------------------------------------------------------------------------

mkLRMenuBar :: forall a t m. (Show a, Eq a, Ord a, MonadWidget t m)
            => Dynamic t (LangMap a)
            -> LRMenuConf (MenuItemConfig t a)
            -> m (Event t a)
mkLRMenuBar dLangMap lrMenuC =
  eNav navAttrs $ do
    _e0 <- eAC (setClasses [bsNavbarBrand] $ hR "#" def) $ text
      $ _lrMenuConfName lrMenuC
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    -- eDiv divCl $ do drawLRMenuBar dLangMap lMenuC rMenuC
    eDiv divCl $ drawLRMenuBar dLangMap (_lrMenuConfStruct lrMenuC)
  where
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ btSubmit
      $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
      $ aControls "navbarSupportedContent" $ aExpanded "false"
      $ aLabel "Toggle navigation" def
    divCl = setClasses [bsCollapse, bsNavbarCollapse]
      $ id_ "navbarSupportedContent" def
    hR u = href (URL u)


drawLRMenuBar :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
              => Dynamic t (LangMap a)
              -> LRMenuStruct (MenuItemConfig t a)
              ->  m (Event t a)
drawLRMenuBar dLangMap lrMitc = do
  ePB <- getPostBuild
  let eLangMap = leftmost [updated dLangMap, tag (current dLangMap) ePB]
      nLR = numLRConf lrMitc
      lrCfs = Z.zip lrMitc nLR :: LRMenuStruct (MenuItemConfig t a, Int)
  rec
    -- let updF = \i -> updMenuItemConfig eLangMap
    let updF i = updMenuItemConfig eLangMap
          (fst $ fromJust $ findElemLR lrCfs i)
          (fst $ fromJust $ findElemLR eForest i)
        -- lrMenuC :: LRMenuStruct (MenuItemConfig t a)
        lrMenuC = fmap (\(_,i) -> updF i) lrCfs
    menuLR <- drawLR lrMenuC
    let eLRs = ffor menuLR
          (\mi -> tag ((current . _menuItemdActivable) mi) (_menuItemeMouseC mi))
        -- next is forest of bools that we lazily use to update configs above.
        eForest = Z.zip (fmap (mkEvTree $ Z.zip menuLR eLRs) menuLR) nLR
  pure $ leftmost $ toList $ fmap _menuItemeMouseC menuLR
  where
    mkEvTree mLst mi = leftmost $ toList $ fmap (\(mj,eAct) ->
        if mi == mj
           then True <$ _menuItemeMouseC mj
           else False <$ ffilter id eAct
        ) mLst
    drawLR :: LRMenuStruct (MenuItemConfig t a) -> m (LRMenuStruct (MenuItem t a))
    drawLR (LRMenuStruct l r) = do
      mL <- drawMenuItemForest l
      mR <- drawMenuItemForest r
      pure $ LRMenuStruct mL mR

--------------------------------------------------------------------------------

mk1LMenuBar :: forall a t m. (Show a, Eq a, Ord a, MonadWidget t m)
            => Dynamic t (LangMap a) -> LMenuConf (MenuItemConfig t a)
            -> m (Event t a)
mk1LMenuBar dLangMap lMenuC =
  eNav navAttrs $ do
    _e0 <- eAC (setClasses [bsNavbarBrand] $ hR "#" def) $ text
      $ _lMenuConfName lMenuC
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    mk1LMenuBarNoHead dLangMap $ _lMenuConfStruct lMenuC
  where
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ btSubmit
      $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
      $ aControls "navbarSupportedContent" $ aExpanded "false"
      $ aLabel "Toggle navigation" def
    hR u = href (URL u)


mk1LMenuBarNoHead :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
                  => Dynamic t (LangMap a)
                  -> LMenuStruct (MenuItemConfig t a)
                  -> m (Event t a)
mk1LMenuBarNoHead dLangMap lMitc = do
  ePB <- getPostBuild
  let eLangMap = leftmost [updated dLangMap, tag (current dLangMap) ePB]
      nL = numLConf lMitc
      lCfs = Z.zip lMitc nL :: LMenuStruct (MenuItemConfig t a, Int)
  eDiv aDiv $ mdo
    let
      updF i = updMenuItemConfig eLangMap
          (fst $ fromJust $ findElemL lCfs i)
          (fst $ fromJust $ findElemL eForest i)
      lMenuC = fmap (\(_,i) -> updF i) lCfs
    menuL <- drawL lMenuC
    let eLs = ffor menuL
          (\mi -> tag ((current . _menuItemdActivable) mi) (_menuItemeMouseC mi))
        eForest = Z.zip (fmap (mkEvTree $ Z.zip menuL eLs) menuL) nL
    pure $ leftmost $ toList $ fmap _menuItemeMouseC menuL
  where
    aDiv = setClasses [bsCollapse, bsNavbarCollapse]
      $ id_ "navbarSupportedContent" def
    mkEvTree mLst mi = leftmost $ toList $ fmap (\(mj,eAct) ->
        if mi == mj
           then True <$ _menuItemeMouseC mj
           else False <$ ffilter id eAct
        ) mLst
    drawL :: LMenuStruct (MenuItemConfig t a) -> m (LMenuStruct (MenuItem t a))
    drawL (LMenuStruct l) = do
      mL <- drawMenuItemForest l
      pure $ LMenuStruct mL

--------------------------------------------------------------------------------

mkMenuBarList :: forall a t m. (Show a, Eq a, Ord a, MonadWidget t m)
              => Dynamic t (LangMap a) -> LstMenuConf (MenuItemConfig t a)
              -> m (Event t a)
mkMenuBarList dLangMap lstMenuC =
  eNav navAttrs $ do
    _e0 <- eAC (setClasses [bsNavbarBrand] $ hR "#" def) $ text
      $ _lstMenuConfName lstMenuC
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    eDiv aDiv $ mkMenuBarListNoHead dLangMap $ _lstMenuConfStruct lstMenuC
  where
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ btSubmit
      $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
      $ aControls "navbarSupportedContent" $ aExpanded "false"
      $ aLabel "Toggle navigation" def
    aDiv = setClasses [bsCollapse, bsNavbarCollapse]
      $ id_ "navbarSupportedContent" def
    hR u = href (URL u)


-- Do we need this method? Combine with the above one? TODO! TODO! TODO!
mkMenuBarListNoHead :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
                    => Dynamic t (LangMap a)
                    -> LstMenuStruct (MenuItemConfig t a)
                    -> m (Event t a)
mkMenuBarListNoHead dLangMap (LstMenuStruct mic) = do
  ePB <- getPostBuild
  let -- is = [0..(length mic - 1)]
      eLangMap = leftmost [updated dLangMap, tag (current dLangMap) ePB]
  eUl aUl $ evHandlerLst mkMenuItem eLangMap mic
  -- eUl aUl $ mdo
  --   let mics = fmap (\i -> updMenuItemConfig eLangMap (mic !! i) (eCls !! i)) is
  --   menuLst :: [MenuItem t a] <- mapM mkMenuItem mics
  --   let eActs = ffor menuLst
  --         (\mi -> tag ((current . _menuItemdActivable) mi) (_menuItemeMouseC mi))
  --       eMIs = zip eActs is
  --       eOth = fmap (\(e,i) -> (e, fmap fst $ filter ((i /=) . snd) eMIs)) eMIs
  --       -- (eCl1:eCls2) = fmap (mkEvCls eMIs) is
  --       (eCl1:eCls2) =
  --         fmap (\(eT, eFs) -> leftmost $ [True <$ eT]
  --           ++ fmap (\e -> False <$ ffilter id e) eFs) eOth
  --       -- ensure that the first item will be active on start
  --       eCls = (leftmost [True <$ ePB, eCl1]) : eCls2
  --   pure $ leftmost $ fmap _menuItemeMouseC menuLst
  where
    aUl = setClasses [bsNavbarNav, bsMrAuto] def

{-
<nav class="navbar navbar-expand-lg navbar-light bg-light">
  <a class="navbar-brand" href="#">Navbar</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
    <span class="navbar-toggler-icon"></span>
  </button>

  <div class="collapse navbar-collapse" id="navbarSupportedContent">
    <ul class="navbar-nav mr-auto">
      <li class="nav-item active">
        <a class="nav-link" href="#">Home <span class="sr-only">(current)</span></a>
      </li>
      <li class="nav-item">
        <a class="nav-link" href="#">Link</a>
      </li>
      <li class="nav-item">
        <a class="nav-link disabled" href="#">Disabled</a>
      </li>
    </ul>
    <form class="form-inline my-2 my-lg-0">
      <input class="form-control mr-sm-2" type="text" placeholder="Search" aria-label="Search">
      <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Search</button>
    </form>
  </div>
</nav>
-}

--------------------------------------------------------------------------------

drawMenuItemForest :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
                 => MenuForestConfig t a -> m (MenuForest t a)
drawMenuItemForest mfc' = do
  let FForest mfc = mfc'
  eUl aUl $ do
    f <- mapM (\mtc@(Node rmic streec) ->
      if null streec
         then do
           mi <- mkMenuItem rmic
           pure $ Node mi []
         else eLi aLi $ drawDropdownTreeNavbar "navbarMIforest" mtc
      ) mfc
    pure $ FForest f
  where
    aLi = setClasses [bsNavItem, bsDropdown] def
    aUl = setClasses [bsNavbarNav, bsMrAuto] def
    -- hR u = href (URL u)

--------------------------------------------------------------------------------

commonMINB :: forall a t m.  (MonadWidget t m, Show a, Eq a, Ord a)
           => MenuItemConfig t a -> m (a, [ClassName], Dynamic t Text)
commonMINB mic = do
  let txt = (T.pack . show) $ _menuItemConfigMenuTag mic
      ico = _menuItemConfigIcons mic
      mtag = _menuItemConfigMenuTag mic
  dLabel   <- holdDyn txt $ _menuItemConfigSetLabel mic
  pure (mtag, ico, dLabel)

commonActivity :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
               => MenuItemConfig t a
               -> m (Dynamic t EA, Dynamic t Bool, Dynamic t Bool)
commonActivity mic = do
  dActive2 <- holdDyn False $ _menuItemConfigSetActive mic
  let dActivable = _menuItemConfigSetActivable mic
      dActive = (&&) <$> dActive2 <*> dActivable
      dEa2 = zipDynWith setActEa dActive $ _menuItemConfigMenuItemAttrs mic
  pure (dEa2, dActive, dActivable)
    where
      setActEa b ea =
        if b
           then setClasses [bsNavLink, bsActive] ea
           else setClasses [bsNavLink] ea


mkMenuItem :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
           => MenuItemConfig t a -> m (MenuItem t a)
mkMenuItem mic = do
  -- let txt = (T.pack . show) $ _menuItemConfigMenuTag mic
  --     ico = _menuItemConfigIcons mic
  --     mtag = _menuItemConfigMenuTag mic
  -- dLabel   <- holdDyn txt $ _menuItemConfigSetLabel mic
  (mtag, ico, dLabel) <- commonMINB mic
  -- dActive2 <- holdDyn False $ _menuItemConfigSetActive mic
  (dEa2, dActive, dActivable) <- commonActivity mic
  -- let dActivable = _menuItemConfigSetActivable mic
  --     dActive = (&&) <$> dActive2 <*> dActivable
  --     dEa2 = zipDynWith setActEa dActive $ _menuItemConfigMenuItemAttrs mic
  let dLi = setActLi <$> dActive <*> def
      setActLi b ea =
        if b
           then setClasses [bsNavItem, bsActive] ea
           else setClasses [bsNavItem] ea
      -- setActEa b ea =
      --   if b
      --      then setClasses [bsNavLink, bsActive] ea
      --      else setClasses [bsNavLink] ea
  -- eLi (setClasses [bsNavItem] def) $ do
  eLiD dLi $ do
    -- (e,_) <- eAD' dEa2 $ do
    (e,_) <- eA' (setClasses [bsNavLink] def) $ do
      if null ico
         then blank
         else eSpan (setClasses ico def) blank
      dynText dLabel
      dyn $ ffor dActive $ \b ->
              if b
                 then eSpan (setClasses [bsSrOnly] def) $ text "(current)"
                 else blank
    let eClick = mtag <$ domEvent Click e
    pure $ MenuItem mtag ico dLabel dActive dActivable dEa2 eClick

--------------------------------------------------------------------------------

-- <div class="btn-group">
--   <button type="button" class="btn btn-danger dropdown-toggle"
--      data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
--     Action
--   </button>
--   <div class="dropdown-menu">
--     <a class="dropdown-item" href="#">Action</a>
--     <a class="dropdown-item" href="#">Another action</a>
--     <a class="dropdown-item" href="#">Something else here</a>
--     <div class="dropdown-divider"></div>
--     <a class="dropdown-item" href="#">Separated link</a>
--   </div>
-- </div>


-- TODO! TODO! TODO! Namings inconsistency (draw vs mk).
drawDropDownNavbar :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
             => Dynamic t (LangMap a) -> Tree (MenuItemConfig t a)
             -> m (Event t a)
drawDropDownNavbar dLangMap ddCfg = mdo
  -- ePB <- getPostBuild
  let
      nTree = numTree ddCfg
      eLangMap = updated dLangMap
      -- here we want to feed the events back
      ddcfs = biTreeMap (,) ddCfg nTree
      updcTree :: Tree (MenuItemConfig t a)
        = fmap (\(_,i) -> updMenuItemConfig eLangMap
          (fst $ fromJust $ getNodeByInt ddcfs i)
          (fst $ fromJust $ getNodeByInt eTree2 i)) ddcfs
  --
  menuTree <- drawDropdownTreeNavbar "dropDown" updcTree -- TODO TODO TODO
  let menuLst = flatten menuTree
      treeBact :: Tree (Behavior t Bool)
        = fmap (current . _menuItemdActivable) menuTree
      -- treeEact = biTreeMap (\b mi -> tag b (_menuItemeMouseC mi)) treeBact menuTree
      treeEact = liftA2 (\b mi -> tag b (_menuItemeMouseC mi)) treeBact menuTree
      lstEacts = flatten treeEact
      menuLst2 = zip lstEacts menuLst
      -- eClk was pressed (deliver it)
      eClk :: Event t a = leftmost $ fmap _menuItemeMouseC menuLst
      -- Here we want to construct the event tree of leftmosts.
      eTree :: Tree (Event t Bool) = fmap (mkEvTree menuLst2) menuTree
      eTree2 = liftA2 (,) eTree nTree
      -- eTree2 = biTreeMap (,) eTree nTree
  pure eClk
    where
      mkEvTree mLst mi = leftmost $ fmap (\(eAct, mj) ->
          if mi == mj
             then True <$ _menuItemeMouseC mj
             else False <$ ffilter id eAct
          ) mLst


--------------------------------------------------------------------------------

mkDropdownList
  :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
  => Dynamic t (LangMap a)
  -> LstDropdownConf (MenuItemConfig t a)
  -> m (Event t a)
mkDropdownList dLangMap
    (LstDropdownConf _nm (LstDropdownStruct micRoot micElems)) = do
  ePB <- getPostBuild
  let
    -- is = [0..(length micElems - 1)]
    eLangMap = leftmost [updated dLangMap, tag (current dLangMap) ePB]
    micR = updMenuItemConfig eLangMap micRoot never
  eDiv (setClasses [bsBtnGroup] def) $ do
    _rootItem <- mkDDrootLabel micR
    eDiv (setClasses [bsDropdownMenu] def) $
      evHandlerLst mkDDmenuItem eLangMap micElems
    -- eDiv (setClasses [bsDropdownMenu] def) $ mdo
    --   let
    --     mics = fmap (\i -> updMenuItemConfig eLangMap (micElems!!i) (eCls!!i)) is
    --   ddLst <- mapM mkDDmenuItem mics
    --   let
    --     eActs = ffor ddLst
    --       (\mi -> tag ((current . _menuItemdActivable) mi) (_menuItemeMouseC mi))
    --     eMIs = zip eActs is
    --     eOth = fmap (\(e,i) -> (e, fmap fst $ filter ((i /=) . snd) eMIs)) eMIs
    --     eCls = fmap (\(eT, eFs) -> leftmost $ [True <$ eT]
    --         ++ fmap (\e -> False <$ ffilter id e) eFs) eOth
    --   pure $ leftmost $ fmap _menuItemeMouseC ddLst


evHandlerLst
  :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
  => (MenuItemConfig t a -> m (MenuItem t a))
  -> Event t (LangMap a)
  -> [MenuItemConfig t a]
  -> m (Event t a)
evHandlerLst fMK eLangMap micEls = mdo
  let is = [0..(length micEls -1)]
      mics = fmap (\i -> updMenuItemConfig eLangMap (micEls!!i) (eCls!!i)) is
  meLst <- mapM fMK mics
  let
    eActs = ffor meLst
      (\mi -> tag ((current . _menuItemdActivable) mi) (_menuItemeMouseC mi))
    eMIs = zip eActs is
    eOth = fmap (\(e,i) -> (e, fst <$> filter ((i /=) . snd) eMIs)) eMIs
    eCls = fmap (\(eT, eFs) -> leftmost $ (True <$ eT) :
        fmap (\e -> False <$ ffilter id e) eFs) eOth
  pure $ leftmost $ fmap _menuItemeMouseC meLst

--------------------------------------------------------------------------------

drawDropdownTreeNavbar
  :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
  => Text -> Tree (MenuItemConfig t a) -> m (Tree (MenuItem t a))
drawDropdownTreeNavbar lblPrefix micTree = do
  let rCfg = rootLabel micTree
      sfCfg = subForest micTree
      -- lblName = "navbarDropdownMenuLink"
      lblName = lblPrefix <> (T.pack . show $ _menuItemConfigMenuTag rCfg)
      aDivA = setClasses [bsDropdownMenu] $ aLabelledby lblName def
  rootItem <- if null sfCfg
                 then mkDDmenuItem rCfg
                 else mkDDrootLabelNavbar rCfg lblName
  sfItems <- if null sfCfg
                then pure []
                else eDiv aDivA
                  $ mapM (drawDropdownTreeNavbar lblPrefix) sfCfg
  pure $ Node rootItem sfItems

--------------------------------------------------------------------------------

mkDDrootLabel :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
              => MenuItemConfig t a -> m (Event t a)
mkDDrootLabel mic = do
  let -- txt = (T.pack . show) $ _menuItemConfigMenuTag mic
      -- _ico = _menuItemConfigIcons mic
      -- mtag = _menuItemConfigMenuTag mic
      dBtnAttrs = setClasses [bsBtn, bsDropdownToggle] $
                btButton $ dToggle "dropdown" $ aHaspopup "true" def
  (mtag, _ico, dLabel) <- commonMINB mic
  -- dLabel <- holdDyn txt $ _menuItemConfigSetLabel mic
  (e,_) <- eButton' dBtnAttrs $ dynText dLabel
  -- (e,_) <- eAD' dEa $ dynText dLabel
  let eClick = mtag <$ domEvent Click e
  pure eClick
  -- pure $ MenuItem mtag ico dLabel
  --  (constDyn False) (constDyn False) dBtnAttrs eClick
  -- We cannot return dBtnAttrs as it is of button type. (And other menuitems
  -- are a's.) Maybe event is enough for this one.


mkDDrootLabelNavbar :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
              => MenuItemConfig t a -> Text -> m (MenuItem t a)
mkDDrootLabelNavbar mic lblName = do
  -- let txt = (T.pack . show) $ _menuItemConfigMenuTag mic
  --     ico = _menuItemConfigIcons mic
  --     mtag = _menuItemConfigMenuTag mic
  (mtag, ico, dLabel) <- commonMINB mic
  -- dLabel <- holdDyn txt $ _menuItemConfigSetLabel mic
      -- dEa = _menuItemConfigMenuItemAttrs mic
  -- dActive2 <- holdDyn False $ _menuItemConfigSetActive mic
  -- let dActivable = _menuItemConfigSetActivable mic
  --     dActive = (&&) <$> dActive2 <*> dActivable
  let dEa = aDropA Mon.<> _menuItemConfigMenuItemAttrs mic
  -- Note, we don't set classes or other things based on active state.
  (e,_) <- eAD' dEa $ dynText dLabel
  let eClick = mtag <$ domEvent Click e
  pure $ MenuItem mtag ico dLabel (constDyn False) (constDyn False) dEa eClick
  where
    aDropA = setClasses [bsNavLink, bsDropdownToggle]
      -- $ href (URL "#") $ id_ "navbarDropdownMenuLink"
      $ href (URL "#") $ id_ lblName
      $ dToggle "dropdown"
      $ aHaspopup "true" $ aExpanded "false"
      def

--------------------------------------------------------------------------------

mkDDmenuItem :: forall a t m. (MonadWidget t m, Show a, Eq a, Ord a)
             => MenuItemConfig t a -> m (MenuItem t a)
mkDDmenuItem mic = do
  -- let txt = (T.pack . show) $ _menuItemConfigMenuTag mic
  --     ico = _menuItemConfigIcons mic
  --     mtag = _menuItemConfigMenuTag mic
  --     _dEa = _menuItemConfigMenuItemAttrs mic
  -- dLabel <- holdDyn txt $ _menuItemConfigSetLabel mic
  (mtag, ico, dLabel) <- commonMINB mic
  (dEa2, dActive, dActivable) <- commonActivity mic
  -- dActive2 <- holdDyn False $ _menuItemConfigSetActive mic
  -- let dActivable = _menuItemConfigSetActivable mic
  --     dActive = (&&) <$> dActive2 <*> dActivable
  --     dEa2 = zipDynWith setActEa dActive $ _menuItemConfigMenuItemAttrs mic
  (e,_) <- eAD' dEa2 $ do
    if null ico
       then blank
       else eSpan (setClasses ico def) blank
    dynText dLabel
    -- text "act:"
    -- dynText $ T.pack . show <$> dActive
  let eClick = mtag <$ domEvent Click e
  pure $ MenuItem mtag ico dLabel dActive dActivable dEa2 eClick
  -- where
  --   setActEa b ea =
  --     if b
  --        then setClasses [bsDropdownItem, bsActive] ea
  --        else setClasses [bsDropdownItem] ea
    -- aA = setClasses [bsDropdownItem] $ href (URL "#") def
      -- dLi = setActLi <$> dActive <*> def
      -- setActLi b ea =
      --   if b
      --      then setClasses [bsNavItem, bsActive] ea
      --      else setClasses [bsNavItem] ea

--------------------------------------------------------------------------------

