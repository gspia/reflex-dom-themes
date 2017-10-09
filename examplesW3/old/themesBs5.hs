{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad (join, mapM)
import Reflex.Dom.Core
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle (JSM, liftJSM)
import GHCJS.Types
import Language.Javascript.JSaddle

#ifdef ghcjs_HOST_OS
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

import Reflex.Dom.HTML5.Attrs as A
import Reflex.Dom.HTML5.Elements as E

import Reflex.Dom.Themes.Raw.BS4 as B
import Reflex.Dom.Icons.Raw.FA as FA

------------------------------------------------------------------------------
------------------------------------------------------------------------------

main :: IO ()
#ifdef ghcjs_HOST_OS
main = liftJSM mainW
#else
main = run mainW
#endif

mainW :: JSM ()
mainW = mainWidget bodyEl


bodyEl :: (MonadWidget t m) => m ()
bodyEl = do
  miD :: Dynamic t MenuItem <- bs4Nav
  eH1 def $ text "Welcome to reflex-dom-themes"
  -- let _h = bs4N
  -- _h <- bs4N
  showMenuContent miD

showMenuContent :: MonadWidget t m => Dynamic t MenuItem -> m ()
showMenuContent mi = do
  let mt = fmap (T.pack . show) mi
  text "The selected menu items was: "
  dynText mt

-- "name" and "icon"
data MenuItem = MenuItem Text ClassName
  deriving (Eq,Show)

data MI t
  = MI { miClicked :: Event t ()
       , miItem    :: MenuItem
       }

menuItems :: [MenuItem]
menuItems =
  [ MenuItem "Active" faAutomobile
  , MenuItem "Link" faAreaChart
  , MenuItem "Another link" faBlind
  , MenuItem "Link4" faBell
  ]
    where
      cl = ClassName ""

menuDyn :: Reflex t => [Dynamic t ClassName]
menuDyn = [ constDyn bsActive , constDyn cl , constDyn cl , constDyn cl ]
    where cl = ClassName ""

menuDyn2 :: Reflex t => [Dynamic t ClassName]
menuDyn2 = [ constDyn cl , constDyn cl , constDyn cl , constDyn cl ]
    where cl = ClassName ""


mkMenuItem :: MonadWidget t m => EA -> MenuItem -> m (MI t)
mkMenuItem cls mi@(MenuItem txt ico) = do
  (e,_) <- eA' cls $ text txt
  return $ MI (domEvent Click e) mi

mkMenuItemD :: MonadWidget t m => Dynamic t EA -> MenuItem -> m (MI t)
mkMenuItemD cls mi@(MenuItem txt ico) = do
  (e,_) <- eAD' cls $ do
    eSpan (setClasses [ico] def) blank
    text txt
  return $ MI (domEvent Click e) mi

mkLiMe :: MonadWidget t m => Dynamic t EA -> MenuItem -> m (MI t)
mkLiMe cls mi = eLi niC $ mkMenuItemD cls mi
  where
    niC = setClasses [bsNavItem] def

mkLiMeFromLsts :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (MI t)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)

mkLiMe2 :: forall t m. MonadWidget t m => (Dynamic t EA, MenuItem) -> m (MI t)
mkLiMe2 (cls,mi) = eLi niC $ mkMenuItemD cls mi
  where
    niC = setClasses [bsNavItem] def
mkLiMeFromLsts2 :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (MI t)
mkLiMeFromLsts2 aLst mLst i = mkLiMe2  (am Prelude.!! i)
  where
    am = zip aLst mLst

mkNavItems :: forall t m.
  MonadWidget t m => [MenuItem] -> m (Dynamic t MenuItem)
mkNavItems menuis = do
  let i = length menuis
      idxLst = [0..(i-1)]
  eUl ulCl $ mdo
    --
    es :: [MI t] <- mapM (\j -> mkLiMeFromLsts mas menuis j) idxLst
    -- es :: [MI t] <- mapM (\j -> mkLiMeFromLsts2 mas menuis j) idxLst
    -- es :: [MI t] <- mapM (\j -> mkLiMe2 j) (zip mas menuis)
    -- e1 :: (MI t) <- mkLiMeFromLsts mas menuis 0
    -- e2 <- mkLiMeFromLsts mas menuis 1
    -- e3 <- mkLiMeFromLsts mas menuis 2
    -- e4 <- mkLiMeFromLsts mas menuis 3
    -- let es = [e1,e2,e3,e4]
    dE :: Dynamic t MenuItem <- holdDyn (head menuis) $
      leftmost (fmap (\e -> miItem e <$ miClicked e) es)
    --
    let mas = fmap (\j -> ffor dE $ \d -> selActive d j) idxLst
    return dE
  where
    selActive :: MenuItem -> Int -> EA
    selActive d i =
        if d == (menuis Prelude.!! i)
           then (hR "#" $ linkCA)
           else (hR "#" $ linkC)
    linkC = setClasses [bsNavLink] def
    linkCA = setClasses [bsNavLink, bsActive] def
    ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)

mkNavItems2 :: forall t m.
  MonadWidget t m => [MenuItem] -> m (Dynamic t MenuItem)
mkNavItems2 menuis = do
  eUl ulCl $ mdo
    let i = length menuis
        idxLst = [0..(i-1)]
    --
    es :: [MI t] <- mapM (\j -> mkLiMeFromLsts mas menuis j) idxLst
    -- e1 :: (MI t) <- mkLiMeFromLsts mas menuis 0
    -- e2 <- mkLiMeFromLsts mas menuis 1
    -- e3 <- mkLiMeFromLsts mas menuis 2
    -- e4 <- mkLiMeFromLsts mas menuis 3
    -- e1 <- eLi niC $ mkMenuItemD mAttr1 (menuis Prelude.!! 0)
    -- e2 <- eLi niC $ mkMenuItemD mAttr2 (menuis Prelude.!! 1)
    -- e3 <- eLi niC $ mkMenuItemD mAttr3 (menuis Prelude.!! 2)
    -- e4 <- eLi niC $ mkMenuItemD mAttr4 (menuis Prelude.!! 3)
    -- let es = [e1,e2,e3,e4]
    dE :: Dynamic t MenuItem <- holdDyn (head menuis) $
      leftmost (fmap (\e -> miItem e <$ miClicked e) es)
    --
    let mas = fmap (\j -> ffor dE $ \d -> selActive d j) idxLst
    return dE
  where
    selActive :: MenuItem -> Int -> EA
    selActive d i =
        if d == (menuis Prelude.!! i)
           then (hR "#" $ linkCA)
           else (hR "#" $ linkC)
    linkC = setClasses [bsNavLink] def
    linkCA = setClasses [bsNavLink, bsActive] def
    ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)



bs4Nav :: (MonadWidget t m) => m (Dynamic t MenuItem)
bs4Nav = do
  mi <- eNav navAttrs $ do
    _e0 <- eAC (setClasses [bsNavbarBrand] $ hR "#" def) $ text "Navbar"
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    dE2 <- mkNavItems menuItems
    return dE2
  return mi
  where
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ typeSubmit
      $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
      $ aControls "navbarSupportedContent" $ aExpanded "false"
      $ aLabel "Toggle navigation" def
    hR u = href (URL u)

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

