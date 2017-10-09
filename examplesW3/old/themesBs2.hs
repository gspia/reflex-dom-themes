{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad (join)
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
  showMenuContent miD

showMenuContent :: MonadWidget t m => Dynamic t MenuItem -> m ()
showMenuContent mi = do
  let mt = fmap (T.pack . show) mi
  text "The selected menu items was: "
  dynText mt

data MenuItem = MInavbar | MIactive | MIlink | MIdisabled |
  MIaction | MIanother | MIsomething | MIseparated | MIsearch
  deriving (Eq,Show)

data MI t
  = MI { miClicked :: Event t ()
       , miItem    :: MenuItem
       }

menuISearch :: (MonadWidget t m, MonadJSM (Performable m))
       => m (MI t)
menuISearch = do
  (ev,_) <- eInput' fiAttrs $ eButton' fibAttrs $ text "Search"
  return  $ MI (domEvent Click ev) MIsearch
    where
      fiAttrs = setClasses [bsFormControl, bsMrSm2]
        $ itText $ placeholder "Search" def
      fibAttrs = setClasses [bsBtn, bsBtnOutlineSuccess, bsMy2, bsMySm0]
        $ typeSubmit def


menuIWithC :: MonadWidget t m => EA -> MenuItem -> Text -> m (MI t)
menuIWithC cls mi txt = do
  (e,_) <- eA' cls $ text txt
  return $ MI (domEvent Click e) mi

-- Compare to the html below
-- Handling events (or dynamic values) from nested elements is a bit
-- cumbersome this way. The below code mixes different ways in order
-- to just see, how one can handle events and dynamics.
-- Look at the bs4Nav2 for a bit cleaner version (below).
bs4Nav :: (MonadWidget t m) => m (Dynamic t MenuItem)
bs4Nav = do
  mi <- eNav navAttrs $ do
    ev1 <- eAC (setClasses [bsNavbarBrand] $ hR "#" def) $ text "Navbar"
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    (e2,e3,e4,e5, ddMi ::Dynamic t MenuItem) <- eDiv divCl $ do
      (ev2,ev3,ev4, ddMi) <- eUl ulCl $ do
        ev2 <- eLi niCA $ do
            (ev2,_) <- eA' (hR "#" $ linkC) $ do
                text "Active"
                eSpan (setClasses [bsSrOnly] def) $ text "(current)"
            return $ MI (domEvent Click ev2) MIactive
        ev3 <- eLi niC  $ menuIWithC (hR "#" $ linkC) MIlink "Link"
        ev4 <- eLi niC  $ menuIWithC (hR "#" $ linkCDis) MIdisabled "Disabled"
        ddMi <- dropDown
        return (ev2, ev3, ev4, ddMi)
      miS <- eForm fCl $ menuISearch
      return (ev2,ev3,ev4,miS, ddMi)
    dE :: Dynamic t MenuItem <- holdDyn MInavbar $
      -- dE :: _ <- holdDyn MInavbar $
      leftmost [ MInavbar  <$ ev1
               , miItem e2 <$ miClicked e2
               , miItem e3 <$ miClicked e3
               , miItem e4 <$ miClicked e4
               , miItem e5 <$ miClicked e5
               ]
    let ud1 :: Event t MenuItem = updated ddMi
        ud2 :: Event t MenuItem = updated dE
    -- let ud1 = updated ddMi
    --     ud2 = updated dE
    ud <- holdDyn dE $ leftmost [ddMi <$ ud1, dE <$ ud2]
    return $ join ud
  return mi
  where
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ typeSubmit
      $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
      $ aControls "navbarSupportedContent" $ aExpanded "false"
      $ aLabel "Toggle navigation" def
    divCl = setClasses [bsCollapse, bsNavbarCollapse]
      $ id_ "navbarSupportedContent" def
    ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    linkC = setClasses [bsNavLink] def
    linkCDis = setClasses [bsNavLink, bsDisabled] def
    niCA = setClasses [bsNavItem, bsActive] def
    niC = setClasses [bsNavItem] def
    hR u = href (URL u)
    fCl = setClasses [bsFormInline, bsMy2, bsMyLg0] def

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

dropDown :: MonadWidget t m => m (Dynamic t MenuItem)
dropDown = do
  dmi <- eDiv (setClasses [bsBtnGroup] def) $ do
    dmi <- eButton bAttrs $ do
      dmi <- eDiv (setClasses [bsDropdownMenu] def) $ do
        e1 <- eAC dItCl $ text "Action"
        e2 <- eAC dItCl $ text "Another"
        e3 <- eAC dItCl $ text "Something"
        eDiv (setClasses [bsDropdownDivider] def) $ blank
        e4 <- eAC dItCl $ text "Separated link"
        dmi <- holdDyn MInavbar $
          leftmost [ MIaction    <$ e1
                   , MIanother   <$ e2
                   , MIsomething <$ e3
                   , MIseparated <$ e4
                   ]
        return dmi
      return dmi
    return dmi
  return dmi
  where
    hR u = href (URL u)
    dItCl = setClasses [bsDropdownItem] $ hR "#" def
    bAttrs = typeButton $ setClasses [bsBtn, bsBtnDanger, bsDropdownToggle]
      $ dToggle "dropdown" $ aHaspopup "true" $ aExpanded "false" def

{-
<!-- Example single danger button -->
<div class="btn-group">
  <button type="button" class="btn btn-danger dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
    Action
  </button>
  <div class="dropdown-menu">
    <a class="dropdown-item" href="#">Action</a>
    <a class="dropdown-item" href="#">Another action</a>
    <a class="dropdown-item" href="#">Something else here</a>
    <div class="dropdown-divider"></div>
    <a class="dropdown-item" href="#">Separated link</a>
  </div>
</div>
-}
