{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Main where

import Reflex.Dom.Core
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle (JSM, liftJSM)

#ifdef ghcjs_HOST_OS
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

import Reflex.Dom.HTML5.Attrs as A
import Reflex.Dom.HTML5.Elements as E

import Reflex.Dom.Themes.Raw.BS4 as B
import Reflex.Dom.Themes.Raw.Foundation as F
import Reflex.Dom.Themes.Raw.Semantic as S
import Reflex.Dom.Themes.Raw.W3 as W
import Reflex.Dom.Icons.Raw.FA as FA
import Reflex.Dom.Icons.Raw.FlagIcons as FI
import qualified Reflex.Dom.Icons.Raw.Elusive as EI
import Reflex.Dom.Icons.Raw.IcoMoon as IM
import Reflex.Dom.Icons.Raw.Ionicons as ION
import Reflex.Dom.Icons.Raw.MfgLabs as ML
import Reflex.Dom.Icons.Raw.MDI as MDI
import Reflex.Dom.Icons.Raw.OpenIconic as OI
import Reflex.Dom.Icons.Raw.Typicons as TC

------------------------------------------------------------------------------
------------------------------------------------------------------------------

main :: IO ()
#ifdef ghcjs_HOST_OS
main = liftJSM mainW
#else
main = run mainW
#endif

mainW :: JSM ()
mainW = mainWidgetWithHead headEl bodyEl

headEl :: MonadWidget t m => m ()
headEl = do
  elAttr "meta" ("charset" =: "utf-8") $ pure ()
  elAttr "meta" (
    ("name" =: "viewport") <>
    ("content" =: "width=device-width, initial-scale=1, shrink-to-fit=no")
                ) $ pure ()
  el "title" $ text "Style theme and icon examples"
  -- styleSheet "https://fonts.googleapis.com/css?family=Raleway"
  -- styleSheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
  styleSheet
    "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  -- styleSheet
  --   "https://maxcdn.bootstrapcdn.com/elusive-icons/2.0.0/css/elusive-icons.min.css"
  styleSheet
    "https://cdn.jsdelivr.net/foundation-icons/3.0/foundation-icons.min.css"
  styleSheet
    "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleSheet
    "https://cdn.materialdesignicons.com/2.0.46/css/materialdesignicons.min.css"
  --
  styS
    "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css"
    "sha384-/Y6pD6FV/Vv2HJnA6t+vslU6fwYXjCFtcEpHbNJ0lyAFsXTsjBbfaDjzALeQsN6M"
  -- styleSheet "./css/styles.css"
  -- script "./js/scripts.js"
  where
    styS lnk intgr =
      eLink ( href (URL lnk)
            $ integrity intgr
            $ corsAnon
            $ ltStylesheet
            $ def
            ) $ return ()
    styleSheet lnk = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", lnk)
      ]) $ return ()
    script lnk = elAttr "script" ("src" =: lnk) $ return ()


bodyEl :: MonadWidget t m => m ()
bodyEl = do
  bs4Nav
  eH1 def $ text "Welcome to reflex-dom-themes"


w3Nav :: MonadWidget t m => m ()
w3Nav = do
  eDiv (setClasses [w3Bar, w3Blue] def) $ do
    text "home"
    e1 <- eA cls $ text "W3"
    e2 <- eA cls $ text "Bootstrap4"
    e3 <- eA cls $ text "Semantic"
    e4 <- eA cls $ text "Foundation"
    blank
  where
    cls = setClasses [w3BarItem, w3Button, w3Mobile, w3HoverGreen] def

bs4Nav :: MonadWidget t m => m ()
bs4Nav = do
  eNav (setClasses
    [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def) $ do
      e1 <- eA (setClasses [bsNavbarBrand] $ href (URL "#") def) $ text "Navbar"
      eButton bAttrs $
        eSpan (setClasses [bsNavbarTogglerIcon] def ) $ blank
      eDivN $ do
        eUlN $ do
          eLiN $ do
            e1 <- eA cls $ text "W3"
            e2 <- eA cls $ text "Bootstrap4"
            e3 <- eA cls $ text "Semantic"
            e4 <- eA cls $ text "Foundation"
            blank
      blank
  where
    bAttrs = setClasses [bsNavbarToggler] def
    cls = setClasses [bsNavItem] def
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
