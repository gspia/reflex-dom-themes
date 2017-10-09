{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad (mapM)
import Reflex.Dom.Core
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle (JSM, liftJSM)
import GHCJS.Types
import Language.Javascript.JSaddle

-- import Language.Javascript.JSaddle            (JSM)
import Language.Javascript.JSaddle.Run        (syncPoint)

#ifdef ghcjs_HOST_OS
#else
-- import Language.Javascript.JSaddle.WebKitGTK (run)
import Language.Javascript.JSaddle.Warp as JSW (run)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai                            (Application)
import Network.Wai.Handler.Warp               (defaultSettings, run, runSettings
                                              , setPort, setTimeout)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Application.Static
import WaiAppStatic.Types
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
main = JSW.run 8000 $ mainW

-- | A @main@ for doing development.
devMain :: Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  putStrLn $ "Running dev server on localhost:" <> show port
  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend
  runSettings (defaultSettings & setTimeout 3600 & setPort port) app

-- | A version of @devMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devMainAutoReload :: Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port

staticServer :: Application
staticServer = staticApp ((defaultFileServerSettings "./static") & noCache)
  where noCache s = s { ssMaxAge = MaxAgeSeconds 0 }

#endif

------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW :: JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headEl bodyEl

------------------------------------------------------------------------------
------------------------------------------------------------------------------

headEl :: MonadWidget t m => m ()
headEl = do
  eTitle def $ text "Main Title"
  eMeta (charSet "utf-8" def) $ blank
  eMeta (name "viewport" $
    content "width=device-width, initial-scale=1, shrink-to-fit=no" $ def)
    $ blank
  eLink ( ltStylesheet
    $ href (URL "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
    $ def) $ blank
  eLink ( ltStylesheet
    $ href (URL "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css")
    $ integrity "sha384-/Y6pD6FV/Vv2HJnA6t+vslU6fwYXjCFtcEpHbNJ0lyAFsXTsjBbfaDjzALeQsN6M"
    $ corsAnon
    $ def) $ blank



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

hasActive :: Reflex t => Dynamic t EA -> Dynamic t Bool
hasActive ea = do
  let dcn = fmap attrGetClassName ea
  fmap (\(ClassName cn) -> (T.count "active" cn) > 0) dcn


mkMenuItemD :: forall t m. (MonadWidget t m)
            => Dynamic t EA -> MenuItem -> m (MI t)
mkMenuItemD cls mi@(MenuItem txt ico) = do
  (e,_) <- eAD' cls $ do
    if ico /= ClassName ""
       then eSpan (setClasses [ico] def) blank
       else blank
    text txt
    let da = hasActive cls
    dyn $ ffor da $ \b ->
            if b
               then eSpan (setClasses [bsSrOnly] def) $ text "(current)"
               else return ()
  return $ MI (domEvent Click e) mi

mkLiMe :: MonadWidget t m => Dynamic t EA -> MenuItem -> m (MI t)
mkLiMe cls mi = eLi (setClasses [bsNavItem] def) $ mkMenuItemD cls mi

mkLiMeFromLsts :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (MI t)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)

mkNavItems :: forall t m.
  MonadWidget t m => [MenuItem] -> m (Dynamic t MenuItem)
mkNavItems menuis = do
  let i = length menuis
      idxLst = [0..(i-1)]
  eUl ulCl $ mdo
    es :: [MI t] <- mapM (\j -> mkLiMeFromLsts mas menuis j) idxLst
    -- e1 :: (MI t) <- mkLiMeFromLsts mas menuis 0
    -- e2 <- mkLiMeFromLsts mas menuis 1
    -- e3 <- mkLiMeFromLsts mas menuis 2
    -- e4 <- mkLiMeFromLsts mas menuis 3
    -- let es = [e1,e2,e3,e4]
    dE :: Dynamic t MenuItem <- holdDyn (head menuis) $
      leftmost (fmap (\e -> miItem e <$ miClicked e) es)
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
    dE2 <- eDiv divCl $ do
      dE2 <- mkNavItems menuItems
      return dE2
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
    divCl = setClasses [bsCollapse, bsNavbarCollapse]
      $ id_ "navbarSupportedContent" def


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

