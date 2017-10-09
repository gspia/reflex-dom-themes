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
import Data.Monoid as Mon
import Data.Semigroup
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
  putStrLn $ "Running dev server on localhost:" Mon.<> show port
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
    $ href (URL "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css")
    $ integrity "sha384-/Y6pD6FV/Vv2HJnA6t+vslU6fwYXjCFtcEpHbNJ0lyAFsXTsjBbfaDjzALeQsN6M"
    $ corsAnon
    $ def) $ blank
  eLink ( ltStylesheet
        $ href (URL "https://fonts.googleapis.com/css?family=Raleway")
        $ def) $ blank
  -- eLink ( ltStylesheet
  --   $ href (URL "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css") $ def) $ blank
  eLink ( ltStylesheet $ href (URL "css/font-awesome.min.css") $ def
        ) blank
  -- eLink ( ltStylesheet
  --   $ href (URL "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  --   $ def) $ blank




myWidget :: (Semigroup w, MonadWidget t m) => EventWriterT t w m ()
myWidget = do
  text "myWidget text"

-- bodyEl :: (MonadWidget t m, Semigroup w) => m ()
bodyEl = do
  eMi :: Event t MenuItem <- bs4Nav menuItems
  eH1 def $ text "Welcome to reflex-dom-themes"
  dMi <- holdDyn (head menuItems) eMi
  -- e <- runEventWriterT evWT
  showMenuContent $ dMi

evWT = do
  text "evWT text"

showMenuContent :: MonadWidget t m => Dynamic t MenuItem -> m ()
showMenuContent mi = do
  let mt = fmap (T.pack . show) mi
  text "The selected menu items was: "
  dynText mt

-- "idnum" "name" and "icon"
data MenuItem = MenuItem Int Text ClassName
  deriving (Eq,Show)

-- data MI t
--   = MI { miClicked :: Event t ()
--        , miItem    :: MenuItem
--        }

menuItems :: [MenuItem]
menuItems =
  [ MenuItem 0 "Active" faAutomobile
  , MenuItem 1 "Link" faAreaChart
  , MenuItem 2 "Another link" faBlind
  , MenuItem 3 "Link4" faBell
  ]

hasActive :: Reflex t => Dynamic t EA -> Dynamic t Bool
hasActive ea = do
  let dcn = fmap attrGetClassName ea
  fmap (\(ClassName cn) -> (T.count "active" cn) > 0) dcn


mkMenuItemD :: forall t m. (MonadWidget t m)
            => Dynamic t EA -> MenuItem -> m (Event t MenuItem)
mkMenuItemD cls mi@(MenuItem i txt ico) = do
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
  return $ mi <$ domEvent Click e

mkLiMe :: MonadWidget t m => Dynamic t EA -> MenuItem -> m (Event t MenuItem)
mkLiMe cls mi = eLi (setClasses [bsNavItem] def) $ mkMenuItemD cls mi

mkLiMeFromLsts :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (Event t MenuItem)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)



mkNavItems :: forall t m.
  MonadWidget t m => [MenuItem] -> m (Event t MenuItem)
mkNavItems menuis = do
  let i = length menuis
      idxLst = [0..(i-1)]
  eUl ulCl $ mdo
    es :: [Event t MenuItem] <- mapM (\j -> mkLiMeFromLsts mas menuis j) idxLst
    let e = leftmost es
    dE :: Dynamic t MenuItem <- holdDyn (head menuis) e
    let mas = fmap (\j -> ffor dE $ \(MenuItem i _ _) -> selActive i j) idxLst
    return e
  where
    selActive :: Int -> Int -> EA
    selActive i j =
      if i == j
         then (hR "#" $ linkCA)
         else (hR "#" $ linkC)
    linkC = setClasses [bsNavLink] def
    linkCA = setClasses [bsNavLink, bsActive] def
    ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)


bs4Nav :: (MonadWidget t m) => [MenuItem] -> m (Event t MenuItem)
bs4Nav menu = do
  eMi <- eNav navAttrs $ do
    _e0 <- eAC (setClasses [bsNavbarBrand] $ hR "#" def) $ text "Navbar"
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    e <- eDiv divCl $ mkNavItems menu
    return e
  return eMi
  where
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ btSubmit
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

