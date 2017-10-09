{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module MainW where

import Control.Monad (mapM)
import Reflex.Dom.Core
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle (JSM, liftJSM)

import Reflex.Dom.HTML5.Attrs as A
import Reflex.Dom.HTML5.Elements as E

import Reflex.Dom.Themes.Raw.Semantic as S
import Reflex.Dom.Icons.Raw.Ionicons as I

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
  eMeta (httpEquiv "x-ua-compatible" $ content "ie=edge,chrome=1" def) $ blank
  eMeta (name "viewport" $
    content "width=device-width, initial-scale=1, maximum-scale=1.0" $ def)
    $ blank
  eLink ( ltStylesheet
    $ href (URL "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css")
    $ def) $ blank
  eLink ( ltStylesheet
    -- $ href (URL "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
    $ href (URL "css/icon.min.css")
    $ def) $ blank
  eLink ( ltStylesheet
    $ href (URL "css/semantic.min.css")
    $ def) $ blank
  eLink ( ltStylesheet
    $ href (URL "css/sem.css")
    $ def) $ blank



bodyEl :: (MonadWidget t m) => m ()
bodyEl = do
  miD :: Dynamic t MenuItem <- semNav
  eDiv (addClass "main" $ setClasses [semUi, semText, semContainer] def) $ do
    eH1 (setClasses [semUi, semHeader] def) $
      text "Welcome to reflex-dom-themes (Semantic)"
    showMenuContent miD
    eP def $ do
      text "Semantic icon trial: "
      eI (setClasses [semInbox,semIcon] def) $ blank
  footer

showMenuContent :: MonadWidget t m => Dynamic t MenuItem -> m ()
showMenuContent mi = do
  let mt = fmap (T.pack . show) mi
  text "The selected menu items was: "
  dynText mt

footer :: MonadWidget t m => m ()
footer = do
  eDiv (addClass "footer" $ setClasses [semUi, semInverted, semVertical, semSegment] def) $
    eDiv (setClasses [semUi, semCenter, semAligned, semContainer] def) $
      eDiv (setClasses [semUi,semStackable,semInverted,semDivided,semGrid] def) $ do
        eDiv (setClasses [semThree, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Group 1"
          eDiv grpAttr $ do
            eA aAttr $ text "Link one"
            eA aAttr $ text "Link two"
            eA aAttr $ text "Link three"
            eA aAttr $ text "Link four"
        eDiv (setClasses [semThree, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Group 2"
          eDiv grpAttr $ do
            eA aAttr $ text "Link one"
            eA aAttr $ text "Link two"
            eA aAttr $ text "Link three"
            eA aAttr $ text "Link four"
        eDiv (setClasses [semThree, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Group 3"
          eDiv grpAttr $ do
            eA aAttr $ text "Link one"
            eA aAttr $ text "Link two"
            eA aAttr $ text "Link three"
            eA aAttr $ text "Link four"
        eDiv (setClasses [semSeven, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Footer Header"
          eP def $ text "Hmm"

  where
    aAttr = setClasses [semItem] $ href (URL "#") def
    grpAttr = setClasses [semUi, semInverted, semLink, semList] def


{-
<div class="ui inverted vertical footer segment">
<div class="ui center aligned container">
  <div class="ui stackable inverted divided grid">
    <div class="three wide column">
      <h4 class="ui inverted header">Group 1</h4>
      <div class="ui inverted link list">
        <a href="#" class="item">Link One</a>
        <a href="#" class="item">Link Two</a>
        <a href="#" class="item">Link Three</a>
        <a href="#" class="item">Link Four</a>
      </div>
    </div>
    <div class="three wide column">
      <h4 class="ui inverted header">Group 2</h4>
      <div class="ui inverted link list">
        <a href="#" class="item">Link One</a>
        <a href="#" class="item">Link Two</a>
        <a href="#" class="item">Link Three</a>
        <a href="#" class="item">Link Four</a>
      </div>
    </div>
    <div class="three wide column">
      <h4 class="ui inverted header">Group 3</h4>
      <div class="ui inverted link list">
        <a href="#" class="item">Link One</a>
        <a href="#" class="item">Link Two</a>
        <a href="#" class="item">Link Three</a>
        <a href="#" class="item">Link Four</a>
      </div>
    </div>
    <div class="seven wide column">
      <h4 class="ui inverted header">Footer Header</h4>
      <p>Extra space for a call to action inside the footer that could help re-engage users.</p>
    </div>
  </div>
  <div class="ui inverted section divider"></div>
  <img src="assets/images/logo.png" class="ui centered mini image">
  <div class="ui horizontal inverted small divided link list">
    <a class="item" href="#">Site Map</a>
    <a class="item" href="#">Contact Us</a>
    <a class="item" href="#">Terms and Conditions</a>
    <a class="item" href="#">Privacy Policy</a>
-}

-- "name" and "icon"
data MenuItem = MenuItem Text ClassName
  deriving (Eq,Show)

data MI t
  = MI { miClicked :: Event t ()
       , miItem    :: MenuItem
       }

menuItems :: [MenuItem]
menuItems =
  [ MenuItem "Active" semExternal
  , MenuItem "Link" semHeartbeat
  , MenuItem "Another link" semEyedropper
  , MenuItem "Link4" semQrcode
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
       then eI (setClasses [ico, semIcon ] def) blank
       else blank
    text txt
    -- let da = hasActive cls
    -- dyn $ ffor da $ \b ->
    --         if b
    --            then eSpan (setClasses [bsSrOnly] def) $ text "(current)"
    --            else return ()
  return $ MI (domEvent Click e) mi

mkLiMe :: MonadWidget t m => Dynamic t EA -> MenuItem -> m (MI t)
-- mkLiMe cls mi = eLi (setClasses [bsNavItem] def) $ mkMenuItemD cls mi
mkLiMe cls mi = mkMenuItemD cls mi

mkLiMeFromLsts :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (MI t)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)

mkSemNavItems :: forall t m.
  MonadWidget t m => [MenuItem] -> m (Dynamic t MenuItem)
mkSemNavItems menuis = mdo
  let i = length menuis
      idxLst = [0..(i-1)]
  -- eUl ulCl $ mdo
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
    linkC = setClasses [semItem] def
    linkCA = setClasses [semItem, semActive] def
    -- ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)


semNav :: (MonadWidget t m) => m (Dynamic t MenuItem)
semNav = do
  mi <- eDiv navAttrs $ do
    mi <- eDiv uiconAttrs $ do
      _e0 <- eAC (setClasses [semHeader, semItem] $ hR "#" def) $
        text "Project name"
      -- eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
      -- dE2 <- eDiv divCl $ do
      dE2 <- mkSemNavItems menuItems
      return dE2
      -- return dE2
    return mi
  return mi
  where
    navAttrs = setClasses [semUi,semFixed, semInverted, semMenu] def
    uiconAttrs = setClasses [semUi, semContainer] def
    -- bAttrs = setClasses [bsNavbarToggler] $ btSubmit
    --   $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
    --   $ aControls "navbarSupportedContent" $ aExpanded "false"
    --   $ aLabel "Toggle navigation" def
    hR u = href (URL u)
    -- divCl = setClasses [bsCollapse, bsNavbarCollapse]
    --   $ id_ "navbarSupportedContent" def

{-
 <div class="ui fixed inverted menu">
  <div class="ui container">
    <a href="#" class="header item">
      <img class="logo" src="assets/images/logo.png">
      Project Name
    </a>
    <a href="#" class="item">Home</a>
    <div class="ui simple dropdown item">
      Dropdown <i class="dropdown icon"></i>
      <div class="menu">
        <a class="item" href="#">Link Item</a>
        <a class="item" href="#">Link Item</a>
        <div class="divider"></div>
        <div class="header">Header Item</div>
        <div class="item">
          <i class="dropdown icon"></i>
          Sub Menu
          <div class="menu">
            <a class="item" href="#">Link Item</a>
            <a class="item" href="#">Link Item</a>
          </div>
        </div>
        <a class="item" href="#">Link Item</a>
      </div>
    </div>
  </div>
</div>
-}
