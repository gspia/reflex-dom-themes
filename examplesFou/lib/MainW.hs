{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

import Reflex.Dom.Themes.Raw.Foundation as F
import Reflex.Dom.Icons.Raw.FI as FI

------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW :: JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headElFou bodyEl

------------------------------------------------------------------------------
------------------------------------------------------------------------------

headElFou :: MonadWidget t m => m ()
headElFou = do
  eTitle def $ text "Main Title Foundation"
  eMeta (charSet "utf-8" def) $ blank
  eMeta (httpEquiv "x-ua-compatible" $ content "ie=edge" def) $ blank
  eMeta (name "viewport" $
    content "width=device-width, initial-scale=1, shrink-to-fit=no" $ def)
    $ blank
  eLink ( ltStylesheet
    $ href (URL "https://cdn.jsdelivr.net/foundation-icons/3.0/foundation-icons.min.css") def) blank
  eLink ( ltStylesheet
    $ href (URL "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css")
    -- $ integrity "sha256-itWEYdFWzZPBG78bJOOiQIn06QCgN/F0wMDcC4nOhxY="
    $ corsAnon def) blank
  eLink ( ltStylesheet
    $ href (URL "css/fou.css") def) blank



bodyEl :: (MonadWidget t m) => m ()
bodyEl = do
  miD :: Dynamic t MenuItem <- fouNav
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

menuItemsL :: [MenuItem]
menuItemsL =
  [ MenuItem "Monday" fiHeart
  , MenuItem "Tuesday" fiStar
  , MenuItem "Wednesday" fiGraphBar
  , MenuItem "Thursday" fiTorso
  , MenuItem "Friday" fiWheelchair
  ]

menuItemsR :: [MenuItem]
menuItemsR =
  [ MenuItem "January" fiBraille
  , MenuItem "February" fiElevator
  , MenuItem "March" fiArrowsOut
  ]
hasFouActive :: Reflex t => Dynamic t EA -> Dynamic t Bool
hasFouActive ea = do
  let dcn = fmap attrGetClassName ea
  fmap (\(ClassName cn) -> (T.count "is-active" cn) > 0) dcn


mkFouMenuItemD :: forall t m. (MonadWidget t m)
            => MenuItem -> m (MI t)
mkFouMenuItemD mi@(MenuItem txt ico) = do
  (e,_) <- eA' (href (URL "#") def) $ do
    if ico /= ClassName ""
       then eI (setClasses [ico] def) blank
       else blank
    eSpan def $ text txt
    -- let da = hasFouActive cls
    -- dyn $ ffor da $ \b ->
    --         if b
    --            then eSpan (setClasses [bsSrOnly] def) $ text "(current)"
    --            else return ()
  return $ MI (domEvent Click e) mi

mkLiMe :: MonadWidget t m => Dynamic t ELi -> MenuItem -> m (MI t)
mkLiMe cls mi = eLiD cls $ mkFouMenuItemD mi

mkLiMeFromLsts :: MonadWidget t m
               => [Dynamic t ELi] -> [MenuItem] -> Int -> m (MI t)
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
    selActive :: MenuItem -> Int -> ELi
    selActive d i =
        if d == (menuis Prelude.!! i)
           then setClasses [fouIsActive] def
           else def
    -- ulCl = setClasses [fouDropdown, fouMediumHorizontal] def
    -- ulCl = setClasses [fouMenu] $ dNmVal "responsive-menu" "drilldown" $ def
    ulCl = setClasses [fouMenu] def
    -- ulCl = setClasses [fouDropdown, fouMenu] $ dNmVal "dropdown-menu" "" $ def
    hR u = href (URL u)


fouNav :: (MonadWidget t m) => m (Dynamic t MenuItem)
fouNav = do
  eDiv (setClasses [fouTitleBar, fouHideForMedium]
    $ dNmVal "responsive-toggle" "responsivemenu"
    $ dNmVal "hide-for" "medium"
    $ def) $ do
      eButton (setClasses [fouMenuIcon]
        $ btButton
        $ dToggle "responsivemenu" $ def) $ blank
      eDiv (setClasses [fouTitleBarTitle] def) $ text "Menu"

  mi <- eDiv navAttrs $ do
    dE2 <- eDiv divClL $ do
      dE2 <- mkNavItems menuItemsL
      return dE2
    return dE2
  return mi
  where
    navAttrs = setClasses [fouTopBar] $ id_ "responsivemenu"
      -- $ dNmVal "topbar" ""
      $ dNmVal "toggler" "" $ def
    divClL = setClasses [fouTopBarLeft] $ def
    divClR = setClasses [fouTopBarRight] $ def

{-
<div class="title-bar" data-responsive-toggle="responsive-menu"
data-hide-for="medium">
<button class="menu-icon" type="button" data-toggle="responsive-menu"></button>
<div class="title-bar-title">Menu</div>
</div>

<div class="top-bar" id="responsive-menu">
  <div class="top-bar-left">
    <ul class="dropdown vertical medium-horizontal menu">
      <li><a href="#">Monday</a></li>
      <li><a href="#">Tuesday</a></li>
      <li><a href="#">Wednesday</a></li>
      <li><a href="#">Thursday</a></li>
      <li><a href="#">Friday</a></li>
    </ul>
  </div>
  <div class="top-bar-right">
    <ul class="dropdown vertical medium-horizontal menu">
      <li><a href="#">January</a></li>
      <li><a href="#">February</a></li>
      <li><a href="#">March</a></li>
      <li><input type="search" placeholder="Search"></li>
    </ul>
  </div>
</div>
-}
