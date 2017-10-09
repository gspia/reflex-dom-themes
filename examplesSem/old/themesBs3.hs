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
data MenuItem = MenuItem Text Text
  deriving (Eq,Show)

data MI t
  = MI { miClicked :: Event t ()
       , miItem    :: MenuItem
       , miDynCl   :: Dynamic t ClassName
       }

menuItems :: [MenuItem]
menuItems =
  [ MenuItem "Active" ""
  , MenuItem "Link" ""
  , MenuItem "Another link" ""
  , MenuItem "Link4" ""
  ]

menuExt :: MonadWidget t m => [m ()]
menuExt =
  [ eSpan (setClasses [bsSrOnly] def) $ text "(current)"
  , blank, blank, blank
  ]

menuDyn :: Reflex t => [Dynamic t ClassName]
menuDyn =
  [ constDyn bsActive
  , constDyn cl
  , constDyn cl
  , constDyn cl
  ]
    where
      cl = ClassName ""
      -- cl = bsNavItem
      -- cl = setClasses [bsNavItem] def

menuDyn2 :: Reflex t => [Dynamic t ClassName]
menuDyn2 =
  [ constDyn cl
  , constDyn cl
  , constDyn cl
  , constDyn cl
  ]
    where
      cl = ClassName ""
      -- cl = bsNavItem
      -- cl = setClasses [bsNavItem] def

-- menuI2W :: MenuItem -> Dynamic t Int -> m (Event t Int)
-- menuI2W

menu2 :: MonadWidget t m => [(MenuItem,m (), Dynamic t ClassName)]
menu2 = zip3 menuItems menuExt menuDyn

mIWithC :: MonadWidget t m => EA -> MenuItem -> m () -> Dynamic t ClassName -> m (MI t)
mIWithC cls mi@(MenuItem txt ico) ma dcl = do
  (e,_) <- eA' cls $ do
    text txt
    ma
  -- return $ MI (domEvent Click e) mi (constDyn $ ClassName "")
  return $ MI (domEvent Click e) mi dcl

menuIS :: (MonadWidget t m, MonadJSM (Performable m)
          ) => m (MI t)
menuIS = do
  (ev,_) <- eInput' fiAttrs $ eButton' fibAttrs $ text "Search"
  return  $ MI (domEvent Click ev) (MenuItem "Search" "") (constDyn $ ClassName "")
    where
      fiAttrs = setClasses [bsFormControl, bsMrSm2]
        $ itText $ placeholder "Search" def
      fibAttrs = setClasses [bsBtn, bsBtnOutlineSuccess, bsMy2, bsMySm0]
        $ typeSubmit def

bs4N  :: (MonadWidget t m, MonadJSM (Performable m))
      => [(MenuItem, m (), Dynamic t ClassName)] -> m [MI t]
bs4N mismes = sequence $ fmap f mismes
  -- sequence
  --   [ eLi niCA $ mIWithC (hR "#" $ linkC)
  --         (menuItems Prelude.!!0) (menuExt Prelude.!!0)
  --   , eLi niC  $ mIWithC (hR "#" $ linkC)
  --         (menuItems Prelude.!!1) (menuExt Prelude.!!1)
  --   ]
  where
    -- link-element with dynamic attributes
    dynAttrs dynclnm = fmap attrs dynclnm
    attrs c = setClasses [bsNavItem, c] def
    f (a,b,c) = eLiD (dynAttrs c) $ mIWithC (hR "#" $ linkC) a b c
    -- f (a,b,c) = eLiD niC $ mIWithC (hR "#" $ linkC) a b
    niC = setClasses [bsNavItem] def
    --niCA = setClasses [bsNavItem, bsActive] def
    hR u = href (URL u)
    linkC = setClasses [bsNavLink] def
    -- linkCDis = setClasses [bsNavLink, bsDisabled] def

mi2Dyn :: MonadWidget t m => [MI t] -> m (Dynamic t MenuItem)
mi2Dyn bs = holdDyn (miItem (head bs)) $
    leftmost $ fmap (\h -> (miItem h) <$ (miClicked h)) bs

-- menuD :: Reflex t => Dynamic t Int -> [Dynamic t ClassName]
-- menuD i = menuDyn
-- menuD i =
--   ffor i $ \iV ->
--               if iV > 0
--                  then menuDyn
--                  else menuDyn2

bs4Nav :: (MonadWidget t m) => m (Dynamic t MenuItem)
bs4Nav = do
  mi <- eNav navAttrs $ do
    e1 <- mIWithC (setClasses [bsNavbarBrand] $ hR "#" $ def)
                    (MenuItem "BrandLogo" "") blank
                    (constDyn $ ClassName "")
    eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
    (miDy, ddMi, evS) <- eDiv divCl $ do
      (miDy, ddMi) <- eUl ulCl $ do
        -- mis <- bs4N menu3
        let i = constDyn (1::Int)
            -- mD2 <- menuD i
        -- let mD = ffor i $ \iV ->
        --           if iV > 0
        --              then menuDyn
        --              else menuDyn2
        mis <- bs4N $ zip3 menuItems menuExt menuDyn
        miDy <- mi2Dyn mis
        let ud3' = updated miDy
        -- i <- holdDyn (0::Int) $ leftmost [(1::Int) <$ ud3']
        -- let menuADyn = menuDyn2

              -- let menuAdyn = if 1 > 0
                            -- then menuDyn2
                            -- else menuDyn
              -- menuADyn1 = replicate 1 (constDyn $ ClassName "")
              -- menuADyn2 = constDyn $ bsActive
              -- menuADyn3 = replicate (4-1) (constDyn $ ClassName "")
        ddMi <- dropDown
        return (miDy, ddMi)
      miS <- eForm fCl $ menuIS -- 5th item
      return (miDy, ddMi, miS)
    dE :: Dynamic t MenuItem <- holdDyn (menuItems Prelude.!!0) $
      leftmost [ miItem e1 <$ miClicked e1
               , miItem evS <$ miClicked evS
               ]
    let ud1 :: Event t MenuItem = updated ddMi
        ud2 :: Event t MenuItem = updated dE
        ud3 :: Event t MenuItem = updated miDy
    ud <- holdDyn dE $ leftmost [ddMi <$ ud1, dE <$ ud2, miDy <$ ud3]
    return $ join ud
  return mi
  where
    menu3 = zip3 menuItems menuExt menuDyn
    navAttrs = setClasses
      [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] def
    bAttrs = setClasses [bsNavbarToggler] $ typeSubmit
      $ dToggle "collapse" $ dTarget "#navbarSupportedContent"
      $ aControls "navbarSupportedContent" $ aExpanded "false"
      $ aLabel "Toggle navigation" def
    divCl = setClasses [bsCollapse, bsNavbarCollapse]
      $ id_ "navbarSupportedContent" def
    ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)
    fCl = setClasses [bsFormInline, bsMy2, bsMyLg0] def


menuItemsDD :: [MenuItem]
menuItemsDD =
  [ MenuItem "Another" ""
  , MenuItem "Action" ""
  , MenuItem "Something" ""
  , MenuItem "Separated link" ""
  ]

-- this is without inner elements
mIWithC2 :: MonadWidget t m => EA -> MenuItem -> m (MI t)
mIWithC2 cls mi@(MenuItem txt ico) = do
  (e,_) <- eA' cls $ text txt
  return $ MI (domEvent Click e) mi (constDyn $ ClassName "")

dropDown :: MonadWidget t m => m (Dynamic t MenuItem)
dropDown = do
  dmi <- eDiv (setClasses [bsBtnGroup] def) $ do
    dmi <- eButton bAttrs $ do
      dmi <- eDiv (setClasses [bsDropdownMenu] def) $ do
        e1 <- mIWithC2 dItCl $ (menuItemsDD Prelude.!!0)
        e2 <- mIWithC2 dItCl $ (menuItemsDD Prelude.!!1)
        e3 <- mIWithC2 dItCl $ (menuItemsDD Prelude.!!2)
        eDiv (setClasses [bsDropdownDivider] def) $ blank
        e4 <- mIWithC2 dItCl $ (menuItemsDD Prelude.!!3)
        dmi <- holdDyn (menuItemsDD Prelude.!!0) $
          leftmost [ miItem e1 <$ miClicked e1
                   , miItem e2 <$ miClicked e2
                   , miItem e3 <$ miClicked e3
                   , miItem e4 <$ miClicked e4
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
