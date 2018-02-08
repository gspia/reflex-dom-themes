{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainW3 where

import           Control.Lens
import           Control.Monad                              (join, mapM)
import           Control.Monad.Fix
import qualified Data.Map                                   as Map
import           Data.Monoid
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import qualified Data.Vector                                as V
import           Language.Javascript.JSaddle                (JSM, liftJSM, MonadJSM)

import           Reflex.Dom                                 hiding (mainWidget,
                                                             mainWidgetWithHead)
import           Reflex.Dom.Core                            (mainWidget,
                                                             mainWidgetWithHead)

import           Reflex.Dom.HTML5.Attrs                     as A
import           Reflex.Dom.HTML5.Elements                  as E

import           Reflex.Dom.Icon.Raw.FA                     as FA
import           Reflex.Dom.Theme.Raw.W3                    as W

import           Reflex.Dom.HTML5.Component.Table

------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW ∷ JSM ()
mainW = mainWidgetWithHead headEl bodyEl

-- mainW = mainWidget bodyEl

------------------------------------------------------------------------------
------------------------------------------------------------------------------

headEl :: MonadWidget t m => m ()
headEl = do
  eTitle def $ text "Main Title"
  eMeta (charSet "utf-8" def) blank
  eMeta (name "viewport" $ content "width=device-width, initial-scale=1" def)
    blank
  eLink ( ltStylesheet
    $ href (URL "https://www.w3schools.com/w3css/4/w3.css") def) blank
  eLink ( ltStylesheet
    $ href (URL "https://fonts.googleapis.com/css?family=Raleway") def) blank
  eLink ( ltStylesheet
    $ href (URL "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
    def) blank


bodyEl :: (MonadWidget t m) => m ()
bodyEl = do
  miD :: Dynamic t MenuItem <- w3Nav
  eDiv (style "margin-top:43px" def) $ do
    eH1 def $ text "Welcome to reflex-dom-themes (W3.css)"
    showMenuContent miD

showMenuContent :: MonadWidget t m => Dynamic t MenuItem -> m ()
showMenuContent mi = do
    let mt = fmap (T.pack . show) mi
    ePN $ do
        text "The selected menu items was: "
        dynText mt
    eH2N $ text "Decorated table example"
    ePN $ do
        dmTxt <- tblWidget
        ePN $ dynText $ maybePersonIdTxt <$> dmTxt
        blank
    where
      maybePersonIdTxt (Just txt) = "Person " <> txt <> " is selected."
      maybePersonIdTxt Nothing    = "No person is selected."


--------------------------------------------------------------------------------

-- | Set the information for the table.
tblContent :: V.Vector (V.Vector Text)
tblContent = V.fromList [r1,r2,r3,r4]
  where
    r1 = V.fromList ["#1", "Firstname1 ", "Lastname 1", "Addr 1", "Phone 1"]
    r2 = V.fromList ["#2", "Firstname2 ", "Lastname 2", "Addr 2", "Phone 2"]
    r3 = V.fromList ["#3", "Firstname3 ", "Lastname 3", "Addr 3", "Phone 3"]
    r4 = V.fromList ["#4", "Firstname4 ", "Lastname 4", "Addr 4", "Phone 4"]

-- | Make a table using mkTableV-component.
tblWidget :: forall t m. (MonadWidget t m) => m (Dynamic t (Maybe Text))
tblWidget = do
    let
        tblHeaders :: MonadWidget t m => Maybe (HeaderConfV t m)
        tblHeaders = Just $ HeaderConfV
            (def & set thfThAttr (const $ constDyn $ style "width: 150px" def)
                 & set (thfADEs . drawEl) drawDivContent)
            (V.fromList $ const (constDyn def) <$> [1..4]) -- vector of empty ECol's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            def
        capDfs = Just (CaptionConf "Table 1. A Person list table example." def)
        tConf = def
            & set tableCaptionV capDfs
            & set tableHeaderV tblHeaders
            & set tableTableAttrV (constDyn $ style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTrAttr) trAttrfun
            & set (tableTdFunsV . tdfTdAttr) (const $ style "padding: 5px" def)
            & set cellDrawBodyV drawDivContent
            & set cellListenerBodyV (listenMyRow 4)
    rec
        tblSt <- eDiv (setClasses [w3TableAll, w3Hoverable] def) $
                  mkTableV tConf tblContent
        let dCell = _tsDynURelease tblSt
            dActElem = _activeStateElem <$> dCell
    let dmId = (giveId tblContent . giveRowNum) <$> dActElem
    pure dmId
    where
    trAttrfun :: Dynamic t (ActiveState t) → ActElem → Dynamic t ETr
    trAttrfun dAst _ae = mkETr <$> join (_activeStateActive <$> dAst)
      where
        mkETr :: Bool -> ETr
        mkETr b =
               if b
                   then setClasses [w3Green] def
                   else setClasses [] def
                  -- then style "background-color: grey" def
                  -- else style "background-color: lightgrey" def
    giveRowNum :: ActElem -> Maybe Int
    giveRowNum (ActERC (i,_)) = Just i
    giveRowNum _ = Nothing
    giveId :: V.Vector (V.Vector Text) -> Maybe Int -> Maybe Text
    giveId _ Nothing = Nothing
    giveId v (Just i) = Just $ (v V.! i) V.! 0

--------------------------------------------------------------------------------

-- Nav-things here: totally in WIP-state atm.


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
  fmap (\(ClassName cn) -> T.count "active" cn > 0) dcn


mkMenuItemD :: forall t m. (MonadWidget t m)
            => Dynamic t EA -> MenuItem -> m (MI t)
mkMenuItemD cls mi@(MenuItem txt ico) = do
  (e,_) <- eAD' cls $ do
    if ico /= ClassName ""
       then eI (addClass "fa" $ setClasses [ico] def) blank
       else blank
    text txt
  pure $ MI (domEvent Click e) mi


mkMenuFromLsts :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (MI t)
mkMenuFromLsts aLst mLst i = mkMenuItemD (aLst Prelude.!! i) (mLst Prelude.!! i)

mkW3NavItems :: forall t m.
  MonadWidget t m => [MenuItem] -> Dynamic t MenuItem -> m (Dynamic t MenuItem)
mkW3NavItems menuis mi = mdo
  let i = length menuis
      idxLst = [0..(i-1)]
  es :: [MI t] <- mapM (mkMenuFromLsts mas menuis) idxLst
  let ev = tagPromptlyDyn mi (updated mi)
  dE :: Dynamic t MenuItem <- holdDyn (head menuis) $
      leftmost $ map (\e -> miItem e <$ miClicked e) es
    -- <- holdDyn (constDyn $ head menuis) $
    -- leftmost $
    --   -- [constDyn (head menuis) <$ ev] ++
    --     (fmap (\e -> constDyn (miItem e) <$ miClicked e) es)
  -- let dE = join ddE
  let mas = fmap (\j -> ffor dE $ \d -> selActive d j) idxLst
  pure dE
  where
    selActive :: MenuItem -> Int -> EA
    selActive d i =
        if d == menuis Prelude.!! i
           then hR "#" linkCA
           else hR "#" linkC
    linkC = setClasses [w3BarItem, w3Button, w3HideSmall, w3Mobile] def
    linkCA = setClasses [w3BarItem, w3Button, w3HideSmall, w3Mobile, w3Gray] def
    -- ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)

mkW3NavItemsSmall :: forall t m.
  MonadWidget t m => [MenuItem] -> Dynamic t Bool ->  m (Dynamic t MenuItem)
mkW3NavItemsSmall menuis dShow = do
  let i = length menuis
      idxLst = [0..(i-1)]
      dDivAttrs = ffor dShow $ \b ->
        if b
           then setClasses [w3Show] def
           else setClasses [w3Hide] def
  eDivD dDivAttrs $ mdo
    es :: [MI t] <- mapM (mkMenuFromLsts mas menuis) idxLst
    dE :: Dynamic t MenuItem <- holdDyn (head menuis) $
      leftmost (fmap (\e -> miItem e <$ miClicked e) es)
    let mas = fmap (\j -> ffor dE $ \d -> selActive d j) idxLst
    pure dE
  where
    selActive :: MenuItem -> Int -> EA
    selActive d i =
        if d == menuis Prelude.!! i
           then hR "#" linkCA
           else hR "#" linkC
    linkC = setClasses [w3BarItem, w3Button, w3HideMedium, w3HideLarge, w3Mobile] def
    linkCA = setClasses [w3BarItem, w3Button, w3HideMedium,
      w3HideLarge, w3Mobile, w3Blue] def
    hR u = href (URL u)

w3Nav :: (MonadWidget t m) => m (Dynamic t MenuItem)
w3Nav =
  eDiv navAttrs $ mdo
    eSpan (setClasses [w3BarItem] def) $ text "Menu"
    eClk <- eButtonC attrs
      $ eI (addClass "fa" $ setClasses [faBars] def ) blank
    dE2 :: Dynamic t MenuItem <- mkW3NavItems menuItems dE
    dES :: Dynamic t MenuItem <- mkW3NavItemsSmall menuItems dNavToggle
    ddE <- holdDyn (constDyn $ head menuItems) $
      leftmost [dE2 <$ updated dE2, dES <$ updated dES]
    let dE = join ddE
    dNavToggle <- toggle False $ leftmost [eClk, () <$ updated dES]
    pure dE
  where
    navAttrs = style "z-index:4" $ setClasses
      [w3Bar, w3Top, w3White, w3Large] def
    attrs = setClasses [w3BarItem, w3Button, w3HideMedium
                       , w3HideLarge, w3HoverNone, w3HoverTextLightGrey] def
    hR u = href (URL u)
    -- divCl = def


{-
<!-- Top container -->
<div class="w3-bar w3-top w3-black w3-large" style="z-index:4">
  <button class="w3-bar-item w3-button w3-hide-large w3-hover-none w3-hover-text-light-grey" onclick="w3_open();"><i class="fa fa-bars"></i>  Menu</button>
  <span class="w3-bar-item w3-right">Logo</span>
</div>

<!-- Sidebar/menu -->
<nav class="w3-sidebar w3-collapse w3-white w3-animate-left" style="z-index:3;width:300px;" id="mySidebar"><br>
  <div class="w3-container w3-row">
    <div class="w3-col s4">
      <img src="/w3images/avatar2.png" class="w3-circle w3-margin-right" style="width:46px">
    </div>
    <div class="w3-col s8 w3-bar">
      <span>Welcome, <strong>Mike</strong></span><br>
      <a href="#" class="w3-bar-item w3-button"><i class="fa fa-envelope"></i></a>
      <a href="#" class="w3-bar-item w3-button"><i class="fa fa-user"></i></a>
      <a href="#" class="w3-bar-item w3-button"><i class="fa fa-cog"></i></a>
    </div>
  </div>
  <hr>
  <div class="w3-container">
    <h5>Dashboard</h5>
  </div>
  <div class="w3-bar-block">
    <a href="#" class="w3-bar-item w3-button w3-padding-16 w3-hide-large w3-dark-grey w3-hover-black" onclick="w3_close()" title="close menu"><i class="fa fa-remove fa-fw"></i>  Close Menu</a>
    <a href="#" class="w3-bar-item w3-button w3-padding w3-blue"><i class="fa fa-users fa-fw"></i>  Overview</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-eye fa-fw"></i>  Views</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-users fa-fw"></i>  Traffic</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-bullseye fa-fw"></i>  Geo</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-diamond fa-fw"></i>  Orders</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-bell fa-fw"></i>  News</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-bank fa-fw"></i>  General</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-history fa-fw"></i>  History</a>
    <a href="#" class="w3-bar-item w3-button w3-padding"><i class="fa fa-cog fa-fw"></i>  Settings</a><br><br>
  </div>
</nav>
-}
