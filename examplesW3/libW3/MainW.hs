{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainW where

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

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.TableV

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


-- | Setup part of the header information.
-- Note: reflex-dom-htmlea clearly needs a handy default-method for this one.
tblHeaders :: forall t. Reflex t => ColHeaderV t
tblHeaders = ColHeaderV
    (V.fromList $ const (constDyn def) <$> [1..4]) -- vector of empty ECol's.
    (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
    def -- empty EThead, too.
    -- (V.Vector (Dynamic t ECol)) (V.Vector Text) (Dynamic t EThead)

-- | Set the information for the table.
tblContent :: V.Vector (V.Vector Text)
tblContent = V.fromList [r1,r2,r3,r4]
  where
    r1 = V.fromList ["#1", "Firstname1 ", "Lastname 1", "Addr 1", "Phone 1"]
    r2 = V.fromList ["#2", "Firstname2 ", "Lastname 2", "Addr 2", "Phone 2"]
    r3 = V.fromList ["#3", "Firstname3 ", "Lastname 3", "Addr 3", "Phone 3"]
    r4 = V.fromList ["#4", "Firstname4 ", "Lastname 4", "Addr 4", "Phone 4"]

-- | Make a table using mkTableV-component.
-- Note: some of the default-structures of reflex-dom-htmlea clearly needs
-- to be changed a bit so that the basic usage of tables would be easier.
-- E.g. we need an easy mechanism to update tr-attributes based on different
-- conditions and events (something like the cells or td's do have).
tblWidget :: forall t m. (MonadWidget t m) => m (Dynamic t (Maybe Text))
tblWidget = do
    let
        hns = defaultThFuns
            { _thfThAttr = const $ constDyn $ style "width: 150px" def
            , _thfADEs = (_thfADEs defaultThFuns)
                { _drawEl = drawDivContent2
                -- , _actSt = listenMyCol 4
                }
            }
        cPair = Just (hns, tblHeaders)
        capDfs = Just (CaptionDef "Table 1. A Person list table example." def)
        fns = defaultTdFuns
            { _tdfADEs = CommonADEfuns (ownListen 4) actMU drawDivContent2 cellEvF }
    rec
        tblSt <- eDiv (setClasses [w3TableAll, w3Hoverable] def) $
            -- mkTableV fns capDfs cPair Nothing tblContent
            ownMkTableV fns capDfs cPair Nothing tblContent dActElem
        let dCell = _tsDynURelease tblSt
            dActElem = _activeStateMe <$> dCell
    let dmId = (giveId tblContent . giveRowNum) <$> dActElem
        dTrA = _tdfTrAttr fns
    pure dmId
    where
    -- Listen for clicks on tbody-cells and activate row on click
    -- (update reflex-dom-htmlea to have a row listening listener).
    ownListen cols ae = actstate ae & \d -> d {_activeStateListen = constDyn ag }
      where
        ag = ActiveGroup $ Set.fromList $ ae: myHF ae
        actstate txt = def & \d1 -> d1 {_activeStateMe = txt }
           & \d2 -> d2 { _activeStateActiveCl = constDyn [w3Green] }
           & \d3 -> d3 { _activeStateNotActiveCl = constDyn [] }
        myHF ∷ ActElem → [ActElem]
        myHF (ActERC (i,_)) = ActErow i: [ActERC (i,j) | j <- [0..(cols-1)]]
        myHF a = [a]
    -- Variant of drawDivContent - (update reflex-dom-htmlea-version to this one)
    drawDivContent2 _me elm actS = do
        let dA = _activeStateActive actS
            dACl = _activeStateActiveCl actS
            dNACl = _activeStateNotActiveCl actS
            dUse :: Dynamic t [ClassName]
            dUse = (\ba acl nacl -> if ba then acl else nacl
                   ) <$> dA <*> dACl <*> dNACl
            dCl = fmap (`setClasses` def) dUse
        (e,_) <- eDivD' dCl $ text elm
        pure e
    giveRowNum :: ActElem -> Maybe Int
    giveRowNum (ActERC (i,_)) = Just i
    giveRowNum _ = Nothing
    giveId :: V.Vector (V.Vector Text) -> Maybe Int -> Maybe Text
    giveId _ Nothing = Nothing
    giveId v (Just i) = Just $ (v V.! i) V.! 0

-- | We make our own version here until this is fixed in reflex-dom-htmlea.
-- Especially, we want to override the mkRow-method.
-- Here we apply a dirty trick.
ownMkTableV ∷ forall t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒  TdFuns t m a
         → Maybe (CaptionDef t)
         → Maybe (ThFuns t m, ColHeaderV t)
         → Maybe (TfootFuns t m, FootDefsV t)
         → V.Vector (V.Vector a)
         -> Dynamic t ActElem
         → m (TableState t)
ownMkTableV tdFs mcapdefs mColdefs mFootdefs acElmsVV dSelRowAE = mdo
    let elms = V.imap mkA4r acElmsVV
    (htTable, tblState) <- eTableD' (_tdfTableAttr tdFs) $ mdo
        mkCaption mcapdefs
        evB2 <- mkTheadV mColdefs evB tblSt
        (_,evB1) <- eTbodyD' (_tdfTbodyAttr tdFs) $ do
            ets <- V.imapM (mkRow evB tblSt) elms
            pure $ leftmost $ V.toList ets
        evB3 <- mkTfootV mFootdefs evB tblSt
        let evB = leftmost [evB1, evB2, evB3]
        tblSt <- updateTableState tblOE evB
        pure tblSt
    tblOE <- tableEvents htTable
    pure tblState
    where
      mkA4r ∷ Int → V.Vector a → V.Vector (ActElem, a)
      mkA4r i = V.imap (\j e -> (ActERC (i,j),e))
      sameRow :: ActElem -> ActElem -> Bool
      sameRow (ActERC (i,_)) (ActERC (k,_)) = i == k
      sameRow (ActERC (i,_)) (ActErow k   ) = i == k
      sameRow (ActErow i   ) (ActERC (k,_)) = i == k
      sameRow (ActErow i   ) (ActErow k   ) = i == k
      sameRow _ _ = False
      mkRow ∷ Event t (TableEvent t)
            → TableState t → Int → V.Vector (ActElem, a)
            → m (Event t (TableEvent t))
      mkRow evB tblSt2 i v = do
          let ae2attr = _tdfTrAttr tdFs :: ActElem -> Dynamic t ETr
              dETr1 = constDyn $ setClasses [w3Green] def :: Dynamic t ETr
              dETr2 = constDyn $ setClasses [] def :: Dynamic t ETr
              ae2a ae = (\sr t1 t2 -> if sameRow ae sr then t1 else t2
                        ) <$> dSelRowAE <*> dETr1 <*> dETr2
          eTrD (ae2a $ ActErow i) $ do
              eds <- V.forM v
                    (\(ve,ae) -> eTdD (_tdfTdAttr tdFs ve)
                        $ _tdfCombFun tdFs tdFs ve ae evB tblSt2 )
              pure $ tblEvFiring $ V.toList eds
              -- Note that in the _tdCombFun tdFs tdFs the first is used
              -- to get the combining function and call it. The second is
              -- a parameter to that function.



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
