{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainW where

import           Control.Monad                              (mapM)
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

import           Reflex.Dom.Icon.Raw.Ionicons               as I
import           Reflex.Dom.Theme.Raw.Semantic              as S

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.TableV

------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW ∷ JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headEl bodyEl

------------------------------------------------------------------------------
------------------------------------------------------------------------------

headEl :: MonadWidget t m => m ()
headEl = do
  eTitle def $ text "Main Title"
  eMeta (charSet "utf-8" def) blank
  eMeta (httpEquiv "x-ua-compatible" $ content "ie=edge,chrome=1" def) blank
  eMeta (name "viewport" $
    content "width=device-width, initial-scale=1, maximum-scale=1.0" def) blank
  eLink ( ltStylesheet
    $ href (URL "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css") def) blank
  eLink ( ltStylesheet
    -- $ href (URL "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
    $ href (URL "css/icon.min.css") def) blank
  eLink ( ltStylesheet
    $ href (URL "css/semantic.min.css") def) blank
  eLink ( ltStylesheet $ href (URL "css/sem.css") def) blank



bodyEl :: (MonadWidget t m) => m ()
bodyEl = do
  miD :: Dynamic t MenuItem <- semNav
  eDiv (addClass "main" $ setClasses [semUi, semText, semContainer] def) $ do
    eH1 (setClasses [semUi, semHeader] def) $
      text "Welcome to reflex-dom-themes (Semantic)"
    eP def $ do
      text "Semantic icon trial: "
      eI (setClasses [semInbox,semIcon] def) blank
    showMenuContent miD
  footer

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

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
        sng = ClassName "single"
        fns = defaultTdFuns
            { _tdfADEs = CommonADEfuns (ownListen 4) actMU drawDivContent2 cellEvF
            , _tdfTableAttr = constDyn $ setClasses [semUi, sng
                                , semLine, semSelectable, semTable] def
            }
    rec
            -- mkTableV fns capDfs cPair Nothing tblContent
        tblSt <- ownMkTableV fns capDfs cPair Nothing tblContent dActElem
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
           & \d2 -> d2 { _activeStateActiveCl = constDyn [semActive] }
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
              dETr1 = constDyn $ setClasses [semActive] def :: Dynamic t ETr
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




------------------------------------------------------------------------------

footer :: MonadWidget t m => m ()
footer =
  eDiv (addClass "footer" $ setClasses [semUi, semInverted
                                       , semVertical, semSegment] def) $
    eDiv (setClasses [semUi, semCenter, semAligned, semContainer] def) $
      eDiv (setClasses [semUi,semStackable
                       ,semInverted,semDivided,semGrid] def) $ do
        eDiv (setClasses [semThree, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Group 1"
          eDiv grpAttr $ do
            eA aAttr $ text "Link g1 one"
            eA aAttr $ text "Link g1 two"
            eA aAttr $ text "Link g1 three"
            eA aAttr $ text "Link g1 four"
        eDiv (setClasses [semThree, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Group 2"
          eDiv grpAttr $ do
            eA aAttr $ text "Link g2 one"
            eA aAttr $ text "Link g2 two"
            eA aAttr $ text "Link g2 three"
            eA aAttr $ text "Link g2 four"
        eDiv (setClasses [semThree, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Group 3"
          eDiv grpAttr $ do
            eA aAttr $ text "Link g3 one"
            eA aAttr $ text "Link g3 two"
            eA aAttr $ text "Link g3 three"
            eA aAttr $ text "Link g3 four"
        eDiv (setClasses [semSeven, semWide, semColumn] def) $ do
          eH4 (setClasses [semUi, semInverted, semHeader] def) $
            text "Footer Header"
          eP def $ text "Hmm"

  where
    aAttr = setClasses [semItem] $ href (URL "#") def
    grpAttr = setClasses [semUi, semInverted, semLink, semList] def

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------


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
  [ MenuItem "Active" semExternal
  , MenuItem "Link" semHeartbeat
  , MenuItem "Another link" semEyedropper
  , MenuItem "Link4" semQrcode
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
       then eI (setClasses [ico, semIcon ] def) blank
       else blank
    text txt
    -- let da = hasActive cls
    -- dyn $ ffor da $ \b ->
    --         if b
    --            then eSpan (setClasses [bsSrOnly] def) $ text "(current)"
    --            else pure ()
  pure $ MI (domEvent Click e) mi

mkLiMe :: MonadWidget t m => Dynamic t EA -> MenuItem -> m (MI t)
-- mkLiMe cls mi = eLi (setClasses [bsNavItem] def) $ mkMenuItemD cls mi
mkLiMe = mkMenuItemD

mkLiMeFromLsts :: MonadWidget t m
               => [Dynamic t EA] -> [MenuItem] -> Int -> m (MI t)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)

mkSemNavItems :: forall t m.
  MonadWidget t m => [MenuItem] -> m (Dynamic t MenuItem)
mkSemNavItems menuis = mdo
  let i = length menuis
      idxLst = [0..(i-1)]
  -- eUl ulCl $ mdo
  es :: [MI t] <- mapM (mkLiMeFromLsts mas menuis) idxLst
  -- e1 :: (MI t) <- mkLiMeFromLsts mas menuis 0
  -- e2 <- mkLiMeFromLsts mas menuis 1
  -- e3 <- mkLiMeFromLsts mas menuis 2
  -- e4 <- mkLiMeFromLsts mas menuis 3
  -- let es = [e1,e2,e3,e4]
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
    linkC = setClasses [semItem] def
    linkCA = setClasses [semItem, semActive] def
    -- ulCl = setClasses [bsNavbarNav, bsMrAuto] def
    hR u = href (URL u)


semNav :: (MonadWidget t m) => m (Dynamic t MenuItem)
semNav =
  eDiv navAttrs $ do
    mi <- eDiv uiconAttrs $ do
      _e0 <- eAC (setClasses [semHeader, semItem] $ hR "#" def) $
        text "Project name"
      -- eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
      -- dE2 <- eDiv divCl $ do
      dE2 <- mkSemNavItems menuItems
      pure dE2
      -- pure dE2
    pure mi
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
