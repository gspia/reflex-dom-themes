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

import           Reflex.Dom.Icon.Raw.FI                     as FI
import           Reflex.Dom.Theme.Raw.Foundation            as F

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.TableV


------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW ∷ JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headElFou bodyEl


------------------------------------------------------------------------------
------------------------------------------------------------------------------

headElFou :: MonadWidget t m => m ()
headElFou = do
  eTitle def $ text "Main Title Foundation"
  eMeta (charSet "utf-8" def) blank
  eMeta (httpEquiv "x-ua-compatible" $ content "ie=edge" def) blank
  eMeta (name "viewport" $
    content "width=device-width, initial-scale=1, shrink-to-fit=no" def) blank
  eLink ( ltStylesheet
    $ href
      (URL "https://cdn.jsdelivr.net/foundation-icons/3.0/foundation-icons.min.css")
      def) blank
  eLink ( ltStylesheet
    $ href (URL "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css")
    -- $ integrity "sha256-itWEYdFWzZPBG78bJOOiQIn06QCgN/F0wMDcC4nOhxY="
    $ corsAnon def) blank
  eLink ( ltStylesheet
    $ href (URL "css/fou.css") def) blank


------------------------------------------------------------------------------

bodyEl :: (MonadWidget t m) => m ()
bodyEl = do
  miD :: Dynamic t MenuItem <- fouNav
  eH1 def $ text "Welcome to reflex-dom-themes"
  showMenuContent miD

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
        fns = defaultTdFuns
            { _tdfADEs = CommonADEfuns (ownListen 4) actMU drawDivContent2 cellEvF
            , _tdfTableAttr = constDyn $ setClasses [fouHover, fouTableScroll
                                                    ] def
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
        myHF ∷ ActElem → [ActElem]
        myHF (ActERC (i,_)) = ActErow i: [ActERC (i,j) | j <- [0..(cols-1)]]
        myHF a = [a]
    -- Variant of drawDivContent - (update reflex-dom-htmlea-version to this one)
    drawDivContent2 ∷ ActElem → Text → ActiveState t
               → m (Element EventResult (DomBuilderSpace m) t)
    drawDivContent2 _me elm _actS = do
        (e,_) <- eDivN' $ text elm
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
              dETr1 = constDyn $ style "background-color: lightgreen" def
                  :: Dynamic t ETr
              dETr2 = constDyn $ style "" def :: Dynamic t ETr
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


-- Nav-things here: totally in WIP-state atm.

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
  fmap (\(ClassName cn) -> T.count "is-active" cn > 0) dcn


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
    --            else pure ()
  pure $ MI (domEvent Click e) mi

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
    $ dNmVal "hide-for" "medium" def) $ do
      eButton (setClasses [fouMenuIcon]
        $ btButton
        $ dToggle "responsivemenu" def) blank
      eDiv (setClasses [fouTitleBarTitle] def) $ text "Menu"

  eDiv navAttrs $ do
    dE2 <- eDiv divClL $ do
      dE2 <- mkNavItems menuItemsL
      pure dE2
    pure dE2
  where
    navAttrs = setClasses [fouTopBar] $ id_ "responsivemenu"
      -- $ dNmVal "topbar" ""
      $ dNmVal "toggler" "" def
    divClL = setClasses [fouTopBarLeft] def
    divClR = setClasses [fouTopBarRight] def

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
