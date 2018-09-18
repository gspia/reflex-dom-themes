{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainSem where

import           Control.Lens
import           Control.Monad                              (mapM, join)
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

import qualified Reflex.Dom.HTML5.Attrs                     as A
import qualified Reflex.Dom.HTML5.Elements                  as E

import           Reflex.Dom.Icon.Raw.Ionicons               as I
import           Reflex.Dom.Theme.Raw.Semantic              as S

import           Reflex.Dom.HTML5.Component.Table

------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW ∷ JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headEl bodyEl

------------------------------------------------------------------------------
------------------------------------------------------------------------------

headEl ∷ MonadWidget t m ⇒ m ()
headEl = do
  E.title E.defTitle $ text "Main Title"
  E.meta (A.charSet "utf-8" E.defMeta) blank
  E.meta (A.httpEquiv "x-ua-compatible" $ A.content "ie=edge,chrome=1"
         E.defMeta) blank
  E.meta (A.name "viewport" $
    A.content "width=device-width, initial-scale=1, maximum-scale=1.0"
        E.defMeta) blank
  E.link ( A.ltStylesheet
    $ A.href
    (A.URL "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css")
    E.defLink) blank
  -- E.link ( A.ltStylesheet
    -- A.href (A.URL "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
    --  A.href (A.URL "css/icon.min.css") E.defLink) blank
  -- E.link ( A.ltStylesheet
  --    A.href (A.URL "css/semantic.min.css") E.defLink) blank
  E.link ( A.ltStylesheet
    $ A.href (A.URL "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.2.3/semantic.min.css") E.defLink) blank
  -- E.link ( A.ltStylesheet $ A.href (A.URL "css/sem.css") E.defLink) blank



bodyEl ∷ (MonadWidget t m) ⇒ m ()
bodyEl = do
  miD ∷ Dynamic t MenuItem ← semNav
  E.div ( A.addClass "main"
        $ A.setClasses [semUi, semText, semContainer] E.defDiv) $ do
    E.h1 (A.setClasses [semUi, semHeader] E.defH1) $
      text "Welcome to reflex-dom-themes (Semantic)"
    E.p E.defP $ do
      text "Semantic icon trial: "
      E.i (A.setClasses [semInbox,semIcon] E.defI) blank
    showMenuContent miD
  footer

------------------------------------------------------------------------------

showMenuContent ∷ MonadWidget t m ⇒ Dynamic t MenuItem → m ()
showMenuContent mi = do
    let mt = fmap (T.pack . show) mi
    E.pN $ do
        text "The selected menu items was: "
        dynText mt
    E.h2N $ text "Decorated table example"
    E.pN $ do
        dmTxt ← tblWidget
        E.pN $ dynText $ maybePersonIdTxt <$> dmTxt
        blank
    where
      maybePersonIdTxt (Just txt) = "Person " <> txt <> " is selected."
      maybePersonIdTxt Nothing    = "No person is selected."

------------------------------------------------------------------------------

-- | Set the information for the table.
tblContent ∷ V.Vector (V.Vector Text)
tblContent = V.fromList [r1,r2,r3,r4]
  where
    r1 = V.fromList ["#1", "Firstname1 ", "Lastname 1", "Addr 1", "Phone 1"]
    r2 = V.fromList ["#2", "Firstname2 ", "Lastname 2", "Addr 2", "Phone 2"]
    r3 = V.fromList ["#3", "Firstname3 ", "Lastname 3", "Addr 3", "Phone 3"]
    r4 = V.fromList ["#4", "Firstname4 ", "Lastname 4", "Addr 4", "Phone 4"]

-- | Make a table using mkTableV-component.
tblWidget ∷ forall t m. (MonadWidget t m)
          ⇒ m (Dynamic t (Maybe Text))
tblWidget = do
    let
        tblHeaders ∷ MonadWidget t m ⇒ Maybe (HeaderConf t m ())
        tblHeaders = Just $ HeaderConf
            (defThFuns
                & set thfThAttr (const $ constDyn $ A.style "width: 150px" E.defTh)
                & set (thfADEs . adeDraw) drawDivContent)
            (V.fromList $ const (constDyn E.defCol) <$> [1..4])
            -- vector of empty E.Col's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            (constDyn E.defThead)
        capDfs = Just (CaptionConf "Table 1. A Person list table example."
                      (constDyn E.defCaption))
        tConf = defTableConf
            & set tableCaption capDfs
            & set tableHeader tblHeaders
            & set tableTableAttr (constDyn $ A.setClasses [semUi
                                 , A.ClassName "single"
                                 , semLine, semSelectable, semTable]
                                $ A.style "border-collapse: collapse" E.defTable)
            & set (tableTdFuns . tdfTrAttr) trAttrfun
            & set (tableTdFuns . tdfTdAttr)
                  (const $ A.style "padding: 5px" (constDyn E.defTd))
            & set cellDrawBody drawDivContent
            & set cellListenerBody (listenMyRow 4)
    rec
        tblSt ∷ CompState t ActElem () ← mkTable tConf tblContent
        let dCell = _csDynURelease tblSt
            dActElem = _activeStateElemId <$> dCell
    let dmId = (giveId tblContent . giveRowNum) <$> dActElem
    pure dmId
    where
    trAttrfun ∷ Dynamic t (ActiveState t ActElem ()) → ActElem → Dynamic t E.Tr
    trAttrfun dAst _ae = mkETr <$> join (_activeStateActive <$> dAst)
      where
        mkETr ∷ Bool → E.Tr
        mkETr b =
               if b
                   then A.setClasses [semActive] E.defTr
                   else A.setClasses [] E.defTr
                  -- then A.style "background-color: grey" E.defTr
                  -- else A.style "background-color: lightgrey" E.defTr
    giveRowNum ∷ ActElem → Maybe Int
    giveRowNum (ActERC (i,_)) = Just i
    giveRowNum _ = Nothing
    giveId ∷ V.Vector (V.Vector Text) → Maybe Int → Maybe Text
    giveId _ Nothing = Nothing
    giveId v (Just i) = Just $ (v V.! i) V.! 0


------------------------------------------------------------------------------

footer ∷ MonadWidget t m ⇒ m ()
footer =
  E.div (A.addClass "footer"
        $ A.setClasses [semUi, semInverted , semVertical, semSegment] E.defDiv) $
    E.div (A.setClasses [semUi, semCenter, semAligned, semContainer] E.defDiv) $
      E.div (A.setClasses [semUi,semStackable
                          ,semInverted,semDivided,semGrid] E.defDiv) $ do
        E.div (A.setClasses [semThree, semWide, semColumn] E.defDiv) $ do
          E.h4 (A.setClasses [semUi, semInverted, semHeader] E.defH4) $
            text "Group 1"
          E.div grpAttr $ do
            E.a aAttr $ text "Link g1 one"
            E.a aAttr $ text "Link g1 two"
            E.a aAttr $ text "Link g1 three"
            E.a aAttr $ text "Link g1 four"
        E.div (A.setClasses [semThree, semWide, semColumn] E.defDiv) $ do
          E.h4 (A.setClasses [semUi, semInverted, semHeader] E.defH4) $
            text "Group 2"
          E.div grpAttr $ do
            E.a aAttr $ text "Link g2 one"
            E.a aAttr $ text "Link g2 two"
            E.a aAttr $ text "Link g2 three"
            E.a aAttr $ text "Link g2 four"
        E.div (A.setClasses [semThree, semWide, semColumn] E.defDiv) $ do
          E.h4 (A.setClasses [semUi, semInverted, semHeader] E.defH4) $
            text "Group 3"
          E.div grpAttr $ do
            E.a aAttr $ text "Link g3 one"
            E.a aAttr $ text "Link g3 two"
            E.a aAttr $ text "Link g3 three"
            E.a aAttr $ text "Link g3 four"
        E.div (A.setClasses [semSeven, semWide, semColumn] E.defDiv) $ do
          E.h4 (A.setClasses [semUi, semInverted, semHeader] E.defH4) $
            text "Footer Header"
          E.p E.defP $ text "Hmm"

  where
    aAttr = A.setClasses [semItem] $ A.href (A.URL "#") E.defA
    grpAttr = A.setClasses [semUi, semInverted, semLink, semList] E.defDiv

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
data MenuItem = MenuItem Text A.ClassName
  deriving (Eq,Show)

data MI t
  = MI { miClicked ∷ Event t ()
       , miItem    ∷ MenuItem
       }

menuItems ∷ [MenuItem]
menuItems =
  [ MenuItem "Active" semExternal
  , MenuItem "Link" semHeartbeat
  , MenuItem "Another link" semEyedropper
  , MenuItem "Link4" semQrcode
  ]

hasActive ∷ Reflex t ⇒ Dynamic t E.A → Dynamic t Bool
hasActive ea = do
  let dcn = fmap A.attrGetClassName ea
  fmap (\(A.ClassName cn) → T.count "active" cn > 0) dcn


mkMenuItemD ∷ forall t m. (MonadWidget t m)
            ⇒ Dynamic t E.A → MenuItem → m (MI t)
mkMenuItemD cls mi@(MenuItem txt ico) = do
  (e,_) ← E.aD' cls $ do
    if ico /= A.ClassName ""
       then E.i (A.setClasses [ico, semIcon ] E.defI) blank
       else blank
    text txt
    -- let da = hasActive cls
    -- dyn $ ffor da $ \b →
    --         if b
    --            then E.span (A.setClasses [bsSrOnly] E.defSpan) $ text "(current)"
    --            else pure ()
  pure $ MI (domEvent Click e) mi

mkLiMe ∷ MonadWidget t m ⇒ Dynamic t E.A → MenuItem → m (MI t)
-- mkLiMe cls mi = E.li (A.setClasses [bsNavItem] E.defLi) $ mkMenuItemD cls mi
mkLiMe = mkMenuItemD

mkLiMeFromLsts ∷ MonadWidget t m
               ⇒ [Dynamic t E.A] → [MenuItem] → Int → m (MI t)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)

mkSemNavItems ∷ forall t m.
  MonadWidget t m ⇒ [MenuItem] → m (Dynamic t MenuItem)
mkSemNavItems menuis = mdo
  let i = length menuis
      idxLst = [0..(i-1)]
  -- eUl ulCl $ mdo
  es ∷ [MI t] ← mapM (mkLiMeFromLsts mas menuis) idxLst
  -- e1 ∷ (MI t) ← mkLiMeFromLsts mas menuis 0
  -- e2 ← mkLiMeFromLsts mas menuis 1
  -- e3 ← mkLiMeFromLsts mas menuis 2
  -- e4 ← mkLiMeFromLsts mas menuis 3
  -- let es = [e1,e2,e3,e4]
  dE ∷ Dynamic t MenuItem ← holdDyn (head menuis) $
    leftmost (fmap (\e → miItem e <$ miClicked e) es)
  let mas = fmap (\j → ffor dE $ \d → selActive d j) idxLst
  pure dE
  where
    selActive ∷ MenuItem → Int → E.A
    selActive d i =
        if d == menuis Prelude.!! i
           then hR "#" linkCA
           else hR "#" linkC
    linkC = A.setClasses [semItem] E.defA
    linkCA = A.setClasses [semItem, semActive] E.defA
    -- ulCl = A.setClasses [bsNavbarNav, bsMrAuto] E.defA
    hR u = A.href (A.URL u)


semNav ∷ (MonadWidget t m) ⇒ m (Dynamic t MenuItem)
semNav =
  E.div navAttrs $ do
    mi ← E.div uiconAttrs $ do
      _e0 ← E.aC (A.setClasses [semHeader, semItem] $ hR "#" E.defA) $
        text "Project name"
      -- eButton bAttrs $ eSpan (setClasses [bsNavbarTogglerIcon] def ) blank
      -- dE2 ← eDiv divCl $ do
      dE2 ← mkSemNavItems menuItems
      pure dE2
      -- pure dE2
    pure mi
  where
    navAttrs = A.setClasses [semUi,semFixed, semInverted, semMenu] E.defDiv
    uiconAttrs = A.setClasses [semUi, semContainer] E.defDiv
    -- bAttrs = A.setClasses [bsNavbarToggler] $ A.btSubmit
    --   $ A.dToggle "collapse" $ A.dTarget "#navbarSupportedContent"
    --   $ A.aControls "navbarSupportedContent" $ A.aExpanded "false"
    --   $ A.aLabel "Toggle navigation" E.defButton
    hR u = A.href (A.URL u)
    -- divCl = A.setClasses [bsCollapse, bsNavbarCollapse]
    --   $ A.id_ "navbarSupportedContent" E.defDiv

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
