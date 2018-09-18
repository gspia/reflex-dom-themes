{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainFou where

import           Control.Lens
import           Control.Monad                              (mapM, join, void)
import           Control.Monad.Fix
import qualified Data.Map                                   as Map
import           Data.Monoid
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Data.Tree
import qualified Data.Vector                                as V
import           Language.Javascript.JSaddle                (JSM, liftJSM, MonadJSM)

import           Reflex.Dom                                 hiding (mainWidget,
                                                             mainWidgetWithHead)
import           Reflex.Dom.Core                            (mainWidget,
                                                             mainWidgetWithHead)

------------------------------------------------------------------------------

import qualified Reflex.Dom.HTML5.Attrs                     as A
import qualified Reflex.Dom.HTML5.Elements                  as E
import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Tree
import           Reflex.Dom.HTML5.Component.Table

------------------------------------------------------------------------------

import           Reflex.Dom.Icon.Raw.FI           -- as FI
import           Reflex.Dom.Icon.Raw.FlagIcons    -- as FL
import           Reflex.Dom.Theme.Raw.Foundation  -- as F


import           Reflex.Dom.Component.MenuCommon
import           Reflex.Dom.Component.Foundation.Menu
import           Reflex.Dom.Component.Foundation.Dropdown
import           Reflex.Dom.Component.NaiveI18n


------------------------------------------------------------------------------

mainW ∷ JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headElFou bodyEl


------------------------------------------------------------------------------
------------------------------------------------------------------------------

headElFou ∷ MonadWidget t m ⇒ m ()
headElFou = do
  E.title E.defTitle $ text "Main Title Foundation"
  E.meta (A.charSet "utf-8" E.defMeta) blank
  E.meta (A.httpEquiv "x-ua-compatible" $ A.content "ie=edge" E.defMeta) blank
  E.meta (A.name "viewport" $
    A.content "width=device-width, initial-scale=1, shrink-to-fit=no" E.defMeta) blank
  E.link ( A.ltStylesheet
    $ A.href
      (A.URL "https://cdn.jsdelivr.net/foundation-icons/3.0/foundation-icons.min.css")
      E.defLink) blank
  E.link ( A.ltStylesheet
     $ A.href (A.URL "https://cdn.jsdelivr.net/npm/foundation-sites@6.5.0-rc.2/dist/css/foundation.min.css")
     $ A.corsAnon E.defLink) blank
  -- E.link ( A.ltStylesheet
  --    A.href (A.URL "https://cdn.jsdelivr.net/npm/foundation-sites@6.5.0-rc.2/dist/js/foundation.min.js")
    --  A.href (A.URL "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css")
    --  A.href (A.URL "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css")
    -- integrity "sha256-itWEYdFWzZPBG78bJOOiQIn06QCgN/F0wMDcC4nOhxY="
    --  A.corsAnon E.defLink) blank
  E.link ( A.ltStylesheet
    $ A.href (A.URL "css/fou.css") E.defLink) blank

{-
<!-- Compressed CSS -->
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/foundation-sites@6.5.0-rc.2/dist/css/foundation.min.css" integrity="sha256-iJQ8dZac/jUYHxiEnZJsyVpKcdq2sQvdA7t02QFmp30= sha384-SplqNBo/0ZlvSdwrP/riIPDozO5ck8+yIm++KVqyMAC53S6m3BaV+2OLpi7ULOOh sha512-ho6hK4sAWdCeqopNZWNy1d9Ok2hzfTLQLcGSr8ZlRzDzh6tNHkVoqSl6wgLsqls3yazwiG9H9dBCtSfPuiLRCQ==" crossorigin="anonymous">

<!-- Compressed JavaScript -->
<script src="https://cdn.jsdelivr.net/npm/foundation-sites@6.5.0-rc.2/dist/js/foundation.min.js" integrity="sha256-G6jsRyH1fxbsvFIXSCuwYmI1aIDYBa28xscrvmYjJy0= sha384-vtoG68NvPc9azmFJr447vvY8qgdyA4FdaJ5/bqvzIM4eAdZfO0iyRRF8l2AAscYI sha512-43seCcNrHA0BQgrtyajB9sp8yOdv5c8QdYvgjP7zJ7v+dmzAcxYDQ2gupb9aztsNWBq1COIp/3NHYkQs4l/dkg==" crossorigin="anonymous"></script>
-}

------------------------------------------------------------------------------

bodyEl ∷ forall t m. (MonadWidget t m)
       ⇒ m ()
bodyEl = do
    evPB ∷ Event t () ← getPostBuild
    let langMapGB = getMiLangMap LaGB
        exTrL  = exTree ∷ Tree (MenuItemConfig t MenuTag MenuGroups)
        exTrLR = exTree2 ∷ Tree (MenuItemConfig t MenuTag MenuGroups)
        exLst  = exList ∷ [MenuItemConfig t MenuTag MenuGroups]
        exHd   = MenuItemConfig MyPrg [] never Nothing
    -- miD ∷ Dynamic t MenuItem ← fouNav
    rec
        dLang ← holdDyn langMapGB eLang
        let eMenuLR  = ffilter (== MenuChoiceLR ) eMt
            eMenuL   = ffilter (== MenuChoiceL  ) eMt
            eMenuLst = ffilter (== MenuChoiceLst) eMt
        deMt ∷ Dynamic t (Event t MenuTag)
              ← widgetHold (mkLRMenuBarTag exTrLR dLang updLang) $ leftmost
                [ mkLMenuBarTag  exTrL  dLang updLang  <$ eMenuL
                , mkLRMenuBarTag exTrLR dLang updLang  <$ eMenuLR
                , mkListMenuBarTag exHd exLst dLang updLang <$ eMenuLst
                ]
        let eMt = switch . current $ deMt
        let eLa ∷ Event t UiLang = -- traceEvent "body, eLa" $
                  (\mt → case mt of
                    LangDe → LaDE
                    LangSe → LaSE
                    LangGb → LaGB
                  ) <$> ffilter
                    (\mt → mt == LangSe || mt == LangGb || mt == LangDe) eMt
            eLang ∷ Event t (LangMap MenuTag) = -- traceEvent "body, eLang" $
                leftmost [ getMiLangMap <$> eLa
                         , langMapGB <$ evPB -- important
                         ]
        dMt ← holdDyn MoPg eMt
    let
        -- eMTable  = traceEvent "body, eMTable" $ ffilter (== TablePg) eMt
        -- eMSelect = traceEvent "body, eMSelect" $ ffilter (== SelectPg) eMt
        eMWDEP = traceEvent "body, eMWDEP (weekday)" $
            ffilter (`elem` [MoPg, TuPg, WePg, ThPg, FrPg]) eMt
    dLa ← holdDyn LaGB eLa
    E.h1N $ text "Welcome to example prg"
    -- E.h1 E.defH1 $ text "Welcome to reflex-dom-themes"
    E.pN $ do
        text "hmm : "
        dynText $ (T.pack . show) <$> dLa
        text " : "
    rec
        void $ widgetHold (blank >> pure never) $ leftmost
                [ showAlmostEmpty dMt <$ eMWDEP
                , showAlmostEmpty dMt <$ evPB
                -- , (blank >> pure never) <$ eMTable
                -- , (blank >> pure never) <$ eMSelect
                ]
        --
        {- devIntR ∷ Dynamic t (Event t Int)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showTableContentR dMt dR <$ eMTable
                , (blank >> pure never) <$ eMWPEP
                , (blank >> pure never) <$ eMSelect
                ]
        let evIntR = leftmost [(-2) <$ evPB, switch . current $ devIntR ]
        dIntR ∷ Dynamic t Int ← holdDyn (-3) evIntR
        dR ∷ Dynamic t Int ← holdUniqDyn dIntR
        --
        devIntC ∷ Dynamic t (Event t Int)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showTableContentC dMt dC <$ eMTable
                , (blank >> pure never) <$ eMWPEP
                , (blank >> pure never) <$ eMSelect
                ]
        let evIntC = leftmost [(-2) <$ evPB, switch . current $ devIntC ]
        dIntC ∷ Dynamic t Int ← holdDyn (-3) evIntC
        dC ∷ Dynamic t Int ← holdUniqDyn dIntC
        -- dC ∷ Dynamic t Int ← holdUniqDyn dIntC
        -- let dIntP = zipDynWith (,) dR dC
        let ddItems = miDD ∷ Tree (MenuItemConfig t DDtags DDgroups)
        -}
        --
        {- devDd ∷ Dynamic t (Event t DDtags)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showSelectContent ddItems dMt dLa dDd updLang <$ eMSelect
                , (blank >> pure never) <$ eMWPEP
                , (blank >> pure never) <$ eMTable
                ]
        let evDd = leftmost [Select0DD <$ evPB, switch . current $ devDd ]
        dDd2 ∷ Dynamic t DDtags ← holdDyn Select0DD evDd
        dDd ∷ Dynamic t DDtags ← holdUniqDyn dDd2
        -}
    -- Show that the state is maintained.
    E.pN $ do
        text "hmm : "
        dynText $ (T.pack . show) <$> dLa
        text " : "
        -- dynText $ (T.pack . show) <$> dDd
        -- text " : "
        -- dynText $ (T.pack . show) <$> dR
        -- text " : "
        -- dynText $ (T.pack . show) <$> dC
        -- text " : "
        -- dynText $ (T.pack . show) <$> dDd
    blank
    -- showMenuContent miD


--------------------------------------------------------------------------------

showAlmostEmpty ∷ forall t m. MonadWidget t m
        ⇒ Dynamic t MenuTag
        → m (Event t ())
showAlmostEmpty dMt = do
    let mt = fmap (T.pack . show) dMt
    E.h2N $ text "A view having no components to show"
    E.pN $ do
        text "The selected menu item was: "
        dynText mt
    pure never



------------------------------------------------------------------------------

{-
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
-}

------------------------------------------------------------------------------

-- | Set the information for the table.
tblContent ∷ V.Vector (V.Vector Text)
tblContent = V.fromList [r1,r2,r3,r4]
  where
    r1 = V.fromList ["#1", "Firstname1 ", "Lastname 1", "Addr 1", "Phone 1"]
    r2 = V.fromList ["#2", "Firstname2 ", "Lastname 2", "Addr 2", "Phone 2"]
    r3 = V.fromList ["#3", "Firstname3 ", "Lastname 3", "Addr 3", "Phone 3"]
    r4 = V.fromList ["#4", "Firstname4 ", "Lastname 4", "Addr 4", "Phone 4"]

-- | Make a table using mkTable-component.
tblWidget ∷ forall t m. (MonadWidget t m)
          ⇒ m (Dynamic t (Maybe Text))
tblWidget = do
    let
        tblHeaders ∷ MonadWidget t m ⇒ Maybe (HeaderConf t m ())
        tblHeaders = Just $ HeaderConf
            (defThFuns
                & set thfThAttr (const $ constDyn $ A.style "width: 150px" E.defTh)
                & set (thfADEs . adeDraw) drawDivContent)
            (V.fromList $ const (constDyn E.defCol) <$> [1..4]) -- vec of empty E.Col's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            (constDyn E.defThead)
        capDfs = Just (CaptionConf "Table 1. A Person list table example."
                      (constDyn E.defCaption))
        tConf = defTableConf
            & set tableCaption capDfs
            & set tableHeader tblHeaders
            & set tableTableAttr (constDyn $ A.setClasses [fouHover, fouTableScroll]
                                $ A.style "border-collapse: collapse" E.defTable)
            & set (tableTdFuns . tdfTrAttr) trAttrfun
            & set (tableTdFuns . tdfTdAttr) (const$ A.style "padding: 5px"
                                            (constDyn E.defTd))
            & set cellDrawBody drawDivContent
            & set cellListenerBody (listenMyRow 4)
    rec
        tblSt ← mkTable tConf tblContent
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
                   then A.style "background-color: lightgreen" E.defTr
                   else A.style "" E.defTr
                  -- then A.style "background-color: grey" E.defTr
                  -- else A.style "background-color: lightgrey" E.defTr
    giveRowNum ∷ ActElem → Maybe Int
    giveRowNum (ActERC (i,_)) = Just i
    giveRowNum _ = Nothing
    giveId ∷ V.Vector (V.Vector Text) → Maybe Int → Maybe Text
    giveId _ Nothing = Nothing
    giveId v (Just i) = Just $ (v V.! i) V.! 0

------------------------------------------------------------------------------

-- Nav-things here: totally in WIP-state atm.


data MenuTag = MyPrg | MoPg | TuPg | WePg | ThPg | FrPg | JaPg | FePg | MaPg
        | MenuChoice | MenuChoiceL | MenuChoiceLst | MenuChoiceLR
        | LoginPg | LogoutPg
        | LangChoice | LangGb | LangDe | LangSe
        | LMenu | RMenu
    deriving (Eq, Show, Ord)

instance ActSretval MenuTag where defRetval = MoPg

data MenuGroups = MenuChGroup | MenuLaGroup deriving (Eq, Show, Ord)

data UiLang = LaGB | LaDE | LaSE | LaIso Text
    deriving (Eq, Show, Ord)

exTree ∷ Reflex t ⇒ Tree (MenuItemConfig t MenuTag MenuGroups)
exTree = Node (MenuItemConfig MyPrg [] never Nothing)
    [ Node (MenuItemConfig MoPg [fiHeart] never Nothing) []
    , Node (MenuItemConfig TuPg [fiStar] never Nothing) []
    , Node (MenuItemConfig WePg [fiGraphBar] never Nothing) []
    , Node (MenuItemConfig ThPg [fiTorso] never Nothing) []
    , Node (MenuItemConfig FrPg [fiWheelchair] never Nothing) []
    , Node (MenuItemConfig MenuChoice [] never Nothing)
        [ Node (MenuItemConfig MenuChoiceLR  [] never (Just MenuChGroup) ) []
        , Node (MenuItemConfig MenuChoiceL   [] never (Just MenuChGroup) ) []
        , Node (MenuItemConfig MenuChoiceLst [] never (Just MenuChGroup) ) []
        ]
    , Node (MenuItemConfig LoginPg  [] never Nothing) []
    , Node (MenuItemConfig LogoutPg [] never Nothing) []
    , Node (MenuItemConfig LangChoice [] never Nothing)
        [ Node (MenuItemConfig LangGb [flagIcon, flagIconGb] never (Just MenuLaGroup) ) []
        , Node (MenuItemConfig LangDe [flagIcon, flagIconDe] never (Just MenuLaGroup) ) []
        , Node (MenuItemConfig LangSe [flagIcon, flagIconSe] never (Just MenuLaGroup) ) []
        ]
    ]

exTree2 ∷ Reflex t ⇒ Tree (MenuItemConfig t MenuTag MenuGroups)
exTree2 = Node (MenuItemConfig MyPrg [] never Nothing)
    [ Node (MenuItemConfig LMenu [] never Nothing)
        [ Node (MenuItemConfig MoPg [fiHeart] never Nothing) []
        , Node (MenuItemConfig TuPg [fiStar] never Nothing) []
        , Node (MenuItemConfig WePg [fiGraphBar] never Nothing) []
        , Node (MenuItemConfig ThPg [fiTorso] never Nothing) []
        , Node (MenuItemConfig FrPg [fiWheelchair] never Nothing) []
        , Node (MenuItemConfig MenuChoice [] never Nothing)
            [ Node (MenuItemConfig MenuChoiceLR  [] never (Just MenuChGroup) ) []
            , Node (MenuItemConfig MenuChoiceL   [] never (Just MenuChGroup) ) []
            , Node (MenuItemConfig MenuChoiceLst [] never (Just MenuChGroup) ) []
            ]
        ]
    , Node (MenuItemConfig RMenu [] never Nothing)
        [ Node (MenuItemConfig LoginPg  [] never Nothing) []
        , Node (MenuItemConfig LogoutPg [] never Nothing) []
        , Node (MenuItemConfig LangChoice [] never Nothing)
            [ Node (MenuItemConfig LangGb [flagIcon, flagIconGb] never (Just MenuLaGroup) ) []
            , Node (MenuItemConfig LangDe [flagIcon, flagIconDe] never (Just MenuLaGroup) ) []
            , Node (MenuItemConfig LangSe [flagIcon, flagIconSe] never (Just MenuLaGroup) ) []
            ]
        ]
    ]

exList ∷ Reflex t ⇒ [MenuItemConfig t MenuTag MenuGroups]
exList =
    [ MenuItemConfig MoPg [] never Nothing
    , MenuItemConfig TuPg [] never Nothing
    , MenuItemConfig WePg [] never Nothing
    , MenuItemConfig ThPg [] never Nothing
    , MenuItemConfig FrPg [] never Nothing
    , MenuItemConfig MenuChoiceLR [] never (Just MenuChGroup)
    , MenuItemConfig MenuChoiceL [] never (Just MenuChGroup)
    , MenuItemConfig MenuChoiceLst [] never (Just MenuChGroup)
    , MenuItemConfig LoginPg [] never Nothing
    , MenuItemConfig LogoutPg [] never Nothing
    , MenuItemConfig LangGb [] never (Just MenuLaGroup)
    , MenuItemConfig LangDe [] never (Just MenuLaGroup)
    , MenuItemConfig LangSe [] never (Just MenuLaGroup)
    ]

{-
menuItemsL ∷ [MenuItem]
menuItemsL =
  [ MenuItem "Monday" fiHeart
  , MenuItem "Tuesday" fiStar
  , MenuItem "Wednesday" fiGraphBar
  , MenuItem "Thursday" fiTorso
  , MenuItem "Friday" fiWheelchair
  ]

menuItemsR ∷ [MenuItem]
menuItemsR =
  [ MenuItem "January" fiBraille
  , MenuItem "February" fiElevator
  , MenuItem "March" fiArrowsOut
  ]
hasFouActive ∷ Reflex t ⇒ Dynamic t E.A → Dynamic t Bool
hasFouActive ea = do
  let dcn = fmap A.attrGetClassName ea
  fmap (\(A.ClassName cn) → T.count "is-active" cn > 0) dcn
-}

updLang ∷ forall t a. (Reflex t, Ord a, Show a)
        ⇒ Dynamic t (LangMap a) → MenuItem t a → MenuItem t a
updLang dLM mi =
    mi { _menuItemLabel
            = fmap (\(lm,oTxt) →
                   case chLb lm of
                    Just nTxt → nTxt
                    Nothing → oTxt
                   ) $ zipDyn dLM (_menuItemLabel mi)
       }
  where
    chLb ∷ LangMap a → Maybe Text
    chLb lm = mtLabel (_menuItemMenuTag mi) lm


{-
mkFouMenuItemD ∷ forall t m. (MonadWidget t m)
            ⇒ MenuItem → m (MI t)
mkFouMenuItemD mi@(MenuItem txt ico) = do
  (e,_) ← E.a' (A.href (A.URL "#") E.defA) $ do
    if ico /= A.ClassName ""
       then E.i (A.setClasses [ico] E.defI) blank
       else blank
    E.span E.defSpan $ text txt
    -- let da = hasFouActive cls
    -- dyn $ ffor da $ \b →
    --         if b
    --            then eSpan (setClasses [bsSrOnly] def) $ text "(current)"
    --            else pure ()
  pure $ MI (domEvent Click e) mi

mkLiMe ∷ MonadWidget t m ⇒ Dynamic t E.Li → MenuItem → m (MI t)
mkLiMe cls mi = E.liD cls $ mkFouMenuItemD mi

mkLiMeFromLsts ∷ MonadWidget t m
               ⇒ [Dynamic t E.Li] → [MenuItem] → Int → m (MI t)
mkLiMeFromLsts aLst mLst i = mkLiMe (aLst Prelude.!! i) (mLst Prelude.!! i)

mkNavItems ∷ forall t m.
  MonadWidget t m ⇒ [MenuItem] → m (Dynamic t MenuItem)
mkNavItems menuis = do
  let i = length menuis
      idxLst = [0..(i-1)]
  E.ul ulCl $ mdo
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
    selActive ∷ MenuItem → Int → E.Li
    selActive d i =
        if d == (menuis Prelude.!! i)
           then A.setClasses [fouIsActive] E.defLi
           else E.defLi
    -- ulCl = setClasses [fouDropdown, fouMediumHorizontal] def
    -- ulCl = setClasses [fouMenu] $ dNmVal "responsive-menu" "drilldown" $ def
    ulCl = A.setClasses [fouMenu] E.defUl
    -- ulCl = setClasses [fouDropdown, fouMenu] $ dNmVal "dropdown-menu" "" $ def
    hR u = A.href (A.URL u)
-}

{-
fouNav ∷ (MonadWidget t m) ⇒ m (Dynamic t MenuItem)
fouNav = do
  E.div (A.setClasses [fouTitleBar, fouHideForMedium]
    $ A.dNmVal "responsive-toggle" "responsivemenu"
    $ A.dNmVal "hide-for" "medium" E.defDiv) $ do
      E.button (A.setClasses [fouMenuIcon]
        $ A.btButton
        $ A.dToggle "responsivemenu" E.defButton) blank
      E.div (A.setClasses [fouTitleBarTitle] E.defDiv) $ text "Menu"

  E.div navAttrs $ do
    dE2 ← E.div divClL $ do
      dE2 ← mkNavItems menuItemsL
      pure dE2
    pure dE2
  where
    navAttrs = A.setClasses [fouTopBar] $ A.id_ "responsivemenu"
      --  dNmVal "topbar" ""
      $ A.dNmVal "toggler" "" E.defDiv
    divClL = A.setClasses [fouTopBarLeft] E.defDiv
    divClR = A.setClasses [fouTopBarRight] E.defDiv
-}

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


--------------------------------------------------------------------------------

-- type LangMap = Map.Map MenuTag Text

miEn ∷ LangMap MenuTag
miEn = Map.fromList [ (MoPg, "Monday")
                    , (TuPg , "Tuesday")
                    , (WePg, "Wednesday")
                    , (ThPg, "Thursday")
                    , (FrPg, "Friday")
                    , (LoginPg, "Login"), (LogoutPg, "Log out")
                    , (MenuChoice, "Menu Choice"), (MenuChoiceLR, "Left-Right Tree")
                    , (MenuChoiceL, "Left Tree") , (MenuChoiceLst, "List")
                    , (LangChoice, "Language"), (LangGb, "British")
                    , (LangDe, "Germany"), (LangSe, "Swedish")
                    ]

miSe ∷ LangMap MenuTag
miSe = Map.fromList [ (MoPg, "Måndag")
                    , (TuPg , "Tisdag")
                    , (WePg, "Onsdag")
                    , (ThPg, "Torsdag")
                    , (FrPg, "Fredag")
                    , (LoginPg, "Logga in"), (LogoutPg, "Logga ut")
                    , (MenuChoice, "Meny Val")
                    , (MenuChoiceLR, "Vänster-Höger Träd")
                    , (MenuChoiceL, "Vänster Träd") , (MenuChoiceLst, "Lista")
                    , (LangChoice, "Språk"), (LangGb, "Engelska")
                    , (LangDe, "Tyska"), (LangSe, "Svenska")
                    ]

miDe ∷ LangMap MenuTag
miDe = Map.fromList [ (MoPg, "Montag")
                    , (TuPg , "Dienstag")
                    , (WePg, "Mittwoch")
                    , (ThPg, "Donnerstag")
                    , (FrPg, "Freitag")
                    , (LoginPg, "Anmelden"), (LogoutPg, "Abmelden")
                    , (MenuChoice, "Menü Auswahl")
                    , (MenuChoiceLR, "Links-Recht Baum")
                    , (MenuChoiceL, "Links Baum") , (MenuChoiceLst, "Liste")
                    , (LangChoice, "Sprache"), (LangGb, "Englisch")
                    , (LangDe, "Deutsch"), (LangSe, "Schwedisch")
                    ]

-- File, Datei, Arkiv
-- Edit, Bearbeiten, Redigera
-- Preferences, Einstellungen, Inställningar
-- Select, Auswahl, Markera
-- View, Ansicht, Visa
-- Language, Sprache, Språk
-- English, German, Swedish
-- Englisch, Deutsch, Schwedisch
-- Engelska, Tyska, Svenska
--
-- Reset, Cancel, Ok
-- Zurücksetzen, Abbrechen, Ok
-- Återställ, Avbryt, Ok


getMiLangMap ∷ UiLang → LangMap MenuTag
getMiLangMap LaGB = miEn
getMiLangMap LaDE = miDe
getMiLangMap LaSE = miSe
getMiLangMap _    = miEn -- default




