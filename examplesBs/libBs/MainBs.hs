{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainBs where

import           Control.Arrow                  ((***))
import           Control.Lens
import           Control.Monad                  (mapM, join, void, (<=<))
import           Control.Monad.Fix
import           Data.List                      ( isPrefixOf,inits,union
                                                , foldl', nub)
import           Data.Maybe
import qualified Data.Map                       as Map
import           Data.Map                       (Map)
-- import           Data.Monoid as Mon
-- import           Data.FileEmbed
import           Data.Semigroup
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Read                 as T
import           Data.Tree
import qualified Data.Vector                    as V
import           Language.Javascript.JSaddle    (JSM, liftJSM, MonadJSM)
import           Reflex.Dom                     hiding (mainWidget,
                                                 mainWidgetWithCss,
                                                 mainWidgetWithHead)
import           Reflex.Dom.Core                (mainWidget, mainWidgetWithCss,
                                                 mainWidgetWithHead)
------------------------------------------------------------------------------

import qualified Reflex.Dom.HTML5.Attrs         as A
import qualified Reflex.Dom.HTML5.Elements      as E
import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Tree
import           Reflex.Dom.HTML5.Component.Table

------------------------------------------------------------------------------

import           Reflex.Dom.Theme.Raw.BS4       as B
-- import Reflex.Dom.Icon.Raw.FA as FA
import           Reflex.Dom.Icon.Raw.FlagIcons  as FI
import           Reflex.Dom.Icon.Raw.OpenIconicBS as OI

import           Reflex.Dom.Component.MenuCommon
import           Reflex.Dom.Component.BS.Menu
import           Reflex.Dom.Component.BS.Dropdown
import           Reflex.Dom.Component.NaiveI18n




------------------------------------------------------------------------------
-- exampleBs -- bootstrap
------------------------------------------------------------------------------

mainW ∷ JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headEl bodyEl
-- It seems that we should implement at least some of the js-functions in order
-- to get corresponding functionality to webkit2gtk. Or do something else to get
-- it working.
-- mainW = mainWidgetWithCss
--   x  ( $(embedFile "assets/css/open-iconic/font/css/open-iconic-bootstrap.css") <>
--   x    $(embedFile "assets/css/flag-icon.css") <>
--   x    $(embedFile "assets/css/bootstrap.min.css") <>
--   x  ) bodyEl
    -- js and webkit2gtk probably won't go together - at least not this way
      -- x $(embedFile "assets/js/jquery-3.2.1.slim.min.js") <>
      -- x $(embedFile "assets/js/popper.min.js") <>
      -- x $(embedFile "assets/js/bootstrap.min.js")

-- "https://code.jquery.com/jquery-3.2.1.slim.min.js"
-- "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.3/umd/popper.min.js"
-- "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js"
--

-- main = run $ mainWidgetWithCss
--     ( $(embedFile "static/css/normalize.css")
--           <> $(embedFile "static/css/skeleton.css")
--           <> $(embedFile "static/css/font-awesome.min.css")
--         ) app


------------------------------------------------------------------------------
------------------------------------------------------------------------------

faURL ∷ Text
faURL = "https://use.fontawesome.com/releases/v5.3.1/css/all.css"
-- faURL = "https://cdnjs.cloudflare.com/ajax/libs/"
--   <> "font-awesome/4.7.0/css/font-awesome.min.css" ∷ Text

oiCSS ∷ Text
oiCSS = "https://cdnjs.cloudflare.com/ajax/libs/open-iconic/1.1.1/font/css/open-iconic-bootstrap.min.css" 
-- oiCSS = "./css/open-iconic/font/css/open-iconic-bootstrap.css" ∷ Text
--  <link href="/open-iconic/font/css/open-iconic-bootstrap.css" rel="stylesheet">


bsURL =
    "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
    ∷ Text
  -- "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"
-- bsInt = "sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB"
--     ∷ Text
    -- "sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb"


headEl ∷ MonadWidget t m ⇒ m ()
headEl = do
    E.titleN $ text "Main Title"
    E.meta (A.charSet "utf-8" E.defMeta) blank
    E.meta (A.name "viewport" $
         A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
         E.defMeta)
        blank
    E.link (A.ltStylesheet $ A.href (A.URL "./css/flag-icon.css")
            E.defLink) blank
    E.link (A.ltStylesheet $ A.href (A.URL bsURL) --  A.integrity bsInt
        $ A.corsAnon E.defLink)
        blank
    E.link (A.ltStylesheet $ A.href (A.URL oiCSS) E.defLink) blank
    E.link (A.ltStylesheet $
         A.href (A.URL "https://fonts.googleapis.com/css?family=Raleway")
         E.defLink)
        blank
  -- eLink ( ltStylesheet $ href (URL faURL) $ def) $ blank


--------------------------------------------------------------------------------

data MenuTag = MyPrg | FilePg | EditPg | PrefPg | SelectPg | TablePg
             | LoginPg | LogoutPg
             | MenuChoice | MenuChoiceLR | MenuChoiceL | MenuChoiceLst
             | LangChoice | LangGb | LangDe | LangSe
             | LMenu | RMenu
  deriving (Eq, Show, Ord)

instance ActSretval MenuTag where defRetval = FilePg
-- instance ActSretval MenuTag where defRetval = LangSe

data MenuGroups = MenuChGroup | MenuLaGroup deriving (Eq,Ord,Show)

data UiLang = LaGB | LaDE | LaSE | LaIso Text
  deriving (Eq, Show, Ord)

exTree ∷ Reflex t ⇒ Tree (MenuItemConfig t MenuTag MenuGroups)
exTree = Node (MenuItemConfig MyPrg [] never Nothing)
    [ Node (MenuItemConfig FilePg [oi, oiFile] never Nothing) []
    , Node (MenuItemConfig EditPg [oi, oiLightbulb] never Nothing) []
    , Node (MenuItemConfig SelectPg [oi, oiPin] never Nothing) []
    , Node (MenuItemConfig PrefPg [oi, oiCog] never Nothing) []
    , Node (MenuItemConfig TablePg [oi, oiSpreadsheet] never Nothing) []
    , Node (MenuItemConfig MenuChoice [oi, oiMenu] never
            Nothing) -- a drop down
        [ Node (MenuItemConfig MenuChoiceLR
                [oi, oiAlignCenter] never (Just MenuChGroup) ) []
        , Node (MenuItemConfig MenuChoiceL
                [oi, oiAlignLeft] never (Just MenuChGroup) ) []
        , Node (MenuItemConfig MenuChoiceLst
                [oi, oiList] never (Just MenuChGroup) ) [] ]
    , Node (MenuItemConfig LoginPg [oi, oiAccountLogin] never Nothing) []
    , Node (MenuItemConfig LogoutPg [oi, oiAccountLogout] never Nothing) []
    , Node (MenuItemConfig LangChoice [] never Nothing) -- a drop down
        [ Node (MenuItemConfig LangGb
                [flagIcon, flagIconGb] never (Just MenuLaGroup) ) []
        , Node (MenuItemConfig LangDe
                [flagIcon, flagIconDe] never (Just MenuLaGroup) ) []
        , Node (MenuItemConfig LangSe
                [flagIcon, flagIconSe] never (Just MenuLaGroup) ) []
        ]
    ]

exTree2 ∷ Reflex t ⇒ Tree (MenuItemConfig t MenuTag MenuGroups)
exTree2 = Node (MenuItemConfig MyPrg [] never Nothing)
    [ Node (MenuItemConfig LMenu [] never Nothing)
          [ Node (MenuItemConfig FilePg [oi, oiFile] never Nothing) []
          , Node (MenuItemConfig EditPg [oi, oiLightbulb] never Nothing) []
          , Node (MenuItemConfig SelectPg [oi, oiPin] never Nothing) []
          , Node (MenuItemConfig PrefPg [oi, oiCog] never Nothing) []
          , Node (MenuItemConfig TablePg [oi, oiSpreadsheet] never Nothing) []
          , Node (MenuItemConfig MenuChoice [oi, oiMenu] never
                  Nothing) -- a drop down
              [ Node (MenuItemConfig MenuChoiceLR
                      [oi, oiAlignCenter] never (Just MenuChGroup) ) []
              , Node (MenuItemConfig MenuChoiceL
                      [oi, oiAlignLeft] never (Just MenuChGroup) ) []
              , Node (MenuItemConfig MenuChoiceLst
                      [oi, oiList] never (Just MenuChGroup) ) [] ]
          ]
    , Node (MenuItemConfig RMenu [] never Nothing)
          [ Node (MenuItemConfig LoginPg [oi, oiAccountLogin] never Nothing) []
          , Node (MenuItemConfig LogoutPg [oi, oiAccountLogout] never Nothing) []
          , Node (MenuItemConfig LangChoice [] never Nothing) -- a drop down
              [ Node (MenuItemConfig LangGb
                      [flagIcon, flagIconGb] never (Just MenuLaGroup) ) []
              , Node (MenuItemConfig LangDe
                      [flagIcon, flagIconDe] never (Just MenuLaGroup) ) []
              , Node (MenuItemConfig LangSe
                      [flagIcon, flagIconSe] never (Just MenuLaGroup) ) []
              ]
          ]
    ]

exList ∷ Reflex t ⇒ [MenuItemConfig t MenuTag MenuGroups]
exList =
    [ MenuItemConfig FilePg [oi, oiFile] never Nothing
    , MenuItemConfig EditPg [oi, oiLightbulb] never Nothing
    , MenuItemConfig PrefPg [oi, oiCog] never Nothing
    , MenuItemConfig SelectPg [oi, oiPin] never Nothing
    , MenuItemConfig TablePg [oi, oiSpreadsheet] never Nothing
    , MenuItemConfig MenuChoiceLR [oi, oiAlignCenter] never (Just MenuChGroup)
    , MenuItemConfig MenuChoiceL [oi, oiAlignLeft] never (Just MenuChGroup)
    , MenuItemConfig MenuChoiceLst [oi, oiList] never (Just MenuChGroup)
    , MenuItemConfig LoginPg [oi, oiAccountLogin] never Nothing
    , MenuItemConfig LogoutPg [oi, oiAccountLogout] never Nothing
    , MenuItemConfig LangGb [flagIcon, flagIconGb] never (Just MenuLaGroup)
    , MenuItemConfig LangDe [flagIcon, flagIconDe] never (Just MenuLaGroup)
    , MenuItemConfig LangSe [flagIcon, flagIconSe] never (Just MenuLaGroup)
    ]


data DDtags = Select0DD | Select1DD | Select2DD | Select3DD
        | SelectDD
    deriving (Eq,Show,Ord)

data DDgroups = DDgroups
    deriving (Eq,Show,Ord)

instance ActSretval DDtags where defRetval = Select0DD

miDD =
    Node (MenuItemConfig SelectDD [] never Nothing)
        [ Node (MenuItemConfig Select0DD [] never (Just DDgroups)) []
        , Node (MenuItemConfig Select1DD [] never (Just DDgroups)) []
        , Node (MenuItemConfig Select2DD [] never (Just DDgroups)) []
        , Node (MenuItemConfig Select3DD [] never (Just DDgroups)) []
        ] ∷ Reflex t ⇒ Tree (MenuItemConfig t DDtags DDgroups)


miDDEn ∷ LangMap DDtags
miDDEn = Map.fromList [ (SelectDD, "Select")
                      , (Select0DD, "Select 0")
                      , (Select1DD, "Select 1")
                      , (Select2DD, "Select 2")
                      , (Select3DD, "Select 3")
                      ]

miDDSe ∷ LangMap DDtags
miDDSe = Map.fromList [ (SelectDD , "Markera")
                      , (Select0DD, "Markera 0")
                      , (Select1DD, "Markera 1")
                      , (Select2DD, "Markera 2")
                      , (Select3DD, "Markera 3")
                      ]

miDDDe ∷ LangMap DDtags
miDDDe = Map.fromList [ (SelectDD , "Auswahl")
                      , (Select0DD, "Auswahl 0")
                      , (Select1DD, "Auswahl 1")
                      , (Select2DD, "Auswahl 2")
                      , (Select3DD, "Auswahl 3")
                      ]


getDDMiLangMap ∷ UiLang → LangMap DDtags
getDDMiLangMap LaGB = miDDEn
getDDMiLangMap LaDE = miDDDe
getDDMiLangMap LaSE = miDDSe
getDDMiLangMap _    = miDDEn -- default

--------------------------------------------------------------------------------


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


bodyEl ∷ forall t m. (MonadWidget t m) -- , DomBuilderSpace m ~ GhcjsDomSpace)
       ⇒ m ()
bodyEl = do
    evPB ∷ Event t () ← getPostBuild
    let langMapGB = getMiLangMap LaGB
        exTrL  = exTree ∷ Tree (MenuItemConfig t MenuTag MenuGroups)
        exTrLR = exTree2 ∷ Tree (MenuItemConfig t MenuTag MenuGroups)
        exLst  = exList ∷ [MenuItemConfig t MenuTag MenuGroups]
        exHd   = MenuItemConfig MyPrg [] never Nothing
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
        dMt ← holdDyn FilePg eMt
    let
        eMTable  = traceEvent "body, eMTable" $ ffilter (== TablePg) eMt
        eMSelect = traceEvent "body, eMSelect" $ ffilter (== SelectPg) eMt
        eMFEP    = traceEvent "body, eMSelect" $
            ffilter (`elem` [FilePg, EditPg, PrefPg]) eMt
    dLa ← holdDyn LaGB eLa
    E.h1N $ text "Welcome to example prg"
    E.pN $ do
        text "hmm : "
        dynText $ (T.pack . show) <$> dLa
        text " : "
        -- Draw one of the widgets (almost empty page, select page, table page).
    rec
        void $ widgetHold (blank >> pure never) $ leftmost
                [ showAlmostEmpty dMt <$ eMFEP
                , showAlmostEmpty dMt <$ evPB
                , (blank >> pure never) <$ eMTable
                , (blank >> pure never) <$ eMSelect
                ]
        --
        devIntR ∷ Dynamic t (Event t Int)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showTableContentR dMt dR <$ eMTable
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMSelect
                ]
        let evIntR = leftmost [(-2) <$ evPB, switch . current $ devIntR ]
        dIntR ∷ Dynamic t Int ← holdDyn (-3) evIntR
        dR ∷ Dynamic t Int ← holdUniqDyn dIntR
        --
        devIntC ∷ Dynamic t (Event t Int)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showTableContentC dMt dC <$ eMTable
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMSelect
                ]
        let evIntC = leftmost [(-2) <$ evPB, switch . current $ devIntC ]
        dIntC ∷ Dynamic t Int ← holdDyn (-3) evIntC
        dC ∷ Dynamic t Int ← holdUniqDyn dIntC
        -- dC ∷ Dynamic t Int ← holdUniqDyn dIntC
        -- let dIntP = zipDynWith (,) dR dC
        let ddItems = miDD ∷ Tree (MenuItemConfig t DDtags DDgroups)
        --
        devDd ∷ Dynamic t (Event t DDtags)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showSelectContent ddItems dMt dLa dDd updLang <$ eMSelect
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMTable
                ]
        let evDd = leftmost [Select0DD <$ evPB, switch . current $ devDd ]
        dDd2 ∷ Dynamic t DDtags ← holdDyn Select0DD evDd
        dDd ∷ Dynamic t DDtags ← holdUniqDyn dDd2
    -- Show that the state is maintained.
    E.pN $ do
        text "hmm : "
        dynText $ (T.pack . show) <$> dLa
        text " : "
        -- dynText $ (T.pack . show) <$> dDd
        -- text " : "
        dynText $ (T.pack . show) <$> dR
        text " : "
        dynText $ (T.pack . show) <$> dC
        text " : "
        dynText $ (T.pack . show) <$> dDd
    blank




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

--------------------------------------------------------------------------------

-- | Here we listen for the language change events, too.
showSelectContent ∷ forall t m. MonadWidget t m
                  ⇒ Tree (MenuItemConfig t DDtags DDgroups)
                  → Dynamic t MenuTag
                  → Dynamic t UiLang
                  → Dynamic t DDtags
                  → (Dynamic t (LangMap DDtags)
                    → MenuItem t DDtags
                    → MenuItem t DDtags)
                  → m (Event t DDtags)
showSelectContent trDD dMt dLa dDdtags updLang = do
    evPB ← getPostBuild
    let -- miDDhead = defRec2Cfg $ head miDD ∷ MenuItemConfig t DDtags
        -- miDDitems = fmap defRec2Cfg (tail miDD) ∷ [MenuItemConfig t DDtags]
        -- miDDStr = LstDropdownStruct miDDhead miDDitems
        eUiLang = fmap (\mt2 →
                     case mt2 of
                         LangDe → LaDE
                         LangSe → LaSE
                         LangGb → LaGB) $
            ffilter (\mt3 → mt3 == LangSe || mt3 == LangGb || mt3 == LangDe) $
            updated dMt
        eDDLang = getDDMiLangMap <$> eUiLang
        eDDdef = tag (current (getDDMiLangMap <$> dLa)) evPB
        eUiLangdef = tag (current dLa) evPB
    dLangDD ← holdDyn (getDDMiLangMap LaGB) $ leftmost [eDDLang, eDDdef]
    -- dUiLang ← holdDyn LaGB $ leftmost [eUiLang, eUiLangdef]
    --
    E.h2N $ text "A selection example (drowdowns)"
    E.pN $ do
        text "The selected menu item was: "
        dynText $ fmap (T.pack . show) dMt
    let dDd2 = fmap (:[]) dDdtags
    eDDtCSEv ← mkDropdown trDD dDd2 dLangDD updLang
    let eOldDD = tag (current dDdtags) evPB
    eDDt2 ← compStateEv2TagEv eDDtCSEv (Right trDD)
    let eDDt = ffilter (/= SelectDD) eDDt2 -- not interested on dd-toggler clicks
    dDDt ← holdDyn Select0DD $ leftmost [eDDt,eOldDD]
    let dDDtxt = fmap (T.pack . show) dDDt
    E.pN $ do
        text "And the selected dropdown item was: "
        dynText dDDtxt
    pure $ updated dDDt

--------------------------------------------------------------------------------

showTableContentR ∷ forall t m. MonadWidget t m
                 ⇒ Dynamic t MenuTag
                 → Dynamic t Int
                 → m (Event t Int)
showTableContentR dMt dRow = do
    evPB ← getPostBuild
    E.h2N $ text "Decorated table example"
    E.pN $ do
        text "Note that the caption comes after the table. "
        text "And dRow = "
        dynText $ (T.pack .show) <$> dRow
        text "."
    evmTxtR ← tblWidget dRow
    dmTxtR ← holdDyn Nothing $ leftmost
        [ evmTxtR
        , (Just . ("#" <>) . T.pack . show) <$> tag (current dRow) evPB
        ]
    E.pN $ dynText $ maybePersonIdTxt <$> dmTxtR
    let evR = updated $ mayInt <$> dmTxtR
    pure evR
    where
      maybePersonIdTxt (Just txt) = "Person " <> txt <> " is selected."
      maybePersonIdTxt Nothing    = "No person is selected."

mayInt ∷ Maybe Text → Int
mayInt (Just txt) =
  let mct = T.uncons txt
   in case mct of
       Just (c,txtrest) →
           case T.decimal txtrest of
               Left _ → -1
               Right (r,_) → fromIntegral r
       Nothing → -1
mayInt Nothing = -1

mayInt2Text ∷ Maybe Int → Maybe Text
mayInt2Text Nothing = Nothing
mayInt2Text (Just i) = Just $ "#" <> (T.pack . show) i

showTableContentC ∷ forall t m. MonadWidget t m
                 ⇒ Dynamic t MenuTag
                 → Dynamic t Int
                 → m (Event t Int)
showTableContentC dMt dCol = do
    evPB ← getPostBuild
    E.pN $ do
        text "And dCol = "
        dynText $ (T.pack .show) <$> dCol
        text "."
    evmIntC ← tblWidget2 dCol
    dmTxtC ← holdDyn Nothing $ leftmost
        [ mayInt2Text <$> evmIntC
        , mayInt2Text . Just  <$> tag (current dCol) evPB
        ]
    E.pN $ dynText $ maybeColumnIdTxt <$> dmTxtC
    let evC = updated $ mayInt <$> dmTxtC
    pure evC
    where
      maybeColumnIdTxt (Just txt) = "Column " <> txt <> " is selected."
      maybeColumnIdTxt Nothing    = "No column is selected."

--------------------------------------------------------------------------------

-- | Set the information for the table.
tblContent ∷ V.Vector (V.Vector Text)
tblContent = V.fromList [r1,r2,r3,r4]
  where
    r1 = V.fromList ["#1", "Firstname1 ", "Lastname 1", "Addr 1", "Phone 1"]
    r2 = V.fromList ["#2", "Firstname2 ", "Lastname 2", "Addr 2", "Phone 2"]
    r3 = V.fromList ["#3", "Firstname3 ", "Lastname 3", "Addr 3", "Phone 3"]
    r4 = V.fromList ["#4", "Firstname4 ", "Lastname 4", "Addr 4", "Phone 4"]

-- | Make a table using mkTable.
tblWidget ∷ forall t m. (MonadWidget t m)
          ⇒ Dynamic t Int → m (Event t (Maybe Text))
tblWidget dRow = do
    let
        tblHeaders ∷ MonadWidget t m ⇒ Maybe (HeaderConf t m ())
        tblHeaders = Just $ HeaderConf
            (defThFuns
                & set thfThAttr (const $ constDyn $ A.style "width: 150px" E.defTh)
                & set (thfADEs . adeDraw) drawDivContent)
            (V.fromList $ const (constDyn E.defCol) <$> [1..5])
            -- vec of empty E.Col's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            (constDyn E.defThead)
        capDfs = Just (CaptionConf "Table 1. A Person list table example."
                      (constDyn E.defCaption))
        dAstInit = (\i → Just
                    [ defActiveState
                        & set activeStateElemId (ActErow (i-1))
                        & set activeStateActive (constDyn True)
                    ]
                   ) <$> dRow
        tConf = defTableConf
            & set tableCaption capDfs
            & set tableHeader tblHeaders
            & set tableTableAttr (constDyn $ A.setClasses [bsTableStriped]
                                $ A.style "border-collapse: collapse" E.defTable)
            & set (tableTdFuns . tdfTrAttr) trAttrfun
            & set (tableTdFuns . tdfTdAttr)
                  (const $ A.style "padding: 5px" (constDyn E.defTd))
            & set cellDrawBody drawDivContent
            & set cellListenerBody (listenMyRow 5)
            & set tableActivityConf dAstInit
    rec
        tblSt ← E.div (A.setClasses [bsTable, bsTableSm, bsTableResponsiveSm
                      , bsTableHover] E.defDiv) $ mkTable tConf tblContent
        let dCell = _csDynURelease tblSt
            dActElem = _activeStateElemId <$> dCell
    let dmId = (giveId tblContent . rowNum) <$> dActElem
    pure $ updated dmId
  where
    giveId ∷ V.Vector (V.Vector Text) → Maybe Int → Maybe Text
    giveId _ Nothing = Nothing
    giveId v (Just i) = Just $ (v V.! i) V.! 0
    trAttrfun ∷ Dynamic t (ActiveState t ActElem ()) → ActElem → Dynamic t E.Tr
    trAttrfun dAst _ae = mkETr <$> join (_activeStateActive <$> dAst)
      where
        mkETr ∷ Bool → E.Tr
        mkETr b =
               if b
                   then A.setClasses [bsTableActive] E.defTr
                   else A.setClasses [] E.defTr
                  -- then A.style "background-color: grey" E.defTr
                  -- else A.style "background-color: lightgrey" E.defTr


-- | Make a table using mkTable. This time, one of the columns is
-- selected.
tblWidget2 ∷ forall t m. (MonadWidget t m)
           ⇒ Dynamic t Int → m (Event t (Maybe Int))
tblWidget2 dCol = do
    let
        tblHeaders ∷ Maybe (HeaderConf t m ())
        tblHeaders = Just $ HeaderConf
            (defThFuns & set (thfADEs . adeDraw) drawDivContent
                 & set thfThAttr myThAttrF
            )
            (V.fromList $ const (constDyn E.defCol)
                <$> [1..5]) -- vec of empty E.Col's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            (constDyn E.defThead)
        capDfs = Just (CaptionConf "Table 2. This time select columns."
                      (constDyn E.defCaption))
        dAstInit ∷ Dynamic t (Maybe [ActiveState t ActElem ()]) = (\i → Just
                    [ defActiveState
                        & set activeStateElemId (ActEcolh i)
                        & set activeStateActive (constDyn True)
                    ]
                   ) <$> dCol
        tConf ∷ TableConf t m Text () = defTableConf
            & set tableCaption capDfs
            & set tableHeader tblHeaders
            & set tableTableAttr (constDyn $ A.setClasses [bsTableStriped]
                                $ A.style "border-collapse: collapse" E.defTable)
            & set (tableTdFuns . tdfTdAttr) myTdAttrF
            & set cellDrawBody drawDivContent
            & set cellListenerBody (listenMyCol 4)
            & set tableActivityConf dAstInit

    rec
        tblSt ← E.div (A.setClasses [bsTable, bsTableSm
                                  , bsTableResponsiveSm] E.defDiv) $
                    mkTable tConf tblContent
        let dCell = _csDynURelease tblSt
            dActElem = _activeStateElemId <$> dCell
    pure $ updated $ colNum <$> dActElem
  where
    dthDef ∷ Dynamic t A.Globals
    dthDef = constDyn $ A.style "width: 150px" A.defGlobals
    myThAttrF ∷ ActiveState t ActElem () → Dynamic t E.Th
    myThAttrF ast = defThAttrF
       ( ast & set activeStateActiveGl
                (A.setClasses [bsTableActive] dthDef)
             & set activeStateNotActiveGl
                (A.setClasses [] dthDef))
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
       ( ast & set activeStateActiveGl (A.setClasses [bsTableActive]
                                       (constDyn A.defGlobals))
             & set activeStateNotActiveGl (A.setClasses []
                                          (constDyn A.defGlobals)))


--------------------------------------------------------------------------------

-- type LangMap = Map.Map MenuTag Text

miEn ∷ LangMap MenuTag
miEn = Map.fromList [(FilePg, "File"), (EditPg, "Edit")
                    , (PrefPg , "Preferences"), (SelectPg, "Select")
                    , (TablePg, "Tables")
                    , (LoginPg, "Login"), (LogoutPg, "Log out")
                    , (MenuChoice, "Menu Choice"), (MenuChoiceLR, "Left-Right Tree")
                    , (MenuChoiceL, "Left Tree") , (MenuChoiceLst, "List")
                    , (LangChoice, "Language"), (LangGb, "British")
                    , (LangDe, "Germany"), (LangSe, "Swedish")
                    ]

miSe ∷ LangMap MenuTag
miSe = Map.fromList [(FilePg, "Arkiv"), (EditPg, "Redigera")
                    , (PrefPg , "Inställningar"), (SelectPg, "Markera")
                    , (TablePg, "Tavlar")
                    , (LoginPg, "Logga in"), (LogoutPg, "Logga ut")
                    , (MenuChoice, "Meny Val")
                    , (MenuChoiceLR, "Vänster-Höger Träd")
                    , (MenuChoiceL, "Vänster Träd") , (MenuChoiceLst, "Lista")
                    , (LangChoice, "Språk"), (LangGb, "Engelska")
                    , (LangDe, "Tyska"), (LangSe, "Svenska")
                    ]

miDe ∷ LangMap MenuTag
miDe = Map.fromList [(FilePg, "Datei"), (EditPg, "Bearbeiten")
                    , (PrefPg , "Einstellungen"), (SelectPg, "Auswahl")
                    , (LoginPg, "Anmelden"), (LogoutPg, "Abmelden")
                    , (TablePg, "Tafel")
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



--------------------------------------------------------------------------------

