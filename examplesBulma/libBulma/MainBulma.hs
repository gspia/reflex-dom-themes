{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainBulma where

import           Control.Lens
import           Control.Monad                  (join, mapM, void)
import           Control.Monad.Fix
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Read                 as T
import           Data.Tree
import qualified Data.Vector                    as V
import           Language.Javascript.JSaddle    (JSM, liftJSM, MonadJSM)

import           Reflex.Dom                     hiding (mainWidget,
                                                        mainWidgetWithHead)
import           Reflex.Dom.Core                (mainWidget, mainWidgetWithHead)

import qualified Reflex.Dom.HTML5.Attrs         as A
import qualified Reflex.Dom.HTML5.Elements      as E

import           Reflex.Dom.Icon.Raw.FA         as FA
import           Reflex.Dom.Icon.Raw.FlagIcons  as FI
import qualified Reflex.Dom.Theme.Raw.Bulma     as B

import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Tree
import           Reflex.Dom.HTML5.Component.Table

import           Reflex.Dom.Component.MenuCommon
import           Reflex.Dom.Component.Bulma.Menu
import           Reflex.Dom.Component.Bulma.Dropdown
import           Reflex.Dom.Component.NaiveI18n


------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW ∷ JSM ()
mainW = mainWidgetWithHead headEl bodyEl

-- mainW = mainWidget bodyEl

------------------------------------------------------------------------------
------------------------------------------------------------------------------

bulmaCss ∷ Text
bulmaCss = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"

faCss ∷ Text
faCss = "https://use.fontawesome.com/releases/v5.3.1/css/all.css"
-- faCss = "https://use.fontawesome.com/releases/v5.1.0/js/all.js"

headEl ∷ MonadWidget t m ⇒ m ()
headEl = do
  E.title E.defTitle $ text "Main Title"
  E.meta (A.charSet "utf-8" E.defMeta) blank
  E.meta (A.name "viewport"
         $ A.content "width=device-width, initial-scale=1" E.defMeta)
    blank
  E.link ( A.ltStylesheet $ A.href (A.URL bulmaCss) E.defLink) blank
  E.link ( A.ltStylesheet $ A.href (A.URL faCss) E.defLink) blank

------------------------------------------------------------------------------
------------------------------------------------------------------------------


bodyEl ∷ (MonadWidget t m) ⇒ m ()
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
    E.section (A.style "margin-top:50px" E.defSection) $ do
        E.h1N $ text "Welcome to example prg"
        E.pN $ do
            text "hmm : "
            dynText $ (T.pack . show) <$> dLa
            text " : "
            -- dynText $ (T.pack . show) <$> dR
            -- text " : "
            -- dynText $ (T.pack . show) <$> dC
            -- text " : "
            -- dynText $ (T.pack . show) <$> dDd
        blank
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
        --
        let ddItems = miDD ∷ Tree (MenuItemConfig t DDtags DDgroups)
        devDd ∷ Dynamic t (Event t DDtags)
            ← widgetHold (blank >> pure never) $ leftmost
                [ showSelectContent ddItems dMt dLa dDd updLang <$ eMSelect
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMTable
                ]
        let evDd = leftmost [Select0DD <$ evPB, switch . current $ devDd ]
        dDd2 ∷ Dynamic t DDtags ← holdDyn Select0DD evDd
        dDd ∷ Dynamic t DDtags ← holdUniqDyn dDd2
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
        -- Draw one of the widgets (almost empty page, select page, table page).
  -- miD ∷ Dynamic t MenuItem ← w3Nav
  -- E.div (A.style "margin-top:43px" E.defDiv) $ do
  --   E.h1 E.defH1 $ text "Welcome to reflex-dom-themes (Bulma)"
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
    -- eDDtCSEv ← mkDropdown exTree (pure []) dLang updLang
    -- mkLMenuBarTag  exTrL  dLang updLang  <$ eMenuL
    let eOldDD = tag (current dDdtags) evPB
    eDDt2 ← compStateEv2TagEv eDDtCSEv (Right trDD)
    let eDDt = ffilter (/= SelectDD) eDDt2 -- not interested on dd-toggler clicks
    dDDt ← holdDyn Select0DD $ leftmost [eDDt,eOldDD]
    let dDDtxt = fmap (T.pack . show) dDDt
    E.pN $ do
        text "And the selected dropdown item was: "
        dynText dDDtxt
    -- exampleT9
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

------------------------------------------------------------------------------

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
            & set tableTableAttr (constDyn $ A.setClasses [B.table, B.isStriped]
                                $ A.style "border-collapse: collapse" E.defTable)
            & set (tableTdFuns . tdfTrAttr) trAttrfun
            & set (tableTdFuns . tdfTdAttr)
                  (const $ A.style "padding: 5px" (constDyn E.defTd))
            & set cellDrawBody drawDivContent
            & set cellListenerBody (listenMyRow 5)
            & set tableActivityConf dAstInit
    rec
        -- tblSt ← E.div (A.setClasses [bsTable, bsTableSm, bsTableResponsiveSm
        --               , bsTableHover] E.defDiv) $ mkTable tConf tblContent
        tblSt ← E.div (A.setClasses [] E.defDiv) $ mkTable tConf tblContent
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
                   then A.setClasses [B.isSelected] E.defTr
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
            & set tableTableAttr (constDyn $ A.setClasses [B.table, B.isStriped]
                                $ A.style "border-collapse: collapse" E.defTable)
            & set (tableTdFuns . tdfTdAttr) myTdAttrF
            & set cellDrawBody drawDivContent
            & set cellListenerBody (listenMyCol 4)
            & set tableActivityConf dAstInit

    rec
        -- tblSt ← E.div (A.setClasses [bsTable, bsTableSm
        --                           , bsTableResponsiveSm] E.defDiv) $
        tblSt ← E.div (A.setClasses [] E.defDiv) $
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
                (A.setClasses [B.isSelected] dthDef)
             & set activeStateNotActiveGl
                (A.setClasses [] dthDef))
    myTdAttrF ∷ ActiveState t ActElem () → Dynamic t E.Td
    myTdAttrF ast = defTdAttrF
       ( ast & set activeStateActiveGl (A.setClasses [B.isSelected]
                                       (constDyn A.defGlobals))
             & set activeStateNotActiveGl (A.setClasses []
                                          (constDyn A.defGlobals)))

------------------------------------------------------------------------------
------------------------------------------------------------------------------


data MenuTag = MyPrg | FilePg | EditPg | SelectPg | TablePg
                     | LoginPg | LogoutPg
             | MenuChoice | MenuChoiceLR | MenuChoiceL | MenuChoiceLst
             | LangChoice | LangGb | LangDe | LangSe
             | PrefPg | PrefChoiceColorPg | PrefChoiceTxtStylePg | PrefChoiceRoundedPg
             | LMenu | RMenu
  deriving (Eq, Show, Ord)

instance ActSretval MenuTag where defRetval = FilePg
-- instance ActSretval MenuTag where defRetval = LangSe

data MenuGroups = MenuChGroup | MenuLaGroup | PrefGroup deriving (Eq,Ord,Show)

data UiLang = LaGB | LaDE | LaSE | LaIso Text
  deriving (Eq, Show, Ord)

exTree ∷ Reflex t ⇒ Tree (MenuItemConfig t MenuTag MenuGroups)
exTree = Node (MenuItemConfig MyPrg [] never Nothing)
    [ Node (MenuItemConfig FilePg [FA.fa, faFile] never Nothing) []
    , Node (MenuItemConfig EditPg [FA.fa, faLightbulb] never Nothing) []
    , Node (MenuItemConfig SelectPg [FA.fa, faFolder] never Nothing) []
    , Node (MenuItemConfig PrefPg [FA.fa, faCog] never Nothing)
        [ Node (MenuItemConfig PrefChoiceColorPg [FA.fa, faFutbol] never (Just PrefGroup)) []
        , Node (MenuItemConfig PrefChoiceTxtStylePg [FA.fa, faFeather] never (Just PrefGroup)) []
        , Node (MenuItemConfig PrefChoiceRoundedPg [FA.fa, faSplotch] never (Just PrefGroup)) []
        ]
    , Node (MenuItemConfig TablePg [FA.fa, faTable] never Nothing) []
    , Node (MenuItemConfig LangChoice [] never Nothing) -- a drop down
        [ Node (MenuItemConfig LangGb
                [flagIcon, flagIconGb] never (Just MenuLaGroup) ) []
        , Node (MenuItemConfig LangDe
                [flagIcon, flagIconDe] never (Just MenuLaGroup) ) []
        , Node (MenuItemConfig LangSe
                [flagIcon, flagIconSe] never (Just MenuLaGroup) ) []
        ]
    , Node (MenuItemConfig MenuChoice [FA.fa, faScrewdriver] never
            Nothing) -- a drop down
        [ Node (MenuItemConfig MenuChoiceLR
                [FA.fa, faAlignCenter] never (Just MenuChGroup) ) []
        , Node (MenuItemConfig MenuChoiceL
                [FA.fa, faAlignLeft] never (Just MenuChGroup) ) []
        , Node (MenuItemConfig MenuChoiceLst
                [FA.fa, faList] never (Just MenuChGroup) ) [] ]
    , Node (MenuItemConfig LoginPg [FA.fa, faSignInAlt] never Nothing) []
    , Node (MenuItemConfig LogoutPg [FA.fa, faSignOutAlt] never Nothing) []
    ]

exTree2 ∷ Reflex t ⇒ Tree (MenuItemConfig t MenuTag MenuGroups)
exTree2 = Node (MenuItemConfig MyPrg [] never Nothing)
    [ Node (MenuItemConfig LMenu [] never Nothing)
          [ Node (MenuItemConfig FilePg [FA.fa, faFile] never Nothing) []
          , Node (MenuItemConfig EditPg [FA.fa, faLightbulb] never Nothing) []
          , Node (MenuItemConfig SelectPg [FA.fa, faFolder] never Nothing) []
          , Node (MenuItemConfig PrefPg [FA.fa, faCog] never Nothing)
              [ Node (MenuItemConfig PrefChoiceColorPg
                     [FA.fa, faFutbol] never (Just PrefGroup)) []
              , Node (MenuItemConfig PrefChoiceTxtStylePg
                     [FA.fa, faFeather] never (Just PrefGroup)) []
              , Node (MenuItemConfig PrefChoiceRoundedPg
                     [FA.fa, faSplotch] never (Just PrefGroup)) []
              ]
          , Node (MenuItemConfig LangChoice [] never Nothing) -- a drop down
              [ Node (MenuItemConfig LangGb
                      [flagIcon, flagIconGb] never (Just MenuLaGroup) ) []
              , Node (MenuItemConfig LangDe
                      [flagIcon, flagIconDe] never (Just MenuLaGroup) ) []
              , Node (MenuItemConfig LangSe
                      [flagIcon, flagIconSe] never (Just MenuLaGroup) ) []
              ]
          , Node (MenuItemConfig TablePg [FA.fa, faTable] never Nothing) []
          , Node (MenuItemConfig MenuChoice [FA.fa, faScrewdriver] never
                  Nothing) -- a drop down
              [ Node (MenuItemConfig MenuChoiceLR
                      [FA.fa, faAlignCenter] never (Just MenuChGroup) ) []
              , Node (MenuItemConfig MenuChoiceL
                      [FA.fa, faAlignLeft] never (Just MenuChGroup) ) []
              , Node (MenuItemConfig MenuChoiceLst
                      [FA.fa, faList] never (Just MenuChGroup) ) [] ]
          ]
    , Node (MenuItemConfig RMenu [] never Nothing)
          [ Node (MenuItemConfig LoginPg [FA.fa, faSignInAlt] never Nothing) []
          , Node (MenuItemConfig LogoutPg [FA.fa, faSignOutAlt] never Nothing) []
          ]
    ]

exList ∷ Reflex t ⇒ [MenuItemConfig t MenuTag MenuGroups]
exList =
    [ MenuItemConfig FilePg [FA.fa, faFile] never Nothing
    , MenuItemConfig EditPg [FA.fa, faLightbulb] never Nothing
    , MenuItemConfig PrefPg [FA.fa, faCog] never Nothing
    , MenuItemConfig SelectPg [FA.fa, faFolder] never Nothing
    , MenuItemConfig TablePg [FA.fa, faTable] never Nothing
    , MenuItemConfig PrefChoiceColorPg [FA.fa, faFutbol] never (Just PrefGroup)
    , MenuItemConfig PrefChoiceTxtStylePg [FA.fa, faFeather] never (Just PrefGroup) 
    , MenuItemConfig PrefChoiceRoundedPg [FA.fa, faSplotch] never (Just PrefGroup) 
    , MenuItemConfig MenuChoiceLR [FA.fa, faAlignCenter] never (Just MenuChGroup)
    , MenuItemConfig MenuChoiceL [FA.fa, faAlignLeft] never (Just MenuChGroup)
    , MenuItemConfig MenuChoiceLst [FA.fa, faList] never (Just MenuChGroup)
    , MenuItemConfig LoginPg [FA.fa, faSignInAlt] never Nothing
    , MenuItemConfig LogoutPg [FA.fa, faSignOutAlt] never Nothing
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
        [ Node (MenuItemConfig Select0DD [] never Nothing) []
        , Node (MenuItemConfig Select1DD [] never Nothing) []
        , Node (MenuItemConfig Select2DD [] never Nothing) []
        , Node (MenuItemConfig Select3DD [] never Nothing) []
        ] ∷ Reflex t ⇒ Tree (MenuItemConfig t DDtags DDgroups)
        -- [ Node (MenuItemConfig Select0DD [] never (Just DDgroups)) []
        -- , Node (MenuItemConfig Select1DD [] never (Just DDgroups)) []
        -- , Node (MenuItemConfig Select2DD [] never (Just DDgroups)) []
        -- , Node (MenuItemConfig Select3DD [] never (Just DDgroups)) []


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

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- type LangMap = Map.Map MenuTag Text

miEn ∷ LangMap MenuTag
miEn = Map.fromList [(FilePg, "File"), (EditPg, "Edit")
                    , (SelectPg, "Select")
                    , (TablePg, "Tables")
                    , (LoginPg, "Login"), (LogoutPg, "Log out")
                    , (MenuChoice, "Menu Choice"), (MenuChoiceLR, "Left-Right Tree")
                    , (MenuChoiceL, "Left Tree") , (MenuChoiceLst, "List")
                    , (LangChoice, "Language"), (LangGb, "British")
                    , (LangDe, "Germany"), (LangSe, "Swedish")
                    , (PrefPg, "Preferences"), (PrefChoiceColorPg, "Color")
                    , (PrefChoiceTxtStylePg, "Text style")
                    , (PrefChoiceRoundedPg, "Rounded boxes")
                    ]

miSe ∷ LangMap MenuTag
miSe = Map.fromList [(FilePg, "Arkiv"), (EditPg, "Redigera")
                    , (SelectPg, "Markera")
                    , (TablePg, "Tavlar")
                    , (LoginPg, "Logga in"), (LogoutPg, "Logga ut")
                    , (MenuChoice, "Meny Val")
                    , (MenuChoiceLR, "Vänster-Höger Träd")
                    , (MenuChoiceL, "Vänster Träd") , (MenuChoiceLst, "Lista")
                    , (LangChoice, "Språk"), (LangGb, "Engelska")
                    , (LangDe, "Tyska"), (LangSe, "Svenska")
                    , (PrefPg, "Inställningar"), (PrefChoiceColorPg, "Färg")
                    , (PrefChoiceTxtStylePg, "Textstil")
                    , (PrefChoiceRoundedPg, "Avrundade Lådor")
                    ]

miDe ∷ LangMap MenuTag
miDe = Map.fromList [(FilePg, "Datei"), (EditPg, "Bearbeiten")
                    , (SelectPg, "Auswahl")
                    , (LoginPg, "Anmelden"), (LogoutPg, "Abmelden")
                    , (TablePg, "Tafel")
                    , (MenuChoice, "Menü Auswahl")
                    , (MenuChoiceLR, "Links-Recht Baum")
                    , (MenuChoiceL, "Links Baum") , (MenuChoiceLst, "Liste")
                    , (LangChoice, "Sprache"), (LangGb, "Englisch")
                    , (LangDe, "Deutsch"), (LangSe, "Schwedisch")
                    , (PrefPg, "Einstellungen"), (PrefChoiceColorPg, "Farbe")
                    , (PrefChoiceTxtStylePg, "Textstil")
                    , (PrefChoiceRoundedPg, "Abgerundete Kästen")
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


exTreeT9 ∷ Tree Text
exTreeT9 = Node "Root"
    [ Node "Level 1, n00"
        [ Node "Level 2, n000" []
        , Node "Level 2, n001" []
        , Node "Level 2, n002" []
        ]
    , Node "Level 1, n01" []
    , Node "Level 1, n02"
        [ Node "Level 2, n020"
            [ Node "Level 3, n0200" []
            ]
        , Node "Level 2, n021" []
        ]
    , Node "Level 1, n03"
        [ Node "Level 2, n030"
            [ Node "Level 3, n0300" []
            , Node "Level 3, n0301" []
            , Node "Level 3, n0302" []
            ]
        , Node "Level 2, n031"
            [ Node "Level 3, n0310" []
            , Node "Level 3, n0311" []
            , Node "Level 3, n0312" []
            ]
        , Node "Level 2, n032"
            [ Node "Level 3, n0320" []
            , Node "Level 3, n0321" []
            , Node "Level 3, n0322" []
            ]
        ]
    ]


exampleT9 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                        , TriggerEvent t m, MonadJSM m, MonadFix m
                        , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
                        ) ⇒ m ()
exampleT9 = do
    let
        fs = CommonADEfuns
            -- listenMe
            -- listenMyAncestors
            (listenMySubTrees exTreeT9)
            actMU -- on mouse up (only one active area)
            drawDivContentEx -- we draw only the content into the div's
            elemEvF
        lvlNFs = defLevelNUlLiFuns
                & set levelNfADEs fs
                -- & set levelNfNodeAttr myLiAttr
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree)
        lvl1Fs = defLevelNUlLiFuns
                & set levelNfADEs fs
                -- & set levelNfNodeAttr myLiAttr
                & set levelNfnext (Just lvlNFs)
                -- & set levelNfMkLevelNF ownMkLevelN
                -- & set (levelNfADEs . adeListen) (listenMySubTrees exTree)
        rootConf = defRootNavDivFuns
            -- & set rootfADEs fs
            & set rootfADEs nothingADE
            -- & set rootfNodeAttr myAAttr
            & set rootfnext lvl1Fs
            & set rootfMkLevelNF own9MkLevelN
        treeConf = defTreeConf & set treeCRootC rootConf
    E.h2N $ text "exampleT9"
    E.pN $ text $ "Defining and using own version of mkLevelN for the 1st level "
        <> "of nodes. This example doesn't show the root node "
        <> "(root uses nothingADE). "
    res ∷ CompState t ActNode () ← mkTree treeConf exTreeT9
    -- This example uses the Component's way of delivering events by inserting
    -- "extra nodes" to be delivered from the Tree-component.
    -- We could use other ways here, as well.
    -- E.g. (not tried / thought out very well)
    --   - IOrefs
    --   - modify mkTree a bit to
    --      - use EventWriter
    --      - return information about other events
    --      - etc.
    -- mkTree is a short and simple method (less than 10 lines) so it should be
    -- straightforward to modify it to other use cases.
    let eNd ∷ Event t ActNode = -- traceEvent "eNd" $
            (_activeStateElemId . _ceMe) <$> _csCompEvent res
        eA = ffilter (==ActNpath [998]) eNd
        eB = ffilter (==ActNpath [999]) eNd
        eO = ffilter (\n → n /= ActNpath [998] && n /= ActNpath [999]) eNd
    _ ← widgetHold blank $ leftmost
        [ (E.p (A.style "color:red" E.defP) $ text "Link pressed") <$ eA
        , (E.p (A.style "color:red" E.defP) $ text "Button pressed") <$ eB
        , blank <$ eO -- note, this will be applied also on enter/leave events
        ]
    showRes exTreeT9 res
  where
    myLiAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    myLiAttr ast = defLiLeafNodeAttrF
      (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
      & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))
    -- myAAttr ∷ ActiveState t ActNode () → Dynamic t A.Attr
    -- myAAttr ast = defANodeAttrF
    --   (ast & set activeStateActiveGl (A.style "color: red" $ pure A.defGlobals)
    --   & set activeStateNotActiveGl (A.style "color: black" $ pure A.defGlobals))



-- | See 'exampleT9'.
own9MkLevelN ∷ forall t m a r. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, Eq a, Show a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace
     , ActSretval r)
       ⇒ LevelNFuns t m a r
       -- ^ Funs and decorations
       →  Dynamic t (Maybe [ActiveState t ActNode r])
       -- ^ User given activity information obtained from tree configurations.
       → Event t (CompEvent t ActNode r)
       -- ^ Event coming from tree (can be from nodes).
       → CompState t ActNode r
       -- ^ A function combining XX's can use the current 'CompState'.
       -- → (ActiveState t ActNode, a)
       → [Int] -- ^ A path from root to the node.
       → Tree (ActiveState t ActNode r, Dynamic t a)
       -- ^ Some content...
       → m [CompEvent t ActNode r]
own9MkLevelN lvlFs dmlst evB trSt pth treeE = do
    -- (evA,_) ← E.aN' $ text "my pre-link"
    cas ← mkLevelN lvlFs dmlst evB trSt pth treeE
    -- (evBtn,_) ← E.buttonN' $ text "and button-after"
    -- This one example, how we can deliver events from link or button
    -- to the caller.
    -- let astA ∷ ActiveState t ActNode r
    --          = defActiveStateTree { _activeStateElemId = ActNpath [998] }
    --     astB ∷ ActiveState t ActNode r
    --          = defActiveStateTree { _activeStateElemId = ActNpath [999] }
    --     cevA ∷ Event t (ActiveState t ActNode r) = astA <$ domEvent Mouseup evA
    --     cevB ∷ Event t (ActiveState t ActNode r) = astB <$ domEvent Mouseup evBtn
    --     ceA ∷ CompEvent t ActNode r = defCompEvent { _ceMe = astA
    --                                              , _ceMUp = cevA
    --                                              }
    --     ceB ∷ CompEvent t ActNode r = defCompEvent { _ceMe = astB
    --                                              , _ceMUp = cevB
    --                                              }
    -- pure $ ceA : ceB : cas
    pure cas

showRes ∷ forall t m r. (Reflex t, DomBuilder t m, PostBuild t m
                       , MonadHold t m, MonadFix m)
        ⇒ Tree Text → CompState t ActNode r → m ()
showRes tree res = do
    let eTxt = (_activeStateElemId . _ceMe) <$> _csCompEvent res
        -- eU = fmap _activeStateElemId $ coincidence $ _ceMUp <$> _csCompEvent res
            -- ::Event t (ActiveState t ActElem)
        -- dUD = _csDynUpLDownR res
        ePth = getPath <$> eTxt
        eNAP = (getNodeAtPath tree) <$> ePth
        eLAP = (isLeafAtPath tree) <$> ePth
    dElm ← holdDyn ActNnone eTxt
    dNAP ← holdDyn Nothing eNAP
    -- The way we use eLAP here is a bit too straightforward, it doesn't
    -- make difference on internal nodes and things not related to tree-nodes.
    -- (E.g when leaving the tree, this says it is an internal node.)
    dLAP ← holdDyn False eLAP
    -- dUpElm ← holdDyn ActNnone eU
    E.pN $ do
        text "Text on last node having some event: "
        dynText $ (T.pack . show) <$> dNAP
        text ". And it is "
        dyn $ ffor dLAP $ \b → do
            if b
                then text "a leaf."
                else text "an internal node."
    E.pN $ do
        text "Last (any) activity is on cell "
        dynText $ (T.pack . show) <$> dElm
        text " and entered cell "
        dynText $ (T.pack . show . _activeStateElemId) <$> _csDynEnter res
        text "."
    E.pN $ do
        let dUact = _csDynURelease res
        text "Released up on "
        dynText $ (T.pack . show . _activeStateElemId) <$> dUact
        text "."
    E.pN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> _csDynDOn res
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> _csDynUOn res
        text "."
    let dOut = _csDynMOutsideBody res
        dIn  = _csDynMInsideBody res
    dLInt ∷ Dynamic t Int ← count $ mouseOutEvent res
    dEInt ∷ Dynamic t Int ← count $ mouseInEvent res
    E.pN $ do
        text "dIn = "
        dynText  $ (T.pack . show) <$> dIn
        text " and dOut = "
        dynText  $ (T.pack . show) <$> dOut
        text "."
    E.pN $ do
        text "Next mouseEnter events: "
        dynText $ (T.pack . show) <$> dEInt
        text " and mouseLeave events: "
        dynText $ (T.pack . show) <$> dLInt
        text "."


