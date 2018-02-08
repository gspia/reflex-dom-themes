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
import           Control.Monad                  (mapM, join, void)
import           Control.Monad.Fix
-- import           Data.Default
import qualified Data.Map                       as Map
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

import           Reflex.Dom.HTML5.Attrs         as A
import           Reflex.Dom.HTML5.Elements      as E

------------------------------------------------------------------------------

import           Reflex.Dom.Theme.Raw.BS4       as B
-- import Reflex.Dom.Icon.Raw.FA as FA
import           Reflex.Dom.Icon.Raw.FlagIcons  as FI
import           Reflex.Dom.Icon.Raw.OpenIconic as OI

import           Reflex.Dom.Component.Menu
import           Reflex.Dom.Component.NaiveI18n

import           Reflex.Dom.HTML5.Component.Table



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
--     ( $(embedFile "assets/css/open-iconic/font/css/open-iconic-bootstrap.css") <>
--       $(embedFile "assets/css/flag-icon.css") <>
--       $(embedFile "assets/css/bootstrap.min.css") <>
--     ) bodyEl
    -- js and webkit2gtk probably won't go together - at least not this way
      -- $(embedFile "assets/js/jquery-3.2.1.slim.min.js") <>
      -- $(embedFile "assets/js/popper.min.js") <>
      -- $(embedFile "assets/js/bootstrap.min.js")

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

faURL = "https://cdnjs.cloudflare.com/ajax/libs/"
  <> "font-awesome/4.7.0/css/font-awesome.min.css" :: Text

oiCSS = "/css/open-iconic/font/css/open-iconic-bootstrap.css" :: Text
--  <link href="/open-iconic/font/css/open-iconic-bootstrap.css" rel="stylesheet">


bsURL =
  "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"
    :: Text
bsInt = "sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb"
  :: Text


headEl :: MonadWidget t m => m ()
headEl = do
    eTitle def $ text "Main Title"
    eMeta (charSet "utf-8" def) blank
    eMeta (name "viewport" $
         content "width=device-width, initial-scale=1, shrink-to-fit=no" def)
        blank
    eLink (ltStylesheet $ href (URL bsURL) $ integrity bsInt $ corsAnon def)
        blank
    eLink (ltStylesheet $ href (URL oiCSS) def) blank
    eLink (ltStylesheet $
         href (URL "https://fonts.googleapis.com/css?family=Raleway") def)
        blank
  -- eLink ( ltStylesheet $ href (URL faURL) $ def) $ blank
    eLink (ltStylesheet $ href (URL "./css/flag-icon.css") def) blank


--------------------------------------------------------------------------------

data MenuTag = MyPrg | FilePg | EditPg | PrefPg | SelectPg | TablePg
             | LoginPg | LogoutPg
             | MenuChoice | MenuChoiceLR | MenuChoiceL | MenuChoiceLst
             | LangChoice | LangGb | LangDe | LangSe
  deriving (Eq, Show, Ord)

data UiLang = LaGB | LaDE | LaSE | LaIso Text
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------

miLeftC =
  [ MIDefRec FilePg [oi, oiFile] "#file" True
  , MIDefRec EditPg [oi, oiLightbulb] "#edit" True
  , MIDefRec PrefPg [oi, oiCog] "#pref" True
  , MIDefRec SelectPg [oi, oiPin] "#select" True
  , MIDefRec TablePg [oi, oiSpreadsheet] "#table" True
  ] :: [MIDefRec MenuTag]

miRightC =
  [ MIDefRec LoginPg [oi, oiAccountLogin] "#login" True
  , MIDefRec LogoutPg [oi, oiAccountLogout] "#logout" True
  ] :: [MIDefRec MenuTag]

menuChoiceCfg = MIDefRec MenuChoice [oi, oiMenu] "#menuchoice" False
  :: MIDefRec MenuTag

menucMiCfg =
  [ MIDefRec MenuChoiceLR  [oi, oiAlignCenter] "#menuchoicelr" False
  , MIDefRec MenuChoiceL   [oi, oiAlignLeft] "#menuchoicel" False
  , MIDefRec MenuChoiceLst [oi, oiList] "#menuchoicelst" False
  ] :: [MIDefRec MenuTag]

langChoiceCfg = MIDefRec LangChoice [] "#" False :: MIDefRec MenuTag

langMiCfg =
  [ MIDefRec LangGb [flagIcon, flagIconGb] "#" False
  , MIDefRec LangDe [flagIcon, flagIconDe] "#" False
  , MIDefRec LangSe [flagIcon, flagIconSe] "#" False
  ] :: [MIDefRec MenuTag]

--------------------------------------------------------------------------------

bodyEl :: forall t m. MonadWidget t m => m ()
bodyEl = do
    let langMapGB = getMiLangMap LaGB
        --
        mkMi mid@(MIDefRec mt _ _ _) = Node (defRec2Cfg mid) []
        --
        miMCDefs = fmap mkMi menucMiCfg
        miMChoice = defRec2Cfg menuChoiceCfg :: MenuItemConfig t MenuTag
        mchoiceM = Node miMChoice miMCDefs :: Tree (MenuItemConfig t MenuTag)
        --
        miLaDefs = fmap mkMi langMiCfg
        miLa = defRec2Cfg langChoiceCfg :: MenuItemConfig t MenuTag
        langM = Node miLa miLaDefs :: Tree (MenuItemConfig t MenuTag)
        --
        miLDefs = FForest $ fmap mkMi miLeftC :: MenuForestConfig t MenuTag
        miRDefs' = fmap mkMi miRightC
        miRDefs = FForest $ miRDefs' ++ [mchoiceM] ++ [langM]
        --
        lrMenuStruct = LRMenuStruct miLDefs miRDefs
        lrMenuConf = LRMenuConf "Menu (LR)" lrMenuStruct
        --
        lMenuStruct = LMenuStruct $ miLDefs <> FForest [mchoiceM, langM]
        lMenuConf = LMenuConf "Menu (L)" lMenuStruct
        --
        miLstDefs = fmap defRec2Cfg
                    $ miLeftC ++ menucMiCfg :: [MenuItemConfig t MenuTag]
        lstMenuStruct = LstMenuStruct miLstDefs
        lstMenuConf = LstMenuConf "Menu (Lst)" lstMenuStruct
    rec
        -- eMt <- mkLRMenuBar dLang lrMenuConf
        -- eMt <- mk1LMenuBar dLang lMenuConf
        -- eMt <- mkMenuBarList dLang lstMenuConf
        deMt :: Dynamic t (Event t MenuTag)
          <- widgetHold (mkLRMenuBar dLang lrMenuConf) $ leftmost
              [ mkLRMenuBar   dLang lrMenuConf  <$ eMenuLR
              , mk1LMenuBar   dLang lMenuConf   <$ eMenuL
              , mkMenuBarList dLang lstMenuConf <$ eMenuLst
              ]
        let eMt = switch . current $ deMt
            eMenuLR  = ffilter (== MenuChoiceLR ) eMt
            eMenuL   = ffilter (== MenuChoiceL  ) eMt
            eMenuLst = ffilter (== MenuChoiceLst) eMt
            eLa = (\mt -> case mt of
                    LangDe -> LaDE
                    LangSe -> LaSE
                    LangGb -> LaGB
                  ) <$> ffilter
                    (\mt -> mt == LangSe || mt == LangGb || mt == LangDe) eMt
            eLang = getMiLangMap <$> eLa
        dMt <- holdDyn FilePg eMt
        dLang <- holdDyn langMapGB eLang
    let
        eMTable  = ffilter (== TablePg) eMt
        eMSelect = ffilter (== SelectPg) eMt
        eMFEP    = ffilter (`elem` [FilePg, EditPg, PrefPg]) eMt
    evPB <- getPostBuild
    dLa <- holdDyn LaGB eLa
    eH1 def $ text "Welcome to example prg"
    rec
        -- Draw one the widgets (almost empty page, select page, table page).
        void $ widgetHold (blank >> pure never) $ leftmost
                [ showAlmostEmpty dMt <$ eMFEP
                , showAlmostEmpty dMt <$ evPB
                , (blank >> pure never) <$ eMTable
                , (blank >> pure never) <$ eMSelect
                ]
        --
        devIntR :: Dynamic t (Event t Int)
            <- widgetHold (blank >> pure never) $ leftmost
                [ showTableContentR dMt dR <$ eMTable
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMSelect
                ]
        let evIntR = leftmost [(-2) <$ evPB, switch . current $ devIntR ]
        dIntR :: Dynamic t Int <- holdDyn (-3) evIntR
        dR :: Dynamic t Int <- holdUniqDyn dIntR
        --
        devIntC :: Dynamic t (Event t Int)
            <- widgetHold (blank >> pure never) $ leftmost
                [ showTableContentC dMt dC <$ eMTable
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMSelect
                ]
        let evIntC = leftmost [(-2) <$ evPB, switch . current $ devIntC ]
        dIntC :: Dynamic t Int <- holdDyn (-3) evIntC
        dC :: Dynamic t Int <- holdUniqDyn dIntC
        -- dC :: Dynamic t Int <- holdUniqDyn dIntC
        -- let dIntP = zipDynWith (,) dR dC
        --
        devDd :: Dynamic t (Event t DDtags)
            <- widgetHold (blank >> pure never) $ leftmost
                [ showSelectContent dMt dLa dDd <$ eMSelect
                , (blank >> pure never) <$ eMFEP
                , (blank >> pure never) <$ eMTable
                ]
        let evDd = leftmost [Select0DD <$ evPB, switch . current $ devDd ]
        dDd2 :: Dynamic t DDtags <- holdDyn Select0DD evDd
        dDd :: Dynamic t DDtags <- holdUniqDyn dDd2
    -- Show that the state is maintained.
    ePN $ do
        text "hmm : "
        dynText $ (T.pack . show) <$> dLa
        text " : "
        dynText $ (T.pack . show) <$> dDd
        text " : "
        dynText $ (T.pack . show) <$> dR
        text " : "
        dynText $ (T.pack . show) <$> dC
    blank


--------------------------------------------------------------------------------

showAlmostEmpty :: forall t m. MonadWidget t m
        => Dynamic t MenuTag
        -> m (Event t ())
showAlmostEmpty dMt = do
    let mt = fmap (T.pack . show) dMt
    eH2N $ text "A view having no components to show"
    ePN $ do
        text "The selected menu item was: "
        dynText mt
    pure never

--------------------------------------------------------------------------------

-- | Here we listen for the language change events, too.
showSelectContent :: forall t m. MonadWidget t m
                  => Dynamic t MenuTag
                  -> Dynamic t UiLang -> Dynamic t DDtags
                  -> m (Event t DDtags)
showSelectContent dMt dLa dDdtags = do
    evPB <- getPostBuild
    let miDDhead = defRec2Cfg $ head miDD :: MenuItemConfig t DDtags
        miDDitems = fmap defRec2Cfg (tail miDD) :: [MenuItemConfig t DDtags]
        miDDStr = LstDropdownStruct miDDhead miDDitems
        eUiLang = fmap (\mt2 ->
                     case mt2 of
                         LangDe -> LaDE
                         LangSe -> LaSE
                         LangGb -> LaGB) $
            ffilter (\mt3 -> mt3 == LangSe || mt3 == LangGb || mt3 == LangDe) $
            updated dMt
        eDDLang = getDDMiLangMap <$> eUiLang
        eDDdef = tag (current (getDDMiLangMap <$> dLa)) evPB
        eUiLangdef = tag (current dLa) evPB
    dLangDD <- holdDyn (getDDMiLangMap LaGB) $ leftmost [eDDLang, eDDdef]
    dUiLang <- holdDyn LaGB $ leftmost [eUiLang, eUiLangdef]
    --
    eH2N $ text "A selection example (drowdowns)"
    ePN $ do
        text "The selected menu item was: "
        dynText $ fmap (T.pack . show) dMt
    eDDt <- mkDropdownList dLangDD $ LstDropdownConf "dd nm" miDDStr
    let eOldDD = tag (current dDdtags) evPB
    dDDt <- holdDyn Select0DD $ leftmost [eDDt,eOldDD]
    let dDDtxt = fmap (T.pack . show) dDDt
    ePN $ do
        text "And the selected dropdown item was: "
        dynText dDDtxt
    pure $ updated dDDt

--------------------------------------------------------------------------------

showTableContentR :: forall t m. MonadWidget t m
                 => Dynamic t MenuTag
                 -> Dynamic t Int
                 -> m (Event t Int)
showTableContentR dMt dRow = do
    evPB <- getPostBuild
    eH2N $ text "Decorated table example"
    ePN $ do
        text "Note that the caption comes after the table. "
        text "And dRow = "
        dynText $ (T.pack .show) <$> dRow
        text "."
    evmTxtR <- tblWidget dRow
    dmTxtR <- holdDyn Nothing $ leftmost
        [ evmTxtR
        , (Just . ("#" <>) . T.pack . show) <$> tag (current dRow) evPB
        ]
    ePN $ dynText $ maybePersonIdTxt <$> dmTxtR
    let evR = updated $ mayInt <$> dmTxtR
    pure evR
    where
      maybePersonIdTxt (Just txt) = "Person " <> txt <> " is selected."
      maybePersonIdTxt Nothing    = "No person is selected."

mayInt :: Maybe Text -> Int
mayInt (Just txt) =
  let mct = T.uncons txt
   in case mct of
       Just (c,txtrest) ->
           case T.decimal txtrest of
               Left _ -> -1
               Right (r,_) -> fromIntegral r
       Nothing -> -1
mayInt Nothing = -1

mayInt2Text :: Maybe Int -> Maybe Text
mayInt2Text Nothing = Nothing
mayInt2Text (Just i) = Just $ "#" <> (T.pack . show) i

showTableContentC :: forall t m. MonadWidget t m
                 => Dynamic t MenuTag
                 -> Dynamic t Int
                 -> m (Event t Int)
showTableContentC dMt dCol = do
    evPB <- getPostBuild
    ePN $ do
        text "And dCol = "
        dynText $ (T.pack .show) <$> dCol
        text "."
    evmIntC <- tblWidget2 dCol
    dmTxtC <- holdDyn Nothing $ leftmost
        [ mayInt2Text <$> evmIntC
        , mayInt2Text . Just  <$> tag (current dCol) evPB
        ]
    ePN $ dynText $ maybeColumnIdTxt <$> dmTxtC
    let evC = updated $ mayInt <$> dmTxtC
    pure evC
    where
      maybeColumnIdTxt (Just txt) = "Column " <> txt <> " is selected."
      maybeColumnIdTxt Nothing    = "No column is selected."

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
tblWidget :: forall t m. (MonadWidget t m)
          => Dynamic t Int -> m (Event t (Maybe Text))
tblWidget dRow = do
    let
        tblHeaders :: MonadWidget t m => Maybe (HeaderConfV t m)
        tblHeaders = Just $ HeaderConfV
            (def & set thfThAttr (const $ constDyn $ style "width: 150px" def)
                 & set (thfADEs . drawEl) drawDivContent)
            (V.fromList $ const (constDyn def) <$> [1..5]) -- vec of empty ECol's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            def
        capDfs = Just (CaptionConf "Table 1. A Person list table example." def)
        dAstInit = (\i -> Just
                    [ def
                        & set activeStateElem (ActErow (i-1))
                        & set activeStateActive (constDyn True)
                    ]
                   ) <$> dRow
        tConf = def
            & set tableCaptionV capDfs
            & set tableHeaderV tblHeaders
            & set tableTableAttrV (constDyn $ setClasses [bsTableStriped]
                                $ style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTrAttr) trAttrfun
            & set (tableTdFunsV . tdfTdAttr) (const $ style "padding: 5px" def)
            & set cellDrawBodyV drawDivContent
            & set cellListenerBodyV (listenMyRow 5)
            & set tableActivityConfV dAstInit
    rec
        tblSt <- eDiv (setClasses [bsTable, bsTableSm
                                  , bsTableResponsiveSm, bsTableHover] def) $
            mkTableV tConf tblContent
        let dCell = _tsDynURelease tblSt
            dActElem = _activeStateElem <$> dCell
    let dmId = (giveId tblContent . rowNum) <$> dActElem
    pure $ updated dmId
    where
    giveId :: V.Vector (V.Vector Text) -> Maybe Int -> Maybe Text
    giveId _ Nothing = Nothing
    giveId v (Just i) = Just $ (v V.! i) V.! 0
    trAttrfun :: Dynamic t (ActiveState t) → ActElem → Dynamic t ETr
    trAttrfun dAst _ae = mkETr <$> join (_activeStateActive <$> dAst)
      where
        mkETr :: Bool -> ETr
        mkETr b =
               if b
                   then setClasses [bsTableActive] def
                   else setClasses [] def
                  -- then style "background-color: grey" def
                  -- else style "background-color: lightgrey" def

    -- trAttrfun dAst ae = mkETr <$> (_activeStateElem <$> dAst)  <*> pure ae
    --   where
    --     mkETr :: ActElem -> ActElem -> ETr
    --     mkETr ae1 ae2 = if sameRowAE ae1 ae2
    --                        then setClasses [bsTableActive] def
    --                        else setClasses [] def
                           -- then style "background-color: lightgreen" def
                           -- else style "" def


-- | Make a table using mkTableV-component. This time, one of the columns is
-- selected.
tblWidget2 :: forall t m. (MonadWidget t m)
           => Dynamic t Int -> m (Event t (Maybe Int))
tblWidget2 dCol = do
    let
        tblHeaders :: Maybe (HeaderConfV t m)
        tblHeaders = Just $ HeaderConfV
            (def & set (thfADEs . drawEl) drawDivContent
                 & set thfThAttr myThAttrF
            )
            (V.fromList $ const (constDyn def) <$> [1..5]) -- vec of empty ECol's.
            (V.fromList ["Id", "First Name", "Surname", "Address", "Phone number"])
            def
        capDfs = Just (CaptionConf "Table 2. This time select columns." def)
        dAstInit :: Dynamic t (Maybe [ActiveState t]) = (\i -> Just
                    [ def
                        & set activeStateElem (ActEcolh i)
                        & set activeStateActive (constDyn True)
                    ]
                   ) <$> dCol
        tConf :: TableConfV t m Text = def
            & set tableCaptionV capDfs
            & set tableHeaderV tblHeaders
            & set tableTableAttrV (constDyn $ setClasses [bsTableStriped]
                                $ style "border-collapse: collapse" def)
            & set (tableTdFunsV . tdfTdAttr) myTdAttrF
            & set cellDrawBodyV drawDivContent
            & set cellListenerBodyV (listenMyCol 4)
            & set tableActivityConfV dAstInit

    rec
        tblSt <- eDiv (setClasses [bsTable, bsTableSm
                                  , bsTableResponsiveSm] def) $
            mkTableV tConf tblContent
        let dCell = _tsDynURelease tblSt
            dActElem = _activeStateElem <$> dCell
    pure $ updated $ colNum <$> dActElem
      where
        dthDef :: Dynamic t Globals
        dthDef = constDyn $ style "width: 150px" def
        myThAttrF :: ActiveState t -> Dynamic t ETh
        myThAttrF ast = defThAttrF
           ( ast & set activeStateActiveGl
                    (setClasses [bsTableActive] dthDef)
                 & set activeStateNotActiveGl
                    (setClasses [] dthDef))
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
           ( ast & set activeStateActiveGl (setClasses [bsTableActive] def)
                 & set activeStateNotActiveGl (setClasses [] def))


--------------------------------------------------------------------------------

-- type LangMap = Map.Map MenuTag Text

miEn :: LangMap MenuTag
miEn = Map.fromList [(FilePg, "File"), (EditPg, "Edit")
                    , (PrefPg , "Preferences"), (SelectPg, "Select")
                    , (TablePg, "Tables")
                    , (LoginPg, "Login"), (LogoutPg, "Log out")
                    , (MenuChoice, "Menu Choice"), (MenuChoiceLR, "Left-Right Tree")
                    , (MenuChoiceL, "Left Tree") , (MenuChoiceLst, "List")
                    , (LangChoice, "Language"), (LangGb, "British")
                    , (LangDe, "Germany"), (LangSe, "Swedish")
                    ]

miSe :: LangMap MenuTag
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

miDe :: LangMap MenuTag
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


getMiLangMap :: UiLang -> LangMap MenuTag
getMiLangMap LaGB = miEn
getMiLangMap LaDE = miDe
getMiLangMap LaSE = miSe
getMiLangMap _    = miEn -- default



--------------------------------------------------------------------------------


data DDtags = Select0DD | Select1DD | Select2DD | Select3DD
  deriving (Eq,Show,Ord)

miDD =
  [ MIDefRec Select0DD [] "#sel0" False
  , MIDefRec Select1DD [] "#sel1" False
  , MIDefRec Select2DD [] "#sel2" False
  , MIDefRec Select3DD [] "#sel3" False
  ] :: [MIDefRec DDtags]


miDDEn :: LangMap DDtags
miDDEn = Map.fromList [ (Select0DD, "Select 0")
                      , (Select1DD, "Select 1")
                      , (Select2DD, "Select 2")
                      , (Select3DD, "Select 3")
                      ]

miDDSe :: LangMap DDtags
miDDSe = Map.fromList [ (Select0DD, "Markera 0")
                      , (Select1DD, "Markera 1")
                      , (Select2DD, "Markera 2")
                      , (Select3DD, "Markera 3")
                      ]

miDDDe :: LangMap DDtags
miDDDe = Map.fromList [ (Select0DD, "Auswahl 0")
                      , (Select1DD, "Auswahl 1")
                      , (Select2DD, "Auswahl 2")
                      , (Select3DD, "Auswahl 3")
                      ]


getDDMiLangMap :: UiLang -> LangMap DDtags
getDDMiLangMap LaGB = miDDEn
getDDMiLangMap LaDE = miDDDe
getDDMiLangMap LaSE = miDDSe
getDDMiLangMap _    = miDDEn -- default

