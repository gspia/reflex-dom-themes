{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module MainW where

import           Control.Arrow                  ((***))
import           Control.Monad                  (mapM, join)
import           Control.Monad.Fix
import qualified Data.Map                       as Map
-- import           Data.Monoid as Mon
-- import           Data.FileEmbed
import           Data.Semigroup
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
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

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Component.TableV



------------------------------------------------------------------------------
-- exampleBs -- bootstrap
------------------------------------------------------------------------------

mainW ∷ JSM ()
-- mainW = mainWidget bodyEl
mainW = mainWidgetWithHead headEl bodyEl
-- It seems that we should implement at least some of the js-functions in order
-- to get corresponding functionality to webkit2gtk.
-- mainW = mainWidgetWithCss
--     ( $(embedFile "assets/css/open-iconic/font/css/open-iconic-bootstrap.css") <>
--       $(embedFile "assets/css/flag-icon.css") <>
--       $(embedFile "assets/css/bootstrap.min.css") <>
--     ) bodyEl
    -- js and webkit2gtk probably won't go together
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
        eLang
          = (\mt ->
              case mt of
                LangDe -> getMiLangMap LaDE
                LangSe -> getMiLangMap LaSE
                LangGb -> langMapGB
                 )
          <$> ffilter (\mt -> mt == LangSe || mt == LangGb || mt == LangDe) eMt
    dMt <- holdDyn FilePg eMt
    dLang <- holdDyn langMapGB eLang
  eH1 def $ text "Welcome to example prg"
  -- showMenuContent dMt
  let
    eMTable  = ffilter (== TablePg) eMt
    eMSelect = ffilter (== SelectPg) eMt
    eMFEP    = ffilter (`elem` [FilePg, EditPg, PrefPg]) eMt
  _ <- widgetHold (showAlmostEmpty dMt) $ leftmost
            [ showAlmostEmpty dMt <$ eMFEP
            , showSelectContent dMt <$ eMSelect
            , showTableContent dMt <$ eMTable
            ]
  blank

 where
    mkMi mid@(MIDefRec mt _ _ _) = Node (defRec2Cfg mid) []

--------------------------------------------------------------------------------

showAlmostEmpty :: forall t m. MonadWidget t m => Dynamic t MenuTag -> m ()
showAlmostEmpty dMt = do
    let mt = fmap (T.pack . show) dMt
    eH2N $ text "A view having no special components to show"
    ePN $ do
        text "The selected menu item was: "
        dynText mt


showSelectContent :: forall t m. MonadWidget t m => Dynamic t MenuTag -> m ()
showSelectContent dMt = do
    let miDDhead = defRec2Cfg $ head miDD :: MenuItemConfig t DDtags
        miDDitems = fmap defRec2Cfg (tail miDD) :: [MenuItemConfig t DDtags]
        miDDStr = LstDropdownStruct miDDhead miDDitems
        mt = fmap (T.pack . show) dMt
        langMapDDGB = getDDMiLangMap LaGB
        eDDLang =
            fmap
                (\mt ->
                     case mt of
                         LangDe -> getDDMiLangMap LaDE
                         LangSe -> getDDMiLangMap LaSE
                         LangGb -> langMapDDGB) $
            ffilter (\mt -> mt == LangSe || mt == LangGb || mt == LangDe) $
            updated dMt
    dLangDD <- holdDyn langMapDDGB eDDLang
    --
    eH2N $ text "A selection example (drowdowns)"
    ePN $ do
        text "The selected menu item was: "
        dynText mt
        -- eLanUpd = updated dLangDD
    -- eDDt <- mkDropdownList dLangDD miDDhead miDDitems
    eDDt <- mkDropdownList dLangDD $ LstDropdownConf "dd nm" miDDStr
    dDDt <- holdDyn Select0DD eDDt
    let dDDtxt = fmap (T.pack . show) dDDt
    ePN $ do
        text "And the selected dropdown item was: "
        dynText dDDtxt

showTableContent :: forall t m. MonadWidget t m => Dynamic t MenuTag -> m ()
showTableContent dMt = do
    -- let miDDhead = defRec2Cfg $ head miDD :: MenuItemConfig t DDtags
    --     miDDitems = fmap defRec2Cfg (tail miDD) :: [MenuItemConfig t DDtags]
    --     miDDStr = LstDropdownStruct miDDhead miDDitems
    --     mt = fmap (T.pack . show) dMt
    --     langMapDDGB = getDDMiLangMap LaGB
    --     eDDLang =
    --         fmap
    --             (\mt ->
    --                  case mt of
    --                      LangDe -> getDDMiLangMap LaDE
    --                      LangSe -> getDDMiLangMap LaSE
    --                      LangGb -> langMapDDGB) $
    --         ffilter (\mt -> mt == LangSe || mt == LangGb || mt == LangDe) $
    --         updated dMt
    -- dLangDD <- holdDyn langMapDDGB eDDLang
    --
    eH2N $ text "Decorated table example"
    ePN $ do
        ePN $ text "Note that the caption comes after the table."
        dmTxt <- tblWidget
        ePN $ dynText $ maybePersonIdTxt <$> dmTxt
        blank
    where
      maybePersonIdTxt (Just txt) = "Person " <> txt <> " is selected."
      maybePersonIdTxt Nothing    = "No person is selected."

--showMenuContent :: forall t m. MonadWidget t m
--    => Dynamic t MenuTag -> m ()
--showMenuContent dMt = do
--    let miDDhead = defRec2Cfg $ head miDD :: MenuItemConfig t DDtags
--        miDDitems = fmap defRec2Cfg (tail miDD) :: [MenuItemConfig t DDtags]
--        miDDStr = LstDropdownStruct miDDhead miDDitems
--        mt = fmap (T.pack . show) dMt
--        langMapDDGB = getDDMiLangMap LaGB
--        eDDLang =
--            fmap
--                (\mt ->
--                     case mt of
--                         LangDe -> getDDMiLangMap LaDE
--                         LangSe -> getDDMiLangMap LaSE
--                         LangGb -> langMapDDGB) $
--            ffilter (\mt -> mt == LangSe || mt == LangGb || mt == LangDe) $
--            updated dMt
--    dLangDD <- holdDyn langMapDDGB eDDLang
--    --
--    ePN $ do
--        text "The selected menu item was: "
--        dynText mt
--        -- eLanUpd = updated dLangDD
--    -- eDDt <- mkDropdownList dLangDD miDDhead miDDitems
--    eDDt <- mkDropdownList dLangDD $ LstDropdownConf "dd nm" miDDStr
--    dDDt <- holdDyn Select0DD eDDt
--    let dDDtxt = fmap (T.pack . show) dDDt
--    ePN $ do
--        text "And the selected dropdown item was: "
--        dynText dDDtxt
--    eH2N $ text "Decorated table example"
--    ePN $ do
--        ePN $ text "Note that the caption comes after the table."
--        dmTxt <- tblWidget
--        ePN $ dynText $ maybePersonIdTxt <$> dmTxt
--        blank
--    where
--      maybePersonIdTxt (Just txt) = "Person " <> txt <> " is selected."
--      maybePersonIdTxt Nothing    = "No person is selected."

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
            , _tdfTableAttr = setClasses [bsTableStriped] def}
    rec
        tblSt <- eDiv (setClasses [bsTable, bsTableSm
                                  , bsTableResponsiveSm, bsTableHover] def) $
            -- mkTableV fns capDfs cPair Nothing tblContent
            ownMkTableV fns capDfs cPair Nothing tblContent dActElem
        let dCell = _tsDynURelease tblSt
            dActElem = _activeStateMe <$> dCell
    let dmId = (giveId tblContent . giveRowNum) <$> dActElem
    pure dmId
    where
    -- Listen for clicks on tbody-cells and activate row on click
    -- (update reflex-dom-htmlea to have a row listening listener).
    ownListen cols ae = actstate ae & \d -> d {_activeStateListen = constDyn ag }
      where
        ag = ActiveGroup $ Set.fromList $ ae: myHF ae
        actstate txt = def & \d1 -> d1 {_activeStateMe = txt }
           -- & \d2 -> d2 { _activeStateActiveCl = constDyn [bsTableActive] }
           -- & \d3 -> d3 { _activeStateNotActiveCl = constDyn [bsTableLight] }
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
              dETr1 = constDyn $ setClasses [bsTableActive] def :: Dynamic t ETr
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

