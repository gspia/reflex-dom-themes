{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction, UnicodeSyntax, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.Component.Bulma.Dropdown
Description : Dropdowns
Copyright   : (c) gspia 2017 -
License     : BSD3
Maintainer  : gspia

= Dropdown-structures

This module contains functions that can be used to construct dropdown
structures.


== Note

-}

module Reflex.Dom.Component.Bulma.Dropdown where

-- import           Control.Applicative
-- import Control.Arrow -- ((&&&), arr)
import           Control.Lens
-- import           Control.Monad (mapM)
-- import Control.Monad.Fix (MonadFix)
-- import Control.Monad.Zip (mzipWith)
import           Control.Monad.State.Lazy

-- import qualified Data.Map as Map
-- import qualified Data.ChunkedZip as Z
-- import           Data.Maybe
-- import qualified Data.Monoid as Mon
import           Data.List              (inits)
import qualified Data.Map               as Map
import           Data.Map               (Map)
-- import           Data.Maybe
import           Data.Semigroup
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tree

import           Language.Javascript.JSaddle
-- import           Reflex
-- import           Reflex.Dom                             hiding (mainWidget)
import           Reflex.Dom.Core

--------------------------------------------------------------------------------

import qualified Reflex.Dom.HTML5.Attrs as A
import qualified Reflex.Dom.HTML5.Elements as E
import           Reflex.Dom.HTML5.Component.Common.CompEvent
import           Reflex.Dom.HTML5.Component.Tree
import           Reflex.Dom.HTML5.Component.Common.RfpUtils

--------------------------------------------------------------------------------

import           Reflex.Dom.Component.NaiveI18n
import           Reflex.Dom.Component.MenuCommon
-- import           Reflex.Dom.Component.Bulma.Menu
import qualified Reflex.Dom.Theme.Raw.Bulma as B
import qualified Reflex.Dom.Icon.Raw.FA     as FA

--------------------------------------------------------------------------------

-- Dropdowns use MenuItemConfig and MenuItem -structures.
--

--------------------------------------------------------------------------------


mkDropdown ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ Tree (MenuItemConfig t a b)
            → Dynamic t [a]
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t (CompState t ActNode (MenuItem t a)))
mkDropdown trMic dInitElms dLang updLang = do
    evPB ← getPostBuild
    lrTree ∷ Tree (MenuItem t a) ← micTr2miTr trMic
    let ogs ∷ Map b [a] = findOneOfGroups (trMic ∷ Tree (MenuItemConfig t a b))
    -- let fs1Lvl = CommonADEfuns listenMe actMU drawDDMI elemEvF
    --     fsNLvl = CommonADEfuns listenMe actMU drawMILeaf elemEvF
    let fs1Lvl = CommonADEfuns listenMe actMU drawDivActElemEx elemEvF
        -- fsNLvl = CommonADEfuns listenMe actMU drawDivActElemEx elemEvF
        -- fsRoot = CommonADEfuns listenMe actMU drawDivActElemEx elemEvF
        -- fsRoot = CommonADEfuns listenMe actSwitchMU drawDivActElemEx elemEvF
        -- fsRoot = CommonADEfuns listenMe actSwitchMU drawRootI elemEvF
        -- fsRoot = CommonADEfuns listenMe actSwitchMU drawNothing elemEvF
        -- fsRoot = CommonADEfuns listenMe actSwitchMU drawDivActElemEx elemEvF
        -- fsRoot = CommonADEfuns listenMe actMU drawDivActElemEx elemEvF
        fsRoot = CommonADEfuns listenMe actMU drawDivContentEx elemEvF
        -- rootConf = defRootDivDivFuns
        rootConf = defRootNavDivFuns
            -- & set rootfADEs nothingADE -- draw nothing, listen nothing
            & set rootfADEs fsRoot 
            -- & set rootfWrapAttr navAttrs
            -- & set rootfNodeAttr divRootNAttrs
            -- & set rootfWrapAttr divRootWAttrs
            -- & set rootfnext lvl1Fs
            -- & set rootfMkLevelNF customMkLevelDD
        lvl1Fs = defLevelNDivAFuns
            & set levelNfADEs fs1Lvl
            -- & set levelNfNodeAttr aAttr
            -- & set levelNfnext (Just lvlNFs)
            -- & set levelNfWrapAttr (divAttr lrTree)
            -- & set levelNfWrapAttr divAttr 
            -- & set levelNfNodeAttr aLvl1Attrs
            -- & set levelNfWrapAttr divLvl1Attrs
        -- lvlNFs = defLevelNDivAFuns --
        --     & set levelNfADEs fsNLvl
            -- & set levelNfNodeAttr aAttr
            -- & set levelNfWrapAttr (divAttr lrTree)
            -- & set levelNfNodeAttr aLvl1Attrs
            -- & set levelNfWrapAttr divLvl1Attrs
            -- & set levelNfWrapAttr divAttr 
        treeConf ∷ TreeConf t m (MenuItem t a) (MenuItem t a)
            = defTreeConf  & set treeCRootC rootConf
        defElmT ∷ ActiveState t ActNode (MenuItem t a)
            = defActiveStateTree & set activeStateActive (constDyn True)
        defElmF ∷ ActiveState t ActNode (MenuItem t a)
            = defActiveStateTree & set activeStateActive (constDyn False)
        mAstSelP exLst = initializeAllNodesWith lrTree exLst defElmF defElmT
        mAstNoSels = initializeAllNodes lrTree defElmT
        pthTree ∷ Tree [Int] = mkPathsTree lrTree
        pthTagTree ∷ Tree (a, [Int])
            = biTreeMap (\mi pth → (_menuItemMenuTag mi,pth)) lrTree pthTree
        parsOOs ∷ Map b (a,[Int])
            = findParents4OneOfs
                (trMic ∷ Tree (MenuItemConfig t a b))
                $ findParents pthTagTree
        prPths ∷ [[Int]] = fmap snd . Map.elems $ parsOOs
        -- chPths ∷ [[Int]] = fmap ( (++ [0]) . snd) . Map.elems $ parsOOs
        pthTagLst = flatten pthTagTree
        diNds = fmap (\initElms → filter (\(tg,_pth) → tg `elem` initElms) pthTagLst
                    ) dInitElms
        initPth ∷ [[Int]] = [] -- Initial menu selection
        -- initPth ∷ [[Int]] = [[0]] -- Initial menu selection
        dInitPth ∷ Dynamic t [[Int]] -- Initials, ch. Menu
            = fmap (fmap snd) diNds -- Initial menu selection
        mAstInit = initializeAllNodesWith lrTree initPth defElmF defElmT
        dLrTree = constDyn lrTree
        -- dTreeConf = constDyn (treeConf & set treeCActivityConf (constDyn mAstInit))
        dmAstDeSelAllI = constDyn $ initializeAllNodes lrTree defElmF
        tCI ∷ TreeConf t m (MenuItem t a) (MenuItem t a)
             = treeConf & set treeCActivityConf dmAstDeSelAllI
        -- initState ∷ Map [Int] [Int]
            -- = Map.fromList $ zip prPths chPths
        dInitState ∷ Dynamic t (Map [Int] [Int])
            = fmap (\ipth → Map.fromList $ zip prPths ipth) dInitPth
    -- display dInitPth
    rec
        -- let ePth = [[0,1]] <$ evPB
        -- let ePth = [[0,7],[0,7,1]] <$ evPB
        dOth ∷ Dynamic t [[Int]] ← holdDyn initPth eOth
        -- let dOth ∷ Dynamic t [[Int]] = dInitPth -- ch. Menu
        -- dOth ∷ Dynamic t [[Int]]
            -- ← switcherDyn dInitPth (dInitPth <$ evPB)
            -- ← switcherDyn dInitPth (dInitPth <$ ePar)
            -- ← switcherDyn dInitPth (constDyn [[0]] <$ ePar)
            -- ← switcherDyn (constDyn initPth) ( dInitPth <$ ePar)
        -- display dOth
        dAInit ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                ← holdDyn mAstNoSels $
                    leftmost [ mAstInit <$ evPB
                             , eAstI
                             ]
        -- let dAInit ∷ Event t (Maybe [ActiveState t ActNode (MenuItem t a)])
                -- = mAstSelP <$> eAstI
        let dAInit2 ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                = mAstSelP <$> dOth
            -- dAInit2 = zipDynWith (\b mb →
                                 -- if b
                                     -- then mb
                                     -- else Nothing
                                 -- ) dParVis dAInit3
        -- let dInitPth = fmap (fmap (fmap _activeStateElemId)) dAInit
        -- let dInitB ∷ Dynamic t (Maybe [Dynamic t Bool])
                -- = fmap (fmap (fmap _activeStateActive)) dAInit
            -- dInitB2  ∷ Dynamic t [Dynamic t Bool] = fmap (\ml →
                           -- case ml of
                               -- Just l → l
                               -- Nothing → []
                           -- ) dInitB
        -- display dInitPth
        -- dyn $ ffor dInitB2 $ \lstD → do
            -- mapM (\eD → dynText (T.pack . show <$> eD)) lstD
        let dLrTree2 = fmap (fmap (updLang dLang)) dLrTree
            -- dTreeConf2 = fmap (set treeCActivityConf dAInit) dTreeConf
        dAstI2 ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)]) ←
            switcherDyn (constDyn mAstNoSels)
            -- switcherDyn dAInit2
                $ leftmost [ dAInit2 <$ eOOChPth
                           , dAInit <$ eAstI
                           , dAInit2 <$ evPB -- ch. Menu
                           ]
        let eTC ∷ Event t (TreeConf t m (MenuItem t a) (MenuItem t a))
                = -- traceEventWith (const "eTC") $
                    (treeConf & set treeCActivityConf dAstI2) <$ (updated dAstI2 )
                -- = (treeConf & set treeCActivityConf dAstI2) <$ (updated dAInit )
                -- = (treeConf & set treeCActivityConf dAInit) <$ (updated dAInit )
        dTreeConf ∷ Dynamic t (TreeConf t m (MenuItem t a) (MenuItem t a))
            ← holdDyn tCI eTC
        -- display dParVis
        eCsAnMi ∷ Event t (CompState t ActNode (MenuItem t a))
            ← mkTreeDD dTreeConf dLrTree2
        let eCe ∷ Event t (Event t [Int]) =
                -- traceEventWith (const "eCe") $
                  (coincidence . fmap (fmap (getPath . _activeStateElemId)))
                  . fmap _ceMUp . _csCompEvent <$> eCsAnMi
        eUpd ∷ Event t [Int] ← switchHold never eCe
        let eMt ∷ Event t a = -- traceEventWith (\a → "eMt:" ++ show a) $
                fmap _menuItemMenuTag $ fmapMaybe (getNodeAtPath lrTree) eUpd
        let eTgPth ∷ Event t (a, [Int]) =
                -- traceEventWith (\a → "eTgPth: " ++ show a) $
                fmap (\mt → head $ filter ((mt ==) . fst)
                                   (flatten pthTagTree)
                      ) eMt
        let eListOneOfs ∷ [Event t (a, [Int])] = evs4OneOfs2 ogs eTgPth
            eOO = leftmost eListOneOfs
            eOOChPth ∷ Event t [[Int]] = -- traceEvent "eOOChPth" $
                fmap ((:[]).snd) eOO
            eParEvs ∷ [Event t (a, [Int])] = parentEvents parsOOs eTgPth
            ePar = -- traceEvent "ePar" $
                leftmost eParEvs
            ePar2 = fmap ( (\p → drop 1 $ inits p) . snd ) ePar
            eUpd2 = fmap (:[]) eUpd
            eOth = -- traceEvent "eOth" $
                difference eUpd2 $ leftmost [ePar2,eOOChPth]
        dParVis ← toggle False $ leftmost [() <$ ePar, () <$ eOO]
        let eStateMap ∷ Event t (Map [Int] [Int]) =
              attachPromptlyDynWith
                (\swt n →
                let par = reverse . drop 1 . reverse $ n
                 in Map.alter (const $ Just n) par swt
                ) dInitState (fmap snd eOO)
        dStateMap ← holdDyn Map.empty eStateMap
        let dStateMap2 = zipDynWith
                (\swt n →
                let par = reverse . drop 1 . reverse $ n
                 in Map.alter (const $ Just n) par swt
                ) dInitState (fmap head dInitPth)
        dStateMap3 ← switcherDyn dStateMap2 (dStateMap <$ eOO)
        -- display dStateMap
        let eParII = -- traceEvent "eParII" $
                attachPromptlyDynWith
                (\mp par →
                    case Map.lookup par mp of
                        -- Just p → [[0],[0,7],[0,7,1]]
                        Just p → drop 1 $ inits p
                        Nothing → [par]
                ) dStateMap3 (fmap snd ePar)
            dTrB = zipDyn dLrTree2 dParVis
            eAstI ∷ Event t (Maybe [ActiveState t ActNode (MenuItem t a)]) =
                -- traceEventWith (const "eAstI") $
                attachWith (\(exTr,b) pth →
                            if b -- why this way, it feels a bit un-intuitive TODO
                                then Nothing
                                else initializeAllNodesWith exTr pth defElmF defElmT
                           -- ) (current dLrTree2) eParII
                           ) (current dTrB) eParII
    -- let eIsAct ∷ Event t (Dynamic t Bool) =
    --         coincidence $
    --         fmap (_activeStateActive . _ceMe ) . _csCompEvent <$> eCsAnMi
    -- ddIsAct ← holdDyn (constDyn False) eIsAct
    -- let dIsAct = join ddIsAct
    pure eCsAnMi
  where
    divRootWAttrs ∷ Dynamic t (ActiveState t ActNode (MenuItem t a))
         → ActNode → Dynamic t A.Attr
    divRootWAttrs _dAst _an = pure $ A.attrMap $  A.setClasses
        [B.dropdown] E.defDiv
    divRootNAttrs ∷ ActiveState t ActNode (MenuItem t a)
         → Dynamic t A.Attr
    divRootNAttrs ast = pure (A.attrMap E.defDiv)
      --   fmap (\idTxt → A.attrMap $
      --   A.id_ idTxt $
      --   E.defDiv ) dIdTxt
      -- where
      --   dIdTxt = ( ("dropdown" <>) . (T.pack . show . _menuItemMenuTag ))
      --       <$> _activeStateRetVal ast
    -- Next one is for drop-down menu items (that is, level 2 or further).
    -- This is for the node elements that are individual items.
    aAttr ∷ ActiveState t ActNode (MenuItem t a)
          → Dynamic t A.Attr
    aAttr ast = defANodeAttrF
        (ast & set activeStateActiveGl
          (A.setClasses [B.isActive] $ pure A.defGlobals)
          -- (pure A.defGlobals)
           & set activeStateNotActiveGl
          -- ( A.setClasses [bsDropdownItem] $ pure A.defGlobals)
          ( pure A.defGlobals)
        )
    -- Next one is for drop-down menu items (that is, level 2 or further).
    -- This is for the wrapping element that collects items.
    divAttr ∷ Tree (MenuItem t a)
            → Dynamic t (ActiveState t ActNode (MenuItem t a))
            -- last node where event occurred
            → ActNode
            -- what we are now making
            → Dynamic t A.Attr
    divAttr trA dAst an = fmap (\cls → A.attrMap $ A.setClasses cls
                               $ A.aLabelledby lblName $ E.defDiv
                               ) dCls
      where
        dCls = fmap (\b → if b then [B.dropdownItem, B.isActive]
                               else [B.dropdownItem]
                    ) dIsAct
        dIsAct = join $
              (\an2 → if an == _activeStateElemId an2
                          then _activeStateActive an2
                          else constDyn False
              ) <$> dAst 
        mNd = getNodeAtPath trA (getPath an)
        lblName = -- "dropdown" <> (T.pack . show . getPath $ an)
            case mNd of
              Just nd → "dropdown" <> (T.pack . show $ _menuItemMenuTag nd)
              -- Just nd → "dropdownMenuLink"
              Nothing → "dropdownUnknown"




--------------------------------------------------------------------------------

drawRootI ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawRootI dMic actS = do
    let -- dTxt = (T.pack . show . _menuItemMenuTag) <$> dMic
        dIco = _menuItemIcons <$> dMic
        dIsDD = _menuItemDD <$> dMic -- True indicates a dropdown
        dActive = _activeStateActive actS
        -- pth = getPath $ _activeStateElemId actS
        dLblName =
            (("dropdown" <> ) . T.pack . show . _menuItemMenuTag) <$> dMic
        dAAttrs =
            fmap (\(isDD,lblName) →
                 if isDD
                     -- then A.setClasses [bsNavLink, bsDropdownToggle]
                     then   A.id_ lblName $ A.dToggle "dropdown"
                            $ A.aHaspopup "true" $ A.aExpanded "false"
                            $ E.defA
                     else E.defA
                     -- else A.setClasses [bsNavLink] E.defA
                 ) $ zipDyn dIsDD dLblName
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    (e,_) ← E.aD' dAAttrs $ do
        _ ← dyn $ ffor dIco $ \ico → do
                if null ico
                   then blank
                   else E.span (A.setClasses ico E.defSpan) blank
        dynText dLbl
        text "hmm RootI"
        -- dyn $ ffor dActive $ \b → do
        --     if b
        --        then E.span (A.setClasses [bsSrOnly] E.defSpan) $ text "(current)"
        --        else blank
            -- if b
               -- then text "active"
               -- else text "not act"
    pure (e, dMic)


drawDDMI ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawDDMI dMic actS = do
    let -- dTxt = (T.pack . show . _menuItemMenuTag) <$> dMic
        dIco = _menuItemIcons <$> dMic
        dIsDD = _menuItemDD <$> dMic -- True indicates a dropdown
        dActive = _activeStateActive actS
        -- pth = getPath $ _activeStateElemId actS
        dLblName =
            (("dropdown" <> ) . T.pack . show . _menuItemMenuTag) <$> dMic
        dAAttrs =
            fmap (\(isDD,lblName) →
                 if isDD
                     -- then A.setClasses [bsNavLink, bsDropdownToggle]
                     then   A.id_ lblName $ A.dToggle "dropdown"
                            $ A.aHaspopup "true" $ A.aExpanded "false"
                            $ E.defA
                     else A.setClasses [A.ClassName "DDMI"] E.defA
                     -- else A.setClasses [bsNavLink] E.defA
                 ) $ zipDyn dIsDD dLblName
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    (e,_) ← E.aD' dAAttrs $ do
        _ ← dyn $ ffor dIco $ \ico → do
                if null ico
                   then blank
                   else E.span (A.setClasses ico E.defSpan) blank
        dynText dLbl
        text "hmm DDMI"
        -- dyn $ ffor dActive $ \b → do
        --     if b
        --        then E.span (A.setClasses [bsSrOnly] E.defSpan) $ text "(current)"
        --        else blank
            -- if b
               -- then text "active"
               -- else text "not act"
    pure (e, dMic)

-- Hmm, this can be found from Menu.
drawMILeaf ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                                                 , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawMILeaf dMic actS = do
    let dUse = elemAttrs E.defDiv actS
        dIco = _menuItemIcons <$> dMic
        dActive = _activeStateActive actS
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    -- (e,_) ← drawDivWdt (constDyn ) dMic actS
    -- (e,_) ← drawDivContentS dMic actS
    -- (e,_) ← drawDivActElemEx dMic actS
    (e,_) ← E.divD' dUse $ do
        dyn $ ffor dIco $ \ico → do
            if null ico
               then blank
               else E.span (A.setClasses ico E.defSpan) blank
        dynText dLbl
        text "hmm MILeaf"
        -- dyn $ ffor dActive $ \b →
        --     if b
        --        then E.span (A.setClasses [bsSrOnly] E.defSpan) $ text "(current)"
        --        else blank
    pure (e, dMic)

--------------------------------------------------------------------------------


customMkLevelDD ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, Eq a, Show a
     , ActSretval a
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
       ⇒ LevelNFuns t m a a
       -- ^ Funs and decorations
       →  Dynamic t (Maybe [ActiveState t ActNode a])
       -- ^ User given activity information obtained from tree configurations.
       → Event t (CompEvent t ActNode a)
       -- ^ Event coming from tree (can be from nodes).
       → CompState t ActNode a
       -- ^ A function combining XX's can use the current 'CompState'.
       → [Int] -- ^ A path from root to the node.
       → Tree (ActiveState t ActNode a, Dynamic t a)
       -- ^ Some content...
       → m [CompEvent t ActNode a]
customMkLevelDD lvlFs dmlst evB trSt pth treeE = do
    let (_anR, dR) = rootLabel treeE
        aAttrs = A.setClasses [B.navbarItem] $ E.defA
        dLbl = (T.pack . show) <$> dR
    (eB,_) ← E.divN' $ do
            E.button ( A.setClasses [B.button] $ A.aHaspopup "true"
                   $ A.aControls "dropdown-menu" E.defButton) $ do
                E.spanN $ do
                    dynText dLbl
                E.span (A.setClasses [B.icon, B.isSmall] E.defSpan) $ do
                    E.i ( A.setClasses [FA.fa, FA.faAngleDown]
                        $ A.aHidden "true" E.defI) blank
    let astB ∷ ActiveState t ActNode a
             = defActiveStateTree { _activeStateElemId = ActNpath [999] }
        cevB ∷ Event t (ActiveState t ActNode a) = astB <$ domEvent Click eB
        ceB ∷ CompEvent t ActNode a = defCompEvent { _ceMe  = astB
                                                   , _ceMUp = cevB
                                                   }
    rec
        dOpen ← toggle False $ leftmost [() <$ _ceMUp ceB, () <$ eClose]
        let sf = subForest treeE
            dDivCl =
                fmap (\o →
                     if o
                         then A.setClasses [ B.dropdownMenu
                                           , B.isActive
                                           ] $ A.id_ "dropdownMenu"
                                           $ A.rMenu E.defDiv
                         else A.setClasses [B.dropdownMenu]
                                           $ A.id_ "dropdownMenu"
                                           $ A.rMenu E.defDiv
                     ) dOpen
        let eCas = () <$ leftmost (fmap _ceMUp $ concat cas)
            eClose = gate (current dOpen)
                $ leftmost [ eCas ]
        cas ∷ [[CompEvent t ActNode a]] ← E.divD dDivCl $
                E.div (A.setClasses [B.dropdownContent,B.isActive] E.defDiv) $ do
                        mapM ( mkLevelN lvlFs dmlst evB trSt pth ) sf
    -- pure $ ceA : ceB : concat cas
    pure $ concat cas

--------------------------------------------------------------------------------




