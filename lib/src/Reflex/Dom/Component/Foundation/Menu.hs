{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction, UnicodeSyntax, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.Component.Foundation.Menu
Description : Menuitems, e.g., for a navbar and other menu-like structures.
Copyright   : (c) gspia 2017 -
License     : BSD3
Maintainer  : gspia

= Menu-structures

This module contains functions that can be used to construct menus.


== Note

-}

module Reflex.Dom.Component.Foundation.Menu where

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
import           Data.List                      (inits)
import qualified Data.Map                       as Map
import           Data.Map                       (Map)
-- import           Data.Maybe
import           Data.Semigroup
-- import           Data.Set                        (Set)
-- import qualified Data.Set                        as Set
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
import           Reflex.Dom.Theme.Raw.Foundation as F
-- import           Reflex.Dom.Theme.Raw.BS4 as B

--------------------------------------------------------------------------------


mkLMenuBar ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ Tree (MenuItemConfig t a b)
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t (CompState t ActNode (MenuItem t a)))
mkLMenuBar = mkMenuBarCommon customMkLevelN

mkLMenuBarTag ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ Tree (MenuItemConfig t a b)
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            -- → m (Event t (CompState t ActNode (MenuItem t a)))
            → m (Event t a)
mkLMenuBarTag tr dLang updLang = do
    eCsAnMi ← mkLMenuBar tr dLang updLang
    ev ∷ Event t a ← compStateEv2TagEv eCsAnMi (Right tr)
    pure ev

--------------------------------------------------------------------------------


mkLRMenuBar ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ Tree (MenuItemConfig t a b)
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t (CompState t ActNode (MenuItem t a)))
mkLRMenuBar = mkMenuBarCommon customMkLevelLR1


mkLRMenuBarTag ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ Tree (MenuItemConfig t a b)
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t a)
mkLRMenuBarTag  tr dLang updLang = do
    eCsAnMi ∷ Event t (CompState t ActNode (MenuItem t a))
        ← mkLRMenuBar tr dLang updLang
    ev ∷ Event t a ← compStateEv2TagEv eCsAnMi (Right tr)
    pure ev


--------------------------------------------------------------------------------


mkListMenuBar ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ MenuItemConfig t a b
            → [MenuItemConfig t a b]
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t (CompState t ActNode (MenuItem t a)))
mkListMenuBar hd exList dLang updLang =
    mkMenuBarCommon customMkLevelN trMic dLang updLang
  where
    trMic = Node hd (fmap (\mi → Node mi [] ) exList)

mkListMenuBarTag ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ MenuItemConfig t a b
            → [MenuItemConfig t a b]
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t a)
mkListMenuBarTag hd lst dLang updLang = do
    eCsAnMi ∷ Event t (CompState t ActNode (MenuItem t a))
        ← mkListMenuBar hd lst dLang updLang
    ev ∷ Event t a ← compStateEv2TagEv eCsAnMi (Left lst)
    pure ev


--------------------------------------------------------------------------------


mkMenuBarCommon ∷ forall t m a b. ( Show a, Eq a, Ord a, Show b, Eq b, Ord b
                              , MonadWidget t m
                              , ActSretval a)
            ⇒ ( LevelNFuns t m (MenuItem t a) (MenuItem t a)
                → Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                → Event t (CompEvent t ActNode (MenuItem t a))
                → CompState t ActNode (MenuItem t a)
                → [Int]
                → Tree ( ActiveState t ActNode (MenuItem t a)
                       , Dynamic t (MenuItem t a))
                → m [CompEvent t ActNode (MenuItem t a)]
              )
            → Tree (MenuItemConfig t a b)
            → Dynamic t (LangMap a)
            → (Dynamic t (LangMap a) → MenuItem t a → MenuItem t a)
            → m (Event t (CompState t ActNode (MenuItem t a)))
mkMenuBarCommon mkL1 trMic dLang updLang = do
    evPB ← getPostBuild
    lrTree ∷ Tree (MenuItem t a) ← micTr2miTr trMic
    let ogs ∷ Map b [a] = findOneOfGroups (trMic ∷ Tree (MenuItemConfig t a b))
    let fs1Lvl = CommonADEfuns listenMe actMU drawMI elemEvF
        -- fs2Lvl = CommonADEfuns listenMe actMU drawMILeaf elemEvF
        fsNLvl = CommonADEfuns listenMe actMU drawMILeaf elemEvF
        -- rootConf = defRootNavDivFuns
        rootConf = defRootDivDivFuns
            & set rootfADEs nothingADE -- draw nothing, listen nothing
            -- & set rootfWrapAttr divNmAttrs
            & set rootfnext lvl1Fs
            -- & set rootfMkLevelNF customMkLevelLR1
            & set rootfMkLevelNF mkL1
        lvl1Fs = defLevelNUlLiFuns
            & set levelNfADEs fs1Lvl
            & set levelNfNodeAttr (liAttr lrTree)
            & set levelNfWrapAttr ulAttr
            & set levelNfnext (Just lvlNFs)
        -- lvlNFs = defLevelNDivAFuns -- Dropdowns on the main menu
        lvlNFs = defLevelNUlLiFuns -- Dropdowns on the main menu
            & set levelNfADEs fsNLvl
            & set levelNfNodeAttr (liDDAttr lrTree)
            & set levelNfWrapAttr ulAttrSM
            -- & set levelNfNodeAttr aAttr
            -- & set levelNfWrapAttr (divDDAttr lrTree)
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
        prPths ∷ [[Int]] = fmap (            snd) . Map.elems $ parsOOs
        chPths ∷ [[Int]] = fmap ( (++ [0]) . snd) . Map.elems $ parsOOs
        initPth ∷ [[Int]] = [[0,0]] -- Initial menu selection
        mAstInit = initializeAllNodesWith lrTree initPth defElmF defElmT
        dLrTree = constDyn lrTree
        -- dTreeConf = constDyn (treeConf & set treeCActivityConf (constDyn mAstInit))
        dmAstDeSelAllI = constDyn $ initializeAllNodes lrTree defElmF
        tCI ∷ TreeConf t m (MenuItem t a) (MenuItem t a)
             = treeConf & set treeCActivityConf dmAstDeSelAllI
        initState ∷ Map [Int] [Int]
            = Map.fromList $ zip prPths chPths
    rec
        -- let ePth = [[0,1]] <$ evPB
        -- let ePth = [[0,7],[0,7,1]] <$ evPB
        dOth ∷ Dynamic t [[Int]] ← holdDyn initPth eOth -- ch. DD
        -- display dOth
        dAInit ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                ← holdDyn mAstNoSels $
                    leftmost [ mAstInit <$ evPB
                             , eAstI
                             ]
        let dAInit2 ∷ Dynamic t (Maybe [ActiveState t ActNode (MenuItem t a)])
                = mAstSelP <$> dOth
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
                $ leftmost [ dAInit2 <$ eOOChPth
                           , dAInit <$ eAstI
                           , dAInit <$ evPB
                           ]
        let eTC ∷ Event t (TreeConf t m (MenuItem t a) (MenuItem t a))
                = (treeConf & set treeCActivityConf dAstI2) <$ (updated dAstI2 )
                -- = (treeConf & set treeCActivityConf dAstI2) <$ (updated dAInit )
                -- = (treeConf & set treeCActivityConf dAInit) <$ (updated dAInit )
        dTreeConf ∷ Dynamic t (TreeConf t m (MenuItem t a) (MenuItem t a))
            ← holdDyn tCI eTC
        eCsAnMi ∷ Event t (CompState t ActNode (MenuItem t a))
            ← mkTreeDD dTreeConf dLrTree2
        let eCe ∷ Event t (Event t [Int]) =
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
        dStateMap ∷ Dynamic t (Map [Int] [Int]) ←
            foldDyn
                (\n swt →
                let par = reverse . drop 1 . reverse $ n
                 in Map.alter (const $ Just n) par swt
                ) initState (fmap snd eOO)
        -- display dStateMap
        let eParII = -- traceEvent "eParII" $
                attachPromptlyDynWith
                (\mp par →
                    case Map.lookup par mp of
                        -- Just p → [[0],[0,7],[0,7,1]]
                        Just p → drop 1 $ inits p
                        Nothing → [par]
                ) dStateMap (fmap snd ePar)
            eAstI ∷ Event t (Maybe [ActiveState t ActNode (MenuItem t a)]) =
                -- traceEventWith (const "eAstI") $
                attachWith (\exTr pth →
                            initializeAllNodesWith exTr pth defElmF defElmT
                           ) (current dLrTree2) eParII
    -- let eIsAct ∷ Event t (Dynamic t Bool) =
    --         coincidence $
    --         fmap (_activeStateActive . _ceMe ) . _csCompEvent <$> eCsAnMi
    -- ddIsAct ← holdDyn (constDyn False) eIsAct
    -- let dIsAct = join ddIsAct
    pure eCsAnMi
  where
    -- not used atm: (could we start using?)
    divNmAttrs ∷ Dynamic t (ActiveState t ActNode (MenuItem t a))
         → ActNode → Dynamic t A.Attr
    divNmAttrs _dAst _an = pure $ A.attrMap
        $ A.setClasses [fouTitleBar, fouHideForMedium]
        $ A.dNmVal "responsive-toggle" "responsivemenu"
        $ A.dNmVal "hide-for" "medium" E.defDiv
    -- navAttrs ∷ Dynamic t (ActiveState t ActNode (MenuItem t a))
    --      → ActNode → Dynamic t A.Attr
    -- navAttrs _dAst _an = pure $ A.attrMap $  A.setClasses
    --     [bsNavbar, bsNavbarExpandLg, bsNavbarLight, bsBgLight] E.defNav
    liAttr ∷ Tree (MenuItem t a)
           → ActiveState t ActNode (MenuItem t a)
           → Dynamic t A.Attr
    liAttr trA ast = 
        if isLeafAtPath trA pth
            then defLiLeafNodeAttrF
              (ast & set activeStateActiveGl
                -- ( pure A.defGlobals)
                  ( A.setClasses [fouIsActive] $ pure A.defGlobals)
               & set activeStateNotActiveGl
                ( pure A.defGlobals)
                --   ( A.setClasses [] $ pure A.defGlobals)
              )
            else defLiLeafNodeAttrF
              (ast & set activeStateActiveGl
                ( A.setClasses [fouIsActive] $ pure A.defGlobals)
                -- ( A.setClasses [A.ClassName "has-submenu", fouIsActive] $ pure A.defGlobals)
                --   ( A.setClasses [fouSubmenu] $ pure A.defGlobals)
               & set activeStateNotActiveGl
                ( pure A.defGlobals)
                -- ( A.setClasses [A.ClassName "has-submenu"] $ pure A.defGlobals)
                --   ( A.setClasses [fouSubmenu] $ pure A.defGlobals)
              )
      where
        pth = getPath $ _activeStateElemId ast
    ulAttr ∷ Dynamic t (ActiveState t ActNode (MenuItem t a))
           → ActNode → Dynamic t A.Attr
    ulAttr _dAst _an =
        pure $ A.attrMap $ A.setClasses [ fouDropdown, fouVertical
                                        , fouMediumHorizontal, fouMenu
                                        , fouIcons
                                        ]
                        $ A.dNmVal "dropdown-menu" ""
                        $ E.defUl
    ulAttrSM ∷ Dynamic t (ActiveState t ActNode (MenuItem t a))
             → ActNode → Dynamic t A.Attr
    ulAttrSM _dAst _ast =
        pure $ A.attrMap $ A.setClasses [ fouSubmenu, fouMenu, fouVertical
                                        , fouIcons
                                        ] 
                        $ A.dNmVal "dropdown-menu" ""
                        $ E.defUl
    -- Next one is for drop-down menu items (that is, level 2 or further).
    -- This is for the wrapping element that collects items.
    liDDAttr ∷ Tree (MenuItem t a)
            → ActiveState t ActNode (MenuItem t a)
            -- last node where event occurred
            → Dynamic t A.Attr
    liDDAttr trA ast =
        if isLeafAtPath trA pth
            then defLiLeafNodeAttrF
              (ast & set activeStateActiveGl
                -- ( pure A.defGlobals)
                  ( A.setClasses [fouIsActive] $ pure A.defGlobals)
               & set activeStateNotActiveGl
                ( pure A.defGlobals)
                --   ( A.setClasses [] $ pure A.defGlobals)
              )
            else defLiLeafNodeAttrF
              (ast & set activeStateActiveGl
                ( A.setClasses [fouIsActive] $ pure A.defGlobals)
                -- ( A.setClasses [A.ClassName "has-submenu", fouIsActive] $ pure A.defGlobals)
                --   ( A.setClasses [fouSubmenu] $ pure A.defGlobals)
               & set activeStateNotActiveGl
                ( pure A.defGlobals)
                -- ( A.setClasses [A.ClassName "has-submenu"] $ pure A.defGlobals)
                --   ( A.setClasses [fouSubmenu] $ pure A.defGlobals)
              )
      where
        pth = getPath $ _activeStateElemId ast



--------------------------------------------------------------------------------


drawMI ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawMI dMic actS = do
    let -- dTxt = (T.pack . show . _menuItemMenuTag) <$> dMic
        dIco = _menuItemIcons <$> dMic
        dIsDD = _menuItemDD <$> dMic -- True indicates a dropdown
        -- dActive = _activeStateActive actS
        -- pth = getPath $ _activeStateElemId actS
        dLblName =
            (("dropdown" <> ) . T.pack . show . _menuItemMenuTag) <$> dMic
        dAAttrs =
            fmap (\(isDD,_lblName) →
                 if isDD
                     -- then A.setClasses [fouMenu] E.defA
                     then E.defA
                     else E.defA
                 ) $ zipDyn dIsDD dLblName
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    (e,_) ← E.aD' dAAttrs $ do
        _ ← dyn $ ffor dIco $ \ico → do
                if null ico
                   then blank
                   else E.i (A.setClasses ico E.defI) blank
        dynText dLbl
        -- dyn $ ffor dActive $ \b →
        --     if b
        --        then E.span (A.setClasses [fouIsActive] E.defSpan) $ text "(current)"
        --        else blank
    pure (e, dMic)

--------------------------------------------------------------------------------

-- We need this because drawDivContentS returns default value (defRetval) 
-- while we want to get hold of the actual MenuItem.
-- This way, we can use the user provided values to guide, what are initial
-- values in menu drop downs.
drawMILeaf ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadHold t m, ActSretval a
                      , Show a, DomBuilderSpace m ~ GhcjsDomSpace
                      )
       ⇒ Dynamic t (MenuItem t a)
       → ActiveState t ActNode (MenuItem t a)
       → m (Element EventResult (DomBuilderSpace m) t
           , Dynamic t (MenuItem t a))
drawMILeaf dMic actS = do
    let dUse = elemAttrs E.defA actS
        dIco = _menuItemIcons <$> dMic
        -- dActive = _activeStateActive actS
        ddLbl2 ∷ Dynamic t (Dynamic t Text) = _menuItemLabel <$> dMic
        dLbl = join ddLbl2
    -- (e,_) ← drawDivWdt (constDyn ) dMic actS
    -- (e,_) ← drawDivContentS dMic actS
    -- (e,_) ← drawDivActElemEx dMic actS
    (e,_) ← E.aD' dUse $ do
        _ ← dyn $ ffor dIco $ \ico → do
                if null ico
                   then blank
                   else E.i (A.setClasses ico E.defI) blank
        dynText dLbl
        -- Is there something similar for the Foundation?
        -- dyn $ ffor dActive $ \b →
        --     if b
        --        then E.span (A.setClasses [fouIsActive] E.defSpan) $ text "(current)"
        --        else blank
    pure (e, dMic)

--------------------------------------------------------------------------------

-- Note: is the current functionality of the Trees enough so
-- that there is no need for customized function? Atm the root node can
-- have its own node element and maybe it is enough.
-- TODO (check)

-- | We use custom one so that we can add the root node as in the
-- bs-examples. This is only applied to the first level of nodes, that is,
-- to those that are first visible.
customMkLevelN ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
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
customMkLevelN lvlFs dmlst evB trSt pth treeE = do
    let (_anR, dR) = rootLabel treeE
    -- (evA,_) ← E.a' (A.setClasses [bsNavbarBrand] E.defA)
    --     $ dynText ((T.pack . show) <$> dR)
    -- E.div (A.setClasses [fouTitleBar, fouHideForMedium]
    --     $ A.dNmVal "responsive-toggle" "responsivemenu"
    --     $ A.dNmVal "hide-for" "medium" E.defDiv) $ do
    --         (_evBtn,_) ← E.button' bAttrs blank
    --         E.div (A.setClasses [fouTitleBarTitle] E.defDiv)
    --             $ dynText ((T.pack . show) <$> dR)
    cas ← E.div divCl $
            E.div (A.setClasses [fouTopBarLeft] E.defDiv) $
                mkLevelN lvlFs dmlst evB trSt pth treeE
    -- let astA ∷ ActiveState t ActNode a
    --          = defActiveStateTree { _activeStateElemId = ActNpath [998] }
    --     astB ∷ ActiveState t ActNode a
    --          = defActiveStateTree { _activeStateElemId = ActNpath [999] }
    --     cevA ∷ Event t (ActiveState t ActNode a) = astA <$ domEvent Click evA
    --     cevB ∷ Event t (ActiveState t ActNode a) = astB <$ domEvent Click evBtn
    --     ceA ∷ CompEvent t ActNode a = defCompEvent { _ceMe = astA
    --                                              , _ceMUp = cevA
    --                                              }
    --     ceB ∷ CompEvent t ActNode a = defCompEvent { _ceMe = astB
    --                                              , _ceMUp = cevB
    --                                              }
    -- pure $ ceA : ceB : cas
    pure $ cas
  where
    -- bAttrs = A.setClasses [bsNavbarToggler] $ A.btSubmit
    --     $ A.dToggle "collapse" $ A.dTarget "#navbarSupportedContent"
    --     $ A.aControls "navbarSupportedContent" $ A.aExpanded "false"
    --     $ A.aLabel "Toggle navigation" E.defButton
    -- divCl = A.setClasses [bsCollapse, bsNavbarCollapse]
    --     $ A.id_ "navbarSupportedContent" E.defDiv
    bAttrs = A.setClasses [fouMenuIcon] $ A.btButton
        $ A.dToggle "responsivemenu" E.defButton
    divCl = A.setClasses [fouTopBar] $ A.id_ "responsivemenu"
      $ A.dNmVal "toggler" "" E.defDiv


--------------------------------------------------------------------------------


customMkLevelLR1 ∷ forall t m a . (Reflex t, DomBuilder t m, PostBuild t m
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
customMkLevelLR1 lvlFs dmlst evB trSt pth treeE = do
    let (_anR, dR) = rootLabel treeE
    -- (evA,_) ← E.a' (A.setClasses [bsNavbarBrand] E.defA) $
    --     dynText ((T.pack . show) <$> dR)
    E.div (A.setClasses [fouTitleBar, fouHideForMedium]
        $ A.dNmVal "responsive-toggle" "responsivemenu"
        $ A.dNmVal "hide-for" "medium" E.defDiv) $ do
            (_evBtn,_) ← E.button' bAttrs blank
            E.div (A.setClasses [fouTitleBarTitle] E.defDiv)
                $ dynText ((T.pack . show) <$> dR)
                -- text "Menu"
    let sf = subForest treeE
    cas ∷ [[CompEvent t ActNode a]] ← E.div divCl $
        case length sf of
            1 → E.div (A.setClasses [fouTopBarLeft] E.defDiv) $
                    mapM ( mkLevelN lvlFs dmlst evB trSt pth ) sf
            2 → do ce0 ← E.div (A.setClasses [fouTopBarLeft] E.defDiv) $ do
                            mkLevelN lvlFs dmlst evB trSt pth (head sf)
                   ce1 ← E.div (A.setClasses [fouTopBarRight] E.defDiv) $ do
                            mkLevelN lvlFs dmlst evB trSt pth (last sf)
                   pure [ce0,ce1]
            _ →  mapM ( mkLevelN lvlFs dmlst evB trSt pth ) sf
    -- let astA ∷ ActiveState t ActNode a
             -- = defActiveStateTree { _activeStateElemId = ActNpath [998] }
        -- astB ∷ ActiveState t ActNode a
        --      = defActiveStateTree { _activeStateElemId = ActNpath [999] }
        -- cevA ∷ Event t (ActiveState t ActNode a) = astA <$ domEvent Click evA
        -- cevB ∷ Event t (ActiveState t ActNode a) = astB <$ domEvent Click evBtn
        -- ceA ∷ CompEvent t ActNode a = defCompEvent { _ceMe = astA
        --                                          , _ceMUp = cevA
        --                                          }
        -- ceB ∷ CompEvent t ActNode a = defCompEvent { _ceMe = astB
        --                                          , _ceMUp = cevB
        --                                          }
    -- pure $ ceA : ceB : concat cas
    pure $ concat cas
  where
    bAttrs = A.setClasses [fouMenuIcon] $ A.btButton
        $ A.dToggle "responsivemenu" E.defButton
    divCl = A.setClasses [fouTopBar] $ A.id_ "responsivemenu"
      $ A.dNmVal "toggler" "" E.defDiv
      -- dNmVal "topbar" ""


--------------------------------------------------------------------------------





