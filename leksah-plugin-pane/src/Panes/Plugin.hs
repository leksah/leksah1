{-# Language DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, RankNTypes, TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Panes.Plugin
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Panes.Plugin where

import Base
import Graphics.Pane
import Leksah
import Graphics.Forms

import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (readIORef, writeIORef, newIORef)
import qualified Data.Map as Map (empty)
import Data.Version (showVersion, Version(..))
import qualified Text.PrettyPrint as PP (text)
import System.FilePath ((<.>), (</>), dropFileName)
import Data.List (nubBy)
import Text.Parsec (parse)
import Control.Monad (when, liftM)


-- -----------------------------------------------
-- * Events the gui frame triggers
--

pluginName = "leksah-plugin-pane"

data LeksahPluginPaneSel = LeksahPluginPaneSel
    deriving (Eq,Ord,Show,Typeable)

instance Selector LeksahPluginPaneSel where
    type ValueType LeksahPluginPaneSel = EventChannel PluginPaneEvent

data PluginPaneEvent = PluginConfigChanged | PluginDescrChanged
        deriving (Eq, Show)

triggerPluginPane :: PluginPaneEvent -> StateM (PluginPaneEvent)
triggerPluginPane = triggerEvent LeksahPluginPaneSel

getPluginPaneEvent :: StateM (EventChannel PluginPaneEvent)
getPluginPaneEvent = getEvent LeksahPluginPaneSel


-- ----------------------------------------------
-- * It's a pane
--

data PluginPane = PluginPane {
    ppTop :: VBox,
    ppInj :: Injector Plugin,
    ppExt :: Extractor Plugin
} deriving (Typeable)


instance PaneInterface PluginPane  where
    data PaneState PluginPane =  PPState (Maybe Plugin)
            deriving(Read,Show)

    getTopWidget    =  \ p   -> castToWidget (ppTop p)
    primPaneName    =  \ dp  -> "Plugin"
    paneType        =  \ _   -> "**Plugin"
    saveState       =  \ p   -> do
        mbVal <- ppExt p
        return $ Just (PPState mbVal)
    recoverState    =  \ pp ps -> do
        nb      <-  getNotebook pp
        mbP     <-  buildPane pp nb builder
        case mbP of
            Nothing -> return Nothing
            Just p  -> case ps of
                        PPState Nothing  -> return $ Just p
                        PPState (Just v) -> (ppInj p) v >>= \ _ -> return $ Just p
    builder         =  buildPluginPane

instance Pane PluginPane

openPluginPane :: Prerequisite -> StateM ()
openPluginPane (name,bounds) = do
    currentConfigPath <- liftM dropFileName getCurrentConfigPath
    res               <- liftIO $ loadPluginDescription currentConfigPath (name,bounds)
    choices           <- liftIO $ getPrereqChoices (dropFileName currentConfigPath)
    let choices' = filter (\ (n,_) -> n /= name) choices
    case res of
        Right errorStr  ->  message Error ("Can't find plugin: " ++ errorStr)
        Left plugin     -> do
            pane :: Maybe PluginPane <- getOrBuildDisplay (Left []) True
            case pane of
                Nothing -> return ()
                Just p -> do
                    (ppInj p) plugin{plChoices = choices'}
            return ()

openPluginPane' :: Plugin -> StateM ()
openPluginPane' plugin = do
    currentConfigPath <- liftM dropFileName getCurrentConfigPath
    choices           <- liftIO $ getPrereqChoices (dropFileName currentConfigPath)
    let choices' = filter (\ (n,_) -> n /= (plName plugin)) choices
    pane :: Maybe PluginPane <- getOrBuildDisplay (Left []) True
    case pane of
        Nothing -> return ()
        Just p -> do
            (ppInj p) plugin{plChoices = choices'}
    return ()



-- ----------------------------------------------
-- * The form
--

pluginDescr ::  FieldDescriptionG Plugin
pluginDescr = VertBoxG defaultParams [
    HoriBoxG defaultParams [
        mkFieldG
            "Name of the plugin"
            defaultParams
            plName
            (\ b a -> a{plName = b})
            (stringEditor (const True) True)
    ,   mkFieldG
            "Version"
            defaultParams
            plVersion
            (\ b a -> a{plVersion = b})
            versionEditor],
    HoriBoxG defaultParams [
        mkFieldG
            "Modul"
            defaultParams
            plModule
            (\ b a -> a{plModule = b})
            (stringEditor (const True) True)
    ,   mkFieldG
            "Interface"
            defaultParams
            plInterface
            (\ b a -> a{plInterface = b})
            (stringEditor (const True) True)],
    HoriBoxG (("VPack", ParaPack PackGrow) <<< defaultParams) [
        mkFieldG
            "Prerequisites"
            (("Direction", ParaDir Vertical) <<<
                ("MinSize", ParaSize (-1,125)) <<< defaultParams)
            (\ plugin -> (plPrerequisites plugin,plChoices plugin))
            (\ b a -> a{plPrerequisites = fst b, plChoices = snd b})
            (prerequisitesEditor Nothing)
    ,   mkFieldG
            "Synopsis"
            (("Synopsis", ParaString "or call it comment") <<< defaultParams)
            plSynopsis
            (\ b a -> a{plSynopsis = b})
            multilineStringEditor]]

prerequisitesEditor mbDeleteHandler =
    selectionEditor
        (ColumnDescr True
            [("Plugin",\ (pluginName',(_,_)) -> [cellText := pluginName'],Nothing),
            ("Lower",\ (_,(lower,_)) -> [cellText := showMbVersion lower],
                Just (\ row@(pn,(lower,upper)) str ->
                    case parse boundParser "" str of
                        Left _  ->  row
                        Right v ->  (pn,(v,upper)))),
            ("Upper",\ (_,(_,upper)) -> [cellText := showMbVersion upper],
                Just (\ old@(pn,(lower,upper)) str ->
                    case parse boundParser "" str of
                        Left _  -> old
                        Right v ->  (pn,(lower,v))))])
        (Just (\ (pluginName1,_) (pluginName2,_) -> compare pluginName1 pluginName2))
        (Just (\ (pluginName1,_) (pluginName2,_) -> pluginName1 == pluginName2))
        mbDeleteHandler
  where
    showMbVersion Nothing        = "any"
    showMbVersion (Just version) = showVersion version

-- ----------------------------------------------
-- * Building the forms pane in standard form
--

buildPluginPane :: PanePath -> Notebook -> Window -> StateM (Maybe PluginPane, Connections)
buildPluginPane = \ pp nb w -> makeValue >>= \ initial ->
                    (buildFormsPane pluginDescr initial formPaneDescr) pp nb w

  where

    makeValue = do
        currentConfigPath   <- getCurrentConfigPath
        choices             <- liftIO $ getPrereqChoices (dropFileName currentConfigPath)
        return defaultPlugin{plChoices = choices}

    formPaneDescr :: FormPaneDescr Plugin PluginPane = FormPaneDescr {
        fpGetPane      = \ top inj ext -> PluginPane top inj ext,
        fpSaveAction   = \ v ->  do
                                    currentConfigPath <- getCurrentConfigPath
                                    liftIO $ writePluginDescr (dropFileName currentConfigPath
                                                        </> getPluginName v <.> ".lkshp") v
                                    triggerPluginPane PluginDescrChanged
                                    return (),
        fpHasChanged   = \ v1 v2 -> v1{plChoices = []} == v2{plChoices = []},
        fpGuiHandlers  = [],
        fpExtraButtons = []}




