{-# Language DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, RankNTypes #-}
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
import Data.Typeable (cast, Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (readIORef, writeIORef, newIORef)
import qualified Data.Map as Map (empty)
import Data.Version (showVersion, Version(..))
import Debug.Trace (trace)
import qualified Text.PrettyPrint as PP (text)
import System.FilePath ((<.>), (</>), dropFileName)
import Data.List (nubBy)
import Text.Parsec (parse)
import Control.Monad (when, liftM)


-- -----------------------------------------------
-- * Events the gui frame triggers
--

pluginName = "leksah-plugin-pane"

data LeksahPluginPaneSelector = LeksahPluginPaneEventSel
    deriving (Eq,Ord,Show,Typeable)

instance Selector LeksahPluginPaneSelector


data PluginPaneEvent = PluginConfigChanged | PluginDescrChanged
        deriving (Eq, Show, Typeable)

triggerPluginPane :: PluginPaneEvent -> StateM (PluginPaneEvent)
triggerPluginPane = triggerEvent LeksahPluginPaneEventSel

getPluginPaneEvent :: StateM (PEvent PluginPaneEvent)
getPluginPaneEvent = getEvent LeksahPluginPaneEventSel

registerPluginPaneEvent :: Handler PluginPaneEvent -> StateM HandlerID
registerPluginPaneEvent handler = getPluginPaneEvent >>= \e -> registerEvent e handler

-- ----------------------------------------------
-- * It's a pane
--

data PluginPane = PluginPane {
    ppTop :: VBox,
    ppInj :: Injector Plugin,
    ppExt :: Extractor Plugin
} deriving Typeable

data PluginPaneState              =   PluginPaneState
    deriving(Eq,Ord,Read,Show,Typeable)

instance PaneInterface PluginPane PluginPaneState where
    getTopWidget    =  \ p   -> castToWidget (ppTop p)
    primPaneName    =  \ dp  -> "Plugin"
    paneId          =  \ _   -> "**Plugin"
    saveState       =  \ s   -> return Nothing
    recoverState    =  \ s _ -> return Nothing
    builder         =  buildPluginPane

instance Pane PluginPane PluginPaneState

openPluginPane :: Prerequisite -> StateM ()
openPluginPane (name,bounds) = do
    currentConfigPath <- liftM dropFileName getCurrentConfigPath
    res               <- liftIO $ loadPluginDescription currentConfigPath (name,bounds)
    choices           <- liftIO $ getPrereqChoices (dropFileName currentConfigPath)
    let choices' = filter (\ (n,_) -> n /= name) choices
    case res of
        Right errorStr  ->  liftIO $ putStrLn ("Can't find plugin: " ++ errorStr)
        Left plugin     -> do
            pane :: Maybe PluginPane <- getAndDisplayPane (Left []) True
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
    pane :: Maybe PluginPane <- getAndDisplayPane (Left []) True
    case pane of
        Nothing -> return ()
        Just p -> do
            (ppInj p) plugin{plChoices = choices'}
    return ()



-- ----------------------------------------------
-- * The form
--

pluginDescr ::  FieldDescription Plugin
pluginDescr = VFD emptyParams [
    HFD (paraName <<<- ParaName "Name and Version" $ emptyParams)  [
        mkField
            (paraPack <<<- (ParaPack PackGrow) $
                paraName <<<- ParaName "Name of the plugin" $ emptyParams)
            plName
            (\ b a -> a{plName = b})
            (stringEditor (const True) True)
    ,   mkField
            (paraPack <<<- (ParaPack PackGrow) $
                paraName <<<- ParaName "Version" $ emptyParams)
            plVersion
            (\ b a -> a{plVersion = b})
            versionEditor],
    HFD emptyParams [
       mkField
            (paraPack <<<- (ParaPack PackGrow) $
                paraName <<<- ParaName "Module" $ emptyParams)
            plModule
            (\ b a -> a{plModule = b})
            (stringEditor (const True) True)
    ,   mkField
            (paraPack <<<- (ParaPack PackGrow) $
                paraName <<<- ParaName "Interface" $ emptyParams)
            plInterface
            (\ b a -> a{plInterface = b})
            (stringEditor (const True) True)]
    ,   mkField
            (paraName <<<- ParaName "Prerequisites"
                    $ paraSynopsis <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,125)
                            $ emptyParams)
            (\ plugin -> (plPrerequisites plugin,plChoices plugin))
            (\ b a -> a{plPrerequisites = fst b, plChoices = snd b})
            (prerequisitesEditor Nothing)
    ,   mkField
             (paraName <<<- ParaName "Synopsis"
                $ paraSynopsis <<<- ParaSynopsis "or call it comment"
                    $ emptyParams)
            plSynopsis
            (\ b a -> a{plSynopsis = b})
            multilineStringEditor]

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

    formPaneDescr :: FormPaneDescr Plugin PluginPane PluginPaneState = FormPaneDescr {
        fpGetPane     = \ top inj ext -> PluginPane top inj ext,
        fpSaveAction  = \ v ->  do
                                    currentConfigPath <- getCurrentConfigPath
                                    liftIO $ writePluginDescr (dropFileName currentConfigPath
                                                        </> getPluginName v <.> ".lkshp") v
                                    triggerPluginPane PluginDescrChanged
                                    return (),
        fpEqual = \ v1 v2 -> v1{plChoices = []} == v2{plChoices = []},
        fpGuiHandlers = [],
        fpExtraButtons = []}




