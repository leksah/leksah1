{-# Language DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Panes.PluginConfig
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | An editor for a plugin config
--
-----------------------------------------------------------------------------

module Panes.PluginConfig where

import Base
import Graphics.Pane
import Graphics.Forms
import Leksah
import Panes.Plugin

import Graphics.UI.Gtk hiding (eventClick)
import Data.Typeable (Typeable(..), cast, Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (writeIORef, readIORef, newIORef)
import qualified Data.Map as Map (empty)
import Data.Version (showVersion, Version(..))
import qualified Text.PrettyPrint as PP (text)
import System.FilePath ((<.>), (</>), dropFileName)
import Data.List (intersperse, nubBy)
import Text.Parsec (parse)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Control.Monad (liftM, when)
import System.Directory (removeFile)
import Control.Concurrent (threadDelay)

-- ----------------------------------------------
-- | It's a plugin
--

pluginPanePluginInterface :: StateM (PluginInterface PluginPaneEvent)
pluginPanePluginInterface = do
    fe <- makeEvent LeksahPluginPaneSel
    return $ PluginInterface {
         piInit1   = init1,
         piInit2   = init2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

-- -----------------------------------------------
-- * Initialization
--

init1 :: BaseEvent -> PEvent PluginPaneEvent -> StateM ()
init1 baseEvent myEvent = message Debug ("init1 " ++ pluginName) >> return ()

init2 :: BaseEvent -> PEvent PluginPaneEvent -> StateM ()
init2 baseEvent myEvent = do
    message Debug ("init2 " ++ pluginName)
    registerFrameEvent handler >> return ()
  where handler (RegisterActions actions) = return $ RegisterActions $ actions ++ myActions
        handler (RegisterPane paneTypes)  = return $ RegisterPane $ paneTypes ++ myPaneTypes
        handler e                         = return e

myActions :: [ActionDescr]
myActions =
    [AD "PluginConfig" "PluginConfig" Nothing Nothing openPluginConfigPane Nothing ActionNormal
        (Just $ MPLast ["Panes"] False) Nothing []]

myPaneTypes :: [(String,GenPane)]
myPaneTypes =
    [asRegisterType (undefined :: PluginConfigPane),
     asRegisterType (undefined :: PluginPane)]

openPluginConfigPane :: StateM ()
openPluginConfigPane = do
    message Debug "Open plugin config pane"
    mbPane :: Maybe PluginConfigPane <- getOrBuildDisplay (Left []) True
    case mbPane of
        Nothing -> return ()
        Just p -> registerRefresh p >> return ()
    return ()

registerRefresh pane = getPluginPaneEvent >>= (\e -> registerEvent' e handler)
  where
    handler PluginDescrChanged =  do
        mbV <-  pcpExt pane
        case mbV of
            Nothing -> return ()
            Just v -> do
                currentConfigPath   <- getCurrentConfigPath
                prerequisites       <- liftIO $ getPrereqChoices
                                            currentConfigPath
                let pluginConfig'   =  v{cfChoices = prerequisites
                                                     ++ cfPlugins v}
                (pcpInj pane) pluginConfig'
    handler otherwise          =  return ()

-- ----------------------------------------------
-- * It's a pane
--

data PluginConfigPane = PluginConfigPane {
    pcpTopW             ::   VBox,
    pcpInj              :: Injector PluginConfig,
    pcpExt              :: Extractor PluginConfig
} deriving Typeable


instance PaneInterface PluginConfigPane where
    data PaneState PluginConfigPane =  PCPaneState
            deriving(Read,Show)

    getTopWidget    =  \ p   -> castToWidget (pcpTopW p)
    primPaneName    =  \ dp  -> "PluginConfig"
    paneType        =  \ _   -> "**PluginConfig"
    saveState       =  \ s   -> return $ Just (PCPaneState)
    recoverState    =  \ pp ps -> do
        nb      <-  getNotebook pp
        mbP     <-  buildPane pp nb builder
        return mbP
    builder         =  buildPluginConfigPane

instance Pane PluginConfigPane

-- ----------------------------------------------
-- * It's contest is a form
--

pluginConfDescr ::  FieldDescriptionG PluginConfig
pluginConfDescr = VertBoxG  defaultParams [
    HoriBoxG  defaultParams [
            mkFieldG
                "Name of the config"
                defaultParams
                cfName
                (\ b a -> a{cfName = b})
                (stringEditor (const True) True)
        ,   mkFieldG
                "Version"
                defaultParams
                cfVersion
                (\ b a -> a{cfVersion = b})
                versionEditor],
    HoriBoxG  (("VPack", ParaPack PackGrow) <<< defaultParams) [
            mkFieldG
                "Plugin list"
                (("Direction", ParaDir Vertical) <<<
                    ("MinSize", ParaSize (-1,125)) <<< defaultParams)
                (\ plugin -> (cfPlugins plugin,cfChoices plugin))
                (\ b a -> a{cfPlugins = fst b, cfChoices = snd b})
                (prerequisitesEditor (Just deleteHandler))
        ,   mkFieldG
                "Synopsis"
                (("Synopsis", ParaString ("or call it comment or use it" ++
                    "\n for whatever you like")) <<< defaultParams)
                cfSynopsis
                (\ b a -> a{cfSynopsis = b})
                multilineStringEditor]]

deleteHandler prereqList     = do
    currentConfigPath   <- getCurrentConfigPath
    window              <- getMainWindow
    md                  <- liftIO $ messageDialogNew (Just window) []
                                        MessageQuestion
                                        ButtonsYesNo
                                        ("Delete the following plugin configuration file(s): "
                                            ++  (concat (intersperse ", " (map fst prereqList))))
    liftIO $ set md [ windowWindowPosition := WinPosCenterOnParent ]
    resp                <- liftIO $ dialogRun md
    liftIO $ widgetDestroy md
    case resp of
        ResponseYes ->  mapM_ (deleteHandler' (dropFileName currentConfigPath)) prereqList
        _           ->  return ()

deleteHandler' currentConfigPath (name,bounds) = do
    res               <- liftIO $ loadPluginDescription currentConfigPath (name,bounds)
    case res of
        Right errorStr  ->  message Error ("Can't find plugin: " ++ errorStr)
        Left plugin     ->  reifyState (\ stateR -> catch
            (do
                removeFile $ currentConfigPath </> getPluginName plugin <.> "lkshp"
                reflectState (triggerPluginPane
                                PluginDescrChanged) stateR
                return ())
            (\e -> reflectState (message Error (show e)) stateR))

-- ----------------------------------------------
-- * Building the pane in standard form
--
buildPluginConfigPane :: PanePath -> Notebook -> Window
                            -> StateM (Maybe PluginConfigPane, Connections)
buildPluginConfigPane = \ pp nb w -> do
    initialValue <-  makeValue
    (buildFormsPane pluginConfDescr initialValue formPaneDescr) pp nb w
  where
    makeValue = do
        currentConfigPath   <- getCurrentConfigPath
        pluginConfig        <- liftIO $ loadPluginConfig currentConfigPath
        prerequisites       <- liftIO $ getPrereqChoices currentConfigPath
        return $ pluginConfig{cfChoices = prerequisites ++ cfPlugins pluginConfig}
    formPaneDescr :: FormPaneDescr PluginConfig PluginConfigPane =
        FormPaneDescr {
            fpGetPane      = \ top inj ext -> PluginConfigPane top inj ext,
            fpSaveAction   = \ v -> do
                currentConfigPath <- getCurrentConfigPath
                liftIO $ writePluginConfig currentConfigPath v
                triggerPluginPane PluginConfigChanged
                return (),
            fpHasChanged   = \ v1 v2 -> v1{cfChoices = []} == v2{cfChoices = []},
            fpGuiHandlers  = handlers,
            fpExtraButtons = [newButton]}
    handlers = [([Selection],(\ event ->
                case geMbSelection event of
                    Nothing -> return event
                    Just (GenSelection sel) -> case cast sel of
                                                    Nothing -> do
                                                        message Error ("cPluginConfig>>buildPluginConfigPane:" ++
                                                         " cast problem")
                                                        return event
                                                    Just s -> do
                                                        openPluginPane s
                                                        return event))]
    newButton = ("gtk-new",openPluginPane' defaultPlugin)
