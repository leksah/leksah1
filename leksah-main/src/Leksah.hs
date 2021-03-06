{-# Language DeriveDataTypeable, StandaloneDeriving, ScopedTypeVariables, TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Leksah
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This starts up the leksah app
--
-----------------------------------------------------------------------------

module Leksah (
    leksahPluginInterface
    , LeksahEvent(..)
    , getLeksahEvent
    , triggerLeksahEvent

-- * Renaming
    , IDEM
    , IDEAction
    , reflectIDE
) where

import Base
import Graphics.Pane
import Graphics.Forms

import Data.Version (showVersion, Version(..))
import Data.IORef (newIORef, IORef)
import Control.Monad.Reader (ReaderT(..))
import qualified Data.Map as Map (empty)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import Control.Concurrent (yield)
import Paths_leksah_main
import Control.Exception (catch, SomeException)
import System.FilePath((</>))
import Prelude hiding(catch)

type IDEM = StateM
type IDEAction = StateAction
reflectIDE = reflectState

-- ------------------------------------------------
-- * It's a plugin
--

pluginName = "leksah-main"

data LeksahEventSel = LeksahEventSel
    deriving (Eq,Ord,Show,Typeable)

instance Selector LeksahEventSel where
    type ValueType LeksahEventSel = EventChannel LeksahEvent

--
-- | Events the gui frame triggers
--
data LeksahEvent = Started
        deriving (Show, Typeable)

triggerLeksahEvent :: LeksahEvent -> StateM(LeksahEvent)
triggerLeksahEvent = triggerEvent LeksahEventSel

getLeksahEvent :: StateM (EventChannel LeksahEvent)
getLeksahEvent = getEvent LeksahEventSel

leksahPluginInterface :: StateM (PluginInterface LeksahEvent)
leksahPluginInterface = do
    ev <- makeEvent LeksahEventSel
    return $ PluginInterface {
         piInit1   = leksahInit1,
         piInit2   = leksahInit2,
         piEvent   = ev,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

data LeksahPrefs = LeksahPrefs
    -- TODO: Preferences plugin

leksahInit1 :: BaseEvent -> EventChannel LeksahEvent -> StateM ()
leksahInit1 baseEvent myEvent = message Debug ("init1 " ++ pluginName) >> return ()

leksahInit2 :: BaseEvent -> EventChannel LeksahEvent -> StateM ()
leksahInit2 baseEvent myEvent = do
    message Debug ("init2 " ++ pluginName)
    registerEvent' baseEvent (\ e -> case e of
                                        StartUp -> startupLeksah
                                        _ -> return ())
    getFrameEvent >>= \e -> registerEvent e
                            (\ s -> case s of
                                RegisterActions actions ->
                                    return $ RegisterActions $ actions ++ myActions
                                otherwise -> return s)
    return ()

myActions :: [ActionDescr]
myActions =
    [AD "Panes" "_Panes" Nothing Nothing (return ()) Nothing ActionSubmenu
        (Just $ MPBefore ["View"]) Nothing [],
     AD "Help" "_Help" Nothing Nothing (return ()) Nothing ActionSubmenu
        (Just $ MPLast [] False) Nothing [],
     AD "HelpAbout" "_About" Nothing (Just "gtk-about") (liftIO aboutDialog) Nothing ActionNormal
        (Just $ MPLast ["Help"] False) Nothing []]

-- ------------------------------------------------
-- * It's a plugin
--

startupLeksah :: StateAction
startupLeksah = do
    initPrefs
    startupFrame "Leksah main" beforeWindowOpen beforeMainGUI

beforeWindowOpen win vb nb = return ()

beforeMainGUI win vb nb = do
    postAsyncState (triggerLeksahEvent Started >> return ())


--
-- | Show the about dialog
--
aboutDialog :: IO ()
aboutDialog = do
    d <- aboutDialogNew
    aboutDialogSetName d "Leksah"
    aboutDialogSetVersion d (showVersion version)
    aboutDialogSetCopyright d "Copyright 2007-2011 Jürgen Nicklisch-Franken, Hamish Mackenzie"
    aboutDialogSetComments d $ "An integrated development environement (IDE) for the " ++
                               "programming language Haskell and the Glasgow Haskell Compiler"
    dd <- getDataDir
    license <- catch(readFile $ dd </> "LICENSE") (\ (_ :: SomeException) -> return "")
    aboutDialogSetLicense d $ Just license
    aboutDialogSetWebsite d "http://leksah.org/"
    aboutDialogSetAuthors d ["Jürgen Nicklisch-Franken","Hamish Mackenzie"]
    dialogRun d
    widgetDestroy d
    return ()

