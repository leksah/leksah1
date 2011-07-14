{-# Language DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Leksah.Dummy
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

module Leksah.Dummy where

import Base
import Leksah
import Graphics.Pane

import Graphics.UI.Gtk
import Data.Typeable (cast, Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef)
import qualified Data.Map as Map (empty)
import Data.Version (Version(..))
import Debug.Trace (trace)

-- ----------------------------------------------
-- | It's a plugin
--
pluginName = "leksah-dummy"

data DummySelectors = DummyEvent
    deriving(Typeable, Show, Ord, Eq)

instance Selector DummySelectors

dummyPluginInterface :: StateM (PluginInterface DummyEvent)
dummyPluginInterface = do
    fe <- makeEvent DummyEvent
    return $ PluginInterface {
         piInit1   = dummyInit1,
         piInit2   = dummyInit2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

-- -----------------------------------------------
-- * Events the gui frame triggers
--

data DummyEvent = HelloWorld
        deriving (Show, Typeable)

triggerDummyEvent :: DummyEvent -> StateM (DummyEvent)
triggerDummyEvent = triggerEvent DummyEvent

getDummyEvent :: StateM (PEvent DummyEvent)
getDummyEvent = getEvent DummyEvent

-- -----------------------------------------------
-- * Initialization
--

dummyInit1 :: BaseEvent -> PEvent DummyEvent -> StateM ()
dummyInit1 baseEvent myEvent = trace ("init1 " ++ pluginName) $ return ()

dummyInit2 :: BaseEvent -> PEvent DummyEvent -> StateM ()
dummyInit2 baseEvent myEvent = trace ("init2 " ++ pluginName) $ do
    getFrameEvent >>= \ev -> registerEvent ev
        (\ e -> case e of
                    RegisterActions actions ->
                        return $ RegisterActions $ actions ++ myActions
                    RegisterPane paneTypes ->
                        return $ RegisterPane $ paneTypes ++ myPaneTypes
                    RegisterSessionExt ext ->
                        return $ RegisterSessionExt $ ext ++ mySessionExt
                    otherwise -> return e)
    return ()

myPaneTypes :: [(String,GenPane)]
myPaneTypes =
    [(paneType (undefined :: DummyPane), PaneC (undefined :: DummyPane))]


myActions :: [ActionDescr]
myActions =
    [AD "Dummy" "Dummy" Nothing Nothing openDummy Nothing ActionNormal
        (Just $ MPLast ["Panes"] False) Nothing []]

mySessionExt :: [GenSessionExtension]
mySessionExt = [GenS (SessionExtension "dummy" (return 5)
                    (\ i -> liftIO $ putStrLn ("recovery " ++ show (i + 1)))),
                GenS (SessionExtension "dummy2" (return 5.2)
                    (\ i -> liftIO $ putStrLn ("recovery2 " ++ show (i + 0.1))))]

openDummy :: StateM ()
openDummy = do
    pane :: Maybe DummyPane <- getAndDisplayPane (Left []) True
    return ()

-- ----------------------------------------------
-- * It's a pane
--

data DummyPane        =   DummyPane {
    sw              ::   VBox
} deriving Typeable

data DummyPaneState              =   DPState
    deriving(Eq,Ord,Read,Show,Typeable)

instance PaneInterface DummyPane  where
    data PaneState DummyPane =  DummyPaneState
            deriving(Eq,Ord,Read,Show)
    getTopWidget    =  \ p   -> castToWidget (sw p)
    primPaneName    =  \ dp  -> "Dummy"
    paneType        =  \ _   -> "**Dummy"
    saveState       =  \ s   -> return Nothing
    recoverState    =  \ s _ -> return Nothing
    builder         = buildDummy

instance Pane DummyPane

buildDummy panePath notebook window = do
    reifyState $ \ stateR -> do
        ibox        <- vBoxNew False 0
        button      <- buttonNew
        buttonSetLabel button "Press here"
        boxPackStartDefaults ibox button
        let info = DummyPane{ sw = ibox }
        button `onButtonPress` \ event -> reflectState (dummyAction >> return True) stateR
        return (Just info,[])

dummyAction = trace "dummy action" $ do
    state <- saveSession
    trace ("###State: " ++ state) recoverSession state
