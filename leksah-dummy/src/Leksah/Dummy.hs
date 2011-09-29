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
import Graphics.Forms

import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef)
import qualified Data.Map as Map (empty)
import Data.Version (Version(..))
import qualified Text.PrettyPrint as PP (text)


-- ----------------------------------------------
-- | It's a plugin
--
pluginName = "leksah-dummy"

data DummySel = DummySel
    deriving(Typeable, Show, Ord, Eq)

instance Selector DummySel where
    type ValueType DummySel = PEvent DummyEvent

dummyPluginInterface :: StateM (PluginInterface DummyEvent)
dummyPluginInterface = do
    fe <- makeEvent DummySel
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
        deriving Show

triggerDummyEvent :: DummyEvent -> StateM (DummyEvent)
triggerDummyEvent = triggerEvent DummySel

getDummyEvent :: StateM (PEvent DummyEvent)
getDummyEvent = getEvent DummySel

-- -----------------------------------------------
-- * Initialization
--
data DummyPrefs = DummyPrefs {
    dummyArg :: String}
    deriving (Eq,Typeable)

dummyInit1 :: BaseEvent -> PEvent DummyEvent -> StateM ()
dummyInit1 baseEvent myEvent = message Debug  ("init1 " ++ pluginName) >> return ()

dummyInit2 :: BaseEvent -> PEvent DummyEvent -> StateM ()
dummyInit2 baseEvent myEvent = do
    message Debug  ("init2 " ++ pluginName)
    getFrameEvent >>= \ev -> registerEvent ev frameEventHandler >> return ()
    getFormsEvent >>= \ev -> registerEvent ev dummyEventHandler >> return ()

dummyEventHandler (RegisterPrefs prefs) = return (RegisterPrefs $ prefs ++
    [("Dummy",GenF dummyPrefsDescr defaultDummyPrefs)])
dummyEventHandler other                     = return other

dummyPrefsDescr :: FieldDescription DummyPrefs
dummyPrefsDescr =
    VertBox defaultParams [
      mkField
            (("Name", ParaString "Dummy arg") <<< defaultParams)
            stringPrinter
            stringParser
            dummyArg
            (\b a -> a{dummyArg = b})
            (stringEditor (const True) True)
            (\_i -> return ())]

defaultDummyPrefs =  DummyPrefs "Just a dummy"

frameEventHandler (RegisterActions actions) = return (RegisterActions $ actions ++ myActions)
frameEventHandler (RegisterPane paneTypes)  = return (RegisterPane $ paneTypes ++ myPaneTypes)
frameEventHandler (RegisterSessionExt ext)  = return (RegisterSessionExt $ ext ++ mySessionExt)
frameEventHandler other                     = return other

myPaneTypes :: [(String,GenPane)]
myPaneTypes =  map asRegisterType [undefined :: DummyPane]

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
openDummy = (getOrBuildDisplay (Left []) True  :: StateM (Maybe DummyPane)) >> return ()

-- ----------------------------------------------
-- * It's a pane
--

data DummyPane        =   DummyPane {
    sw              ::   VBox
} deriving Typeable

data DummyPaneState              =   DPState
    deriving(Eq,Ord,Read,Show)

instance PaneInterface DummyPane  where
    data PaneState DummyPane =  DummyPaneState
            deriving(Eq,Ord,Read,Show)
    getTopWidget    =  \ p   -> castToWidget (sw p)
    primPaneName    =  \ dp  -> "Dummy"
    paneType        =  \ _   -> "**Dummy"
    saveState       =  \ s   -> return Nothing
    recoverState    =  \ s _ -> return Nothing
    builder         =  buildDummy

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

dummyAction = do
    state <- saveSession
    message Debug state
    recoverSession state

