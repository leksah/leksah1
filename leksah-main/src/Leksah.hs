{-# Language DeriveDataTypeable, StandaloneDeriving #-}
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
    , registerLeksahEvent
) where

import Base.Event
import Base.PluginTypes
import Base.State
import Graphics.Frame
import Graphics.Panes

import Data.Version (Version(..))
import Data.IORef (newIORef, IORef)
import Control.Monad.Reader (ReaderT(..))
import qualified Data.Map as Map (empty)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO(unsafePerformIO)
import Debug.Trace (trace)
import Graphics.UI.Gtk
import Data.Typeable (cast, Typeable)
import Control.Concurrent (yield)

-- ------------------------------------------------
-- * It's a plugin
--

pluginName = "leksah-main"

--
-- | Events the gui frame triggers
--
data LeksahEvent = Started
        deriving (Show, Typeable)

triggerLeksahEvent :: LeksahEvent -> StateM(LeksahEvent)
triggerLeksahEvent = triggerEvent pluginName

getLeksahEvent :: StateM (PEvent LeksahEvent)
getLeksahEvent = getEvent pluginName

registerLeksahEvent :: Handler LeksahEvent -> StateM HandlerID
registerLeksahEvent handler = getLeksahEvent >>= \e -> registerEvent e handler

leksahPluginInterface :: StateM (PluginInterface LeksahEvent)
leksahPluginInterface = do
    ev <- makeEvent pluginName
    return $ PluginInterface {
         piInit1   = leksahInit1,
         piInit2   = leksahInit2,
         piEvent   = ev,
         piName    = "billeksah-main",
         piVersion = Version [1,0,0][]}

data LeksahPrefs = LeksahPrefs
    -- TODO: Preferences plugin

leksahInit1 :: BaseEvent -> PEvent LeksahEvent -> StateM ()
leksahInit1 baseEvent myEvent = trace ("init1 " ++ pluginName) $ do
    uiManager <- liftIO $ do
        unsafeInitGUIForThreadedRTS
        timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- maybe switch back to
        uiManagerNew
    registerState (pluginName ++ "." ++ "uiManager") uiManager
    return ()

leksahInit2 :: BaseEvent -> PEvent LeksahEvent -> StateM ()
leksahInit2 baseEvent myEvent = trace ("init2 " ++ pluginName) $ do
    registerEvent baseEvent (\ baseEvent ->
        case baseEvent of
            StartUp       -> startupLeksah >> return baseEvent
            otherwise     -> return baseEvent)
    return ()

-- ------------------------------------------------
-- * It's a plugin
--

startupLeksah :: StateAction
startupLeksah = startupFrame "Leksah main" openDummy

openDummy = postAsyncState (triggerLeksahEvent Started >> return ())



