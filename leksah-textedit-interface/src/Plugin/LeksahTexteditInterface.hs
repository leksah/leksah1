{-# Language EmptyDataDecls, TypeFamilies, DeriveDataTypeable #-}
----------------------------------------------------------------------------
--
-- Module      :  plugin.TextEditorBackend
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | The Plugin for a GTK Sourceview implementation of a text editor
--
-----------------------------------------------------------------------------

module Plugin.LeksahTexteditInterface (
    module Text.TextEditorInterface,
    module Text.BufferModeInterface,
    leksahTexteditInterfaceInterface
) where

import Text.TextEditorInterface
import Text.BufferModeInterface

import Base
import Data.Version (Version(..))
import Data.Typeable (Typeable)

-- ----------------------------------------------
-- | It's a plugin
--
pluginName = "leksah-textedit-interface"

leksahTexteditInterfaceInterface :: StateM (PluginInterface TexteditInterfaceEvent)
leksahTexteditInterfaceInterface = do
    fe <- makeEvent TexteditInterfaceSel
    return $ PluginInterface {
         piInit1   = init1,
         piInit2   = init2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

init1 :: BaseEvent -> EventChannel TexteditInterfaceEvent -> StateM ()
init1 baseEvent myEvent = message Debug  ("init1 " ++ pluginName) >> return ()

init2 :: BaseEvent -> EventChannel TexteditInterfaceEvent -> StateM ()
init2 baseEvent myEvent = message Debug  ("init2 " ++ pluginName) >> return ()

-- -----------------------------------------------
-- * No Events
--
data TexteditInterfaceSel = TexteditInterfaceSel
    deriving(Typeable, Show, Ord, Eq)

instance Selector TexteditInterfaceSel where
    type ValueType TexteditInterfaceSel = EventChannel TexteditInterfaceEvent

data TexteditInterfaceEvent

