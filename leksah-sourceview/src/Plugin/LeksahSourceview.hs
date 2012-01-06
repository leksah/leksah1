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

module Plugin.LeksahSourceview (
    module Text.TextEditor,
    sourceviewPluginInterface
) where

import Text.TextEditor
import Base
import Data.Version (Version(..))
import Data.Typeable (Typeable)

-- ----------------------------------------------
-- | It's a plugin
--
pluginName = "leksah-sourceview"

sourceviewPluginInterface :: StateM (PluginInterface SourceviewEvent)
sourceviewPluginInterface = do
    fe <- makeEvent SourceviewSel
    return $ PluginInterface {
         piInit1   = init1,
         piInit2   = init2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

init1 :: BaseEvent -> EventChannel SourceviewEvent -> StateM ()
init1 baseEvent myEvent = message Debug  ("init1 " ++ pluginName) >> return ()

init2 :: BaseEvent -> EventChannel SourceviewEvent -> StateM ()
init2 baseEvent myEvent = message Debug  ("init2 " ++ pluginName) >> return ()

-- -----------------------------------------------
-- * No Events
--
data SourceviewSel = SourceviewSel
    deriving(Typeable, Show, Ord, Eq)

instance Selector SourceviewSel where
    type ValueType SourceviewSel = EventChannel SourceviewEvent

data SourceviewEvent

