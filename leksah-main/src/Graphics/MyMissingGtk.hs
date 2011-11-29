{-# Language CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.MyMissingGtk
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for missing base functions
--
------------------------------------------------------------------------------

module Graphics.MyMissingGtk (
    colorHexString,
    controlIsPressed
) where

import Numeric (showHex)
import Graphics.UI.Gtk.Gdk.GC (Color(..))
import qualified Graphics.UI.Gtk.Gdk.Events as G (Event(..))
import qualified Graphics.UI.Gtk
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Gdk.EventM (Modifier(..))
#else
import Graphics.UI.Gtk.Gdk.Enums (Modifier(..))
#endif

-- This should probably be in Gtk2Hs allong with a suitable parser
colorHexString (Color r g b) = '#' : (pad $ showHex r "")
                                  ++ (pad $ showHex g "")
                                  ++ (pad $ showHex b "")
    where pad s = replicate (4 - length s) '0' ++ s


controlIsPressed :: G.Event -> Bool
controlIsPressed (G.Button _ _ _ _ _ mods _ _ _) | Control `elem` mods = True
controlIsPressed _                                                   = False
