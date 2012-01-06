{-# Language EmptyDataDecls, TypeFamilies, DeriveDataTypeable, ExistentialQuantification #-}
----------------------------------------------------------------------------
--
-- Module      :  Plugin.TextEditorInterface
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | The Plugin for the interface definitions for the text editor backend component
--
-----------------------------------------------------------------------------

module Text.TextEditorInterface (
    TextEditorBackend(..),
) where

import Base
import Leksah (IDEM)
import Graphics.Pane (Connection)


--
-- | The interface to a text editor backend
--

class TextEditorBackend alpha where
    data EditorBuffer alpha :: *
    data EditorView alpha :: *
    data EditorMark alpha :: *
    data EditorIter alpha :: *
    data EditorTagTable alpha :: *
    data EditorClipboard alpha :: *
    data EditorTag alpha
    data EditorDrawWindow alpha
    data EditorRectangle alpha
    data EditorScrolledWindow alpha
    data EditorTextSearchFlags alpha
    data EditorColor alpha
    data EditorUnderline alpha
    data EditorMenu alpha
    data EditorEvent alpha
    data EditorModifier alpha
    data EditorKeyVal alpha


-- Buffer

    newBuffer              :: Maybe FilePath -> String -> FilePath -> IDEM (EditorBuffer alpha)
    simpleBuffer           :: String -> IDEM (EditorBuffer alpha)
    applyTagByName         :: EditorBuffer alpha
                  -> String
                  -> EditorIter alpha
                  -> EditorIter alpha
                  -> IDEM ()
    beginNotUndoableAction :: EditorBuffer alpha -> IDEM ()
    beginUserAction        :: EditorBuffer alpha -> IDEM ()
    canRedo                :: EditorBuffer alpha -> IDEM Bool
    canUndo                :: EditorBuffer alpha -> IDEM Bool
    copyClipboard          :: EditorBuffer alpha -> EditorClipboard alpha -> IDEM ()
    createMark             :: EditorBuffer alpha
              -> EditorIter alpha
              -> Bool
              -> IDEM  (EditorMark alpha)
    cutClipboard           :: EditorBuffer alpha -> EditorClipboard alpha -> Bool -> IDEM ()
    delete                 :: EditorBuffer alpha ->  EditorIter alpha ->  EditorIter alpha -> IDEM ()
    deleteSelection        :: EditorBuffer alpha -> Bool -> Bool -> IDEM ()
    endNotUndoableAction   :: EditorBuffer alpha -> IDEM ()
    endUserAction          :: EditorBuffer alpha -> IDEM ()
    getEndIter             :: EditorBuffer alpha -> IDEM  (EditorIter alpha)
    getInsertMark          :: EditorBuffer alpha -> IDEM  (EditorMark alpha)
    getInsertIter          :: EditorBuffer alpha -> IDEM  (EditorIter alpha)
    getIterAtLine          :: EditorBuffer alpha -> Int -> IDEM  (EditorIter alpha)
    getIterAtMark          :: EditorBuffer alpha -> EditorMark alpha -> IDEM  (EditorIter alpha)
    getIterAtOffset        :: EditorBuffer alpha -> Int -> IDEM  (EditorIter alpha)
    getLineCount           :: EditorBuffer alpha -> IDEM Int
    getModified            :: EditorBuffer alpha -> IDEM Bool
    getSelectionBoundMark  :: EditorBuffer alpha -> IDEM  (EditorMark alpha)
    getSelectionBounds     :: EditorBuffer alpha -> IDEM (EditorIter alpha,  EditorIter alpha)
    getSlice               :: EditorBuffer alpha
            -> EditorIter alpha
            -> EditorIter alpha
            -> Bool
            -> IDEM String
    getStartIter           :: EditorBuffer alpha -> IDEM (EditorIter alpha)
    getTagTable            :: EditorBuffer alpha -> IDEM (EditorTagTable alpha)
    getText                :: EditorBuffer alpha
           -> EditorIter alpha
           -> EditorIter alpha
           -> Bool
           -> IDEM String
    hasSelection           :: EditorBuffer alpha -> IDEM Bool
    insert                 :: EditorBuffer alpha ->  EditorIter alpha -> String -> IDEM ()
    moveMark               :: EditorBuffer alpha ->  EditorMark alpha ->  EditorIter alpha -> IDEM ()
    newView                :: EditorBuffer alpha -> Maybe String -> Bool -> IDEM  (EditorView alpha)
    pasteClipboard         :: EditorBuffer alpha
                  -> EditorClipboard alpha
                  -> EditorIter alpha
                  -> Bool
                  -> IDEM ()
    placeCursor            :: EditorBuffer alpha ->  EditorIter alpha -> IDEM ()
    redo                   :: EditorBuffer alpha -> IDEM ()
    removeTagByName        :: EditorBuffer alpha
                   -> String
                   -> EditorIter alpha
                   -> EditorIter alpha
                   -> IDEM ()
    selectRange            :: EditorBuffer alpha ->  EditorIter alpha ->  EditorIter alpha -> IDEM ()
    setModified            :: EditorBuffer alpha -> Bool -> IDEM ()
    setStyle               :: EditorBuffer alpha -> Maybe String -> IDEM ()
    setText                :: EditorBuffer alpha -> String -> IDEM ()
    undo                   :: EditorBuffer alpha -> IDEM ()
-- * View
    bufferToWindowCoords  ::  EditorView alpha -> (Int, Int) -> IDEM (Int, Int)
    drawTabs              ::  EditorView alpha -> IDEM ()
    getBuffer             ::  EditorView alpha -> IDEM (EditorBuffer alpha)
    getDrawWindow         ::  EditorView alpha -> IDEM (EditorDrawWindow alpha)
    getIterLocation       ::  EditorView alpha ->  EditorIter alpha -> IDEM (EditorRectangle alpha)
    getOverwrite          ::  EditorView alpha -> IDEM Bool
    getScrolledWindow     ::  EditorView alpha -> IDEM (EditorScrolledWindow alpha)
    grabFocus             ::  EditorView alpha -> IDEM ()
    scrollToMark          ::  EditorView alpha
                -> EditorMark alpha
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
    scrollToIter          ::  EditorView alpha
                -> EditorIter alpha
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
    setFont               ::  EditorView alpha -> Maybe String -> IDEM ()
    setIndentWidth        ::  EditorView alpha -> Int -> IDEM ()
    setWrapMode           ::  EditorView alpha -> Bool-> IDEM ()
    setRightMargin        ::  EditorView alpha -> Maybe Int -> IDEM ()
    setShowLineNumbers    ::  EditorView alpha -> Bool -> IDEM ()
    setTabWidth           ::  EditorView alpha -> Int -> IDEM ()
-- * Iter
    backwardCharC         ::  EditorIter alpha -> IDEM (EditorIter alpha)
    backwardFindCharC     ::  EditorIter alpha
                    -> (Char -> Bool)
                    -> Maybe (EditorIter alpha)
                    -> IDEM (Maybe (EditorIter alpha))
    backwardWordStartC    ::  EditorIter alpha -> IDEM (Maybe (EditorIter alpha))
    backwardToLineStartC  ::  EditorIter alpha -> IDEM (EditorIter alpha)
    endsWord              ::  EditorIter alpha -> IDEM Bool
    forwardCharC          ::  EditorIter alpha -> IDEM (EditorIter alpha)
    forwardCharsC         ::  EditorIter alpha -> Int -> IDEM (EditorIter alpha)
    forwardFindCharC      ::  EditorIter alpha
                   -> (Char -> Bool)
                   -> Maybe (EditorIter alpha)
                   -> IDEM (Maybe (EditorIter alpha))
    forwardToLineEndC     ::  EditorIter alpha -> IDEM (EditorIter alpha)
    forwardWordEndC       ::  EditorIter alpha -> IDEM (Maybe (EditorIter alpha))
    forwardSearch         ::  EditorIter alpha
                 -> String
                 -> [EditorTextSearchFlags alpha]
                 -> Maybe  (EditorIter alpha)
                 -> IDEM (Maybe (EditorIter alpha, EditorIter alpha))
    getChar               ::  EditorIter alpha -> IDEM (Maybe Char)
    getCharsInLine        ::  EditorIter alpha -> IDEM Int
    getLine               ::  EditorIter alpha -> IDEM Int
    getLineOffset         ::  EditorIter alpha -> IDEM Int
    getOffset             ::  EditorIter alpha -> IDEM Int
    isStart               ::  EditorIter alpha -> IDEM Bool
    isEnd                 ::  EditorIter alpha -> IDEM Bool
    iterEqual             ::  EditorIter alpha ->  EditorIter alpha -> IDEM Bool
    startsLine            ::  EditorIter alpha -> IDEM Bool
    atEnd                 ::  EditorIter alpha -> IDEM (EditorIter alpha)
    atLine                ::  EditorIter alpha -> Int -> IDEM (EditorIter alpha)
    atLineOffset          ::  EditorIter alpha -> Int -> IDEM (EditorIter alpha)
    atOffset              ::  EditorIter alpha -> Int -> IDEM (EditorIter alpha)
    atStart               ::  EditorIter alpha -> IDEM (EditorIter alpha)
-- * Tags
    newTag                :: EditorTagTable alpha -> String -> IDEM (EditorTag alpha)
    lookupTag             :: EditorTagTable alpha -> String -> IDEM (Maybe (EditorTag alpha))
    background            :: EditorTag alpha -> EditorColor alpha -> IDEM ()
    underline             :: EditorTag alpha -> EditorUnderline alpha -> IDEM ()
-- * Events
    afterFocusIn          ::  EditorView alpha -> IDEM () -> IDEM [Connection]
    afterModifiedChanged  :: EditorBuffer alpha-> IDEM () -> IDEM [Connection]
    afterMoveCursor       ::  EditorView alpha -> IDEM () -> IDEM [Connection]
    afterToggleOverwrite  ::  EditorView alpha -> IDEM () -> IDEM [Connection]
    onButtonPress         ::  EditorView alpha
                 -> (EditorEvent alpha -> IDEM Bool)
                 -> IDEM [Connection]
    onButtonRelease       ::  EditorView alpha
                 -> (EditorEvent alpha -> IDEM Bool)
                 -> IDEM [Connection]
    onCompletion          ::  EditorView alpha -> IDEM () -> IDEM () -> IDEM [Connection]
    onKeyPress            ::  EditorView alpha
              -> (String -> [EditorModifier alpha] -> EditorKeyVal alpha -> IDEM Bool)
              -> IDEM [Connection]
    onKeyRelease          ::  EditorView alpha
                -> (String -> [EditorModifier alpha] -> EditorKeyVal alpha -> IDEM Bool)
                -> IDEM [Connection]
    onLookupInfo          ::  EditorView alpha -> IDEM () -> IDEM [Connection]
    onPopulatePopup       ::  EditorView alpha -> (EditorMenu alpha -> IDEM ()) -> IDEM [Connection]

