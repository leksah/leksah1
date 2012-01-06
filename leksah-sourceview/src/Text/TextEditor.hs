{-# Language TypeSynonymInstances, TypeFamilies, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
--
-- Module      :  Text.TextEditor
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | The GTK Sourcview implementation of a text editor
--
-----------------------------------------------------------------------------

module Text.TextEditor (
    GtkSourceView(..)
) where

import Leksah
import Graphics.Panes
import Graphics.MyMissingGtk
import Text.TextEditorInterface

import Prelude hiding(getChar, getLine)
import Data.Char (isAlphaNum, isSymbol)
import Data.Maybe (fromJust)
import Control.Monad (when)
import Control.Monad.Reader (liftIO, ask)
import Control.Applicative ((<$>))

import qualified Graphics.UI.Gtk as Gtk hiding(afterToggleOverwrite)
import qualified Graphics.UI.Gtk.SourceView as Gtk
import qualified Graphics.UI.Gtk.Multiline.TextView as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GtkOld
import System.Glib.Attributes (AttrOp(..))
import System.Glib.Signals (ConnectId(..))
import Data.Typeable
-- import System.GIO.File.ContentType (contentTypeGuess)

import System.FilePath ((</>))
import Graphics.Pane (Connection)

data GtkSourceView = GtkSourceView
    deriving Typeable

-- Data types
instance TextEditorBackend GtkSourceView where
    data EditorBuffer GtkSourceView          = GtkEditorBuffer Gtk.SourceBuffer
    data EditorView GtkSourceView            = GtkEditorView Gtk.SourceView
    data EditorMark GtkSourceView            = GtkEditorMark Gtk.TextMark
    data EditorIter GtkSourceView            = GtkEditorIter Gtk.TextIter
    data EditorTagTable GtkSourceView        = GtkEditorTagTable Gtk.TextTagTable
    data EditorTag GtkSourceView             = GtkEditorTag Gtk.TextTag
    data EditorClipboard GtkSourceView       = GtkEditorClipboard Gtk.Clipboard
    data EditorDrawWindow GtkSourceView      = GtkEditorDrawWindow Gtk.DrawWindow
    data EditorRectangle GtkSourceView       = GtkEditorRectangle Gtk.Rectangle
    data EditorScrolledWindow GtkSourceView  = GtkEditorScrolledWindow Gtk.ScrolledWindow
    data EditorTextSearchFlags GtkSourceView = GtkEditorTextSearchFlags Gtk.TextSearchFlags
    data EditorColor GtkSourceView           = GtkEditorColor Gtk.Color
    data EditorUnderline GtkSourceView       = GtkEditorUnderline Gtk.Underline
    data EditorEvent GtkSourceView           = GtkEditorEvent GtkOld.Event
    data EditorModifier GtkSourceView        = GtkEditorModifier GtkOld.Modifier
    data EditorKeyVal GtkSourceView          = GtkEditorKeyVal Gtk.KeyVal
    data EditorMenu GtkSourceView            = GtkEditorMenu Gtk.Menu

    newBuffer              = newBuffer'
    simpleBuffer           = simpleBuffer'
    applyTagByName         = applyTagByName'
    beginNotUndoableAction = beginNotUndoableAction'
    beginUserAction        = beginUserAction'
    canRedo                = canRedo'
    canUndo                = canUndo'
    copyClipboard          = copyClipboard'
    createMark             = createMark'
    cutClipboard           = cutClipboard'
    delete                 = delete'
    deleteSelection        = deleteSelection'
    endNotUndoableAction   = endNotUndoableAction'
    endUserAction          = endUserAction'
    getEndIter             = getEndIter'
    getInsertMark          = getInsertMark'
    getInsertIter          = getInsertIter'
    getIterAtLine          = getIterAtLine'
    getIterAtMark          = getIterAtMark'
    getIterAtOffset        = getIterAtOffset'
    getLineCount           = getLineCount'
    getModified            = getModified'
    getSelectionBoundMark  = getSelectionBoundMark'
    getSelectionBounds     = getSelectionBounds'
    getSlice               = getSlice'
    getStartIter           = getStartIter'
    getTagTable            = getTagTable'
    getText                = getText'
    hasSelection           = hasSelection'
    insert                 = insert'
    moveMark               = moveMark'
    newView                = newView'
    pasteClipboard         = pasteClipboard'
    placeCursor            = placeCursor'
    redo                   = redo'
    removeTagByName        = removeTagByName'
    selectRange            = selectRange'
    setModified            = setModified'
    setStyle               = setStyle'
    setText                = setText'
    undo                   = undo'

-- View

    bufferToWindowCoords  = bufferToWindowCoords'
    drawTabs              = drawTabs'
    getBuffer             = getBuffer'
    getDrawWindow         = getDrawWindow'
    getIterLocation       = getIterLocation'
    getOverwrite          = getOverwrite'
    getScrolledWindow     = getScrolledWindow'
    grabFocus             = grabFocus'
    scrollToMark          = scrollToMark'
    scrollToIter          = scrollToIter'
    setFont               = setFont'
    setIndentWidth        = setIndentWidth'
    setWrapMode           = setWrapMode'
    setRightMargin        = setRightMargin'
    setShowLineNumbers    = setShowLineNumbers'
    setTabWidth           = setTabWidth'
-- Iterator
    backwardCharC         = backwardCharC'
    backwardFindCharC     = backwardFindCharC'
    backwardWordStartC    = backwardWordStartC'
    backwardToLineStartC  = backwardToLineStartC'
    endsWord              = endsWord'
    forwardCharC          = forwardCharC'
    forwardCharsC         = forwardCharsC'
    forwardFindCharC      = forwardFindCharC'
    forwardToLineEndC     = forwardToLineEndC'
    forwardWordEndC       = forwardWordEndC'
    forwardSearch         = forwardSearch'
    getChar               = getChar'
    getCharsInLine        = getCharsInLine'
    getLine               = getLine'
    getLineOffset         = getLineOffset'
    getOffset             = getOffset'
    isStart               = isStart'
    isEnd                 = isEnd'
    iterEqual             = iterEqual'
    startsLine            = startsLine'
    atEnd                 = atEnd'
    atLine                = atLine'
    atLineOffset          = atLineOffset'
    atOffset              = atOffset'
    atStart               = atStart'
-- Tags
    newTag                = newTag'
    lookupTag             = lookupTag'
    background            = background'
    underline             = underline'
-- Events
    afterFocusIn          = afterFocusIn'
    afterModifiedChanged  = afterModifiedChanged'
    afterMoveCursor       = afterMoveCursor'
    afterToggleOverwrite  = afterToggleOverwrite'
    onButtonPress         = onButtonPress'
    onButtonRelease       = onButtonRelease'
    onCompletion          = onCompletion'
    onKeyPress            = onKeyPress'
    onKeyRelease          = onKeyRelease'
    onLookupInfo          = onLookupInfo'
    onPopulatePopup       = onPopulatePopup'

-- Buffer
newBuffer' :: Maybe FilePath -> String -> FilePath -> IDEM (EditorBuffer GtkSourceView)
newBuffer' mbFilename contents dataDir = liftIO $ do
    lm      <- Gtk.sourceLanguageManagerNew
    oldPath <- Gtk.sourceLanguageManagerGetSearchPath lm
    Gtk.sourceLanguageManagerSetSearchPath lm (Just $ (dataDir </> "language-specs") : oldPath)
    mbLang  <- case mbFilename of
                Just filename -> Gtk.sourceLanguageManagerGuessLanguage lm (Just filename) Nothing
                Nothing       -> Gtk.sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
    buffer <- case mbLang of
        Just sLang -> Gtk.sourceBufferNewWithLanguage sLang
        Nothing -> Gtk.sourceBufferNew Nothing
    Gtk.sourceBufferSetMaxUndoLevels buffer (-1)
    Gtk.sourceBufferBeginNotUndoableAction buffer
    Gtk.textBufferSetText buffer contents
    Gtk.sourceBufferEndNotUndoableAction buffer
    return $ GtkEditorBuffer buffer

-- Buffer
simpleBuffer' :: String -> IDEM (EditorBuffer GtkSourceView)
simpleBuffer' contents = liftIO $ do
    buffer <- Gtk.sourceBufferNew Nothing
    Gtk.textBufferSetText buffer contents
    return $ GtkEditorBuffer buffer


applyTagByName' :: (EditorBuffer GtkSourceView)
                  -> String
                  -> (EditorIter GtkSourceView)
                  -> (EditorIter GtkSourceView)
                  -> IDEM ()
applyTagByName' (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferApplyTagByName sb name first last

beginNotUndoableAction' :: EditorBuffer GtkSourceView -> IDEM ()
beginNotUndoableAction' (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferBeginNotUndoableAction sb

beginUserAction' :: EditorBuffer GtkSourceView -> IDEM ()
beginUserAction' (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferBeginUserAction sb

canRedo' :: EditorBuffer GtkSourceView -> IDEM Bool
canRedo' (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferGetCanRedo sb

canUndo' :: EditorBuffer GtkSourceView -> IDEM Bool
canUndo' (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferGetCanUndo sb

copyClipboard' :: EditorBuffer GtkSourceView -> EditorClipboard GtkSourceView -> IDEM ()
copyClipboard' (GtkEditorBuffer sb) (GtkEditorClipboard clipboard)
                            = liftIO $ Gtk.textBufferCopyClipboard sb clipboard

createMark' :: EditorBuffer GtkSourceView
              -> EditorIter GtkSourceView
              -> Bool
              -> IDEM (EditorMark GtkSourceView)
createMark' (GtkEditorBuffer sb) (GtkEditorIter i) leftGravity = liftIO $
    GtkEditorMark <$> Gtk.textBufferCreateMark sb Nothing i leftGravity

cutClipboard' :: EditorBuffer GtkSourceView -> EditorClipboard GtkSourceView -> Bool -> IDEM ()
cutClipboard' (GtkEditorBuffer sb) (GtkEditorClipboard clipboard) defaultEditable = liftIO $ Gtk.textBufferCutClipboard sb clipboard defaultEditable

delete' :: EditorBuffer GtkSourceView -> EditorIter GtkSourceView -> EditorIter GtkSourceView -> IDEM ()
delete' (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferDelete sb first last

deleteSelection' :: EditorBuffer GtkSourceView -> Bool -> Bool -> IDEM ()
deleteSelection' (GtkEditorBuffer sb) interactive defaultEditable = liftIO $
    Gtk.textBufferDeleteSelection sb interactive defaultEditable >> return ()

endNotUndoableAction' :: EditorBuffer GtkSourceView -> IDEM ()
endNotUndoableAction' (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferEndNotUndoableAction sb

endUserAction' :: EditorBuffer GtkSourceView -> IDEM ()
endUserAction' (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferEndUserAction sb

getEndIter' :: EditorBuffer GtkSourceView -> IDEM (EditorIter GtkSourceView)
getEndIter' (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorIter $ Gtk.textBufferGetEndIter sb

getInsertMark' :: EditorBuffer GtkSourceView -> IDEM (EditorMark GtkSourceView)
getInsertMark' (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorMark $ Gtk.textBufferGetInsert sb

getIterAtLine' :: EditorBuffer GtkSourceView -> Int -> IDEM (EditorIter GtkSourceView)
getIterAtLine' (GtkEditorBuffer sb) line = liftIO $ fmap GtkEditorIter $ Gtk.textBufferGetIterAtLine sb line

getIterAtMark' :: EditorBuffer GtkSourceView -> EditorMark GtkSourceView -> IDEM (EditorIter GtkSourceView)
getIterAtMark' (GtkEditorBuffer sb) (GtkEditorMark m) = liftIO $ fmap GtkEditorIter $  Gtk.textBufferGetIterAtMark sb m

getIterAtOffset' :: EditorBuffer GtkSourceView -> Int -> IDEM (EditorIter GtkSourceView)
getIterAtOffset' (GtkEditorBuffer sb) offset = liftIO $ fmap GtkEditorIter $  Gtk.textBufferGetIterAtOffset sb offset

getLineCount' :: EditorBuffer GtkSourceView -> IDEM Int
getLineCount' (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferGetLineCount sb

getModified' :: EditorBuffer GtkSourceView -> IDEM Bool
getModified' (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferGetModified sb

getSelectionBoundMark' :: EditorBuffer GtkSourceView -> IDEM (EditorMark GtkSourceView)
getSelectionBoundMark' (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorMark $ Gtk.textBufferGetSelectionBound sb

getSelectionBounds' :: EditorBuffer GtkSourceView -> IDEM (EditorIter GtkSourceView, EditorIter GtkSourceView)
getSelectionBounds' (GtkEditorBuffer sb) = liftIO $ do
    (first, last) <- Gtk.textBufferGetSelectionBounds sb
    return (GtkEditorIter first, GtkEditorIter last)

getInsertIter' :: EditorBuffer GtkSourceView -> IDEM (EditorIter GtkSourceView)
getInsertIter' (GtkEditorBuffer sb) = liftIO $ do
    insertMark <- Gtk.textBufferGetInsert sb
    insertIter <- Gtk.textBufferGetIterAtMark sb insertMark
    return (GtkEditorIter insertIter)

getSlice' :: EditorBuffer GtkSourceView
            -> EditorIter GtkSourceView
            -> EditorIter GtkSourceView
            -> Bool
            -> IDEM String
getSlice' (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars = liftIO $
    Gtk.textBufferGetSlice sb first last includeHidenChars

getStartIter' :: EditorBuffer GtkSourceView -> IDEM (EditorIter GtkSourceView)
getStartIter' (GtkEditorBuffer sb) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetStartIter sb

getTagTable' :: EditorBuffer GtkSourceView -> IDEM (EditorTagTable GtkSourceView)
getTagTable' (GtkEditorBuffer sb) = liftIO $ GtkEditorTagTable <$> Gtk.textBufferGetTagTable sb

getText' :: EditorBuffer GtkSourceView
           -> EditorIter GtkSourceView
           -> EditorIter GtkSourceView
           -> Bool
           -> IDEM String
getText' (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars = liftIO $
    Gtk.textBufferGetText sb first last includeHidenChars

hasSelection' :: EditorBuffer GtkSourceView -> IDEM Bool
hasSelection' (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferHasSelection sb

insert' :: EditorBuffer GtkSourceView -> EditorIter GtkSourceView -> String -> IDEM ()
insert' (GtkEditorBuffer sb) (GtkEditorIter i) text = liftIO $ Gtk.textBufferInsert sb i text

moveMark' :: EditorBuffer GtkSourceView -> EditorMark GtkSourceView -> EditorIter GtkSourceView -> IDEM ()
moveMark' (GtkEditorBuffer sb) (GtkEditorMark m) (GtkEditorIter i)
    = liftIO $ Gtk.textBufferMoveMark sb m i

newView' :: EditorBuffer GtkSourceView -> Maybe String -> Bool -> IDEM (EditorView GtkSourceView)
newView' (GtkEditorBuffer sb) mbFontString wrapLines = do
    fd <- fontDescription mbFontString
    liftIO $ do
        sv <- Gtk.sourceViewNewWithBuffer sb
        Gtk.sourceViewSetHighlightCurrentLine sv True
        Gtk.sourceViewSetInsertSpacesInsteadOfTabs sv True
        Gtk.sourceViewSetIndentOnTab sv True
        Gtk.sourceViewSetAutoIndent sv True
        Gtk.sourceViewSetSmartHomeEnd sv Gtk.SourceSmartHomeEndBefore
        if wrapLines
            then Gtk.textViewSetWrapMode sv Gtk.WrapWord
            else Gtk.textViewSetWrapMode sv Gtk.WrapNone
        sw <- Gtk.scrolledWindowNew Nothing Nothing
        Gtk.containerAdd sw sv
        Gtk.widgetModifyFont sv (Just fd)
        return (GtkEditorView sv)

pasteClipboard' :: EditorBuffer GtkSourceView
                  -> EditorClipboard GtkSourceView
                  -> EditorIter GtkSourceView
                  -> Bool
                  -> IDEM ()
pasteClipboard' (GtkEditorBuffer sb) (GtkEditorClipboard clipboard) (GtkEditorIter i) defaultEditable = liftIO $
    Gtk.textBufferPasteClipboard sb clipboard i defaultEditable

placeCursor' :: EditorBuffer GtkSourceView -> EditorIter GtkSourceView -> IDEM ()
placeCursor' (GtkEditorBuffer sb) (GtkEditorIter i) = liftIO $ Gtk.textBufferPlaceCursor sb i

redo' :: EditorBuffer GtkSourceView -> IDEM ()
redo' (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferRedo sb

removeTagByName' :: EditorBuffer GtkSourceView
                   -> String
                   -> EditorIter GtkSourceView
                   -> EditorIter GtkSourceView
                   -> IDEM ()
removeTagByName' (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferRemoveTagByName sb name first last

selectRange' :: EditorBuffer GtkSourceView -> EditorIter GtkSourceView -> EditorIter GtkSourceView -> IDEM ()
selectRange' (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferSelectRange sb first last

setModified' :: EditorBuffer GtkSourceView -> Bool -> IDEM ()
setModified' (GtkEditorBuffer sb) modified = liftIO $ Gtk.textBufferSetModified sb modified >> return ()

setStyle' :: EditorBuffer GtkSourceView -> Maybe String -> IDEM ()
setStyle' (GtkEditorBuffer sb) mbStyle = liftIO $ do
    case mbStyle of
        Nothing  -> return ()
        Just str -> do
            styleManager <- Gtk.sourceStyleSchemeManagerNew
            ids <- Gtk.sourceStyleSchemeManagerGetSchemeIds styleManager
            when (elem str ids) $ do
                scheme <- Gtk.sourceStyleSchemeManagerGetScheme styleManager str
#if MIN_VERSION_gtksourceview2(0,12,0)
                Gtk.sourceBufferSetStyleScheme sb (Just scheme)
#else
                Gtk.sourceBufferSetStyleScheme sb scheme
#endif


setText' :: EditorBuffer GtkSourceView -> String -> IDEM ()
setText' (GtkEditorBuffer sb) text = liftIO $ Gtk.textBufferSetText sb text

undo' :: EditorBuffer GtkSourceView -> IDEM ()
undo' (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferUndo sb

-- View
bufferToWindowCoords' :: EditorView GtkSourceView -> (Int, Int) -> IDEM (Int, Int)
bufferToWindowCoords' (GtkEditorView sv) point = liftIO $ Gtk.textViewBufferToWindowCoords sv Gtk.TextWindowWidget point

drawTabs' :: EditorView GtkSourceView -> IDEM ()
drawTabs' (GtkEditorView sv) = liftIO $ Gtk.sourceViewSetDrawSpaces sv Gtk.SourceDrawSpacesTab

getBuffer' :: EditorView GtkSourceView -> (IDEM (EditorBuffer GtkSourceView))
getBuffer' (GtkEditorView sv) = liftIO $ fmap (GtkEditorBuffer . Gtk.castToSourceBuffer) $ sv `Gtk.get` Gtk.textViewBuffer

getDrawWindow' :: EditorView GtkSourceView -> IDEM (EditorDrawWindow GtkSourceView)
getDrawWindow' (GtkEditorView sv) = liftIO $ GtkEditorDrawWindow <$> Gtk.widgetGetDrawWindow sv

getIterLocation' :: EditorView GtkSourceView -> EditorIter GtkSourceView -> IDEM (EditorRectangle GtkSourceView)
getIterLocation' (GtkEditorView sv) (GtkEditorIter i) = liftIO $ GtkEditorRectangle <$> Gtk.textViewGetIterLocation sv i

getOverwrite' :: EditorView GtkSourceView -> IDEM Bool
getOverwrite' (GtkEditorView sv) = liftIO $ Gtk.textViewGetOverwrite sv

getScrolledWindow' :: EditorView GtkSourceView -> IDEM (EditorScrolledWindow GtkSourceView)
getScrolledWindow' (GtkEditorView sv) = liftIO
    $ fmap (GtkEditorScrolledWindow . Gtk.castToScrolledWindow . fromJust) $ Gtk.widgetGetParent sv

grabFocus' :: EditorView GtkSourceView -> IDEM ()
grabFocus' (GtkEditorView sv) = liftIO $ Gtk.widgetGrabFocus sv

scrollToMark' :: EditorView GtkSourceView
                -> EditorMark GtkSourceView
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
scrollToMark' (GtkEditorView sv) (GtkEditorMark m) withMargin mbAlign = liftIO $ Gtk.textViewScrollToMark sv m withMargin mbAlign

scrollToIter' :: EditorView GtkSourceView
                -> EditorIter GtkSourceView
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
scrollToIter' (GtkEditorView sv) (GtkEditorIter i) withMargin mbAlign = liftIO $ Gtk.textViewScrollToIter sv i withMargin mbAlign >> return ()

fontDescription :: Maybe String -> IDEM Gtk.FontDescription
fontDescription mbFontString = liftIO $ do
    case mbFontString of
        Just str -> do
            Gtk.fontDescriptionFromString str
        Nothing -> do
            f <- Gtk.fontDescriptionNew
            Gtk.fontDescriptionSetFamily f "Monospace"
            return f

setFont' :: EditorView GtkSourceView -> Maybe String -> IDEM ()
setFont' (GtkEditorView sv) mbFontString = do
    fd <- fontDescription mbFontString
    liftIO $ Gtk.widgetModifyFont sv (Just fd)

setIndentWidth' :: EditorView GtkSourceView -> Int -> IDEM ()
setIndentWidth' (GtkEditorView sv) width = liftIO $ Gtk.sourceViewSetIndentWidth sv width

setWrapMode' :: EditorView GtkSourceView -> Bool-> IDEM ()
setWrapMode' (GtkEditorView sv) wrapLines = do
    GtkEditorScrolledWindow sw <- getScrolledWindow'  (GtkEditorView sv)
    if wrapLines
        then liftIO $ do
            Gtk.textViewSetWrapMode sv Gtk.WrapWord
            Gtk.scrolledWindowSetPolicy sw Gtk.PolicyNever Gtk.PolicyAutomatic
        else liftIO $ do
            Gtk.textViewSetWrapMode sv Gtk.WrapNone
            Gtk.scrolledWindowSetPolicy sw Gtk.PolicyAutomatic Gtk.PolicyAutomatic

setRightMargin' :: EditorView GtkSourceView -> Maybe Int -> IDEM ()
setRightMargin' (GtkEditorView sv) mbRightMargin = liftIO $ do
    case mbRightMargin of
        Just n -> do
            Gtk.sourceViewSetShowRightMargin sv True
            Gtk.sourceViewSetRightMarginPosition sv (fromIntegral n)
        Nothing -> Gtk.sourceViewSetShowRightMargin sv False

setShowLineNumbers' :: EditorView GtkSourceView -> Bool -> IDEM ()
setShowLineNumbers' (GtkEditorView sv) show = liftIO $ Gtk.sourceViewSetShowLineNumbers sv show

setTabWidth' :: EditorView GtkSourceView -> Int -> IDEM ()
setTabWidth' (GtkEditorView sv) width = liftIO $ Gtk.sourceViewSetTabWidth sv width

-- Iterator
transformGtkIter :: Gtk.TextIter -> (Gtk.TextIter -> IO a) -> IDEM (EditorIter GtkSourceView)
transformGtkIter i f = do
    new <- liftIO $ Gtk.textIterCopy i
    liftIO $ f new
    return $ GtkEditorIter new

transformGtkIterMaybe :: Gtk.TextIter -> (Gtk.TextIter -> IO Bool) ->
    IDEM (Maybe (EditorIter GtkSourceView))
transformGtkIterMaybe i f = do
    new <- liftIO $ Gtk.textIterCopy i
    found <- liftIO $ f new
    return $ if found
        then Just $ GtkEditorIter new
        else Nothing

backwardCharC' :: EditorIter GtkSourceView -> IDEM (EditorIter GtkSourceView)
backwardCharC' (GtkEditorIter i) = transformGtkIter i Gtk.textIterBackwardChar

backwardFindCharC' :: EditorIter GtkSourceView
                    -> (Char -> Bool)
                    -> Maybe (EditorIter GtkSourceView)
                    -> IDEM (Maybe (EditorIter GtkSourceView))
backwardFindCharC' (GtkEditorIter i) pred mbLimit = transformGtkIterMaybe i $ \x ->
    Gtk.textIterBackwardFindChar x pred $
        case mbLimit of
            Just (GtkEditorIter limit) -> Just limit
            Nothing                    -> Nothing

backwardWordStartC' :: EditorIter GtkSourceView -> IDEM (Maybe (EditorIter GtkSourceView))
backwardWordStartC' (GtkEditorIter i) = transformGtkIterMaybe i Gtk.textIterBackwardWordStart

backwardToLineStartC' :: EditorIter GtkSourceView -> IDEM (EditorIter GtkSourceView)
backwardToLineStartC' (GtkEditorIter i) = transformGtkIter i $ \new -> do
    n <- Gtk.textIterGetLineOffset new
    Gtk.textIterBackwardChars new n
    return ()

endsWord' :: EditorIter GtkSourceView -> IDEM Bool
endsWord' (GtkEditorIter i) = liftIO $ Gtk.textIterEndsWord i

forwardCharC' :: EditorIter GtkSourceView -> IDEM (EditorIter GtkSourceView)
forwardCharC' (GtkEditorIter i) = transformGtkIter i Gtk.textIterForwardChar

forwardCharsC' :: EditorIter GtkSourceView -> Int -> IDEM (EditorIter GtkSourceView)
forwardCharsC' (GtkEditorIter i) n = transformGtkIter i $ flip Gtk.textIterForwardChars n

forwardFindCharC' :: EditorIter GtkSourceView
                   -> (Char -> Bool)
                   -> Maybe (EditorIter GtkSourceView)
                   -> IDEM (Maybe (EditorIter GtkSourceView))
forwardFindCharC' (GtkEditorIter i) pred mbLimit = transformGtkIterMaybe i $ \x ->
    Gtk.textIterForwardFindChar x pred $
        case mbLimit of
            Just (GtkEditorIter limit) -> Just limit
            Nothing                    -> Nothing

forwardSearch' :: EditorIter GtkSourceView
                 -> String
                 -> [EditorTextSearchFlags GtkSourceView]
                 -> Maybe (EditorIter GtkSourceView)
                 -> IDEM (Maybe (EditorIter GtkSourceView, EditorIter GtkSourceView))
forwardSearch' (GtkEditorIter i) str flags mbLimit = liftIO $ do
    fmap (fmap (\(start, end) -> (GtkEditorIter start, GtkEditorIter end))) $
        Gtk.textIterForwardSearch i str (map (\ (GtkEditorTextSearchFlags f) -> f) flags) $
            case mbLimit of
                Just (GtkEditorIter limit) -> Just limit
                Nothing                    -> Nothing

forwardToLineEndC' :: EditorIter GtkSourceView -> IDEM (EditorIter GtkSourceView)
forwardToLineEndC' (GtkEditorIter i) = transformGtkIter i Gtk.textIterForwardToLineEnd

forwardWordEndC' :: EditorIter GtkSourceView -> IDEM (Maybe (EditorIter GtkSourceView))
forwardWordEndC' (GtkEditorIter i) = transformGtkIterMaybe i Gtk.textIterForwardWordEnd

getChar' :: EditorIter GtkSourceView -> IDEM (Maybe Char)
getChar' (GtkEditorIter i) = liftIO $ Gtk.textIterGetChar i

getCharsInLine' :: EditorIter GtkSourceView -> IDEM Int
getCharsInLine' (GtkEditorIter i) = liftIO $ Gtk.textIterGetCharsInLine i

getLine' :: EditorIter GtkSourceView -> IDEM Int
getLine' (GtkEditorIter i) = liftIO $ Gtk.textIterGetLine i

getLineOffset' :: EditorIter GtkSourceView -> IDEM Int
getLineOffset' (GtkEditorIter i) = liftIO $ Gtk.textIterGetLineOffset i

getOffset' :: EditorIter GtkSourceView -> IDEM Int
getOffset' (GtkEditorIter i) = liftIO $ Gtk.textIterGetOffset i

isStart' :: EditorIter GtkSourceView -> IDEM Bool
isStart' (GtkEditorIter i) = liftIO $ Gtk.textIterIsStart i

isEnd' :: EditorIter GtkSourceView -> IDEM Bool
isEnd' (GtkEditorIter i) = liftIO $ Gtk.textIterIsEnd i

iterEqual' :: EditorIter GtkSourceView -> EditorIter GtkSourceView -> IDEM Bool
iterEqual' (GtkEditorIter i1) (GtkEditorIter i2) = liftIO $ Gtk.textIterEqual i1 i2

startsLine' :: EditorIter GtkSourceView -> IDEM Bool
startsLine' (GtkEditorIter i) = liftIO $ Gtk.textIterStartsLine i

atEnd' :: EditorIter GtkSourceView -> IDEM (EditorIter GtkSourceView)
atEnd' (GtkEditorIter i) = liftIO $ GtkEditorIter <$> do
    buffer <- Gtk.textIterGetBuffer i
    Gtk.textBufferGetEndIter buffer

atLine' :: EditorIter GtkSourceView -> Int -> IDEM (EditorIter GtkSourceView)
atLine' (GtkEditorIter i) line = transformGtkIter i $ flip Gtk.textIterSetLine line

atLineOffset' :: EditorIter GtkSourceView -> Int -> IDEM (EditorIter GtkSourceView)
atLineOffset' (GtkEditorIter i) column = transformGtkIter i $ flip Gtk.textIterSetLineOffset column

atOffset' :: EditorIter GtkSourceView -> Int -> IDEM (EditorIter GtkSourceView)
atOffset' (GtkEditorIter i) offset = transformGtkIter i $ flip Gtk.textIterSetOffset offset

atStart' :: EditorIter GtkSourceView -> IDEM (EditorIter GtkSourceView)
atStart' (GtkEditorIter i) = liftIO $ GtkEditorIter <$> do
    buffer <- Gtk.textIterGetBuffer i
    Gtk.textBufferGetEndIter buffer

-- Tag Table
newTag' :: EditorTagTable GtkSourceView -> String -> IDEM (EditorTag GtkSourceView)
newTag' (GtkEditorTagTable tt) name = liftIO $ do
    t <- Gtk.textTagNew (Just name)
    Gtk.textTagTableAdd tt t
    return $ GtkEditorTag t

lookupTag' :: EditorTagTable GtkSourceView -> String -> IDEM (Maybe (EditorTag GtkSourceView))
lookupTag' (GtkEditorTagTable tt) name = liftIO $ fmap (fmap GtkEditorTag) $ Gtk.textTagTableLookup tt name

-- Tag
background' :: EditorTag GtkSourceView -> EditorColor GtkSourceView -> IDEM ()
background' (GtkEditorTag t) (GtkEditorColor color) = liftIO $ Gtk.set t [Gtk.textTagBackground := colorHexString color]

underline' :: EditorTag GtkSourceView -> EditorUnderline GtkSourceView -> IDEM ()
underline' (GtkEditorTag t) (GtkEditorUnderline value) = liftIO $ Gtk.set t [Gtk.textTagUnderline := value]

-- Events
afterFocusIn' :: EditorView GtkSourceView -> IDEM () -> IDEM [Connection]
afterFocusIn' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.afterFocusIn` \_ -> reflectIDE f ideR >> return False
        return [castCID id1]

afterModifiedChanged' :: EditorBuffer GtkSourceView -> IDEM () -> IDEM [Connection]
afterModifiedChanged' (GtkEditorBuffer sb) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sb `Gtk.afterModifiedChanged` reflectIDE f ideR
        return [castCID id1]

afterMoveCursor' :: EditorView GtkSourceView -> IDEM () -> IDEM [Connection]
afterMoveCursor' (GtkEditorView sv) f = do
    ideR <- ask
    GtkEditorBuffer sb <- getBuffer' (GtkEditorView sv)
    liftIO $ do
#if MIN_VERSION_gtk(0,10,5)
        id1 <- sv `Gtk.after` Gtk.moveCursor $ \_ _ _ -> reflectIDE f ideR
#else
        id1 <- sv `Gtk.afterMoveCursor` \_ _ _ -> reflectIDE f ideR
#endif
        sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
        id2 <- sv `Gtk.onButtonRelease` \_ -> reflectIDE f ideR >> return False
        id3 <- sb `Gtk.afterEndUserAction` reflectIDE f ideR
        return [castCID id1, castCID id2, castCID id3]

afterToggleOverwrite' :: EditorView GtkSourceView -> IDEM () -> IDEM [Connection]
afterToggleOverwrite' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
#if MIN_VERSION_gtk(0,10,5)
        id1 <- sv `Gtk.after` Gtk.toggleOverwrite $ reflectIDE f ideR
#else
        id1 <- sv `Gtk.afterToggleOverwrite` reflectIDE f ideR
#endif
        return [castCID id1]

onButtonPress' :: EditorView GtkSourceView
                 -> (EditorEvent GtkSourceView -> IDEM Bool)
                 -> IDEM [Connection]
onButtonPress' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.onButtonPress` \event -> reflectIDE (f (GtkEditorEvent event)) ideR
        return [castCID id1]

onButtonRelease' :: EditorView GtkSourceView
                 -> (EditorEvent GtkSourceView -> IDEM Bool)
                 -> IDEM [Connection]
onButtonRelease' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.onButtonRelease` \event
            -> reflectIDE (f (GtkEditorEvent event)) ideR
        return [castCID id1]

onCompletion' :: EditorView GtkSourceView -> IDEM () -> IDEM () -> IDEM [Connection]
onCompletion' (GtkEditorView sv) start cancel = do
    ideR <- ask
    GtkEditorBuffer sb <- getBuffer' (GtkEditorView sv)
    liftIO $ do
        id1 <- sb `Gtk.afterBufferInsertText` \iter text -> do
            let isIdent a = isAlphaNum a || a == '\'' || a == '_' || a == '.'
            let isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                         || a == '!'  || a == '@' || a == '%' || a == '&' || a == '?'
            if (all isIdent text) || (all isOp text)
                then do
                    hasSel <- Gtk.textBufferHasSelection sb
                    if not hasSel
                        then do
                            (iterC, _) <- Gtk.textBufferGetSelectionBounds sb
                            atC <- Gtk.textIterEqual iter iterC
                            when atC $ reflectIDE start ideR
                        else
                            reflectIDE cancel ideR
                else
                    reflectIDE cancel ideR
#if MIN_VERSION_gtk(0,10,5)
        id2 <- sv `Gtk.on` Gtk.moveCursor $ \_ _ _ -> reflectIDE cancel ideR
#else
        id2 <- sv `Gtk.onMoveCursor` \_ _ _ -> reflectIDE cancel ideR
#endif
        id3 <- sv `Gtk.onButtonPress` \_ -> reflectIDE cancel ideR >> return False
        id4 <- sv `Gtk.onFocusOut` \_ -> reflectIDE cancel ideR >> return False
        return [castCID id1, castCID id2, castCID id3, castCID id4]

onKeyPress' :: EditorView GtkSourceView
              -> (String -> [EditorModifier GtkSourceView] -> EditorKeyVal GtkSourceView -> IDEM Bool)
              -> IDEM [Connection]
onKeyPress' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.on` Gtk.keyPressEvent $ do
            name        <- Gtk.eventKeyName
            modifier    <- Gtk.eventModifier
            keyVal      <- Gtk.eventKeyVal
            liftIO $ reflectIDE (f name (map GtkEditorModifier modifier)
                        (GtkEditorKeyVal keyVal)) ideR
        return [castCID id1]

onKeyRelease' :: EditorView GtkSourceView
                -> (String -> [EditorModifier GtkSourceView] -> EditorKeyVal GtkSourceView -> IDEM Bool)
                -> IDEM [Connection]
onKeyRelease' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.on` Gtk.keyReleaseEvent $ do
            name        <- Gtk.eventKeyName
            modifier    <- Gtk.eventModifier
            keyVal      <- Gtk.eventKeyVal
            liftIO $ reflectIDE (f name (map GtkEditorModifier modifier)  (GtkEditorKeyVal keyVal)) ideR
        return [castCID id1]

onLookupInfo' :: EditorView GtkSourceView -> IDEM () -> IDEM [Connection]
onLookupInfo' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
        id1 <- sv `Gtk.onButtonRelease` \e -> when (controlIsPressed e) (reflectIDE f ideR) >> return False
        return [castCID id1]

onPopulatePopup' :: EditorView GtkSourceView -> (EditorMenu GtkSourceView -> IDEM ()) -> IDEM [Connection]
onPopulatePopup' (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
#if MIN_VERSION_gtk(0,10,5)
        id1 <- sv `Gtk.on` Gtk.populatePopup $ \menu -> reflectIDE (f (GtkEditorMenu menu)) ideR
#else
        id1 <- sv `Gtk.onPopulatePopup` \menu -> reflectIDE (f (GtkEditorMenu menu)) ideR
#endif
        return [castCID id1]

