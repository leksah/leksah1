{-# LANGUAGE EmptyDataDecls, TypeFamilies, DeriveDataTypeable, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Plugin.TextEditor
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Plugin.TextEditor  where

import Graphics.Pane.TextEditor

import Base
import Graphics.Forms
import Graphics.Frame
import Graphics.Pane

import Data.Version (Version(..))
import Data.Typeable (Typeable)
import qualified Graphics.UI.Gtk as Gtk
import qualified Text.PrettyPrint as PP (text)
import Text.TextEditorInterface
       (background, lookupTag, getTagTable, setStyle, getBuffer,
        setWrapMode, setIndentWidth, setRightMargin, setFont,
        setShowLineNumbers, fromGtkColor, TextEditorBackend)
import Graphics.UI.Gtk.General.Enums (ShadowType(..))
import Control.Monad (forM_)
import Text.ParserCombinators.Parsec (CharParser)
import Text.Parsec (string)
import qualified Text.Parsec.Token as P
       (integer, makeTokenParser, TokenParser)
import Text.TextEditor (GtkSourceView(..))


-- ----------------------------------------------
-- | It's a plugin
--
pluginName = "leksah-texteditor"

leksahTexteditorInterface = do
    fe <- makeEvent TexteditorSel
    return $ PluginInterface {
         piInit1   = init1,
         piInit2   = init2,
         piEvent   = fe,
         piName    = pluginName,
         piVersion = Version [1,0,0][]}

init1 :: BaseEvent -> EventChannel TexteditorEvent -> StateM ()
init1 baseEvent myEvent = message Debug  ("init1 " ++ pluginName) >> return ()

init2 :: BaseEvent -> EventChannel TexteditorEvent -> StateM ()
init2 baseEvent myEvent = do
    message Debug  ("init2 " ++ pluginName)
    getFormsEvent >>= \ev -> registerEvent ev formsEventHandler >> return ()
    getFrameEvent >>= \ev -> registerEvent ev frameEventHandler >> return ()

-- | Handle the registerPrefs event
-- TODO: make backend configurable
-- How do we know about it at this early point???
formsEventHandler (RegisterPrefs prefs) = return (RegisterPrefs $ prefs ++
    [("TextEditor",GenF (textEditorPrefsDescr GtkSourceView) defaultTextEditorPrefs)])
formsEventHandler other                     = return other

frameEventHandler (RegisterActions actions) = return (RegisterActions $ actions ++ myActions)
frameEventHandler (RegisterPane paneTypes)  = return (RegisterPane $ paneTypes ++ myPaneTypes GtkSourceView)

myPaneTypes :: forall alpha . TextEditorBackend alpha => alpha -> [(String,GenPane)]
myPaneTypes _ =  map asRegisterType [undefined :: IDEBuffer alpha]

-- -----------------------------------------------
-- * Menu and Toolbar entries for Texteditor
--

myActions :: [ActionDescr]
myActions =
    [AD "File" "_File" Nothing Nothing (return ()) [] False
    ,AD "FileNewTextFile" "_New Text File" Nothing Nothing
        fileNew [] False
    ,AD "FileOpen" "_Open..." Nothing (Just "gtk-open")
        fileOpen [] False
    ,AD "RecentFiles" "Open _Recent" Nothing Nothing (return ()) [] False
    ,AD "FileRevert" "_Revert" Nothing Nothing
        fileRevert [] False
    ,AD "FileSave" "_Save" Nothing (Just "gtk-save")
        (do fileSave False; return ()) [] False
    ,AD "FileSaveAs" "Save _As..." Nothing (Just "gtk-save-as")
        (do fileSave True; return ()) [] False
    ,AD "FileSaveAll" "Save A_ll" Nothing Nothing
        (do fileSaveAll (\ b -> return (ibBufferName b /= "_Eval.hs")); return ()) [] False
    ,AD "FileClose" "_Close" Nothing (Just "gtk-close")
        (do fileClose; return ()) [] False
    ,AD "FileCloseAll" "Close All" Nothing Nothing
        (do fileCloseAll (\ b -> return (ibBufferName b /= "_Eval.hs")); return ()) [] False

    ,AD "Edit" "_Edit" Nothing Nothing (return ()) [] False
    ,AD "EditUndo" "_Undo" Nothing (Just "gtk-undo")
        editUndo [] False
    ,AD "EditRedo" "_Redo" Nothing (Just "gtk-redo")
        editRedo [] False
    ,AD "EditCut" "Cu_t" Nothing (Just "gtk-cut")
        editCut [] {--Just "<control>X"--} False
    ,AD "EditCopy" "_Copy"  Nothing  (Just "gtk-copy")
        editCopy [] {--Just "<control>C"--} False
    ,AD "EditPaste" "_Paste" Nothing (Just "gtk-paste")
        editPaste [] {--Just "<control>V"--} False
    ,AD "EditDelete" "_Delete" Nothing (Just "gtk-delete")
        editDelete [] False
    ,AD "EditSelectAll" "Select_All" Nothing (Just "gtk-select-all")
        editSelectAll [] False

--  TODO Findbar
--    ,AD "EditFind" "Find" Nothing (Just "gtk-find")
--        (editFindInc Initial) [] False
--    ,AD "EditFindNext" "Find _Next" Nothing (Just "gtk-find-next")
--        (editFindInc Forward) [] False
--    ,AD "EditFindPrevious" "Find _Previous" Nothing (Just "gtk-find-previous")
--        (editFindInc Backward) [] False
--    ,AD "EditGotoLine" "_Goto Line" Nothing (Just "gtk-jump")
--        editGotoLine [] False

    ,AD "EditComment" "_Comment" Nothing Nothing
        editComment [] False
    ,AD "EditUncomment" "_Uncomment" Nothing Nothing
        editUncomment [] False

    --,AD "Align" "_Align" Nothing Nothing (return ()) [] False
    ,AD "EditAlignEqual" "Align _=" Nothing Nothing
        (align '=') [] False
    ,AD "EditAlignRightArrow" "Align -_>" Nothing Nothing
        (align '>') [] False
    ,AD "EditAlignLeftArrow" "Align _<-" Nothing Nothing
        (align '<') [] False
    ,AD "EditAlignTypeSig" "Align _::" Nothing Nothing
        (align ':') [] False]

-- -----------------------------------------------
-- * Texteditor Events
--
data TexteditorSel = TexteditorSel
    deriving(Typeable, Show, Ord, Eq)

instance Selector TexteditorSel where
    type ValueType TexteditorSel = EventChannel TexteditorEvent

data TexteditorEvent

-- -----------------------------------------------
-- * Texteditor Preferences
--
--  | Define the prefs you need
data TexteditorPrefs = TexteditorPrefs {
        tepShowLineNumbers     ::   Bool
    ,   tepRightMargin         ::   (Bool, Int)
    ,   tepTabWidth            ::   Int
    ,   tepWrapLines           ::   Bool
    ,   tepSourceCandy         ::   (Bool,String)
    ,   tepKeymapName          ::   String
    ,   tepForceLineEnds       ::   Bool
    ,   tepRemoveTBlanks       ::   Bool
    ,   tepTextviewFont        ::   Maybe String
    ,   tepSourceStyle         ::   (Bool, String)
    ,   tepFoundBackground     ::   Gtk.Color
    ,   tepContextBackground   ::   Gtk.Color
    ,   tepBreakpointBackground ::  Gtk.Color
    ,   tepAutoLoad            ::   Bool
    ,   tepUseYi               ::   Bool}
    deriving (Eq,Typeable)

defaultTextEditorPrefs = TexteditorPrefs {
        tepShowLineNumbers     =   True
    ,   tepRightMargin         =   (True,100)
    ,   tepTabWidth            =   4
    ,   tepWrapLines           =   False
    ,   tepSourceCandy         =   (False,"candy")
    ,   tepKeymapName          =   "keymap"
    ,   tepForceLineEnds       =   True
    ,   tepRemoveTBlanks       =   True
    ,   tepTextviewFont        =   Nothing
    ,   tepSourceStyle         =   (False,"classic")
    ,   tepFoundBackground     =   Gtk.Color 65535 65535 32768
    ,   tepContextBackground   =   Gtk.Color 65535 49152 49152
    ,   tepBreakpointBackground =  Gtk.Color 65535 49152 32768
    ,   tepUseYi               =   False
    ,   tepAutoLoad            =   False}


textEditorPrefsDescr :: forall alpha . TextEditorBackend alpha => alpha -> FieldDescription TexteditorPrefs
textEditorPrefsDescr _ =
    VertBox defaultParams [
       mkField
            (("Name", ParaString "Show line numbers")
                <<< ("Synopsis", ParaString "(True/False)")
                    <<< defaultParams)
            (PP.text . show)
            boolParser
            tepShowLineNumbers
            (\ b a -> a{tepShowLineNumbers = b})
            boolEditor
            (\b -> do
                buffers :: [IDEBuffer alpha] <- allBuffers
                mapM_ (\buf -> setShowLineNumbers (ibSourceView buf) b) buffers)
    ,   mkField
            (("Name", ParaString "TextView Font") <<< defaultParams)
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if null str then Nothing else Just (str)))
            tepTextviewFont
            (\ b a -> a{tepTextviewFont = b})
            fontEditor
            (\mbs -> do
                buffers  :: [IDEBuffer alpha] <- allBuffers
                mapM_ (\buf -> setFont (ibSourceView buf) mbs) buffers)
    ,   mkField
            (("Name", ParaString "Right margin")
                <<< ("Synopsis" , ParaString "Size or 0 for no right margin")
                    <<< ("Shadow" , ParaShadow ShadowIn) <<< defaultParams)
            (PP.text . show)
            readParser
            tepRightMargin
            (\b a -> a{tepRightMargin = b})
            (disableEditor (intEditor (1.0, 200.0, 5.0), ("Name", ParaString "Position")
                    <<< defaultParams)
                    True "Show it ?")
            (\b -> do
                buffers :: [IDEBuffer alpha] <- allBuffers
                mapM_ (\buf -> setRightMargin ( ibSourceView buf)
                                (case b of
                                    (True,v) -> Just v
                                    (False,_) -> Nothing)) buffers)
    ,   mkField
            (("Name", ParaString "Tab width") <<< defaultParams)
            (PP.text . show)
            intParser
            tepTabWidth
            (\b a -> a{tepTabWidth = b})
            (intEditor (1.0, 20.0, 1.0))
            (\i -> do
                buffers :: [IDEBuffer alpha] <- allBuffers
                mapM_ (\buf -> setIndentWidth ( ibSourceView buf) i) buffers)
    ,   mkField
            (("Name", ParaString "Wrap lines") <<< defaultParams)
            (PP.text . show)
            boolParser
            tepWrapLines
            (\b a -> a{tepWrapLines = b})
            boolEditor
            (\b -> do
                buffers :: [IDEBuffer alpha]  <- allBuffers
                mapM_ (\buf -> setWrapMode ( ibSourceView buf) b) buffers)
    ,   mkField
            (("Name", ParaString "Use standard line ends even on windows") <<< defaultParams)
            (PP.text . show)
            boolParser
            tepForceLineEnds
            (\b a -> a{tepForceLineEnds = b})
            boolEditor
            (\i -> return ())
    ,   mkField
            (("Name", ParaString "Remove trailing blanks when saving a file") <<< defaultParams)
            (PP.text . show)
            boolParser
            tepRemoveTBlanks
            (\b a -> a{tepRemoveTBlanks = b})
            boolEditor
            (\i -> return ())
    ,   mkField
            (("Name", ParaString "Source candy")
                <<< ("Synopsis", ParaString
                    "Empty for do not use or the name of a candy file in a config dir")
                    <<< ("Shadow", ParaShadow ShadowIn) <<< defaultParams)
            (PP.text . show)
            readParser
            tepSourceCandy (\b a -> a{tepSourceCandy = b})
            (disableEditor (stringEditor (\s -> not (null s)) True,
                ("Name", ParaString "Candy specification") <<< defaultParams)
                    True "Use it ?")
            (\cs -> return ())
-- TODO: recover
--            case cs of
--                        (False,_) -> do
--                            setCandyState False
--                            editCandy
--                        (True,name) -> do
--                            setCandyState True
--                            editCandy)
-- TODO specialize on Sourceview
{--    ,   mkField
            (("Name", ParaString "Editor Style") <<< defaultParams)
            (\a -> PP.text (case a of (False,_) -> show ""; (True, s) -> show s))
            (do str <- stringParser
                return (if null str then (False,"classic") else (True,str)))
            tepSourceStyle
            (\b a -> a{tepSourceStyle = b})
            styleEditor
            (\mbs -> do
                buffers <- allBuffers
                mapM_ (\buf -> do
                    ebuf <- getBuffer ( ibSourceView buf)
                    setStyle ebuf (case mbs of
                                    (False,_) -> Nothing
                                    (True,s) -> Just s)) buffers) --}
    ,   mkField
            (("Name", ParaString "Found Text Background") <<< defaultParams)
            (PP.text . show)
            colorParser
            tepFoundBackground
            (\ b a -> a{tepFoundBackground = b})
            colorEditor
            (\c -> do
                buffers :: [IDEBuffer alpha] <- allBuffers
                forM_ buffers $ \buf -> do
                    ebuf     <- getBuffer ( ibSourceView buf)
                    tagTable <- getTagTable ebuf
                    mbTag    <- lookupTag tagTable "found"
                    case mbTag of
                        Just tag -> background tag (fromGtkColor c)
                        Nothing  -> return ())
    ,   mkField
            (("Name", ParaString "Execution Context Text Background") <<< defaultParams)
            (PP.text . show)
            colorParser
            tepContextBackground
            (\ b a -> a{tepContextBackground = b})
            colorEditor
            (\c -> do
                buffers  :: [IDEBuffer alpha] <- allBuffers
                forM_ buffers $ \buf -> do
                    ebuf     <- getBuffer ( ibSourceView buf)
                    tagTable <- getTagTable ebuf
                    --  TODO find and set the tag background
                    return ())
    ,   mkField
            (("Name", ParaString "Breakpoint Text Background") <<< defaultParams)
            (PP.text . show)
            colorParser
            tepBreakpointBackground
            (\ b a -> a{tepBreakpointBackground = b})
            colorEditor
            (\c -> do
                buffers  :: [IDEBuffer alpha] <- allBuffers
                forM_ buffers $ \buf -> do
                    ebuf     <- getBuffer ( ibSourceView buf)
                    tagTable <- getTagTable ebuf
                    --  TODO find and set the tag background
                    return ())
    ,   mkField
            (("Name", ParaString "Automatically load modified files modified outside of Leksah") <<< defaultParams)
            (PP.text . show)
            boolParser
            tepAutoLoad
            (\b a -> a{tepAutoLoad = b})
            boolEditor
            (\i -> return ())]

colorParser :: CharParser () Gtk.Color
colorParser = do
    string "Color"
    whiteSpace
    r <- integer
    whiteSpace
    g <- integer
    whiteSpace
    b <- integer
    return $ Gtk.Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

lexer :: P.TokenParser st
lexer = P.makeTokenParser prefsStyle

integer = P.integer lexer

{--
styleEditor :: Editor (Bool, String)
styleEditor p n = do
    styleManager <- sourceStyleSchemeManagerNew
    ids          <- sourceStyleSchemeManagerGetSchemeIds styleManager
    disableEditor (comboSelectionEditor ids id, p) True "Select a special style?" p n
--}
