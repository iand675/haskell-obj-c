{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSStandardKeyBindingResponding@.
--
-- Usage:
--
-- @
-- delegate <- newNSStandardKeyBindingResponding defaultNSStandardKeyBindingRespondingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSStandardKeyBindingResponding
  ( NSStandardKeyBindingRespondingOverrides(..)
  , defaultNSStandardKeyBindingRespondingOverrides
  , newNSStandardKeyBindingResponding
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol NSStandardKeyBindingResponding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSStandardKeyBindingRespondingOverrides = NSStandardKeyBindingRespondingOverrides
  { _insertText :: !(Maybe (RawId -> IO ()))
  , _doCommandBySelector :: !(Maybe (Sel -> IO ()))
  , _moveForward :: !(Maybe (RawId -> IO ()))
  , _moveRight :: !(Maybe (RawId -> IO ()))
  , _moveBackward :: !(Maybe (RawId -> IO ()))
  , _moveLeft :: !(Maybe (RawId -> IO ()))
  , _moveUp :: !(Maybe (RawId -> IO ()))
  , _moveDown :: !(Maybe (RawId -> IO ()))
  , _moveWordForward :: !(Maybe (RawId -> IO ()))
  , _moveWordBackward :: !(Maybe (RawId -> IO ()))
  , _moveToBeginningOfLine :: !(Maybe (RawId -> IO ()))
  , _moveToEndOfLine :: !(Maybe (RawId -> IO ()))
  , _moveToBeginningOfParagraph :: !(Maybe (RawId -> IO ()))
  , _moveToEndOfParagraph :: !(Maybe (RawId -> IO ()))
  , _moveToEndOfDocument :: !(Maybe (RawId -> IO ()))
  , _moveToBeginningOfDocument :: !(Maybe (RawId -> IO ()))
  , _pageDown :: !(Maybe (RawId -> IO ()))
  , _pageUp :: !(Maybe (RawId -> IO ()))
  , _centerSelectionInVisibleArea :: !(Maybe (RawId -> IO ()))
  , _moveBackwardAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveForwardAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveWordForwardAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveWordBackwardAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveUpAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveDownAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToBeginningOfLineAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToEndOfLineAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToBeginningOfParagraphAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToEndOfParagraphAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToEndOfDocumentAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToBeginningOfDocumentAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _pageDownAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _pageUpAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveParagraphForwardAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveParagraphBackwardAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveWordRight :: !(Maybe (RawId -> IO ()))
  , _moveWordLeft :: !(Maybe (RawId -> IO ()))
  , _moveRightAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveLeftAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveWordRightAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveWordLeftAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToLeftEndOfLine :: !(Maybe (RawId -> IO ()))
  , _moveToRightEndOfLine :: !(Maybe (RawId -> IO ()))
  , _moveToLeftEndOfLineAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _moveToRightEndOfLineAndModifySelection :: !(Maybe (RawId -> IO ()))
  , _scrollPageUp :: !(Maybe (RawId -> IO ()))
  , _scrollPageDown :: !(Maybe (RawId -> IO ()))
  , _scrollLineUp :: !(Maybe (RawId -> IO ()))
  , _scrollLineDown :: !(Maybe (RawId -> IO ()))
  , _scrollToBeginningOfDocument :: !(Maybe (RawId -> IO ()))
  , _scrollToEndOfDocument :: !(Maybe (RawId -> IO ()))
  , _transpose :: !(Maybe (RawId -> IO ()))
  , _transposeWords :: !(Maybe (RawId -> IO ()))
  , _selectAll :: !(Maybe (RawId -> IO ()))
  , _selectParagraph :: !(Maybe (RawId -> IO ()))
  , _selectLine :: !(Maybe (RawId -> IO ()))
  , _selectWord :: !(Maybe (RawId -> IO ()))
  , _indent :: !(Maybe (RawId -> IO ()))
  , _insertTab :: !(Maybe (RawId -> IO ()))
  , _insertBacktab :: !(Maybe (RawId -> IO ()))
  , _insertNewline :: !(Maybe (RawId -> IO ()))
  , _insertParagraphSeparator :: !(Maybe (RawId -> IO ()))
  , _insertNewlineIgnoringFieldEditor :: !(Maybe (RawId -> IO ()))
  , _insertTabIgnoringFieldEditor :: !(Maybe (RawId -> IO ()))
  , _insertLineBreak :: !(Maybe (RawId -> IO ()))
  , _insertContainerBreak :: !(Maybe (RawId -> IO ()))
  , _insertSingleQuoteIgnoringSubstitution :: !(Maybe (RawId -> IO ()))
  , _insertDoubleQuoteIgnoringSubstitution :: !(Maybe (RawId -> IO ()))
  , _changeCaseOfLetter :: !(Maybe (RawId -> IO ()))
  , _uppercaseWord :: !(Maybe (RawId -> IO ()))
  , _lowercaseWord :: !(Maybe (RawId -> IO ()))
  , _capitalizeWord :: !(Maybe (RawId -> IO ()))
  , _deleteForward :: !(Maybe (RawId -> IO ()))
  , _deleteBackward :: !(Maybe (RawId -> IO ()))
  , _deleteBackwardByDecomposingPreviousCharacter :: !(Maybe (RawId -> IO ()))
  , _deleteWordForward :: !(Maybe (RawId -> IO ()))
  , _deleteWordBackward :: !(Maybe (RawId -> IO ()))
  , _deleteToBeginningOfLine :: !(Maybe (RawId -> IO ()))
  , _deleteToEndOfLine :: !(Maybe (RawId -> IO ()))
  , _deleteToBeginningOfParagraph :: !(Maybe (RawId -> IO ()))
  , _deleteToEndOfParagraph :: !(Maybe (RawId -> IO ()))
  , _yank :: !(Maybe (RawId -> IO ()))
  , _complete :: !(Maybe (RawId -> IO ()))
  , _setMark :: !(Maybe (RawId -> IO ()))
  , _deleteToMark :: !(Maybe (RawId -> IO ()))
  , _selectToMark :: !(Maybe (RawId -> IO ()))
  , _swapWithMark :: !(Maybe (RawId -> IO ()))
  , _cancelOperation :: !(Maybe (RawId -> IO ()))
  , _makeBaseWritingDirectionNatural :: !(Maybe (RawId -> IO ()))
  , _makeBaseWritingDirectionLeftToRight :: !(Maybe (RawId -> IO ()))
  , _makeBaseWritingDirectionRightToLeft :: !(Maybe (RawId -> IO ()))
  , _makeTextWritingDirectionNatural :: !(Maybe (RawId -> IO ()))
  , _makeTextWritingDirectionLeftToRight :: !(Maybe (RawId -> IO ()))
  , _makeTextWritingDirectionRightToLeft :: !(Maybe (RawId -> IO ()))
  , _quickLookPreviewItems :: !(Maybe (RawId -> IO ()))
  , _showContextMenuForSelection :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSStandardKeyBindingRespondingOverrides :: NSStandardKeyBindingRespondingOverrides
defaultNSStandardKeyBindingRespondingOverrides = NSStandardKeyBindingRespondingOverrides
  { _insertText = Nothing
  , _doCommandBySelector = Nothing
  , _moveForward = Nothing
  , _moveRight = Nothing
  , _moveBackward = Nothing
  , _moveLeft = Nothing
  , _moveUp = Nothing
  , _moveDown = Nothing
  , _moveWordForward = Nothing
  , _moveWordBackward = Nothing
  , _moveToBeginningOfLine = Nothing
  , _moveToEndOfLine = Nothing
  , _moveToBeginningOfParagraph = Nothing
  , _moveToEndOfParagraph = Nothing
  , _moveToEndOfDocument = Nothing
  , _moveToBeginningOfDocument = Nothing
  , _pageDown = Nothing
  , _pageUp = Nothing
  , _centerSelectionInVisibleArea = Nothing
  , _moveBackwardAndModifySelection = Nothing
  , _moveForwardAndModifySelection = Nothing
  , _moveWordForwardAndModifySelection = Nothing
  , _moveWordBackwardAndModifySelection = Nothing
  , _moveUpAndModifySelection = Nothing
  , _moveDownAndModifySelection = Nothing
  , _moveToBeginningOfLineAndModifySelection = Nothing
  , _moveToEndOfLineAndModifySelection = Nothing
  , _moveToBeginningOfParagraphAndModifySelection = Nothing
  , _moveToEndOfParagraphAndModifySelection = Nothing
  , _moveToEndOfDocumentAndModifySelection = Nothing
  , _moveToBeginningOfDocumentAndModifySelection = Nothing
  , _pageDownAndModifySelection = Nothing
  , _pageUpAndModifySelection = Nothing
  , _moveParagraphForwardAndModifySelection = Nothing
  , _moveParagraphBackwardAndModifySelection = Nothing
  , _moveWordRight = Nothing
  , _moveWordLeft = Nothing
  , _moveRightAndModifySelection = Nothing
  , _moveLeftAndModifySelection = Nothing
  , _moveWordRightAndModifySelection = Nothing
  , _moveWordLeftAndModifySelection = Nothing
  , _moveToLeftEndOfLine = Nothing
  , _moveToRightEndOfLine = Nothing
  , _moveToLeftEndOfLineAndModifySelection = Nothing
  , _moveToRightEndOfLineAndModifySelection = Nothing
  , _scrollPageUp = Nothing
  , _scrollPageDown = Nothing
  , _scrollLineUp = Nothing
  , _scrollLineDown = Nothing
  , _scrollToBeginningOfDocument = Nothing
  , _scrollToEndOfDocument = Nothing
  , _transpose = Nothing
  , _transposeWords = Nothing
  , _selectAll = Nothing
  , _selectParagraph = Nothing
  , _selectLine = Nothing
  , _selectWord = Nothing
  , _indent = Nothing
  , _insertTab = Nothing
  , _insertBacktab = Nothing
  , _insertNewline = Nothing
  , _insertParagraphSeparator = Nothing
  , _insertNewlineIgnoringFieldEditor = Nothing
  , _insertTabIgnoringFieldEditor = Nothing
  , _insertLineBreak = Nothing
  , _insertContainerBreak = Nothing
  , _insertSingleQuoteIgnoringSubstitution = Nothing
  , _insertDoubleQuoteIgnoringSubstitution = Nothing
  , _changeCaseOfLetter = Nothing
  , _uppercaseWord = Nothing
  , _lowercaseWord = Nothing
  , _capitalizeWord = Nothing
  , _deleteForward = Nothing
  , _deleteBackward = Nothing
  , _deleteBackwardByDecomposingPreviousCharacter = Nothing
  , _deleteWordForward = Nothing
  , _deleteWordBackward = Nothing
  , _deleteToBeginningOfLine = Nothing
  , _deleteToEndOfLine = Nothing
  , _deleteToBeginningOfParagraph = Nothing
  , _deleteToEndOfParagraph = Nothing
  , _yank = Nothing
  , _complete = Nothing
  , _setMark = Nothing
  , _deleteToMark = Nothing
  , _selectToMark = Nothing
  , _swapWithMark = Nothing
  , _cancelOperation = Nothing
  , _makeBaseWritingDirectionNatural = Nothing
  , _makeBaseWritingDirectionLeftToRight = Nothing
  , _makeBaseWritingDirectionRightToLeft = Nothing
  , _makeTextWritingDirectionNatural = Nothing
  , _makeTextWritingDirectionLeftToRight = Nothing
  , _makeTextWritingDirectionRightToLeft = Nothing
  , _quickLookPreviewItems = Nothing
  , _showContextMenuForSelection = Nothing
  }

foreign import ccall "wrapper"
  wrap_sel_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsStandardKeyBindingRespondingDelegateClass #-}
nsStandardKeyBindingRespondingDelegateClass :: Class
nsStandardKeyBindingRespondingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSStandardKeyBindingResponding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_insertText = unSelector (mkSelector "insertText:")
      sel_doCommandBySelector = unSelector (mkSelector "doCommandBySelector:")
      sel_moveForward = unSelector (mkSelector "moveForward:")
      sel_moveRight = unSelector (mkSelector "moveRight:")
      sel_moveBackward = unSelector (mkSelector "moveBackward:")
      sel_moveLeft = unSelector (mkSelector "moveLeft:")
      sel_moveUp = unSelector (mkSelector "moveUp:")
      sel_moveDown = unSelector (mkSelector "moveDown:")
      sel_moveWordForward = unSelector (mkSelector "moveWordForward:")
      sel_moveWordBackward = unSelector (mkSelector "moveWordBackward:")
      sel_moveToBeginningOfLine = unSelector (mkSelector "moveToBeginningOfLine:")
      sel_moveToEndOfLine = unSelector (mkSelector "moveToEndOfLine:")
      sel_moveToBeginningOfParagraph = unSelector (mkSelector "moveToBeginningOfParagraph:")
      sel_moveToEndOfParagraph = unSelector (mkSelector "moveToEndOfParagraph:")
      sel_moveToEndOfDocument = unSelector (mkSelector "moveToEndOfDocument:")
      sel_moveToBeginningOfDocument = unSelector (mkSelector "moveToBeginningOfDocument:")
      sel_pageDown = unSelector (mkSelector "pageDown:")
      sel_pageUp = unSelector (mkSelector "pageUp:")
      sel_centerSelectionInVisibleArea = unSelector (mkSelector "centerSelectionInVisibleArea:")
      sel_moveBackwardAndModifySelection = unSelector (mkSelector "moveBackwardAndModifySelection:")
      sel_moveForwardAndModifySelection = unSelector (mkSelector "moveForwardAndModifySelection:")
      sel_moveWordForwardAndModifySelection = unSelector (mkSelector "moveWordForwardAndModifySelection:")
      sel_moveWordBackwardAndModifySelection = unSelector (mkSelector "moveWordBackwardAndModifySelection:")
      sel_moveUpAndModifySelection = unSelector (mkSelector "moveUpAndModifySelection:")
      sel_moveDownAndModifySelection = unSelector (mkSelector "moveDownAndModifySelection:")
      sel_moveToBeginningOfLineAndModifySelection = unSelector (mkSelector "moveToBeginningOfLineAndModifySelection:")
      sel_moveToEndOfLineAndModifySelection = unSelector (mkSelector "moveToEndOfLineAndModifySelection:")
      sel_moveToBeginningOfParagraphAndModifySelection = unSelector (mkSelector "moveToBeginningOfParagraphAndModifySelection:")
      sel_moveToEndOfParagraphAndModifySelection = unSelector (mkSelector "moveToEndOfParagraphAndModifySelection:")
      sel_moveToEndOfDocumentAndModifySelection = unSelector (mkSelector "moveToEndOfDocumentAndModifySelection:")
      sel_moveToBeginningOfDocumentAndModifySelection = unSelector (mkSelector "moveToBeginningOfDocumentAndModifySelection:")
      sel_pageDownAndModifySelection = unSelector (mkSelector "pageDownAndModifySelection:")
      sel_pageUpAndModifySelection = unSelector (mkSelector "pageUpAndModifySelection:")
      sel_moveParagraphForwardAndModifySelection = unSelector (mkSelector "moveParagraphForwardAndModifySelection:")
      sel_moveParagraphBackwardAndModifySelection = unSelector (mkSelector "moveParagraphBackwardAndModifySelection:")
      sel_moveWordRight = unSelector (mkSelector "moveWordRight:")
      sel_moveWordLeft = unSelector (mkSelector "moveWordLeft:")
      sel_moveRightAndModifySelection = unSelector (mkSelector "moveRightAndModifySelection:")
      sel_moveLeftAndModifySelection = unSelector (mkSelector "moveLeftAndModifySelection:")
      sel_moveWordRightAndModifySelection = unSelector (mkSelector "moveWordRightAndModifySelection:")
      sel_moveWordLeftAndModifySelection = unSelector (mkSelector "moveWordLeftAndModifySelection:")
      sel_moveToLeftEndOfLine = unSelector (mkSelector "moveToLeftEndOfLine:")
      sel_moveToRightEndOfLine = unSelector (mkSelector "moveToRightEndOfLine:")
      sel_moveToLeftEndOfLineAndModifySelection = unSelector (mkSelector "moveToLeftEndOfLineAndModifySelection:")
      sel_moveToRightEndOfLineAndModifySelection = unSelector (mkSelector "moveToRightEndOfLineAndModifySelection:")
      sel_scrollPageUp = unSelector (mkSelector "scrollPageUp:")
      sel_scrollPageDown = unSelector (mkSelector "scrollPageDown:")
      sel_scrollLineUp = unSelector (mkSelector "scrollLineUp:")
      sel_scrollLineDown = unSelector (mkSelector "scrollLineDown:")
      sel_scrollToBeginningOfDocument = unSelector (mkSelector "scrollToBeginningOfDocument:")
      sel_scrollToEndOfDocument = unSelector (mkSelector "scrollToEndOfDocument:")
      sel_transpose = unSelector (mkSelector "transpose:")
      sel_transposeWords = unSelector (mkSelector "transposeWords:")
      sel_selectAll = unSelector (mkSelector "selectAll:")
      sel_selectParagraph = unSelector (mkSelector "selectParagraph:")
      sel_selectLine = unSelector (mkSelector "selectLine:")
      sel_selectWord = unSelector (mkSelector "selectWord:")
      sel_indent = unSelector (mkSelector "indent:")
      sel_insertTab = unSelector (mkSelector "insertTab:")
      sel_insertBacktab = unSelector (mkSelector "insertBacktab:")
      sel_insertNewline = unSelector (mkSelector "insertNewline:")
      sel_insertParagraphSeparator = unSelector (mkSelector "insertParagraphSeparator:")
      sel_insertNewlineIgnoringFieldEditor = unSelector (mkSelector "insertNewlineIgnoringFieldEditor:")
      sel_insertTabIgnoringFieldEditor = unSelector (mkSelector "insertTabIgnoringFieldEditor:")
      sel_insertLineBreak = unSelector (mkSelector "insertLineBreak:")
      sel_insertContainerBreak = unSelector (mkSelector "insertContainerBreak:")
      sel_insertSingleQuoteIgnoringSubstitution = unSelector (mkSelector "insertSingleQuoteIgnoringSubstitution:")
      sel_insertDoubleQuoteIgnoringSubstitution = unSelector (mkSelector "insertDoubleQuoteIgnoringSubstitution:")
      sel_changeCaseOfLetter = unSelector (mkSelector "changeCaseOfLetter:")
      sel_uppercaseWord = unSelector (mkSelector "uppercaseWord:")
      sel_lowercaseWord = unSelector (mkSelector "lowercaseWord:")
      sel_capitalizeWord = unSelector (mkSelector "capitalizeWord:")
      sel_deleteForward = unSelector (mkSelector "deleteForward:")
      sel_deleteBackward = unSelector (mkSelector "deleteBackward:")
      sel_deleteBackwardByDecomposingPreviousCharacter = unSelector (mkSelector "deleteBackwardByDecomposingPreviousCharacter:")
      sel_deleteWordForward = unSelector (mkSelector "deleteWordForward:")
      sel_deleteWordBackward = unSelector (mkSelector "deleteWordBackward:")
      sel_deleteToBeginningOfLine = unSelector (mkSelector "deleteToBeginningOfLine:")
      sel_deleteToEndOfLine = unSelector (mkSelector "deleteToEndOfLine:")
      sel_deleteToBeginningOfParagraph = unSelector (mkSelector "deleteToBeginningOfParagraph:")
      sel_deleteToEndOfParagraph = unSelector (mkSelector "deleteToEndOfParagraph:")
      sel_yank = unSelector (mkSelector "yank:")
      sel_complete = unSelector (mkSelector "complete:")
      sel_setMark = unSelector (mkSelector "setMark:")
      sel_deleteToMark = unSelector (mkSelector "deleteToMark:")
      sel_selectToMark = unSelector (mkSelector "selectToMark:")
      sel_swapWithMark = unSelector (mkSelector "swapWithMark:")
      sel_cancelOperation = unSelector (mkSelector "cancelOperation:")
      sel_makeBaseWritingDirectionNatural = unSelector (mkSelector "makeBaseWritingDirectionNatural:")
      sel_makeBaseWritingDirectionLeftToRight = unSelector (mkSelector "makeBaseWritingDirectionLeftToRight:")
      sel_makeBaseWritingDirectionRightToLeft = unSelector (mkSelector "makeBaseWritingDirectionRightToLeft:")
      sel_makeTextWritingDirectionNatural = unSelector (mkSelector "makeTextWritingDirectionNatural:")
      sel_makeTextWritingDirectionLeftToRight = unSelector (mkSelector "makeTextWritingDirectionLeftToRight:")
      sel_makeTextWritingDirectionRightToLeft = unSelector (mkSelector "makeTextWritingDirectionRightToLeft:")
      sel_quickLookPreviewItems = unSelector (mkSelector "quickLookPreviewItems:")
      sel_showContextMenuForSelection = unSelector (mkSelector "showContextMenuForSelection:")
  -- insertText:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertText rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertText:" "v@:@" stub_0

  -- doCommandBySelector:
  stub_1 <- wrap_sel_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _doCommandBySelector rec_ of
      Nothing -> pure ()
      Just f -> f (Selector arg0)
  addObjCMethod cls "doCommandBySelector:" "v@::" stub_1

  -- moveForward:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveForward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveForward:" "v@:@" stub_2

  -- moveRight:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveRight rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveRight:" "v@:@" stub_3

  -- moveBackward:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveBackward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveBackward:" "v@:@" stub_4

  -- moveLeft:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveLeft rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveLeft:" "v@:@" stub_5

  -- moveUp:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveUp rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveUp:" "v@:@" stub_6

  -- moveDown:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveDown rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveDown:" "v@:@" stub_7

  -- moveWordForward:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordForward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordForward:" "v@:@" stub_8

  -- moveWordBackward:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordBackward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordBackward:" "v@:@" stub_9

  -- moveToBeginningOfLine:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToBeginningOfLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToBeginningOfLine:" "v@:@" stub_10

  -- moveToEndOfLine:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToEndOfLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToEndOfLine:" "v@:@" stub_11

  -- moveToBeginningOfParagraph:
  stub_12 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToBeginningOfParagraph rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToBeginningOfParagraph:" "v@:@" stub_12

  -- moveToEndOfParagraph:
  stub_13 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToEndOfParagraph rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToEndOfParagraph:" "v@:@" stub_13

  -- moveToEndOfDocument:
  stub_14 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToEndOfDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToEndOfDocument:" "v@:@" stub_14

  -- moveToBeginningOfDocument:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToBeginningOfDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToBeginningOfDocument:" "v@:@" stub_15

  -- pageDown:
  stub_16 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _pageDown rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pageDown:" "v@:@" stub_16

  -- pageUp:
  stub_17 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _pageUp rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pageUp:" "v@:@" stub_17

  -- centerSelectionInVisibleArea:
  stub_18 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _centerSelectionInVisibleArea rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "centerSelectionInVisibleArea:" "v@:@" stub_18

  -- moveBackwardAndModifySelection:
  stub_19 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveBackwardAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveBackwardAndModifySelection:" "v@:@" stub_19

  -- moveForwardAndModifySelection:
  stub_20 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveForwardAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveForwardAndModifySelection:" "v@:@" stub_20

  -- moveWordForwardAndModifySelection:
  stub_21 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordForwardAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordForwardAndModifySelection:" "v@:@" stub_21

  -- moveWordBackwardAndModifySelection:
  stub_22 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordBackwardAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordBackwardAndModifySelection:" "v@:@" stub_22

  -- moveUpAndModifySelection:
  stub_23 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveUpAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveUpAndModifySelection:" "v@:@" stub_23

  -- moveDownAndModifySelection:
  stub_24 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveDownAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveDownAndModifySelection:" "v@:@" stub_24

  -- moveToBeginningOfLineAndModifySelection:
  stub_25 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToBeginningOfLineAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToBeginningOfLineAndModifySelection:" "v@:@" stub_25

  -- moveToEndOfLineAndModifySelection:
  stub_26 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToEndOfLineAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToEndOfLineAndModifySelection:" "v@:@" stub_26

  -- moveToBeginningOfParagraphAndModifySelection:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToBeginningOfParagraphAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToBeginningOfParagraphAndModifySelection:" "v@:@" stub_27

  -- moveToEndOfParagraphAndModifySelection:
  stub_28 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToEndOfParagraphAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToEndOfParagraphAndModifySelection:" "v@:@" stub_28

  -- moveToEndOfDocumentAndModifySelection:
  stub_29 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToEndOfDocumentAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToEndOfDocumentAndModifySelection:" "v@:@" stub_29

  -- moveToBeginningOfDocumentAndModifySelection:
  stub_30 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToBeginningOfDocumentAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToBeginningOfDocumentAndModifySelection:" "v@:@" stub_30

  -- pageDownAndModifySelection:
  stub_31 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _pageDownAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pageDownAndModifySelection:" "v@:@" stub_31

  -- pageUpAndModifySelection:
  stub_32 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _pageUpAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pageUpAndModifySelection:" "v@:@" stub_32

  -- moveParagraphForwardAndModifySelection:
  stub_33 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveParagraphForwardAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveParagraphForwardAndModifySelection:" "v@:@" stub_33

  -- moveParagraphBackwardAndModifySelection:
  stub_34 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveParagraphBackwardAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveParagraphBackwardAndModifySelection:" "v@:@" stub_34

  -- moveWordRight:
  stub_35 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordRight rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordRight:" "v@:@" stub_35

  -- moveWordLeft:
  stub_36 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordLeft rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordLeft:" "v@:@" stub_36

  -- moveRightAndModifySelection:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveRightAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveRightAndModifySelection:" "v@:@" stub_37

  -- moveLeftAndModifySelection:
  stub_38 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveLeftAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveLeftAndModifySelection:" "v@:@" stub_38

  -- moveWordRightAndModifySelection:
  stub_39 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordRightAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordRightAndModifySelection:" "v@:@" stub_39

  -- moveWordLeftAndModifySelection:
  stub_40 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveWordLeftAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveWordLeftAndModifySelection:" "v@:@" stub_40

  -- moveToLeftEndOfLine:
  stub_41 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToLeftEndOfLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToLeftEndOfLine:" "v@:@" stub_41

  -- moveToRightEndOfLine:
  stub_42 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToRightEndOfLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToRightEndOfLine:" "v@:@" stub_42

  -- moveToLeftEndOfLineAndModifySelection:
  stub_43 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToLeftEndOfLineAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToLeftEndOfLineAndModifySelection:" "v@:@" stub_43

  -- moveToRightEndOfLineAndModifySelection:
  stub_44 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _moveToRightEndOfLineAndModifySelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "moveToRightEndOfLineAndModifySelection:" "v@:@" stub_44

  -- scrollPageUp:
  stub_45 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _scrollPageUp rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scrollPageUp:" "v@:@" stub_45

  -- scrollPageDown:
  stub_46 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _scrollPageDown rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scrollPageDown:" "v@:@" stub_46

  -- scrollLineUp:
  stub_47 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _scrollLineUp rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scrollLineUp:" "v@:@" stub_47

  -- scrollLineDown:
  stub_48 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _scrollLineDown rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scrollLineDown:" "v@:@" stub_48

  -- scrollToBeginningOfDocument:
  stub_49 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _scrollToBeginningOfDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scrollToBeginningOfDocument:" "v@:@" stub_49

  -- scrollToEndOfDocument:
  stub_50 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _scrollToEndOfDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "scrollToEndOfDocument:" "v@:@" stub_50

  -- transpose:
  stub_51 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _transpose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "transpose:" "v@:@" stub_51

  -- transposeWords:
  stub_52 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _transposeWords rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "transposeWords:" "v@:@" stub_52

  -- selectAll:
  stub_53 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _selectAll rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectAll:" "v@:@" stub_53

  -- selectParagraph:
  stub_54 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _selectParagraph rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectParagraph:" "v@:@" stub_54

  -- selectLine:
  stub_55 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _selectLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectLine:" "v@:@" stub_55

  -- selectWord:
  stub_56 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _selectWord rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectWord:" "v@:@" stub_56

  -- indent:
  stub_57 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _indent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "indent:" "v@:@" stub_57

  -- insertTab:
  stub_58 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertTab rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertTab:" "v@:@" stub_58

  -- insertBacktab:
  stub_59 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertBacktab rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertBacktab:" "v@:@" stub_59

  -- insertNewline:
  stub_60 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertNewline rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertNewline:" "v@:@" stub_60

  -- insertParagraphSeparator:
  stub_61 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertParagraphSeparator rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertParagraphSeparator:" "v@:@" stub_61

  -- insertNewlineIgnoringFieldEditor:
  stub_62 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertNewlineIgnoringFieldEditor rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertNewlineIgnoringFieldEditor:" "v@:@" stub_62

  -- insertTabIgnoringFieldEditor:
  stub_63 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertTabIgnoringFieldEditor rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertTabIgnoringFieldEditor:" "v@:@" stub_63

  -- insertLineBreak:
  stub_64 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertLineBreak rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertLineBreak:" "v@:@" stub_64

  -- insertContainerBreak:
  stub_65 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertContainerBreak rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertContainerBreak:" "v@:@" stub_65

  -- insertSingleQuoteIgnoringSubstitution:
  stub_66 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertSingleQuoteIgnoringSubstitution rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertSingleQuoteIgnoringSubstitution:" "v@:@" stub_66

  -- insertDoubleQuoteIgnoringSubstitution:
  stub_67 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _insertDoubleQuoteIgnoringSubstitution rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertDoubleQuoteIgnoringSubstitution:" "v@:@" stub_67

  -- changeCaseOfLetter:
  stub_68 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _changeCaseOfLetter rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "changeCaseOfLetter:" "v@:@" stub_68

  -- uppercaseWord:
  stub_69 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _uppercaseWord rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "uppercaseWord:" "v@:@" stub_69

  -- lowercaseWord:
  stub_70 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _lowercaseWord rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lowercaseWord:" "v@:@" stub_70

  -- capitalizeWord:
  stub_71 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _capitalizeWord rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "capitalizeWord:" "v@:@" stub_71

  -- deleteForward:
  stub_72 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteForward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteForward:" "v@:@" stub_72

  -- deleteBackward:
  stub_73 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteBackward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteBackward:" "v@:@" stub_73

  -- deleteBackwardByDecomposingPreviousCharacter:
  stub_74 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteBackwardByDecomposingPreviousCharacter rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteBackwardByDecomposingPreviousCharacter:" "v@:@" stub_74

  -- deleteWordForward:
  stub_75 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteWordForward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteWordForward:" "v@:@" stub_75

  -- deleteWordBackward:
  stub_76 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteWordBackward rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteWordBackward:" "v@:@" stub_76

  -- deleteToBeginningOfLine:
  stub_77 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteToBeginningOfLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteToBeginningOfLine:" "v@:@" stub_77

  -- deleteToEndOfLine:
  stub_78 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteToEndOfLine rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteToEndOfLine:" "v@:@" stub_78

  -- deleteToBeginningOfParagraph:
  stub_79 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteToBeginningOfParagraph rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteToBeginningOfParagraph:" "v@:@" stub_79

  -- deleteToEndOfParagraph:
  stub_80 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteToEndOfParagraph rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteToEndOfParagraph:" "v@:@" stub_80

  -- yank:
  stub_81 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _yank rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "yank:" "v@:@" stub_81

  -- complete:
  stub_82 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _complete rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "complete:" "v@:@" stub_82

  -- setMark:
  stub_83 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _setMark rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setMark:" "v@:@" stub_83

  -- deleteToMark:
  stub_84 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _deleteToMark rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deleteToMark:" "v@:@" stub_84

  -- selectToMark:
  stub_85 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _selectToMark rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectToMark:" "v@:@" stub_85

  -- swapWithMark:
  stub_86 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _swapWithMark rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "swapWithMark:" "v@:@" stub_86

  -- cancelOperation:
  stub_87 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _cancelOperation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cancelOperation:" "v@:@" stub_87

  -- makeBaseWritingDirectionNatural:
  stub_88 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _makeBaseWritingDirectionNatural rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "makeBaseWritingDirectionNatural:" "v@:@" stub_88

  -- makeBaseWritingDirectionLeftToRight:
  stub_89 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _makeBaseWritingDirectionLeftToRight rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "makeBaseWritingDirectionLeftToRight:" "v@:@" stub_89

  -- makeBaseWritingDirectionRightToLeft:
  stub_90 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _makeBaseWritingDirectionRightToLeft rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "makeBaseWritingDirectionRightToLeft:" "v@:@" stub_90

  -- makeTextWritingDirectionNatural:
  stub_91 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _makeTextWritingDirectionNatural rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "makeTextWritingDirectionNatural:" "v@:@" stub_91

  -- makeTextWritingDirectionLeftToRight:
  stub_92 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _makeTextWritingDirectionLeftToRight rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "makeTextWritingDirectionLeftToRight:" "v@:@" stub_92

  -- makeTextWritingDirectionRightToLeft:
  stub_93 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _makeTextWritingDirectionRightToLeft rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "makeTextWritingDirectionRightToLeft:" "v@:@" stub_93

  -- quickLookPreviewItems:
  stub_94 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _quickLookPreviewItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "quickLookPreviewItems:" "v@:@" stub_94

  -- showContextMenuForSelection:
  stub_95 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    case _showContextMenuForSelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "showContextMenuForSelection:" "v@:@" stub_95

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStandardKeyBindingRespondingOverrides
    if queriedSel == sel_insertText then pure (maybe 0 (const 1) (_insertText rec_))
    else if queriedSel == sel_doCommandBySelector then pure (maybe 0 (const 1) (_doCommandBySelector rec_))
    else if queriedSel == sel_moveForward then pure (maybe 0 (const 1) (_moveForward rec_))
    else if queriedSel == sel_moveRight then pure (maybe 0 (const 1) (_moveRight rec_))
    else if queriedSel == sel_moveBackward then pure (maybe 0 (const 1) (_moveBackward rec_))
    else if queriedSel == sel_moveLeft then pure (maybe 0 (const 1) (_moveLeft rec_))
    else if queriedSel == sel_moveUp then pure (maybe 0 (const 1) (_moveUp rec_))
    else if queriedSel == sel_moveDown then pure (maybe 0 (const 1) (_moveDown rec_))
    else if queriedSel == sel_moveWordForward then pure (maybe 0 (const 1) (_moveWordForward rec_))
    else if queriedSel == sel_moveWordBackward then pure (maybe 0 (const 1) (_moveWordBackward rec_))
    else if queriedSel == sel_moveToBeginningOfLine then pure (maybe 0 (const 1) (_moveToBeginningOfLine rec_))
    else if queriedSel == sel_moveToEndOfLine then pure (maybe 0 (const 1) (_moveToEndOfLine rec_))
    else if queriedSel == sel_moveToBeginningOfParagraph then pure (maybe 0 (const 1) (_moveToBeginningOfParagraph rec_))
    else if queriedSel == sel_moveToEndOfParagraph then pure (maybe 0 (const 1) (_moveToEndOfParagraph rec_))
    else if queriedSel == sel_moveToEndOfDocument then pure (maybe 0 (const 1) (_moveToEndOfDocument rec_))
    else if queriedSel == sel_moveToBeginningOfDocument then pure (maybe 0 (const 1) (_moveToBeginningOfDocument rec_))
    else if queriedSel == sel_pageDown then pure (maybe 0 (const 1) (_pageDown rec_))
    else if queriedSel == sel_pageUp then pure (maybe 0 (const 1) (_pageUp rec_))
    else if queriedSel == sel_centerSelectionInVisibleArea then pure (maybe 0 (const 1) (_centerSelectionInVisibleArea rec_))
    else if queriedSel == sel_moveBackwardAndModifySelection then pure (maybe 0 (const 1) (_moveBackwardAndModifySelection rec_))
    else if queriedSel == sel_moveForwardAndModifySelection then pure (maybe 0 (const 1) (_moveForwardAndModifySelection rec_))
    else if queriedSel == sel_moveWordForwardAndModifySelection then pure (maybe 0 (const 1) (_moveWordForwardAndModifySelection rec_))
    else if queriedSel == sel_moveWordBackwardAndModifySelection then pure (maybe 0 (const 1) (_moveWordBackwardAndModifySelection rec_))
    else if queriedSel == sel_moveUpAndModifySelection then pure (maybe 0 (const 1) (_moveUpAndModifySelection rec_))
    else if queriedSel == sel_moveDownAndModifySelection then pure (maybe 0 (const 1) (_moveDownAndModifySelection rec_))
    else if queriedSel == sel_moveToBeginningOfLineAndModifySelection then pure (maybe 0 (const 1) (_moveToBeginningOfLineAndModifySelection rec_))
    else if queriedSel == sel_moveToEndOfLineAndModifySelection then pure (maybe 0 (const 1) (_moveToEndOfLineAndModifySelection rec_))
    else if queriedSel == sel_moveToBeginningOfParagraphAndModifySelection then pure (maybe 0 (const 1) (_moveToBeginningOfParagraphAndModifySelection rec_))
    else if queriedSel == sel_moveToEndOfParagraphAndModifySelection then pure (maybe 0 (const 1) (_moveToEndOfParagraphAndModifySelection rec_))
    else if queriedSel == sel_moveToEndOfDocumentAndModifySelection then pure (maybe 0 (const 1) (_moveToEndOfDocumentAndModifySelection rec_))
    else if queriedSel == sel_moveToBeginningOfDocumentAndModifySelection then pure (maybe 0 (const 1) (_moveToBeginningOfDocumentAndModifySelection rec_))
    else if queriedSel == sel_pageDownAndModifySelection then pure (maybe 0 (const 1) (_pageDownAndModifySelection rec_))
    else if queriedSel == sel_pageUpAndModifySelection then pure (maybe 0 (const 1) (_pageUpAndModifySelection rec_))
    else if queriedSel == sel_moveParagraphForwardAndModifySelection then pure (maybe 0 (const 1) (_moveParagraphForwardAndModifySelection rec_))
    else if queriedSel == sel_moveParagraphBackwardAndModifySelection then pure (maybe 0 (const 1) (_moveParagraphBackwardAndModifySelection rec_))
    else if queriedSel == sel_moveWordRight then pure (maybe 0 (const 1) (_moveWordRight rec_))
    else if queriedSel == sel_moveWordLeft then pure (maybe 0 (const 1) (_moveWordLeft rec_))
    else if queriedSel == sel_moveRightAndModifySelection then pure (maybe 0 (const 1) (_moveRightAndModifySelection rec_))
    else if queriedSel == sel_moveLeftAndModifySelection then pure (maybe 0 (const 1) (_moveLeftAndModifySelection rec_))
    else if queriedSel == sel_moveWordRightAndModifySelection then pure (maybe 0 (const 1) (_moveWordRightAndModifySelection rec_))
    else if queriedSel == sel_moveWordLeftAndModifySelection then pure (maybe 0 (const 1) (_moveWordLeftAndModifySelection rec_))
    else if queriedSel == sel_moveToLeftEndOfLine then pure (maybe 0 (const 1) (_moveToLeftEndOfLine rec_))
    else if queriedSel == sel_moveToRightEndOfLine then pure (maybe 0 (const 1) (_moveToRightEndOfLine rec_))
    else if queriedSel == sel_moveToLeftEndOfLineAndModifySelection then pure (maybe 0 (const 1) (_moveToLeftEndOfLineAndModifySelection rec_))
    else if queriedSel == sel_moveToRightEndOfLineAndModifySelection then pure (maybe 0 (const 1) (_moveToRightEndOfLineAndModifySelection rec_))
    else if queriedSel == sel_scrollPageUp then pure (maybe 0 (const 1) (_scrollPageUp rec_))
    else if queriedSel == sel_scrollPageDown then pure (maybe 0 (const 1) (_scrollPageDown rec_))
    else if queriedSel == sel_scrollLineUp then pure (maybe 0 (const 1) (_scrollLineUp rec_))
    else if queriedSel == sel_scrollLineDown then pure (maybe 0 (const 1) (_scrollLineDown rec_))
    else if queriedSel == sel_scrollToBeginningOfDocument then pure (maybe 0 (const 1) (_scrollToBeginningOfDocument rec_))
    else if queriedSel == sel_scrollToEndOfDocument then pure (maybe 0 (const 1) (_scrollToEndOfDocument rec_))
    else if queriedSel == sel_transpose then pure (maybe 0 (const 1) (_transpose rec_))
    else if queriedSel == sel_transposeWords then pure (maybe 0 (const 1) (_transposeWords rec_))
    else if queriedSel == sel_selectAll then pure (maybe 0 (const 1) (_selectAll rec_))
    else if queriedSel == sel_selectParagraph then pure (maybe 0 (const 1) (_selectParagraph rec_))
    else if queriedSel == sel_selectLine then pure (maybe 0 (const 1) (_selectLine rec_))
    else if queriedSel == sel_selectWord then pure (maybe 0 (const 1) (_selectWord rec_))
    else if queriedSel == sel_indent then pure (maybe 0 (const 1) (_indent rec_))
    else if queriedSel == sel_insertTab then pure (maybe 0 (const 1) (_insertTab rec_))
    else if queriedSel == sel_insertBacktab then pure (maybe 0 (const 1) (_insertBacktab rec_))
    else if queriedSel == sel_insertNewline then pure (maybe 0 (const 1) (_insertNewline rec_))
    else if queriedSel == sel_insertParagraphSeparator then pure (maybe 0 (const 1) (_insertParagraphSeparator rec_))
    else if queriedSel == sel_insertNewlineIgnoringFieldEditor then pure (maybe 0 (const 1) (_insertNewlineIgnoringFieldEditor rec_))
    else if queriedSel == sel_insertTabIgnoringFieldEditor then pure (maybe 0 (const 1) (_insertTabIgnoringFieldEditor rec_))
    else if queriedSel == sel_insertLineBreak then pure (maybe 0 (const 1) (_insertLineBreak rec_))
    else if queriedSel == sel_insertContainerBreak then pure (maybe 0 (const 1) (_insertContainerBreak rec_))
    else if queriedSel == sel_insertSingleQuoteIgnoringSubstitution then pure (maybe 0 (const 1) (_insertSingleQuoteIgnoringSubstitution rec_))
    else if queriedSel == sel_insertDoubleQuoteIgnoringSubstitution then pure (maybe 0 (const 1) (_insertDoubleQuoteIgnoringSubstitution rec_))
    else if queriedSel == sel_changeCaseOfLetter then pure (maybe 0 (const 1) (_changeCaseOfLetter rec_))
    else if queriedSel == sel_uppercaseWord then pure (maybe 0 (const 1) (_uppercaseWord rec_))
    else if queriedSel == sel_lowercaseWord then pure (maybe 0 (const 1) (_lowercaseWord rec_))
    else if queriedSel == sel_capitalizeWord then pure (maybe 0 (const 1) (_capitalizeWord rec_))
    else if queriedSel == sel_deleteForward then pure (maybe 0 (const 1) (_deleteForward rec_))
    else if queriedSel == sel_deleteBackward then pure (maybe 0 (const 1) (_deleteBackward rec_))
    else if queriedSel == sel_deleteBackwardByDecomposingPreviousCharacter then pure (maybe 0 (const 1) (_deleteBackwardByDecomposingPreviousCharacter rec_))
    else if queriedSel == sel_deleteWordForward then pure (maybe 0 (const 1) (_deleteWordForward rec_))
    else if queriedSel == sel_deleteWordBackward then pure (maybe 0 (const 1) (_deleteWordBackward rec_))
    else if queriedSel == sel_deleteToBeginningOfLine then pure (maybe 0 (const 1) (_deleteToBeginningOfLine rec_))
    else if queriedSel == sel_deleteToEndOfLine then pure (maybe 0 (const 1) (_deleteToEndOfLine rec_))
    else if queriedSel == sel_deleteToBeginningOfParagraph then pure (maybe 0 (const 1) (_deleteToBeginningOfParagraph rec_))
    else if queriedSel == sel_deleteToEndOfParagraph then pure (maybe 0 (const 1) (_deleteToEndOfParagraph rec_))
    else if queriedSel == sel_yank then pure (maybe 0 (const 1) (_yank rec_))
    else if queriedSel == sel_complete then pure (maybe 0 (const 1) (_complete rec_))
    else if queriedSel == sel_setMark then pure (maybe 0 (const 1) (_setMark rec_))
    else if queriedSel == sel_deleteToMark then pure (maybe 0 (const 1) (_deleteToMark rec_))
    else if queriedSel == sel_selectToMark then pure (maybe 0 (const 1) (_selectToMark rec_))
    else if queriedSel == sel_swapWithMark then pure (maybe 0 (const 1) (_swapWithMark rec_))
    else if queriedSel == sel_cancelOperation then pure (maybe 0 (const 1) (_cancelOperation rec_))
    else if queriedSel == sel_makeBaseWritingDirectionNatural then pure (maybe 0 (const 1) (_makeBaseWritingDirectionNatural rec_))
    else if queriedSel == sel_makeBaseWritingDirectionLeftToRight then pure (maybe 0 (const 1) (_makeBaseWritingDirectionLeftToRight rec_))
    else if queriedSel == sel_makeBaseWritingDirectionRightToLeft then pure (maybe 0 (const 1) (_makeBaseWritingDirectionRightToLeft rec_))
    else if queriedSel == sel_makeTextWritingDirectionNatural then pure (maybe 0 (const 1) (_makeTextWritingDirectionNatural rec_))
    else if queriedSel == sel_makeTextWritingDirectionLeftToRight then pure (maybe 0 (const 1) (_makeTextWritingDirectionLeftToRight rec_))
    else if queriedSel == sel_makeTextWritingDirectionRightToLeft then pure (maybe 0 (const 1) (_makeTextWritingDirectionRightToLeft rec_))
    else if queriedSel == sel_quickLookPreviewItems then pure (maybe 0 (const 1) (_quickLookPreviewItems rec_))
    else if queriedSel == sel_showContextMenuForSelection then pure (maybe 0 (const 1) (_showContextMenuForSelection rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newNSStandardKeyBindingResponding :: NSStandardKeyBindingRespondingOverrides -> IO RawId
newNSStandardKeyBindingResponding overrides = do
  inst <- class_createInstance nsStandardKeyBindingRespondingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
