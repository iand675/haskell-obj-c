{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextViewDelegate defaultNSTextViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextViewDelegate
  ( NSTextViewDelegateOverrides(..)
  , defaultNSTextViewDelegateOverrides
  , newNSTextViewDelegate
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

-- | Overrides record for @\@protocol NSTextViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextViewDelegateOverrides = NSTextViewDelegateOverrides
  { _textView_clickedOnLink_atIndex :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  , _textView_writablePasteboardTypesForCell_atIndex :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _textView_writeCell_atIndex_toPasteboard_type :: !(Maybe (RawId -> RawId -> Int -> RawId -> RawId -> IO Bool))
  , _textView_willChangeSelectionFromCharacterRanges_toCharacterRanges :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _textView_shouldChangeTextInRanges_replacementStrings :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _textView_shouldChangeTypingAttributes_toAttributes :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _textViewDidChangeSelection :: !(Maybe (RawId -> IO ()))
  , _textViewDidChangeTypingAttributes :: !(Maybe (RawId -> IO ()))
  , _textView_willDisplayToolTip_forCharacterAtIndex :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _textView_doCommandBySelector :: !(Maybe (RawId -> Sel -> IO Bool))
  , _textView_menu_forEvent_atIndex :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO RawId))
  , _textView_URLForContentsOfTextAttachment_atIndex :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _textView_willShowSharingServicePicker_forItems :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _undoManagerForTextView :: !(Maybe (RawId -> IO RawId))
  , _textView_shouldUpdateTouchBarItemIdentifiers :: !(Maybe (RawId -> RawId -> IO RawId))
  , _textView_shouldSelectCandidateAtIndex :: !(Maybe (RawId -> Int -> IO Bool))
  , _textViewWritingToolsWillBegin :: !(Maybe (RawId -> IO ()))
  , _textViewWritingToolsDidEnd :: !(Maybe (RawId -> IO ()))
  , _textView_clickedOnLink :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextViewDelegateOverrides :: NSTextViewDelegateOverrides
defaultNSTextViewDelegateOverrides = NSTextViewDelegateOverrides
  { _textView_clickedOnLink_atIndex = Nothing
  , _textView_writablePasteboardTypesForCell_atIndex = Nothing
  , _textView_writeCell_atIndex_toPasteboard_type = Nothing
  , _textView_willChangeSelectionFromCharacterRanges_toCharacterRanges = Nothing
  , _textView_shouldChangeTextInRanges_replacementStrings = Nothing
  , _textView_shouldChangeTypingAttributes_toAttributes = Nothing
  , _textViewDidChangeSelection = Nothing
  , _textViewDidChangeTypingAttributes = Nothing
  , _textView_willDisplayToolTip_forCharacterAtIndex = Nothing
  , _textView_doCommandBySelector = Nothing
  , _textView_menu_forEvent_atIndex = Nothing
  , _textView_URLForContentsOfTextAttachment_atIndex = Nothing
  , _textView_willShowSharingServicePicker_forItems = Nothing
  , _undoManagerForTextView = Nothing
  , _textView_shouldUpdateTouchBarItemIdentifiers = Nothing
  , _textView_shouldSelectCandidateAtIndex = Nothing
  , _textViewWritingToolsWillBegin = Nothing
  , _textViewWritingToolsDidEnd = Nothing
  , _textView_clickedOnLink = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_sel_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_Q_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextViewDelegateDelegateClass #-}
nsTextViewDelegateDelegateClass :: Class
nsTextViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_textView_clickedOnLink_atIndex = unSelector (mkSelector "textView:clickedOnLink:atIndex:")
      sel_textView_writablePasteboardTypesForCell_atIndex = unSelector (mkSelector "textView:writablePasteboardTypesForCell:atIndex:")
      sel_textView_writeCell_atIndex_toPasteboard_type = unSelector (mkSelector "textView:writeCell:atIndex:toPasteboard:type:")
      sel_textView_willChangeSelectionFromCharacterRanges_toCharacterRanges = unSelector (mkSelector "textView:willChangeSelectionFromCharacterRanges:toCharacterRanges:")
      sel_textView_shouldChangeTextInRanges_replacementStrings = unSelector (mkSelector "textView:shouldChangeTextInRanges:replacementStrings:")
      sel_textView_shouldChangeTypingAttributes_toAttributes = unSelector (mkSelector "textView:shouldChangeTypingAttributes:toAttributes:")
      sel_textViewDidChangeSelection = unSelector (mkSelector "textViewDidChangeSelection:")
      sel_textViewDidChangeTypingAttributes = unSelector (mkSelector "textViewDidChangeTypingAttributes:")
      sel_textView_willDisplayToolTip_forCharacterAtIndex = unSelector (mkSelector "textView:willDisplayToolTip:forCharacterAtIndex:")
      sel_textView_doCommandBySelector = unSelector (mkSelector "textView:doCommandBySelector:")
      sel_textView_menu_forEvent_atIndex = unSelector (mkSelector "textView:menu:forEvent:atIndex:")
      sel_textView_URLForContentsOfTextAttachment_atIndex = unSelector (mkSelector "textView:URLForContentsOfTextAttachment:atIndex:")
      sel_textView_willShowSharingServicePicker_forItems = unSelector (mkSelector "textView:willShowSharingServicePicker:forItems:")
      sel_undoManagerForTextView = unSelector (mkSelector "undoManagerForTextView:")
      sel_textView_shouldUpdateTouchBarItemIdentifiers = unSelector (mkSelector "textView:shouldUpdateTouchBarItemIdentifiers:")
      sel_textView_shouldSelectCandidateAtIndex = unSelector (mkSelector "textView:shouldSelectCandidateAtIndex:")
      sel_textViewWritingToolsWillBegin = unSelector (mkSelector "textViewWritingToolsWillBegin:")
      sel_textViewWritingToolsDidEnd = unSelector (mkSelector "textViewWritingToolsDidEnd:")
      sel_textView_clickedOnLink = unSelector (mkSelector "textView:clickedOnLink:")
  -- textView:clickedOnLink:atIndex:
  stub_0 <- wrap_at_at_Q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_clickedOnLink_atIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "textView:clickedOnLink:atIndex:" "B@:@@Q" stub_0

  -- textView:writablePasteboardTypesForCell:atIndex:
  stub_1 <- wrap_at_at_Q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_writablePasteboardTypesForCell_atIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:writablePasteboardTypesForCell:atIndex:" "@@:@@Q" stub_1

  -- textView:writeCell:atIndex:toPasteboard:type:
  stub_2 <- wrap_at_at_Q_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_writeCell_atIndex_toPasteboard_type rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (RawId arg3) (RawId arg4)
        pure (if r then 1 else 0)
  addObjCMethod cls "textView:writeCell:atIndex:toPasteboard:type:" "B@:@@Q@@" stub_2

  -- textView:willChangeSelectionFromCharacterRanges:toCharacterRanges:
  stub_3 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_willChangeSelectionFromCharacterRanges_toCharacterRanges rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:willChangeSelectionFromCharacterRanges:toCharacterRanges:" "@@:@@@" stub_3

  -- textView:shouldChangeTextInRanges:replacementStrings:
  stub_4 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_shouldChangeTextInRanges_replacementStrings rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "textView:shouldChangeTextInRanges:replacementStrings:" "B@:@@@" stub_4

  -- textView:shouldChangeTypingAttributes:toAttributes:
  stub_5 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_shouldChangeTypingAttributes_toAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:shouldChangeTypingAttributes:toAttributes:" "@@:@@@" stub_5

  -- textViewDidChangeSelection:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textViewDidChangeSelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textViewDidChangeSelection:" "v@:@" stub_6

  -- textViewDidChangeTypingAttributes:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textViewDidChangeTypingAttributes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textViewDidChangeTypingAttributes:" "v@:@" stub_7

  -- textView:willDisplayToolTip:forCharacterAtIndex:
  stub_8 <- wrap_at_at_Q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_willDisplayToolTip_forCharacterAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:willDisplayToolTip:forCharacterAtIndex:" "@@:@@Q" stub_8

  -- textView:doCommandBySelector:
  stub_9 <- wrap_at_sel_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_doCommandBySelector rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (Selector arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "textView:doCommandBySelector:" "B@:@:" stub_9

  -- textView:menu:forEvent:atIndex:
  stub_10 <- wrap_at_at_at_Q_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_menu_forEvent_atIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:menu:forEvent:atIndex:" "@@:@@@Q" stub_10

  -- textView:URLForContentsOfTextAttachment:atIndex:
  stub_11 <- wrap_at_at_Q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_URLForContentsOfTextAttachment_atIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:URLForContentsOfTextAttachment:atIndex:" "@@:@@Q" stub_11

  -- textView:willShowSharingServicePicker:forItems:
  stub_12 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_willShowSharingServicePicker_forItems rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:willShowSharingServicePicker:forItems:" "@@:@@@" stub_12

  -- undoManagerForTextView:
  stub_13 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _undoManagerForTextView rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "undoManagerForTextView:" "@@:@" stub_13

  -- textView:shouldUpdateTouchBarItemIdentifiers:
  stub_14 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_shouldUpdateTouchBarItemIdentifiers rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textView:shouldUpdateTouchBarItemIdentifiers:" "@@:@@" stub_14

  -- textView:shouldSelectCandidateAtIndex:
  stub_15 <- wrap_at_Q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_shouldSelectCandidateAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "textView:shouldSelectCandidateAtIndex:" "B@:@Q" stub_15

  -- textViewWritingToolsWillBegin:
  stub_16 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textViewWritingToolsWillBegin rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textViewWritingToolsWillBegin:" "v@:@" stub_16

  -- textViewWritingToolsDidEnd:
  stub_17 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textViewWritingToolsDidEnd rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textViewWritingToolsDidEnd:" "v@:@" stub_17

  -- textView:clickedOnLink:
  stub_18 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    case _textView_clickedOnLink rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "textView:clickedOnLink:" "B@:@@" stub_18

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewDelegateOverrides
    if queriedSel == sel_textView_clickedOnLink_atIndex then pure (maybe 0 (const 1) (_textView_clickedOnLink_atIndex rec_))
    else if queriedSel == sel_textView_writablePasteboardTypesForCell_atIndex then pure (maybe 0 (const 1) (_textView_writablePasteboardTypesForCell_atIndex rec_))
    else if queriedSel == sel_textView_writeCell_atIndex_toPasteboard_type then pure (maybe 0 (const 1) (_textView_writeCell_atIndex_toPasteboard_type rec_))
    else if queriedSel == sel_textView_willChangeSelectionFromCharacterRanges_toCharacterRanges then pure (maybe 0 (const 1) (_textView_willChangeSelectionFromCharacterRanges_toCharacterRanges rec_))
    else if queriedSel == sel_textView_shouldChangeTextInRanges_replacementStrings then pure (maybe 0 (const 1) (_textView_shouldChangeTextInRanges_replacementStrings rec_))
    else if queriedSel == sel_textView_shouldChangeTypingAttributes_toAttributes then pure (maybe 0 (const 1) (_textView_shouldChangeTypingAttributes_toAttributes rec_))
    else if queriedSel == sel_textViewDidChangeSelection then pure (maybe 0 (const 1) (_textViewDidChangeSelection rec_))
    else if queriedSel == sel_textViewDidChangeTypingAttributes then pure (maybe 0 (const 1) (_textViewDidChangeTypingAttributes rec_))
    else if queriedSel == sel_textView_willDisplayToolTip_forCharacterAtIndex then pure (maybe 0 (const 1) (_textView_willDisplayToolTip_forCharacterAtIndex rec_))
    else if queriedSel == sel_textView_doCommandBySelector then pure (maybe 0 (const 1) (_textView_doCommandBySelector rec_))
    else if queriedSel == sel_textView_menu_forEvent_atIndex then pure (maybe 0 (const 1) (_textView_menu_forEvent_atIndex rec_))
    else if queriedSel == sel_textView_URLForContentsOfTextAttachment_atIndex then pure (maybe 0 (const 1) (_textView_URLForContentsOfTextAttachment_atIndex rec_))
    else if queriedSel == sel_textView_willShowSharingServicePicker_forItems then pure (maybe 0 (const 1) (_textView_willShowSharingServicePicker_forItems rec_))
    else if queriedSel == sel_undoManagerForTextView then pure (maybe 0 (const 1) (_undoManagerForTextView rec_))
    else if queriedSel == sel_textView_shouldUpdateTouchBarItemIdentifiers then pure (maybe 0 (const 1) (_textView_shouldUpdateTouchBarItemIdentifiers rec_))
    else if queriedSel == sel_textView_shouldSelectCandidateAtIndex then pure (maybe 0 (const 1) (_textView_shouldSelectCandidateAtIndex rec_))
    else if queriedSel == sel_textViewWritingToolsWillBegin then pure (maybe 0 (const 1) (_textViewWritingToolsWillBegin rec_))
    else if queriedSel == sel_textViewWritingToolsDidEnd then pure (maybe 0 (const 1) (_textViewWritingToolsDidEnd rec_))
    else if queriedSel == sel_textView_clickedOnLink then pure (maybe 0 (const 1) (_textView_clickedOnLink rec_))
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
newNSTextViewDelegate :: NSTextViewDelegateOverrides -> IO RawId
newNSTextViewDelegate overrides = do
  inst <- class_createInstance nsTextViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
