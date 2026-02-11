{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol BETextInputDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newBETextInputDelegate defaultBETextInputDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.BrowserEngineKit.Delegate.BETextInputDelegate
  ( BETextInputDelegateOverrides(..)
  , defaultBETextInputDelegateOverrides
  , newBETextInputDelegate
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

-- | Overrides record for @\@protocol BETextInputDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data BETextInputDelegateOverrides = BETextInputDelegateOverrides
  { _shouldDeferEventHandlingToSystemForTextInput_context :: !(Maybe (RawId -> RawId -> IO Bool))
  , _textInput_setCandidateSuggestions :: !(Maybe (RawId -> RawId -> IO ()))
  , _selectionWillChangeForTextInput :: !(Maybe (RawId -> IO ()))
  , _selectionDidChangeForTextInput :: !(Maybe (RawId -> IO ()))
  , _textInput_deferReplaceTextActionToSystem :: !(Maybe (RawId -> RawId -> IO ()))
  , _invalidateTextEntryContextForTextInput :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultBETextInputDelegateOverrides :: BETextInputDelegateOverrides
defaultBETextInputDelegateOverrides = BETextInputDelegateOverrides
  { _shouldDeferEventHandlingToSystemForTextInput_context = Nothing
  , _textInput_setCandidateSuggestions = Nothing
  , _selectionWillChangeForTextInput = Nothing
  , _selectionDidChangeForTextInput = Nothing
  , _textInput_deferReplaceTextActionToSystem = Nothing
  , _invalidateTextEntryContextForTextInput = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE beTextInputDelegateDelegateClass #-}
beTextInputDelegateDelegateClass :: Class
beTextInputDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsBETextInputDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_shouldDeferEventHandlingToSystemForTextInput_context = unSelector (mkSelector "shouldDeferEventHandlingToSystemForTextInput:context:")
      sel_textInput_setCandidateSuggestions = unSelector (mkSelector "textInput:setCandidateSuggestions:")
      sel_selectionWillChangeForTextInput = unSelector (mkSelector "selectionWillChangeForTextInput:")
      sel_selectionDidChangeForTextInput = unSelector (mkSelector "selectionDidChangeForTextInput:")
      sel_textInput_deferReplaceTextActionToSystem = unSelector (mkSelector "textInput:deferReplaceTextActionToSystem:")
      sel_invalidateTextEntryContextForTextInput = unSelector (mkSelector "invalidateTextEntryContextForTextInput:")
  -- shouldDeferEventHandlingToSystemForTextInput:context:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    case _shouldDeferEventHandlingToSystemForTextInput_context rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldDeferEventHandlingToSystemForTextInput:context:" "B@:@@" stub_0

  -- textInput:setCandidateSuggestions:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    case _textInput_setCandidateSuggestions rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "textInput:setCandidateSuggestions:" "v@:@@" stub_1

  -- selectionWillChangeForTextInput:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    case _selectionWillChangeForTextInput rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectionWillChangeForTextInput:" "v@:@" stub_2

  -- selectionDidChangeForTextInput:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    case _selectionDidChangeForTextInput rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectionDidChangeForTextInput:" "v@:@" stub_3

  -- textInput:deferReplaceTextActionToSystem:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    case _textInput_deferReplaceTextActionToSystem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "textInput:deferReplaceTextActionToSystem:" "v@:@@" stub_4

  -- invalidateTextEntryContextForTextInput:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    case _invalidateTextEntryContextForTextInput rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "invalidateTextEntryContextForTextInput:" "v@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BETextInputDelegateOverrides
    if queriedSel == sel_shouldDeferEventHandlingToSystemForTextInput_context then pure (maybe 0 (const 1) (_shouldDeferEventHandlingToSystemForTextInput_context rec_))
    else if queriedSel == sel_textInput_setCandidateSuggestions then pure (maybe 0 (const 1) (_textInput_setCandidateSuggestions rec_))
    else if queriedSel == sel_selectionWillChangeForTextInput then pure (maybe 0 (const 1) (_selectionWillChangeForTextInput rec_))
    else if queriedSel == sel_selectionDidChangeForTextInput then pure (maybe 0 (const 1) (_selectionDidChangeForTextInput rec_))
    else if queriedSel == sel_textInput_deferReplaceTextActionToSystem then pure (maybe 0 (const 1) (_textInput_deferReplaceTextActionToSystem rec_))
    else if queriedSel == sel_invalidateTextEntryContextForTextInput then pure (maybe 0 (const 1) (_invalidateTextEntryContextForTextInput rec_))
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
newBETextInputDelegate :: BETextInputDelegateOverrides -> IO RawId
newBETextInputDelegate overrides = do
  inst <- class_createInstance beTextInputDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
