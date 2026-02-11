{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVSpeechSynthesizerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVSpeechSynthesizerDelegate defaultAVSpeechSynthesizerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFAudio.Delegate.AVSpeechSynthesizerDelegate
  ( AVSpeechSynthesizerDelegateOverrides(..)
  , defaultAVSpeechSynthesizerDelegateOverrides
  , newAVSpeechSynthesizerDelegate
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

-- | Overrides record for @\@protocol AVSpeechSynthesizerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVSpeechSynthesizerDelegateOverrides = AVSpeechSynthesizerDelegateOverrides
  { _speechSynthesizer_didStartSpeechUtterance :: !(Maybe (RawId -> RawId -> IO ()))
  , _speechSynthesizer_didFinishSpeechUtterance :: !(Maybe (RawId -> RawId -> IO ()))
  , _speechSynthesizer_didPauseSpeechUtterance :: !(Maybe (RawId -> RawId -> IO ()))
  , _speechSynthesizer_didContinueSpeechUtterance :: !(Maybe (RawId -> RawId -> IO ()))
  , _speechSynthesizer_didCancelSpeechUtterance :: !(Maybe (RawId -> RawId -> IO ()))
  , _speechSynthesizer_willSpeakMarker_utterance :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVSpeechSynthesizerDelegateOverrides :: AVSpeechSynthesizerDelegateOverrides
defaultAVSpeechSynthesizerDelegateOverrides = AVSpeechSynthesizerDelegateOverrides
  { _speechSynthesizer_didStartSpeechUtterance = Nothing
  , _speechSynthesizer_didFinishSpeechUtterance = Nothing
  , _speechSynthesizer_didPauseSpeechUtterance = Nothing
  , _speechSynthesizer_didContinueSpeechUtterance = Nothing
  , _speechSynthesizer_didCancelSpeechUtterance = Nothing
  , _speechSynthesizer_willSpeakMarker_utterance = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avSpeechSynthesizerDelegateDelegateClass #-}
avSpeechSynthesizerDelegateDelegateClass :: Class
avSpeechSynthesizerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVSpeechSynthesizerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_speechSynthesizer_didStartSpeechUtterance = unSelector (mkSelector "speechSynthesizer:didStartSpeechUtterance:")
      sel_speechSynthesizer_didFinishSpeechUtterance = unSelector (mkSelector "speechSynthesizer:didFinishSpeechUtterance:")
      sel_speechSynthesizer_didPauseSpeechUtterance = unSelector (mkSelector "speechSynthesizer:didPauseSpeechUtterance:")
      sel_speechSynthesizer_didContinueSpeechUtterance = unSelector (mkSelector "speechSynthesizer:didContinueSpeechUtterance:")
      sel_speechSynthesizer_didCancelSpeechUtterance = unSelector (mkSelector "speechSynthesizer:didCancelSpeechUtterance:")
      sel_speechSynthesizer_willSpeakMarker_utterance = unSelector (mkSelector "speechSynthesizer:willSpeakMarker:utterance:")
  -- speechSynthesizer:didStartSpeechUtterance:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didStartSpeechUtterance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "speechSynthesizer:didStartSpeechUtterance:" "v@:@@" stub_0

  -- speechSynthesizer:didFinishSpeechUtterance:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didFinishSpeechUtterance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "speechSynthesizer:didFinishSpeechUtterance:" "v@:@@" stub_1

  -- speechSynthesizer:didPauseSpeechUtterance:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didPauseSpeechUtterance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "speechSynthesizer:didPauseSpeechUtterance:" "v@:@@" stub_2

  -- speechSynthesizer:didContinueSpeechUtterance:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didContinueSpeechUtterance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "speechSynthesizer:didContinueSpeechUtterance:" "v@:@@" stub_3

  -- speechSynthesizer:didCancelSpeechUtterance:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didCancelSpeechUtterance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "speechSynthesizer:didCancelSpeechUtterance:" "v@:@@" stub_4

  -- speechSynthesizer:willSpeakMarker:utterance:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_willSpeakMarker_utterance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "speechSynthesizer:willSpeakMarker:utterance:" "v@:@@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVSpeechSynthesizerDelegateOverrides
    if queriedSel == sel_speechSynthesizer_didStartSpeechUtterance then pure (maybe 0 (const 1) (_speechSynthesizer_didStartSpeechUtterance rec_))
    else if queriedSel == sel_speechSynthesizer_didFinishSpeechUtterance then pure (maybe 0 (const 1) (_speechSynthesizer_didFinishSpeechUtterance rec_))
    else if queriedSel == sel_speechSynthesizer_didPauseSpeechUtterance then pure (maybe 0 (const 1) (_speechSynthesizer_didPauseSpeechUtterance rec_))
    else if queriedSel == sel_speechSynthesizer_didContinueSpeechUtterance then pure (maybe 0 (const 1) (_speechSynthesizer_didContinueSpeechUtterance rec_))
    else if queriedSel == sel_speechSynthesizer_didCancelSpeechUtterance then pure (maybe 0 (const 1) (_speechSynthesizer_didCancelSpeechUtterance rec_))
    else if queriedSel == sel_speechSynthesizer_willSpeakMarker_utterance then pure (maybe 0 (const 1) (_speechSynthesizer_willSpeakMarker_utterance rec_))
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
newAVSpeechSynthesizerDelegate :: AVSpeechSynthesizerDelegateOverrides -> IO RawId
newAVSpeechSynthesizerDelegate overrides = do
  inst <- class_createInstance avSpeechSynthesizerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
