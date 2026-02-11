{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSpeechSynthesizerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSSpeechSynthesizerDelegate defaultNSSpeechSynthesizerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSSpeechSynthesizerDelegate
  ( NSSpeechSynthesizerDelegateOverrides(..)
  , defaultNSSpeechSynthesizerDelegateOverrides
  , newNSSpeechSynthesizerDelegate
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

-- | Overrides record for @\@protocol NSSpeechSynthesizerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSpeechSynthesizerDelegateOverrides = NSSpeechSynthesizerDelegateOverrides
  { _speechSynthesizer_didFinishSpeaking :: !(Maybe (RawId -> Bool -> IO ()))
  , _speechSynthesizer_didEncounterErrorAtIndex_ofString_message :: !(Maybe (RawId -> Int -> RawId -> RawId -> IO ()))
  , _speechSynthesizer_didEncounterSyncMessage :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSpeechSynthesizerDelegateOverrides :: NSSpeechSynthesizerDelegateOverrides
defaultNSSpeechSynthesizerDelegateOverrides = NSSpeechSynthesizerDelegateOverrides
  { _speechSynthesizer_didFinishSpeaking = Nothing
  , _speechSynthesizer_didEncounterErrorAtIndex_ofString_message = Nothing
  , _speechSynthesizer_didEncounterSyncMessage = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsSpeechSynthesizerDelegateDelegateClass #-}
nsSpeechSynthesizerDelegateDelegateClass :: Class
nsSpeechSynthesizerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSpeechSynthesizerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_speechSynthesizer_didFinishSpeaking = unSelector (mkSelector "speechSynthesizer:didFinishSpeaking:")
      sel_speechSynthesizer_didEncounterErrorAtIndex_ofString_message = unSelector (mkSelector "speechSynthesizer:didEncounterErrorAtIndex:ofString:message:")
      sel_speechSynthesizer_didEncounterSyncMessage = unSelector (mkSelector "speechSynthesizer:didEncounterSyncMessage:")
  -- speechSynthesizer:didFinishSpeaking:
  stub_0 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didFinishSpeaking rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "speechSynthesizer:didFinishSpeaking:" "v@:@B" stub_0

  -- speechSynthesizer:didEncounterErrorAtIndex:ofString:message:
  stub_1 <- wrap_at_Q_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didEncounterErrorAtIndex_ofString_message rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "speechSynthesizer:didEncounterErrorAtIndex:ofString:message:" "v@:@Q@@" stub_1

  -- speechSynthesizer:didEncounterSyncMessage:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpeechSynthesizerDelegateOverrides
    case _speechSynthesizer_didEncounterSyncMessage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "speechSynthesizer:didEncounterSyncMessage:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSpeechSynthesizerDelegateOverrides
    if queriedSel == sel_speechSynthesizer_didFinishSpeaking then pure (maybe 0 (const 1) (_speechSynthesizer_didFinishSpeaking rec_))
    else if queriedSel == sel_speechSynthesizer_didEncounterErrorAtIndex_ofString_message then pure (maybe 0 (const 1) (_speechSynthesizer_didEncounterErrorAtIndex_ofString_message rec_))
    else if queriedSel == sel_speechSynthesizer_didEncounterSyncMessage then pure (maybe 0 (const 1) (_speechSynthesizer_didEncounterSyncMessage rec_))
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
newNSSpeechSynthesizerDelegate :: NSSpeechSynthesizerDelegateOverrides -> IO RawId
newNSSpeechSynthesizerDelegate overrides = do
  inst <- class_createInstance nsSpeechSynthesizerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
