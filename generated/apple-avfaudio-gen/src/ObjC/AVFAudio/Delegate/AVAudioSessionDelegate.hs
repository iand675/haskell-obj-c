{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAudioSessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVAudioSessionDelegate defaultAVAudioSessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFAudio.Delegate.AVAudioSessionDelegate
  ( AVAudioSessionDelegateOverrides(..)
  , defaultAVAudioSessionDelegateOverrides
  , newAVAudioSessionDelegate
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

-- | Overrides record for @\@protocol AVAudioSessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAudioSessionDelegateOverrides = AVAudioSessionDelegateOverrides
  { _beginInterruption :: !(Maybe (IO ()))
  , _endInterruptionWithFlags :: !(Maybe (Int -> IO ()))
  , _endInterruption :: !(Maybe (IO ()))
  , _inputIsAvailableChanged :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAudioSessionDelegateOverrides :: AVAudioSessionDelegateOverrides
defaultAVAudioSessionDelegateOverrides = AVAudioSessionDelegateOverrides
  { _beginInterruption = Nothing
  , _endInterruptionWithFlags = Nothing
  , _endInterruption = Nothing
  , _inputIsAvailableChanged = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avAudioSessionDelegateDelegateClass #-}
avAudioSessionDelegateDelegateClass :: Class
avAudioSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAudioSessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginInterruption = unSelector (mkSelector "beginInterruption")
      sel_endInterruptionWithFlags = unSelector (mkSelector "endInterruptionWithFlags:")
      sel_endInterruption = unSelector (mkSelector "endInterruption")
      sel_inputIsAvailableChanged = unSelector (mkSelector "inputIsAvailableChanged:")
  -- beginInterruption
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioSessionDelegateOverrides
    case _beginInterruption rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "beginInterruption" "v@:" stub_0

  -- endInterruptionWithFlags:
  stub_1 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioSessionDelegateOverrides
    case _endInterruptionWithFlags rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "endInterruptionWithFlags:" "v@:Q" stub_1

  -- endInterruption
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioSessionDelegateOverrides
    case _endInterruption rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "endInterruption" "v@:" stub_2

  -- inputIsAvailableChanged:
  stub_3 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioSessionDelegateOverrides
    case _inputIsAvailableChanged rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "inputIsAvailableChanged:" "v@:B" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioSessionDelegateOverrides
    if queriedSel == sel_beginInterruption then pure (maybe 0 (const 1) (_beginInterruption rec_))
    else if queriedSel == sel_endInterruptionWithFlags then pure (maybe 0 (const 1) (_endInterruptionWithFlags rec_))
    else if queriedSel == sel_endInterruption then pure (maybe 0 (const 1) (_endInterruption rec_))
    else if queriedSel == sel_inputIsAvailableChanged then pure (maybe 0 (const 1) (_inputIsAvailableChanged rec_))
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
newAVAudioSessionDelegate :: AVAudioSessionDelegateOverrides -> IO RawId
newAVAudioSessionDelegate overrides = do
  inst <- class_createInstance avAudioSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
