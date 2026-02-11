{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CHHapticPatternPlayer@.
--
-- Usage:
--
-- @
-- delegate <- newCHHapticPatternPlayer defaultCHHapticPatternPlayerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreHaptics.Delegate.CHHapticPatternPlayer
  ( CHHapticPatternPlayerOverrides(..)
  , defaultCHHapticPatternPlayerOverrides
  , newCHHapticPatternPlayer
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

-- | Overrides record for @\@protocol CHHapticPatternPlayer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CHHapticPatternPlayerOverrides = CHHapticPatternPlayerOverrides
  { _startAtTime_error :: !(Maybe (Double -> RawId -> IO Bool))
  , _stopAtTime_error :: !(Maybe (Double -> RawId -> IO Bool))
  , _sendParameters_atTime_error :: !(Maybe (RawId -> Double -> RawId -> IO Bool))
  , _scheduleParameterCurve_atTime_error :: !(Maybe (RawId -> Double -> RawId -> IO Bool))
  , _cancelAndReturnError :: !(Maybe (RawId -> IO Bool))
  , _isMuted :: !(Maybe (IO Bool))
  , _setIsMuted :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCHHapticPatternPlayerOverrides :: CHHapticPatternPlayerOverrides
defaultCHHapticPatternPlayerOverrides = CHHapticPatternPlayerOverrides
  { _startAtTime_error = Nothing
  , _stopAtTime_error = Nothing
  , _sendParameters_atTime_error = Nothing
  , _scheduleParameterCurve_atTime_error = Nothing
  , _cancelAndReturnError = Nothing
  , _isMuted = Nothing
  , _setIsMuted = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_d_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_d_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE chHapticPatternPlayerDelegateClass #-}
chHapticPatternPlayerDelegateClass :: Class
chHapticPatternPlayerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCHHapticPatternPlayer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_startAtTime_error = unSelector (mkSelector "startAtTime:error:")
      sel_stopAtTime_error = unSelector (mkSelector "stopAtTime:error:")
      sel_sendParameters_atTime_error = unSelector (mkSelector "sendParameters:atTime:error:")
      sel_scheduleParameterCurve_atTime_error = unSelector (mkSelector "scheduleParameterCurve:atTime:error:")
      sel_cancelAndReturnError = unSelector (mkSelector "cancelAndReturnError:")
      sel_isMuted = unSelector (mkSelector "isMuted")
      sel_setIsMuted = unSelector (mkSelector "setIsMuted:")
  -- startAtTime:error:
  stub_0 <- wrap_d_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _startAtTime_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (realToFrac arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "startAtTime:error:" "B@:d@" stub_0

  -- stopAtTime:error:
  stub_1 <- wrap_d_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _stopAtTime_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (realToFrac arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "stopAtTime:error:" "B@:d@" stub_1

  -- sendParameters:atTime:error:
  stub_2 <- wrap_at_d_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _sendParameters_atTime_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "sendParameters:atTime:error:" "B@:@d@" stub_2

  -- scheduleParameterCurve:atTime:error:
  stub_3 <- wrap_at_d_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _scheduleParameterCurve_atTime_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "scheduleParameterCurve:atTime:error:" "B@:@d@" stub_3

  -- cancelAndReturnError:
  stub_4 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _cancelAndReturnError rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "cancelAndReturnError:" "B@:@" stub_4

  -- isMuted
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _isMuted rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "isMuted" "B@:" stub_5

  -- setIsMuted:
  stub_6 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    case _setIsMuted rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setIsMuted:" "v@:B" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticPatternPlayerOverrides
    if queriedSel == sel_startAtTime_error then pure (maybe 0 (const 1) (_startAtTime_error rec_))
    else if queriedSel == sel_stopAtTime_error then pure (maybe 0 (const 1) (_stopAtTime_error rec_))
    else if queriedSel == sel_sendParameters_atTime_error then pure (maybe 0 (const 1) (_sendParameters_atTime_error rec_))
    else if queriedSel == sel_scheduleParameterCurve_atTime_error then pure (maybe 0 (const 1) (_scheduleParameterCurve_atTime_error rec_))
    else if queriedSel == sel_cancelAndReturnError then pure (maybe 0 (const 1) (_cancelAndReturnError rec_))
    else if queriedSel == sel_isMuted then pure (maybe 0 (const 1) (_isMuted rec_))
    else if queriedSel == sel_setIsMuted then pure (maybe 0 (const 1) (_setIsMuted rec_))
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
newCHHapticPatternPlayer :: CHHapticPatternPlayerOverrides -> IO RawId
newCHHapticPatternPlayer overrides = do
  inst <- class_createInstance chHapticPatternPlayerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
