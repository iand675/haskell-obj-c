{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CHHapticAdvancedPatternPlayer@.
--
-- Usage:
--
-- @
-- delegate <- newCHHapticAdvancedPatternPlayer defaultCHHapticAdvancedPatternPlayerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreHaptics.Delegate.CHHapticAdvancedPatternPlayer
  ( CHHapticAdvancedPatternPlayerOverrides(..)
  , defaultCHHapticAdvancedPatternPlayerOverrides
  , newCHHapticAdvancedPatternPlayer
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

-- | Overrides record for @\@protocol CHHapticAdvancedPatternPlayer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CHHapticAdvancedPatternPlayerOverrides = CHHapticAdvancedPatternPlayerOverrides
  { _pauseAtTime_error :: !(Maybe (Double -> RawId -> IO Bool))
  , _resumeAtTime_error :: !(Maybe (Double -> RawId -> IO Bool))
  , _seekToOffset_error :: !(Maybe (Double -> RawId -> IO Bool))
  , _loopEnabled :: !(Maybe (IO Bool))
  , _setLoopEnabled :: !(Maybe (Bool -> IO ()))
  , _loopEnd :: !(Maybe (IO Double))
  , _setLoopEnd :: !(Maybe (Double -> IO ()))
  , _playbackRate :: !(Maybe (IO Float))
  , _setPlaybackRate :: !(Maybe (Float -> IO ()))
  , _isMuted :: !(Maybe (IO Bool))
  , _setIsMuted :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCHHapticAdvancedPatternPlayerOverrides :: CHHapticAdvancedPatternPlayerOverrides
defaultCHHapticAdvancedPatternPlayerOverrides = CHHapticAdvancedPatternPlayerOverrides
  { _pauseAtTime_error = Nothing
  , _resumeAtTime_error = Nothing
  , _seekToOffset_error = Nothing
  , _loopEnabled = Nothing
  , _setLoopEnabled = Nothing
  , _loopEnd = Nothing
  , _setLoopEnd = Nothing
  , _playbackRate = Nothing
  , _setPlaybackRate = Nothing
  , _isMuted = Nothing
  , _setIsMuted = Nothing
  }

foreign import ccall "wrapper"
  wrap_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))

foreign import ccall "wrapper"
  wrap_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_d_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE chHapticAdvancedPatternPlayerDelegateClass #-}
chHapticAdvancedPatternPlayerDelegateClass :: Class
chHapticAdvancedPatternPlayerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCHHapticAdvancedPatternPlayer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pauseAtTime_error = unSelector (mkSelector "pauseAtTime:error:")
      sel_resumeAtTime_error = unSelector (mkSelector "resumeAtTime:error:")
      sel_seekToOffset_error = unSelector (mkSelector "seekToOffset:error:")
      sel_loopEnabled = unSelector (mkSelector "loopEnabled")
      sel_setLoopEnabled = unSelector (mkSelector "setLoopEnabled:")
      sel_loopEnd = unSelector (mkSelector "loopEnd")
      sel_setLoopEnd = unSelector (mkSelector "setLoopEnd:")
      sel_playbackRate = unSelector (mkSelector "playbackRate")
      sel_setPlaybackRate = unSelector (mkSelector "setPlaybackRate:")
      sel_isMuted = unSelector (mkSelector "isMuted")
      sel_setIsMuted = unSelector (mkSelector "setIsMuted:")
  -- pauseAtTime:error:
  stub_0 <- wrap_d_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _pauseAtTime_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (realToFrac arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "pauseAtTime:error:" "B@:d@" stub_0

  -- resumeAtTime:error:
  stub_1 <- wrap_d_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _resumeAtTime_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (realToFrac arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "resumeAtTime:error:" "B@:d@" stub_1

  -- seekToOffset:error:
  stub_2 <- wrap_d_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _seekToOffset_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (realToFrac arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "seekToOffset:error:" "B@:d@" stub_2

  -- loopEnabled
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _loopEnabled rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "loopEnabled" "B@:" stub_3

  -- setLoopEnabled:
  stub_4 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _setLoopEnabled rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setLoopEnabled:" "v@:B" stub_4

  -- loopEnd
  stub_5 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _loopEnd rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "loopEnd" "d@:" stub_5

  -- setLoopEnd:
  stub_6 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _setLoopEnd rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setLoopEnd:" "v@:d" stub_6

  -- playbackRate
  stub_7 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _playbackRate rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "playbackRate" "f@:" stub_7

  -- setPlaybackRate:
  stub_8 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _setPlaybackRate rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setPlaybackRate:" "v@:f" stub_8

  -- isMuted
  stub_9 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _isMuted rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "isMuted" "B@:" stub_9

  -- setIsMuted:
  stub_10 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    case _setIsMuted rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setIsMuted:" "v@:B" stub_10

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticAdvancedPatternPlayerOverrides
    if queriedSel == sel_pauseAtTime_error then pure (maybe 0 (const 1) (_pauseAtTime_error rec_))
    else if queriedSel == sel_resumeAtTime_error then pure (maybe 0 (const 1) (_resumeAtTime_error rec_))
    else if queriedSel == sel_seekToOffset_error then pure (maybe 0 (const 1) (_seekToOffset_error rec_))
    else if queriedSel == sel_loopEnabled then pure (maybe 0 (const 1) (_loopEnabled rec_))
    else if queriedSel == sel_setLoopEnabled then pure (maybe 0 (const 1) (_setLoopEnabled rec_))
    else if queriedSel == sel_loopEnd then pure (maybe 0 (const 1) (_loopEnd rec_))
    else if queriedSel == sel_setLoopEnd then pure (maybe 0 (const 1) (_setLoopEnd rec_))
    else if queriedSel == sel_playbackRate then pure (maybe 0 (const 1) (_playbackRate rec_))
    else if queriedSel == sel_setPlaybackRate then pure (maybe 0 (const 1) (_setPlaybackRate rec_))
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
newCHHapticAdvancedPatternPlayer :: CHHapticAdvancedPatternPlayerOverrides -> IO RawId
newCHHapticAdvancedPatternPlayer overrides = do
  inst <- class_createInstance chHapticAdvancedPatternPlayerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
