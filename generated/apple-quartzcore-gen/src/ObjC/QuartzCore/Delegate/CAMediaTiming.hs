{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CAMediaTiming@.
--
-- Usage:
--
-- @
-- delegate <- newCAMediaTiming defaultCAMediaTimingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.QuartzCore.Delegate.CAMediaTiming
  ( CAMediaTimingOverrides(..)
  , defaultCAMediaTimingOverrides
  , newCAMediaTiming
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

-- | Overrides record for @\@protocol CAMediaTiming@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CAMediaTimingOverrides = CAMediaTimingOverrides
  { _beginTime :: !(Maybe (IO Double))
  , _setBeginTime :: !(Maybe (Double -> IO ()))
  , _duration :: !(Maybe (IO Double))
  , _setDuration :: !(Maybe (Double -> IO ()))
  , _speed :: !(Maybe (IO Float))
  , _setSpeed :: !(Maybe (Float -> IO ()))
  , _timeOffset :: !(Maybe (IO Double))
  , _setTimeOffset :: !(Maybe (Double -> IO ()))
  , _repeatCount :: !(Maybe (IO Float))
  , _setRepeatCount :: !(Maybe (Float -> IO ()))
  , _repeatDuration :: !(Maybe (IO Double))
  , _setRepeatDuration :: !(Maybe (Double -> IO ()))
  , _autoreverses :: !(Maybe (IO Bool))
  , _setAutoreverses :: !(Maybe (Bool -> IO ()))
  , _fillMode :: !(Maybe (IO RawId))
  , _setFillMode :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCAMediaTimingOverrides :: CAMediaTimingOverrides
defaultCAMediaTimingOverrides = CAMediaTimingOverrides
  { _beginTime = Nothing
  , _setBeginTime = Nothing
  , _duration = Nothing
  , _setDuration = Nothing
  , _speed = Nothing
  , _setSpeed = Nothing
  , _timeOffset = Nothing
  , _setTimeOffset = Nothing
  , _repeatCount = Nothing
  , _setRepeatCount = Nothing
  , _repeatDuration = Nothing
  , _setRepeatDuration = Nothing
  , _autoreverses = Nothing
  , _setAutoreverses = Nothing
  , _fillMode = Nothing
  , _setFillMode = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE caMediaTimingDelegateClass #-}
caMediaTimingDelegateClass :: Class
caMediaTimingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCAMediaTiming" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginTime = unSelector (mkSelector "beginTime")
      sel_setBeginTime = unSelector (mkSelector "setBeginTime:")
      sel_duration = unSelector (mkSelector "duration")
      sel_setDuration = unSelector (mkSelector "setDuration:")
      sel_speed = unSelector (mkSelector "speed")
      sel_setSpeed = unSelector (mkSelector "setSpeed:")
      sel_timeOffset = unSelector (mkSelector "timeOffset")
      sel_setTimeOffset = unSelector (mkSelector "setTimeOffset:")
      sel_repeatCount = unSelector (mkSelector "repeatCount")
      sel_setRepeatCount = unSelector (mkSelector "setRepeatCount:")
      sel_repeatDuration = unSelector (mkSelector "repeatDuration")
      sel_setRepeatDuration = unSelector (mkSelector "setRepeatDuration:")
      sel_autoreverses = unSelector (mkSelector "autoreverses")
      sel_setAutoreverses = unSelector (mkSelector "setAutoreverses:")
      sel_fillMode = unSelector (mkSelector "fillMode")
      sel_setFillMode = unSelector (mkSelector "setFillMode:")
  -- beginTime
  stub_0 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _beginTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "beginTime" "d@:" stub_0

  -- setBeginTime:
  stub_1 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setBeginTime rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setBeginTime:" "v@:d" stub_1

  -- duration
  stub_2 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _duration rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "duration" "d@:" stub_2

  -- setDuration:
  stub_3 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setDuration rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setDuration:" "v@:d" stub_3

  -- speed
  stub_4 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _speed rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "speed" "f@:" stub_4

  -- setSpeed:
  stub_5 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setSpeed rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setSpeed:" "v@:f" stub_5

  -- timeOffset
  stub_6 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _timeOffset rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "timeOffset" "d@:" stub_6

  -- setTimeOffset:
  stub_7 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setTimeOffset rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setTimeOffset:" "v@:d" stub_7

  -- repeatCount
  stub_8 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _repeatCount rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "repeatCount" "f@:" stub_8

  -- setRepeatCount:
  stub_9 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setRepeatCount rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setRepeatCount:" "v@:f" stub_9

  -- repeatDuration
  stub_10 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _repeatDuration rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "repeatDuration" "d@:" stub_10

  -- setRepeatDuration:
  stub_11 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setRepeatDuration rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setRepeatDuration:" "v@:d" stub_11

  -- autoreverses
  stub_12 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _autoreverses rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "autoreverses" "B@:" stub_12

  -- setAutoreverses:
  stub_13 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setAutoreverses rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAutoreverses:" "v@:B" stub_13

  -- fillMode
  stub_14 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _fillMode rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "fillMode" "@@:" stub_14

  -- setFillMode:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    case _setFillMode rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setFillMode:" "v@:@" stub_15

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMediaTimingOverrides
    if queriedSel == sel_beginTime then pure (maybe 0 (const 1) (_beginTime rec_))
    else if queriedSel == sel_setBeginTime then pure (maybe 0 (const 1) (_setBeginTime rec_))
    else if queriedSel == sel_duration then pure (maybe 0 (const 1) (_duration rec_))
    else if queriedSel == sel_setDuration then pure (maybe 0 (const 1) (_setDuration rec_))
    else if queriedSel == sel_speed then pure (maybe 0 (const 1) (_speed rec_))
    else if queriedSel == sel_setSpeed then pure (maybe 0 (const 1) (_setSpeed rec_))
    else if queriedSel == sel_timeOffset then pure (maybe 0 (const 1) (_timeOffset rec_))
    else if queriedSel == sel_setTimeOffset then pure (maybe 0 (const 1) (_setTimeOffset rec_))
    else if queriedSel == sel_repeatCount then pure (maybe 0 (const 1) (_repeatCount rec_))
    else if queriedSel == sel_setRepeatCount then pure (maybe 0 (const 1) (_setRepeatCount rec_))
    else if queriedSel == sel_repeatDuration then pure (maybe 0 (const 1) (_repeatDuration rec_))
    else if queriedSel == sel_setRepeatDuration then pure (maybe 0 (const 1) (_setRepeatDuration rec_))
    else if queriedSel == sel_autoreverses then pure (maybe 0 (const 1) (_autoreverses rec_))
    else if queriedSel == sel_setAutoreverses then pure (maybe 0 (const 1) (_setAutoreverses rec_))
    else if queriedSel == sel_fillMode then pure (maybe 0 (const 1) (_fillMode rec_))
    else if queriedSel == sel_setFillMode then pure (maybe 0 (const 1) (_setFillMode rec_))
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
newCAMediaTiming :: CAMediaTimingOverrides -> IO RawId
newCAMediaTiming overrides = do
  inst <- class_createInstance caMediaTimingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
