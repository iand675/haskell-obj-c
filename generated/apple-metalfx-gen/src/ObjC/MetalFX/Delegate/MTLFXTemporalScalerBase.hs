{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLFXTemporalScalerBase@.
--
-- Usage:
--
-- @
-- delegate <- newMTLFXTemporalScalerBase defaultMTLFXTemporalScalerBaseOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalFX.Delegate.MTLFXTemporalScalerBase
  ( MTLFXTemporalScalerBaseOverrides(..)
  , defaultMTLFXTemporalScalerBaseOverrides
  , newMTLFXTemporalScalerBase
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

-- | Overrides record for @\@protocol MTLFXTemporalScalerBase@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLFXTemporalScalerBaseOverrides = MTLFXTemporalScalerBaseOverrides
  { _inputContentWidth :: !(Maybe (IO Int))
  , _setInputContentWidth :: !(Maybe (Int -> IO ()))
  , _inputContentHeight :: !(Maybe (IO Int))
  , _setInputContentHeight :: !(Maybe (Int -> IO ()))
  , _colorTexture :: !(Maybe (IO RawId))
  , _setColorTexture :: !(Maybe (RawId -> IO ()))
  , _depthTexture :: !(Maybe (IO RawId))
  , _setDepthTexture :: !(Maybe (RawId -> IO ()))
  , _motionTexture :: !(Maybe (IO RawId))
  , _setMotionTexture :: !(Maybe (RawId -> IO ()))
  , _outputTexture :: !(Maybe (IO RawId))
  , _setOutputTexture :: !(Maybe (RawId -> IO ()))
  , _exposureTexture :: !(Maybe (IO RawId))
  , _setExposureTexture :: !(Maybe (RawId -> IO ()))
  , _reactiveMaskTexture :: !(Maybe (IO RawId))
  , _setReactiveMaskTexture :: !(Maybe (RawId -> IO ()))
  , _preExposure :: !(Maybe (IO Float))
  , _setPreExposure :: !(Maybe (Float -> IO ()))
  , _jitterOffsetX :: !(Maybe (IO Float))
  , _setJitterOffsetX :: !(Maybe (Float -> IO ()))
  , _jitterOffsetY :: !(Maybe (IO Float))
  , _setJitterOffsetY :: !(Maybe (Float -> IO ()))
  , _motionVectorScaleX :: !(Maybe (IO Float))
  , _setMotionVectorScaleX :: !(Maybe (Float -> IO ()))
  , _motionVectorScaleY :: !(Maybe (IO Float))
  , _setMotionVectorScaleY :: !(Maybe (Float -> IO ()))
  , _reset :: !(Maybe (IO Bool))
  , _setReset :: !(Maybe (Bool -> IO ()))
  , _depthReversed :: !(Maybe (IO Bool))
  , _setDepthReversed :: !(Maybe (Bool -> IO ()))
  , _inputWidth :: !(Maybe (IO Int))
  , _inputHeight :: !(Maybe (IO Int))
  , _outputWidth :: !(Maybe (IO Int))
  , _outputHeight :: !(Maybe (IO Int))
  , _inputContentMinScale :: !(Maybe (IO Float))
  , _inputContentMaxScale :: !(Maybe (IO Float))
  , _fence :: !(Maybe (IO RawId))
  , _setFence :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLFXTemporalScalerBaseOverrides :: MTLFXTemporalScalerBaseOverrides
defaultMTLFXTemporalScalerBaseOverrides = MTLFXTemporalScalerBaseOverrides
  { _inputContentWidth = Nothing
  , _setInputContentWidth = Nothing
  , _inputContentHeight = Nothing
  , _setInputContentHeight = Nothing
  , _colorTexture = Nothing
  , _setColorTexture = Nothing
  , _depthTexture = Nothing
  , _setDepthTexture = Nothing
  , _motionTexture = Nothing
  , _setMotionTexture = Nothing
  , _outputTexture = Nothing
  , _setOutputTexture = Nothing
  , _exposureTexture = Nothing
  , _setExposureTexture = Nothing
  , _reactiveMaskTexture = Nothing
  , _setReactiveMaskTexture = Nothing
  , _preExposure = Nothing
  , _setPreExposure = Nothing
  , _jitterOffsetX = Nothing
  , _setJitterOffsetX = Nothing
  , _jitterOffsetY = Nothing
  , _setJitterOffsetY = Nothing
  , _motionVectorScaleX = Nothing
  , _setMotionVectorScaleX = Nothing
  , _motionVectorScaleY = Nothing
  , _setMotionVectorScaleY = Nothing
  , _reset = Nothing
  , _setReset = Nothing
  , _depthReversed = Nothing
  , _setDepthReversed = Nothing
  , _inputWidth = Nothing
  , _inputHeight = Nothing
  , _outputWidth = Nothing
  , _outputHeight = Nothing
  , _inputContentMinScale = Nothing
  , _inputContentMaxScale = Nothing
  , _fence = Nothing
  , _setFence = Nothing
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
  wrap_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlfxTemporalScalerBaseDelegateClass #-}
mtlfxTemporalScalerBaseDelegateClass :: Class
mtlfxTemporalScalerBaseDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLFXTemporalScalerBase" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_inputContentWidth = unSelector (mkSelector "inputContentWidth")
      sel_setInputContentWidth = unSelector (mkSelector "setInputContentWidth:")
      sel_inputContentHeight = unSelector (mkSelector "inputContentHeight")
      sel_setInputContentHeight = unSelector (mkSelector "setInputContentHeight:")
      sel_colorTexture = unSelector (mkSelector "colorTexture")
      sel_setColorTexture = unSelector (mkSelector "setColorTexture:")
      sel_depthTexture = unSelector (mkSelector "depthTexture")
      sel_setDepthTexture = unSelector (mkSelector "setDepthTexture:")
      sel_motionTexture = unSelector (mkSelector "motionTexture")
      sel_setMotionTexture = unSelector (mkSelector "setMotionTexture:")
      sel_outputTexture = unSelector (mkSelector "outputTexture")
      sel_setOutputTexture = unSelector (mkSelector "setOutputTexture:")
      sel_exposureTexture = unSelector (mkSelector "exposureTexture")
      sel_setExposureTexture = unSelector (mkSelector "setExposureTexture:")
      sel_reactiveMaskTexture = unSelector (mkSelector "reactiveMaskTexture")
      sel_setReactiveMaskTexture = unSelector (mkSelector "setReactiveMaskTexture:")
      sel_preExposure = unSelector (mkSelector "preExposure")
      sel_setPreExposure = unSelector (mkSelector "setPreExposure:")
      sel_jitterOffsetX = unSelector (mkSelector "jitterOffsetX")
      sel_setJitterOffsetX = unSelector (mkSelector "setJitterOffsetX:")
      sel_jitterOffsetY = unSelector (mkSelector "jitterOffsetY")
      sel_setJitterOffsetY = unSelector (mkSelector "setJitterOffsetY:")
      sel_motionVectorScaleX = unSelector (mkSelector "motionVectorScaleX")
      sel_setMotionVectorScaleX = unSelector (mkSelector "setMotionVectorScaleX:")
      sel_motionVectorScaleY = unSelector (mkSelector "motionVectorScaleY")
      sel_setMotionVectorScaleY = unSelector (mkSelector "setMotionVectorScaleY:")
      sel_reset = unSelector (mkSelector "reset")
      sel_setReset = unSelector (mkSelector "setReset:")
      sel_depthReversed = unSelector (mkSelector "depthReversed")
      sel_setDepthReversed = unSelector (mkSelector "setDepthReversed:")
      sel_inputWidth = unSelector (mkSelector "inputWidth")
      sel_inputHeight = unSelector (mkSelector "inputHeight")
      sel_outputWidth = unSelector (mkSelector "outputWidth")
      sel_outputHeight = unSelector (mkSelector "outputHeight")
      sel_inputContentMinScale = unSelector (mkSelector "inputContentMinScale")
      sel_inputContentMaxScale = unSelector (mkSelector "inputContentMaxScale")
      sel_fence = unSelector (mkSelector "fence")
      sel_setFence = unSelector (mkSelector "setFence:")
  -- inputContentWidth
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _inputContentWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputContentWidth" "Q@:" stub_0

  -- setInputContentWidth:
  stub_1 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setInputContentWidth rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setInputContentWidth:" "v@:Q" stub_1

  -- inputContentHeight
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _inputContentHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputContentHeight" "Q@:" stub_2

  -- setInputContentHeight:
  stub_3 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setInputContentHeight rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setInputContentHeight:" "v@:Q" stub_3

  -- colorTexture
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _colorTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "colorTexture" "@@:" stub_4

  -- setColorTexture:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setColorTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setColorTexture:" "v@:@" stub_5

  -- depthTexture
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _depthTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "depthTexture" "@@:" stub_6

  -- setDepthTexture:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setDepthTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDepthTexture:" "v@:@" stub_7

  -- motionTexture
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _motionTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "motionTexture" "@@:" stub_8

  -- setMotionTexture:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setMotionTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setMotionTexture:" "v@:@" stub_9

  -- outputTexture
  stub_10 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _outputTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outputTexture" "@@:" stub_10

  -- setOutputTexture:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setOutputTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setOutputTexture:" "v@:@" stub_11

  -- exposureTexture
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _exposureTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "exposureTexture" "@@:" stub_12

  -- setExposureTexture:
  stub_13 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setExposureTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setExposureTexture:" "v@:@" stub_13

  -- reactiveMaskTexture
  stub_14 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _reactiveMaskTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "reactiveMaskTexture" "@@:" stub_14

  -- setReactiveMaskTexture:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setReactiveMaskTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setReactiveMaskTexture:" "v@:@" stub_15

  -- preExposure
  stub_16 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _preExposure rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "preExposure" "f@:" stub_16

  -- setPreExposure:
  stub_17 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setPreExposure rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setPreExposure:" "v@:f" stub_17

  -- jitterOffsetX
  stub_18 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _jitterOffsetX rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "jitterOffsetX" "f@:" stub_18

  -- setJitterOffsetX:
  stub_19 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setJitterOffsetX rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setJitterOffsetX:" "v@:f" stub_19

  -- jitterOffsetY
  stub_20 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _jitterOffsetY rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "jitterOffsetY" "f@:" stub_20

  -- setJitterOffsetY:
  stub_21 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setJitterOffsetY rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setJitterOffsetY:" "v@:f" stub_21

  -- motionVectorScaleX
  stub_22 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _motionVectorScaleX rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "motionVectorScaleX" "f@:" stub_22

  -- setMotionVectorScaleX:
  stub_23 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setMotionVectorScaleX rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setMotionVectorScaleX:" "v@:f" stub_23

  -- motionVectorScaleY
  stub_24 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _motionVectorScaleY rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "motionVectorScaleY" "f@:" stub_24

  -- setMotionVectorScaleY:
  stub_25 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setMotionVectorScaleY rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setMotionVectorScaleY:" "v@:f" stub_25

  -- reset
  stub_26 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _reset rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "reset" "B@:" stub_26

  -- setReset:
  stub_27 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setReset rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setReset:" "v@:B" stub_27

  -- depthReversed
  stub_28 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _depthReversed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "depthReversed" "B@:" stub_28

  -- setDepthReversed:
  stub_29 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setDepthReversed rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setDepthReversed:" "v@:B" stub_29

  -- inputWidth
  stub_30 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _inputWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputWidth" "Q@:" stub_30

  -- inputHeight
  stub_31 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _inputHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputHeight" "Q@:" stub_31

  -- outputWidth
  stub_32 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _outputWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "outputWidth" "Q@:" stub_32

  -- outputHeight
  stub_33 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _outputHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "outputHeight" "Q@:" stub_33

  -- inputContentMinScale
  stub_34 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _inputContentMinScale rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "inputContentMinScale" "f@:" stub_34

  -- inputContentMaxScale
  stub_35 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _inputContentMaxScale rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "inputContentMaxScale" "f@:" stub_35

  -- fence
  stub_36 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _fence rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "fence" "@@:" stub_36

  -- setFence:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    case _setFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setFence:" "v@:@" stub_37

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalScalerBaseOverrides
    if queriedSel == sel_inputContentWidth then pure (maybe 0 (const 1) (_inputContentWidth rec_))
    else if queriedSel == sel_setInputContentWidth then pure (maybe 0 (const 1) (_setInputContentWidth rec_))
    else if queriedSel == sel_inputContentHeight then pure (maybe 0 (const 1) (_inputContentHeight rec_))
    else if queriedSel == sel_setInputContentHeight then pure (maybe 0 (const 1) (_setInputContentHeight rec_))
    else if queriedSel == sel_colorTexture then pure (maybe 0 (const 1) (_colorTexture rec_))
    else if queriedSel == sel_setColorTexture then pure (maybe 0 (const 1) (_setColorTexture rec_))
    else if queriedSel == sel_depthTexture then pure (maybe 0 (const 1) (_depthTexture rec_))
    else if queriedSel == sel_setDepthTexture then pure (maybe 0 (const 1) (_setDepthTexture rec_))
    else if queriedSel == sel_motionTexture then pure (maybe 0 (const 1) (_motionTexture rec_))
    else if queriedSel == sel_setMotionTexture then pure (maybe 0 (const 1) (_setMotionTexture rec_))
    else if queriedSel == sel_outputTexture then pure (maybe 0 (const 1) (_outputTexture rec_))
    else if queriedSel == sel_setOutputTexture then pure (maybe 0 (const 1) (_setOutputTexture rec_))
    else if queriedSel == sel_exposureTexture then pure (maybe 0 (const 1) (_exposureTexture rec_))
    else if queriedSel == sel_setExposureTexture then pure (maybe 0 (const 1) (_setExposureTexture rec_))
    else if queriedSel == sel_reactiveMaskTexture then pure (maybe 0 (const 1) (_reactiveMaskTexture rec_))
    else if queriedSel == sel_setReactiveMaskTexture then pure (maybe 0 (const 1) (_setReactiveMaskTexture rec_))
    else if queriedSel == sel_preExposure then pure (maybe 0 (const 1) (_preExposure rec_))
    else if queriedSel == sel_setPreExposure then pure (maybe 0 (const 1) (_setPreExposure rec_))
    else if queriedSel == sel_jitterOffsetX then pure (maybe 0 (const 1) (_jitterOffsetX rec_))
    else if queriedSel == sel_setJitterOffsetX then pure (maybe 0 (const 1) (_setJitterOffsetX rec_))
    else if queriedSel == sel_jitterOffsetY then pure (maybe 0 (const 1) (_jitterOffsetY rec_))
    else if queriedSel == sel_setJitterOffsetY then pure (maybe 0 (const 1) (_setJitterOffsetY rec_))
    else if queriedSel == sel_motionVectorScaleX then pure (maybe 0 (const 1) (_motionVectorScaleX rec_))
    else if queriedSel == sel_setMotionVectorScaleX then pure (maybe 0 (const 1) (_setMotionVectorScaleX rec_))
    else if queriedSel == sel_motionVectorScaleY then pure (maybe 0 (const 1) (_motionVectorScaleY rec_))
    else if queriedSel == sel_setMotionVectorScaleY then pure (maybe 0 (const 1) (_setMotionVectorScaleY rec_))
    else if queriedSel == sel_reset then pure (maybe 0 (const 1) (_reset rec_))
    else if queriedSel == sel_setReset then pure (maybe 0 (const 1) (_setReset rec_))
    else if queriedSel == sel_depthReversed then pure (maybe 0 (const 1) (_depthReversed rec_))
    else if queriedSel == sel_setDepthReversed then pure (maybe 0 (const 1) (_setDepthReversed rec_))
    else if queriedSel == sel_inputWidth then pure (maybe 0 (const 1) (_inputWidth rec_))
    else if queriedSel == sel_inputHeight then pure (maybe 0 (const 1) (_inputHeight rec_))
    else if queriedSel == sel_outputWidth then pure (maybe 0 (const 1) (_outputWidth rec_))
    else if queriedSel == sel_outputHeight then pure (maybe 0 (const 1) (_outputHeight rec_))
    else if queriedSel == sel_inputContentMinScale then pure (maybe 0 (const 1) (_inputContentMinScale rec_))
    else if queriedSel == sel_inputContentMaxScale then pure (maybe 0 (const 1) (_inputContentMaxScale rec_))
    else if queriedSel == sel_fence then pure (maybe 0 (const 1) (_fence rec_))
    else if queriedSel == sel_setFence then pure (maybe 0 (const 1) (_setFence rec_))
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
newMTLFXTemporalScalerBase :: MTLFXTemporalScalerBaseOverrides -> IO RawId
newMTLFXTemporalScalerBase overrides = do
  inst <- class_createInstance mtlfxTemporalScalerBaseDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
