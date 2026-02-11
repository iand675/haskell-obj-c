{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLFXTemporalDenoisedScalerBase@.
--
-- Usage:
--
-- @
-- delegate <- newMTLFXTemporalDenoisedScalerBase defaultMTLFXTemporalDenoisedScalerBaseOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalFX.Delegate.MTLFXTemporalDenoisedScalerBase
  ( MTLFXTemporalDenoisedScalerBaseOverrides(..)
  , defaultMTLFXTemporalDenoisedScalerBaseOverrides
  , newMTLFXTemporalDenoisedScalerBase
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

-- | Overrides record for @\@protocol MTLFXTemporalDenoisedScalerBase@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLFXTemporalDenoisedScalerBaseOverrides = MTLFXTemporalDenoisedScalerBaseOverrides
  { _colorTexture :: !(Maybe (IO RawId))
  , _setColorTexture :: !(Maybe (RawId -> IO ()))
  , _depthTexture :: !(Maybe (IO RawId))
  , _setDepthTexture :: !(Maybe (RawId -> IO ()))
  , _motionTexture :: !(Maybe (IO RawId))
  , _setMotionTexture :: !(Maybe (RawId -> IO ()))
  , _diffuseAlbedoTexture :: !(Maybe (IO RawId))
  , _setDiffuseAlbedoTexture :: !(Maybe (RawId -> IO ()))
  , _specularAlbedoTexture :: !(Maybe (IO RawId))
  , _setSpecularAlbedoTexture :: !(Maybe (RawId -> IO ()))
  , _normalTexture :: !(Maybe (IO RawId))
  , _setNormalTexture :: !(Maybe (RawId -> IO ()))
  , _roughnessTexture :: !(Maybe (IO RawId))
  , _setRoughnessTexture :: !(Maybe (RawId -> IO ()))
  , _specularHitDistanceTexture :: !(Maybe (IO RawId))
  , _setSpecularHitDistanceTexture :: !(Maybe (RawId -> IO ()))
  , _denoiseStrengthMaskTexture :: !(Maybe (IO RawId))
  , _setDenoiseStrengthMaskTexture :: !(Maybe (RawId -> IO ()))
  , _transparencyOverlayTexture :: !(Maybe (IO RawId))
  , _setTransparencyOverlayTexture :: !(Maybe (RawId -> IO ()))
  , _outputTexture :: !(Maybe (IO RawId))
  , _setOutputTexture :: !(Maybe (RawId -> IO ()))
  , _exposureTexture :: !(Maybe (IO RawId))
  , _setExposureTexture :: !(Maybe (RawId -> IO ()))
  , _preExposure :: !(Maybe (IO Float))
  , _setPreExposure :: !(Maybe (Float -> IO ()))
  , _reactiveMaskTexture :: !(Maybe (IO RawId))
  , _setReactiveMaskTexture :: !(Maybe (RawId -> IO ()))
  , _jitterOffsetX :: !(Maybe (IO Float))
  , _setJitterOffsetX :: !(Maybe (Float -> IO ()))
  , _jitterOffsetY :: !(Maybe (IO Float))
  , _setJitterOffsetY :: !(Maybe (Float -> IO ()))
  , _motionVectorScaleX :: !(Maybe (IO Float))
  , _setMotionVectorScaleX :: !(Maybe (Float -> IO ()))
  , _motionVectorScaleY :: !(Maybe (IO Float))
  , _setMotionVectorScaleY :: !(Maybe (Float -> IO ()))
  , _shouldResetHistory :: !(Maybe (IO Bool))
  , _setShouldResetHistory :: !(Maybe (Bool -> IO ()))
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
defaultMTLFXTemporalDenoisedScalerBaseOverrides :: MTLFXTemporalDenoisedScalerBaseOverrides
defaultMTLFXTemporalDenoisedScalerBaseOverrides = MTLFXTemporalDenoisedScalerBaseOverrides
  { _colorTexture = Nothing
  , _setColorTexture = Nothing
  , _depthTexture = Nothing
  , _setDepthTexture = Nothing
  , _motionTexture = Nothing
  , _setMotionTexture = Nothing
  , _diffuseAlbedoTexture = Nothing
  , _setDiffuseAlbedoTexture = Nothing
  , _specularAlbedoTexture = Nothing
  , _setSpecularAlbedoTexture = Nothing
  , _normalTexture = Nothing
  , _setNormalTexture = Nothing
  , _roughnessTexture = Nothing
  , _setRoughnessTexture = Nothing
  , _specularHitDistanceTexture = Nothing
  , _setSpecularHitDistanceTexture = Nothing
  , _denoiseStrengthMaskTexture = Nothing
  , _setDenoiseStrengthMaskTexture = Nothing
  , _transparencyOverlayTexture = Nothing
  , _setTransparencyOverlayTexture = Nothing
  , _outputTexture = Nothing
  , _setOutputTexture = Nothing
  , _exposureTexture = Nothing
  , _setExposureTexture = Nothing
  , _preExposure = Nothing
  , _setPreExposure = Nothing
  , _reactiveMaskTexture = Nothing
  , _setReactiveMaskTexture = Nothing
  , _jitterOffsetX = Nothing
  , _setJitterOffsetX = Nothing
  , _jitterOffsetY = Nothing
  , _setJitterOffsetY = Nothing
  , _motionVectorScaleX = Nothing
  , _setMotionVectorScaleX = Nothing
  , _motionVectorScaleY = Nothing
  , _setMotionVectorScaleY = Nothing
  , _shouldResetHistory = Nothing
  , _setShouldResetHistory = Nothing
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
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlfxTemporalDenoisedScalerBaseDelegateClass #-}
mtlfxTemporalDenoisedScalerBaseDelegateClass :: Class
mtlfxTemporalDenoisedScalerBaseDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLFXTemporalDenoisedScalerBase" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_colorTexture = unSelector (mkSelector "colorTexture")
      sel_setColorTexture = unSelector (mkSelector "setColorTexture:")
      sel_depthTexture = unSelector (mkSelector "depthTexture")
      sel_setDepthTexture = unSelector (mkSelector "setDepthTexture:")
      sel_motionTexture = unSelector (mkSelector "motionTexture")
      sel_setMotionTexture = unSelector (mkSelector "setMotionTexture:")
      sel_diffuseAlbedoTexture = unSelector (mkSelector "diffuseAlbedoTexture")
      sel_setDiffuseAlbedoTexture = unSelector (mkSelector "setDiffuseAlbedoTexture:")
      sel_specularAlbedoTexture = unSelector (mkSelector "specularAlbedoTexture")
      sel_setSpecularAlbedoTexture = unSelector (mkSelector "setSpecularAlbedoTexture:")
      sel_normalTexture = unSelector (mkSelector "normalTexture")
      sel_setNormalTexture = unSelector (mkSelector "setNormalTexture:")
      sel_roughnessTexture = unSelector (mkSelector "roughnessTexture")
      sel_setRoughnessTexture = unSelector (mkSelector "setRoughnessTexture:")
      sel_specularHitDistanceTexture = unSelector (mkSelector "specularHitDistanceTexture")
      sel_setSpecularHitDistanceTexture = unSelector (mkSelector "setSpecularHitDistanceTexture:")
      sel_denoiseStrengthMaskTexture = unSelector (mkSelector "denoiseStrengthMaskTexture")
      sel_setDenoiseStrengthMaskTexture = unSelector (mkSelector "setDenoiseStrengthMaskTexture:")
      sel_transparencyOverlayTexture = unSelector (mkSelector "transparencyOverlayTexture")
      sel_setTransparencyOverlayTexture = unSelector (mkSelector "setTransparencyOverlayTexture:")
      sel_outputTexture = unSelector (mkSelector "outputTexture")
      sel_setOutputTexture = unSelector (mkSelector "setOutputTexture:")
      sel_exposureTexture = unSelector (mkSelector "exposureTexture")
      sel_setExposureTexture = unSelector (mkSelector "setExposureTexture:")
      sel_preExposure = unSelector (mkSelector "preExposure")
      sel_setPreExposure = unSelector (mkSelector "setPreExposure:")
      sel_reactiveMaskTexture = unSelector (mkSelector "reactiveMaskTexture")
      sel_setReactiveMaskTexture = unSelector (mkSelector "setReactiveMaskTexture:")
      sel_jitterOffsetX = unSelector (mkSelector "jitterOffsetX")
      sel_setJitterOffsetX = unSelector (mkSelector "setJitterOffsetX:")
      sel_jitterOffsetY = unSelector (mkSelector "jitterOffsetY")
      sel_setJitterOffsetY = unSelector (mkSelector "setJitterOffsetY:")
      sel_motionVectorScaleX = unSelector (mkSelector "motionVectorScaleX")
      sel_setMotionVectorScaleX = unSelector (mkSelector "setMotionVectorScaleX:")
      sel_motionVectorScaleY = unSelector (mkSelector "motionVectorScaleY")
      sel_setMotionVectorScaleY = unSelector (mkSelector "setMotionVectorScaleY:")
      sel_shouldResetHistory = unSelector (mkSelector "shouldResetHistory")
      sel_setShouldResetHistory = unSelector (mkSelector "setShouldResetHistory:")
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
  -- colorTexture
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _colorTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "colorTexture" "@@:" stub_0

  -- setColorTexture:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setColorTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setColorTexture:" "v@:@" stub_1

  -- depthTexture
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _depthTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "depthTexture" "@@:" stub_2

  -- setDepthTexture:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setDepthTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDepthTexture:" "v@:@" stub_3

  -- motionTexture
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _motionTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "motionTexture" "@@:" stub_4

  -- setMotionTexture:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setMotionTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setMotionTexture:" "v@:@" stub_5

  -- diffuseAlbedoTexture
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _diffuseAlbedoTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "diffuseAlbedoTexture" "@@:" stub_6

  -- setDiffuseAlbedoTexture:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setDiffuseAlbedoTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDiffuseAlbedoTexture:" "v@:@" stub_7

  -- specularAlbedoTexture
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _specularAlbedoTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "specularAlbedoTexture" "@@:" stub_8

  -- setSpecularAlbedoTexture:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setSpecularAlbedoTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setSpecularAlbedoTexture:" "v@:@" stub_9

  -- normalTexture
  stub_10 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _normalTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "normalTexture" "@@:" stub_10

  -- setNormalTexture:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setNormalTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setNormalTexture:" "v@:@" stub_11

  -- roughnessTexture
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _roughnessTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "roughnessTexture" "@@:" stub_12

  -- setRoughnessTexture:
  stub_13 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setRoughnessTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setRoughnessTexture:" "v@:@" stub_13

  -- specularHitDistanceTexture
  stub_14 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _specularHitDistanceTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "specularHitDistanceTexture" "@@:" stub_14

  -- setSpecularHitDistanceTexture:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setSpecularHitDistanceTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setSpecularHitDistanceTexture:" "v@:@" stub_15

  -- denoiseStrengthMaskTexture
  stub_16 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _denoiseStrengthMaskTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "denoiseStrengthMaskTexture" "@@:" stub_16

  -- setDenoiseStrengthMaskTexture:
  stub_17 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setDenoiseStrengthMaskTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDenoiseStrengthMaskTexture:" "v@:@" stub_17

  -- transparencyOverlayTexture
  stub_18 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _transparencyOverlayTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "transparencyOverlayTexture" "@@:" stub_18

  -- setTransparencyOverlayTexture:
  stub_19 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setTransparencyOverlayTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setTransparencyOverlayTexture:" "v@:@" stub_19

  -- outputTexture
  stub_20 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _outputTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outputTexture" "@@:" stub_20

  -- setOutputTexture:
  stub_21 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setOutputTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setOutputTexture:" "v@:@" stub_21

  -- exposureTexture
  stub_22 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _exposureTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "exposureTexture" "@@:" stub_22

  -- setExposureTexture:
  stub_23 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setExposureTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setExposureTexture:" "v@:@" stub_23

  -- preExposure
  stub_24 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _preExposure rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "preExposure" "f@:" stub_24

  -- setPreExposure:
  stub_25 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setPreExposure rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setPreExposure:" "v@:f" stub_25

  -- reactiveMaskTexture
  stub_26 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _reactiveMaskTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "reactiveMaskTexture" "@@:" stub_26

  -- setReactiveMaskTexture:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setReactiveMaskTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setReactiveMaskTexture:" "v@:@" stub_27

  -- jitterOffsetX
  stub_28 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _jitterOffsetX rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "jitterOffsetX" "f@:" stub_28

  -- setJitterOffsetX:
  stub_29 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setJitterOffsetX rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setJitterOffsetX:" "v@:f" stub_29

  -- jitterOffsetY
  stub_30 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _jitterOffsetY rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "jitterOffsetY" "f@:" stub_30

  -- setJitterOffsetY:
  stub_31 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setJitterOffsetY rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setJitterOffsetY:" "v@:f" stub_31

  -- motionVectorScaleX
  stub_32 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _motionVectorScaleX rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "motionVectorScaleX" "f@:" stub_32

  -- setMotionVectorScaleX:
  stub_33 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setMotionVectorScaleX rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setMotionVectorScaleX:" "v@:f" stub_33

  -- motionVectorScaleY
  stub_34 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _motionVectorScaleY rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "motionVectorScaleY" "f@:" stub_34

  -- setMotionVectorScaleY:
  stub_35 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setMotionVectorScaleY rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setMotionVectorScaleY:" "v@:f" stub_35

  -- shouldResetHistory
  stub_36 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _shouldResetHistory rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldResetHistory" "B@:" stub_36

  -- setShouldResetHistory:
  stub_37 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setShouldResetHistory rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setShouldResetHistory:" "v@:B" stub_37

  -- depthReversed
  stub_38 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _depthReversed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "depthReversed" "B@:" stub_38

  -- setDepthReversed:
  stub_39 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setDepthReversed rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setDepthReversed:" "v@:B" stub_39

  -- inputWidth
  stub_40 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _inputWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputWidth" "Q@:" stub_40

  -- inputHeight
  stub_41 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _inputHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputHeight" "Q@:" stub_41

  -- outputWidth
  stub_42 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _outputWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "outputWidth" "Q@:" stub_42

  -- outputHeight
  stub_43 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _outputHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "outputHeight" "Q@:" stub_43

  -- inputContentMinScale
  stub_44 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _inputContentMinScale rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "inputContentMinScale" "f@:" stub_44

  -- inputContentMaxScale
  stub_45 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _inputContentMaxScale rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "inputContentMaxScale" "f@:" stub_45

  -- fence
  stub_46 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _fence rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "fence" "@@:" stub_46

  -- setFence:
  stub_47 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    case _setFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setFence:" "v@:@" stub_47

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXTemporalDenoisedScalerBaseOverrides
    if queriedSel == sel_colorTexture then pure (maybe 0 (const 1) (_colorTexture rec_))
    else if queriedSel == sel_setColorTexture then pure (maybe 0 (const 1) (_setColorTexture rec_))
    else if queriedSel == sel_depthTexture then pure (maybe 0 (const 1) (_depthTexture rec_))
    else if queriedSel == sel_setDepthTexture then pure (maybe 0 (const 1) (_setDepthTexture rec_))
    else if queriedSel == sel_motionTexture then pure (maybe 0 (const 1) (_motionTexture rec_))
    else if queriedSel == sel_setMotionTexture then pure (maybe 0 (const 1) (_setMotionTexture rec_))
    else if queriedSel == sel_diffuseAlbedoTexture then pure (maybe 0 (const 1) (_diffuseAlbedoTexture rec_))
    else if queriedSel == sel_setDiffuseAlbedoTexture then pure (maybe 0 (const 1) (_setDiffuseAlbedoTexture rec_))
    else if queriedSel == sel_specularAlbedoTexture then pure (maybe 0 (const 1) (_specularAlbedoTexture rec_))
    else if queriedSel == sel_setSpecularAlbedoTexture then pure (maybe 0 (const 1) (_setSpecularAlbedoTexture rec_))
    else if queriedSel == sel_normalTexture then pure (maybe 0 (const 1) (_normalTexture rec_))
    else if queriedSel == sel_setNormalTexture then pure (maybe 0 (const 1) (_setNormalTexture rec_))
    else if queriedSel == sel_roughnessTexture then pure (maybe 0 (const 1) (_roughnessTexture rec_))
    else if queriedSel == sel_setRoughnessTexture then pure (maybe 0 (const 1) (_setRoughnessTexture rec_))
    else if queriedSel == sel_specularHitDistanceTexture then pure (maybe 0 (const 1) (_specularHitDistanceTexture rec_))
    else if queriedSel == sel_setSpecularHitDistanceTexture then pure (maybe 0 (const 1) (_setSpecularHitDistanceTexture rec_))
    else if queriedSel == sel_denoiseStrengthMaskTexture then pure (maybe 0 (const 1) (_denoiseStrengthMaskTexture rec_))
    else if queriedSel == sel_setDenoiseStrengthMaskTexture then pure (maybe 0 (const 1) (_setDenoiseStrengthMaskTexture rec_))
    else if queriedSel == sel_transparencyOverlayTexture then pure (maybe 0 (const 1) (_transparencyOverlayTexture rec_))
    else if queriedSel == sel_setTransparencyOverlayTexture then pure (maybe 0 (const 1) (_setTransparencyOverlayTexture rec_))
    else if queriedSel == sel_outputTexture then pure (maybe 0 (const 1) (_outputTexture rec_))
    else if queriedSel == sel_setOutputTexture then pure (maybe 0 (const 1) (_setOutputTexture rec_))
    else if queriedSel == sel_exposureTexture then pure (maybe 0 (const 1) (_exposureTexture rec_))
    else if queriedSel == sel_setExposureTexture then pure (maybe 0 (const 1) (_setExposureTexture rec_))
    else if queriedSel == sel_preExposure then pure (maybe 0 (const 1) (_preExposure rec_))
    else if queriedSel == sel_setPreExposure then pure (maybe 0 (const 1) (_setPreExposure rec_))
    else if queriedSel == sel_reactiveMaskTexture then pure (maybe 0 (const 1) (_reactiveMaskTexture rec_))
    else if queriedSel == sel_setReactiveMaskTexture then pure (maybe 0 (const 1) (_setReactiveMaskTexture rec_))
    else if queriedSel == sel_jitterOffsetX then pure (maybe 0 (const 1) (_jitterOffsetX rec_))
    else if queriedSel == sel_setJitterOffsetX then pure (maybe 0 (const 1) (_setJitterOffsetX rec_))
    else if queriedSel == sel_jitterOffsetY then pure (maybe 0 (const 1) (_jitterOffsetY rec_))
    else if queriedSel == sel_setJitterOffsetY then pure (maybe 0 (const 1) (_setJitterOffsetY rec_))
    else if queriedSel == sel_motionVectorScaleX then pure (maybe 0 (const 1) (_motionVectorScaleX rec_))
    else if queriedSel == sel_setMotionVectorScaleX then pure (maybe 0 (const 1) (_setMotionVectorScaleX rec_))
    else if queriedSel == sel_motionVectorScaleY then pure (maybe 0 (const 1) (_motionVectorScaleY rec_))
    else if queriedSel == sel_setMotionVectorScaleY then pure (maybe 0 (const 1) (_setMotionVectorScaleY rec_))
    else if queriedSel == sel_shouldResetHistory then pure (maybe 0 (const 1) (_shouldResetHistory rec_))
    else if queriedSel == sel_setShouldResetHistory then pure (maybe 0 (const 1) (_setShouldResetHistory rec_))
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
newMTLFXTemporalDenoisedScalerBase :: MTLFXTemporalDenoisedScalerBaseOverrides -> IO RawId
newMTLFXTemporalDenoisedScalerBase overrides = do
  inst <- class_createInstance mtlfxTemporalDenoisedScalerBaseDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
