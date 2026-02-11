{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLFXFrameInterpolatorBase@.
--
-- Usage:
--
-- @
-- delegate <- newMTLFXFrameInterpolatorBase defaultMTLFXFrameInterpolatorBaseOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalFX.Delegate.MTLFXFrameInterpolatorBase
  ( MTLFXFrameInterpolatorBaseOverrides(..)
  , defaultMTLFXFrameInterpolatorBaseOverrides
  , newMTLFXFrameInterpolatorBase
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

-- | Overrides record for @\@protocol MTLFXFrameInterpolatorBase@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLFXFrameInterpolatorBaseOverrides = MTLFXFrameInterpolatorBaseOverrides
  { _inputWidth :: !(Maybe (IO Int))
  , _inputHeight :: !(Maybe (IO Int))
  , _outputWidth :: !(Maybe (IO Int))
  , _outputHeight :: !(Maybe (IO Int))
  , _colorTexture :: !(Maybe (IO RawId))
  , _setColorTexture :: !(Maybe (RawId -> IO ()))
  , _prevColorTexture :: !(Maybe (IO RawId))
  , _setPrevColorTexture :: !(Maybe (RawId -> IO ()))
  , _depthTexture :: !(Maybe (IO RawId))
  , _setDepthTexture :: !(Maybe (RawId -> IO ()))
  , _motionTexture :: !(Maybe (IO RawId))
  , _setMotionTexture :: !(Maybe (RawId -> IO ()))
  , _motionVectorScaleX :: !(Maybe (IO Float))
  , _setMotionVectorScaleX :: !(Maybe (Float -> IO ()))
  , _motionVectorScaleY :: !(Maybe (IO Float))
  , _setMotionVectorScaleY :: !(Maybe (Float -> IO ()))
  , _deltaTime :: !(Maybe (IO Float))
  , _setDeltaTime :: !(Maybe (Float -> IO ()))
  , _nearPlane :: !(Maybe (IO Float))
  , _setNearPlane :: !(Maybe (Float -> IO ()))
  , _farPlane :: !(Maybe (IO Float))
  , _setFarPlane :: !(Maybe (Float -> IO ()))
  , _fieldOfView :: !(Maybe (IO Float))
  , _setFieldOfView :: !(Maybe (Float -> IO ()))
  , _aspectRatio :: !(Maybe (IO Float))
  , _setAspectRatio :: !(Maybe (Float -> IO ()))
  , _uiTexture :: !(Maybe (IO RawId))
  , _setUiTexture :: !(Maybe (RawId -> IO ()))
  , _jitterOffsetX :: !(Maybe (IO Float))
  , _setJitterOffsetX :: !(Maybe (Float -> IO ()))
  , _jitterOffsetY :: !(Maybe (IO Float))
  , _setJitterOffsetY :: !(Maybe (Float -> IO ()))
  , _uiTextureComposited :: !(Maybe (IO Bool))
  , _setUiTextureComposited :: !(Maybe (Bool -> IO ()))
  , _shouldResetHistory :: !(Maybe (IO Bool))
  , _setShouldResetHistory :: !(Maybe (Bool -> IO ()))
  , _outputTexture :: !(Maybe (IO RawId))
  , _setOutputTexture :: !(Maybe (RawId -> IO ()))
  , _fence :: !(Maybe (IO RawId))
  , _setFence :: !(Maybe (RawId -> IO ()))
  , _depthReversed :: !(Maybe (IO Bool))
  , _setDepthReversed :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLFXFrameInterpolatorBaseOverrides :: MTLFXFrameInterpolatorBaseOverrides
defaultMTLFXFrameInterpolatorBaseOverrides = MTLFXFrameInterpolatorBaseOverrides
  { _inputWidth = Nothing
  , _inputHeight = Nothing
  , _outputWidth = Nothing
  , _outputHeight = Nothing
  , _colorTexture = Nothing
  , _setColorTexture = Nothing
  , _prevColorTexture = Nothing
  , _setPrevColorTexture = Nothing
  , _depthTexture = Nothing
  , _setDepthTexture = Nothing
  , _motionTexture = Nothing
  , _setMotionTexture = Nothing
  , _motionVectorScaleX = Nothing
  , _setMotionVectorScaleX = Nothing
  , _motionVectorScaleY = Nothing
  , _setMotionVectorScaleY = Nothing
  , _deltaTime = Nothing
  , _setDeltaTime = Nothing
  , _nearPlane = Nothing
  , _setNearPlane = Nothing
  , _farPlane = Nothing
  , _setFarPlane = Nothing
  , _fieldOfView = Nothing
  , _setFieldOfView = Nothing
  , _aspectRatio = Nothing
  , _setAspectRatio = Nothing
  , _uiTexture = Nothing
  , _setUiTexture = Nothing
  , _jitterOffsetX = Nothing
  , _setJitterOffsetX = Nothing
  , _jitterOffsetY = Nothing
  , _setJitterOffsetY = Nothing
  , _uiTextureComposited = Nothing
  , _setUiTextureComposited = Nothing
  , _shouldResetHistory = Nothing
  , _setShouldResetHistory = Nothing
  , _outputTexture = Nothing
  , _setOutputTexture = Nothing
  , _fence = Nothing
  , _setFence = Nothing
  , _depthReversed = Nothing
  , _setDepthReversed = Nothing
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
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlfxFrameInterpolatorBaseDelegateClass #-}
mtlfxFrameInterpolatorBaseDelegateClass :: Class
mtlfxFrameInterpolatorBaseDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLFXFrameInterpolatorBase" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_inputWidth = unSelector (mkSelector "inputWidth")
      sel_inputHeight = unSelector (mkSelector "inputHeight")
      sel_outputWidth = unSelector (mkSelector "outputWidth")
      sel_outputHeight = unSelector (mkSelector "outputHeight")
      sel_colorTexture = unSelector (mkSelector "colorTexture")
      sel_setColorTexture = unSelector (mkSelector "setColorTexture:")
      sel_prevColorTexture = unSelector (mkSelector "prevColorTexture")
      sel_setPrevColorTexture = unSelector (mkSelector "setPrevColorTexture:")
      sel_depthTexture = unSelector (mkSelector "depthTexture")
      sel_setDepthTexture = unSelector (mkSelector "setDepthTexture:")
      sel_motionTexture = unSelector (mkSelector "motionTexture")
      sel_setMotionTexture = unSelector (mkSelector "setMotionTexture:")
      sel_motionVectorScaleX = unSelector (mkSelector "motionVectorScaleX")
      sel_setMotionVectorScaleX = unSelector (mkSelector "setMotionVectorScaleX:")
      sel_motionVectorScaleY = unSelector (mkSelector "motionVectorScaleY")
      sel_setMotionVectorScaleY = unSelector (mkSelector "setMotionVectorScaleY:")
      sel_deltaTime = unSelector (mkSelector "deltaTime")
      sel_setDeltaTime = unSelector (mkSelector "setDeltaTime:")
      sel_nearPlane = unSelector (mkSelector "nearPlane")
      sel_setNearPlane = unSelector (mkSelector "setNearPlane:")
      sel_farPlane = unSelector (mkSelector "farPlane")
      sel_setFarPlane = unSelector (mkSelector "setFarPlane:")
      sel_fieldOfView = unSelector (mkSelector "fieldOfView")
      sel_setFieldOfView = unSelector (mkSelector "setFieldOfView:")
      sel_aspectRatio = unSelector (mkSelector "aspectRatio")
      sel_setAspectRatio = unSelector (mkSelector "setAspectRatio:")
      sel_uiTexture = unSelector (mkSelector "uiTexture")
      sel_setUiTexture = unSelector (mkSelector "setUiTexture:")
      sel_jitterOffsetX = unSelector (mkSelector "jitterOffsetX")
      sel_setJitterOffsetX = unSelector (mkSelector "setJitterOffsetX:")
      sel_jitterOffsetY = unSelector (mkSelector "jitterOffsetY")
      sel_setJitterOffsetY = unSelector (mkSelector "setJitterOffsetY:")
      sel_uiTextureComposited = unSelector (mkSelector "uiTextureComposited")
      sel_setUiTextureComposited = unSelector (mkSelector "setUiTextureComposited:")
      sel_shouldResetHistory = unSelector (mkSelector "shouldResetHistory")
      sel_setShouldResetHistory = unSelector (mkSelector "setShouldResetHistory:")
      sel_outputTexture = unSelector (mkSelector "outputTexture")
      sel_setOutputTexture = unSelector (mkSelector "setOutputTexture:")
      sel_fence = unSelector (mkSelector "fence")
      sel_setFence = unSelector (mkSelector "setFence:")
      sel_depthReversed = unSelector (mkSelector "depthReversed")
      sel_setDepthReversed = unSelector (mkSelector "setDepthReversed:")
  -- inputWidth
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _inputWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputWidth" "Q@:" stub_0

  -- inputHeight
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _inputHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "inputHeight" "Q@:" stub_1

  -- outputWidth
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _outputWidth rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "outputWidth" "Q@:" stub_2

  -- outputHeight
  stub_3 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _outputHeight rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "outputHeight" "Q@:" stub_3

  -- colorTexture
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _colorTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "colorTexture" "@@:" stub_4

  -- setColorTexture:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setColorTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setColorTexture:" "v@:@" stub_5

  -- prevColorTexture
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _prevColorTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "prevColorTexture" "@@:" stub_6

  -- setPrevColorTexture:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setPrevColorTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setPrevColorTexture:" "v@:@" stub_7

  -- depthTexture
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _depthTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "depthTexture" "@@:" stub_8

  -- setDepthTexture:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setDepthTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDepthTexture:" "v@:@" stub_9

  -- motionTexture
  stub_10 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _motionTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "motionTexture" "@@:" stub_10

  -- setMotionTexture:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setMotionTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setMotionTexture:" "v@:@" stub_11

  -- motionVectorScaleX
  stub_12 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _motionVectorScaleX rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "motionVectorScaleX" "f@:" stub_12

  -- setMotionVectorScaleX:
  stub_13 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setMotionVectorScaleX rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setMotionVectorScaleX:" "v@:f" stub_13

  -- motionVectorScaleY
  stub_14 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _motionVectorScaleY rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "motionVectorScaleY" "f@:" stub_14

  -- setMotionVectorScaleY:
  stub_15 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setMotionVectorScaleY rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setMotionVectorScaleY:" "v@:f" stub_15

  -- deltaTime
  stub_16 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _deltaTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "deltaTime" "f@:" stub_16

  -- setDeltaTime:
  stub_17 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setDeltaTime rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setDeltaTime:" "v@:f" stub_17

  -- nearPlane
  stub_18 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _nearPlane rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "nearPlane" "f@:" stub_18

  -- setNearPlane:
  stub_19 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setNearPlane rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setNearPlane:" "v@:f" stub_19

  -- farPlane
  stub_20 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _farPlane rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "farPlane" "f@:" stub_20

  -- setFarPlane:
  stub_21 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setFarPlane rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setFarPlane:" "v@:f" stub_21

  -- fieldOfView
  stub_22 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _fieldOfView rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "fieldOfView" "f@:" stub_22

  -- setFieldOfView:
  stub_23 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setFieldOfView rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setFieldOfView:" "v@:f" stub_23

  -- aspectRatio
  stub_24 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _aspectRatio rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "aspectRatio" "f@:" stub_24

  -- setAspectRatio:
  stub_25 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setAspectRatio rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setAspectRatio:" "v@:f" stub_25

  -- uiTexture
  stub_26 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _uiTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "uiTexture" "@@:" stub_26

  -- setUiTexture:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setUiTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setUiTexture:" "v@:@" stub_27

  -- jitterOffsetX
  stub_28 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _jitterOffsetX rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "jitterOffsetX" "f@:" stub_28

  -- setJitterOffsetX:
  stub_29 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setJitterOffsetX rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setJitterOffsetX:" "v@:f" stub_29

  -- jitterOffsetY
  stub_30 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _jitterOffsetY rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "jitterOffsetY" "f@:" stub_30

  -- setJitterOffsetY:
  stub_31 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setJitterOffsetY rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setJitterOffsetY:" "v@:f" stub_31

  -- uiTextureComposited
  stub_32 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _uiTextureComposited rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "uiTextureComposited" "B@:" stub_32

  -- setUiTextureComposited:
  stub_33 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setUiTextureComposited rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setUiTextureComposited:" "v@:B" stub_33

  -- shouldResetHistory
  stub_34 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _shouldResetHistory rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldResetHistory" "B@:" stub_34

  -- setShouldResetHistory:
  stub_35 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setShouldResetHistory rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setShouldResetHistory:" "v@:B" stub_35

  -- outputTexture
  stub_36 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _outputTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outputTexture" "@@:" stub_36

  -- setOutputTexture:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setOutputTexture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setOutputTexture:" "v@:@" stub_37

  -- fence
  stub_38 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _fence rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "fence" "@@:" stub_38

  -- setFence:
  stub_39 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setFence rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setFence:" "v@:@" stub_39

  -- depthReversed
  stub_40 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _depthReversed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "depthReversed" "B@:" stub_40

  -- setDepthReversed:
  stub_41 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    case _setDepthReversed rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setDepthReversed:" "v@:B" stub_41

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFXFrameInterpolatorBaseOverrides
    if queriedSel == sel_inputWidth then pure (maybe 0 (const 1) (_inputWidth rec_))
    else if queriedSel == sel_inputHeight then pure (maybe 0 (const 1) (_inputHeight rec_))
    else if queriedSel == sel_outputWidth then pure (maybe 0 (const 1) (_outputWidth rec_))
    else if queriedSel == sel_outputHeight then pure (maybe 0 (const 1) (_outputHeight rec_))
    else if queriedSel == sel_colorTexture then pure (maybe 0 (const 1) (_colorTexture rec_))
    else if queriedSel == sel_setColorTexture then pure (maybe 0 (const 1) (_setColorTexture rec_))
    else if queriedSel == sel_prevColorTexture then pure (maybe 0 (const 1) (_prevColorTexture rec_))
    else if queriedSel == sel_setPrevColorTexture then pure (maybe 0 (const 1) (_setPrevColorTexture rec_))
    else if queriedSel == sel_depthTexture then pure (maybe 0 (const 1) (_depthTexture rec_))
    else if queriedSel == sel_setDepthTexture then pure (maybe 0 (const 1) (_setDepthTexture rec_))
    else if queriedSel == sel_motionTexture then pure (maybe 0 (const 1) (_motionTexture rec_))
    else if queriedSel == sel_setMotionTexture then pure (maybe 0 (const 1) (_setMotionTexture rec_))
    else if queriedSel == sel_motionVectorScaleX then pure (maybe 0 (const 1) (_motionVectorScaleX rec_))
    else if queriedSel == sel_setMotionVectorScaleX then pure (maybe 0 (const 1) (_setMotionVectorScaleX rec_))
    else if queriedSel == sel_motionVectorScaleY then pure (maybe 0 (const 1) (_motionVectorScaleY rec_))
    else if queriedSel == sel_setMotionVectorScaleY then pure (maybe 0 (const 1) (_setMotionVectorScaleY rec_))
    else if queriedSel == sel_deltaTime then pure (maybe 0 (const 1) (_deltaTime rec_))
    else if queriedSel == sel_setDeltaTime then pure (maybe 0 (const 1) (_setDeltaTime rec_))
    else if queriedSel == sel_nearPlane then pure (maybe 0 (const 1) (_nearPlane rec_))
    else if queriedSel == sel_setNearPlane then pure (maybe 0 (const 1) (_setNearPlane rec_))
    else if queriedSel == sel_farPlane then pure (maybe 0 (const 1) (_farPlane rec_))
    else if queriedSel == sel_setFarPlane then pure (maybe 0 (const 1) (_setFarPlane rec_))
    else if queriedSel == sel_fieldOfView then pure (maybe 0 (const 1) (_fieldOfView rec_))
    else if queriedSel == sel_setFieldOfView then pure (maybe 0 (const 1) (_setFieldOfView rec_))
    else if queriedSel == sel_aspectRatio then pure (maybe 0 (const 1) (_aspectRatio rec_))
    else if queriedSel == sel_setAspectRatio then pure (maybe 0 (const 1) (_setAspectRatio rec_))
    else if queriedSel == sel_uiTexture then pure (maybe 0 (const 1) (_uiTexture rec_))
    else if queriedSel == sel_setUiTexture then pure (maybe 0 (const 1) (_setUiTexture rec_))
    else if queriedSel == sel_jitterOffsetX then pure (maybe 0 (const 1) (_jitterOffsetX rec_))
    else if queriedSel == sel_setJitterOffsetX then pure (maybe 0 (const 1) (_setJitterOffsetX rec_))
    else if queriedSel == sel_jitterOffsetY then pure (maybe 0 (const 1) (_jitterOffsetY rec_))
    else if queriedSel == sel_setJitterOffsetY then pure (maybe 0 (const 1) (_setJitterOffsetY rec_))
    else if queriedSel == sel_uiTextureComposited then pure (maybe 0 (const 1) (_uiTextureComposited rec_))
    else if queriedSel == sel_setUiTextureComposited then pure (maybe 0 (const 1) (_setUiTextureComposited rec_))
    else if queriedSel == sel_shouldResetHistory then pure (maybe 0 (const 1) (_shouldResetHistory rec_))
    else if queriedSel == sel_setShouldResetHistory then pure (maybe 0 (const 1) (_setShouldResetHistory rec_))
    else if queriedSel == sel_outputTexture then pure (maybe 0 (const 1) (_outputTexture rec_))
    else if queriedSel == sel_setOutputTexture then pure (maybe 0 (const 1) (_setOutputTexture rec_))
    else if queriedSel == sel_fence then pure (maybe 0 (const 1) (_fence rec_))
    else if queriedSel == sel_setFence then pure (maybe 0 (const 1) (_setFence rec_))
    else if queriedSel == sel_depthReversed then pure (maybe 0 (const 1) (_depthReversed rec_))
    else if queriedSel == sel_setDepthReversed then pure (maybe 0 (const 1) (_setDepthReversed rec_))
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
newMTLFXFrameInterpolatorBase :: MTLFXFrameInterpolatorBaseOverrides -> IO RawId
newMTLFXFrameInterpolatorBase overrides = do
  inst <- class_createInstance mtlfxFrameInterpolatorBaseDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
