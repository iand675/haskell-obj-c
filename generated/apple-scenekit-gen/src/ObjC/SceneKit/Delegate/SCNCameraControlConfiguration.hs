{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNCameraControlConfiguration@.
--
-- Usage:
--
-- @
-- delegate <- newSCNCameraControlConfiguration defaultSCNCameraControlConfigurationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNCameraControlConfiguration
  ( SCNCameraControlConfigurationOverrides(..)
  , defaultSCNCameraControlConfigurationOverrides
  , newSCNCameraControlConfiguration
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

-- | Overrides record for @\@protocol SCNCameraControlConfiguration@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNCameraControlConfigurationOverrides = SCNCameraControlConfigurationOverrides
  { _autoSwitchToFreeCamera :: !(Maybe (IO Bool))
  , _setAutoSwitchToFreeCamera :: !(Maybe (Bool -> IO ()))
  , _allowsTranslation :: !(Maybe (IO Bool))
  , _setAllowsTranslation :: !(Maybe (Bool -> IO ()))
  , _flyModeVelocity :: !(Maybe (IO Double))
  , _setFlyModeVelocity :: !(Maybe (Double -> IO ()))
  , _panSensitivity :: !(Maybe (IO Double))
  , _setPanSensitivity :: !(Maybe (Double -> IO ()))
  , _truckSensitivity :: !(Maybe (IO Double))
  , _setTruckSensitivity :: !(Maybe (Double -> IO ()))
  , _rotationSensitivity :: !(Maybe (IO Double))
  , _setRotationSensitivity :: !(Maybe (Double -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNCameraControlConfigurationOverrides :: SCNCameraControlConfigurationOverrides
defaultSCNCameraControlConfigurationOverrides = SCNCameraControlConfigurationOverrides
  { _autoSwitchToFreeCamera = Nothing
  , _setAutoSwitchToFreeCamera = Nothing
  , _allowsTranslation = Nothing
  , _setAllowsTranslation = Nothing
  , _flyModeVelocity = Nothing
  , _setFlyModeVelocity = Nothing
  , _panSensitivity = Nothing
  , _setPanSensitivity = Nothing
  , _truckSensitivity = Nothing
  , _setTruckSensitivity = Nothing
  , _rotationSensitivity = Nothing
  , _setRotationSensitivity = Nothing
  }

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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnCameraControlConfigurationDelegateClass #-}
scnCameraControlConfigurationDelegateClass :: Class
scnCameraControlConfigurationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNCameraControlConfiguration" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_autoSwitchToFreeCamera = unSelector (mkSelector "autoSwitchToFreeCamera")
      sel_setAutoSwitchToFreeCamera = unSelector (mkSelector "setAutoSwitchToFreeCamera:")
      sel_allowsTranslation = unSelector (mkSelector "allowsTranslation")
      sel_setAllowsTranslation = unSelector (mkSelector "setAllowsTranslation:")
      sel_flyModeVelocity = unSelector (mkSelector "flyModeVelocity")
      sel_setFlyModeVelocity = unSelector (mkSelector "setFlyModeVelocity:")
      sel_panSensitivity = unSelector (mkSelector "panSensitivity")
      sel_setPanSensitivity = unSelector (mkSelector "setPanSensitivity:")
      sel_truckSensitivity = unSelector (mkSelector "truckSensitivity")
      sel_setTruckSensitivity = unSelector (mkSelector "setTruckSensitivity:")
      sel_rotationSensitivity = unSelector (mkSelector "rotationSensitivity")
      sel_setRotationSensitivity = unSelector (mkSelector "setRotationSensitivity:")
  -- autoSwitchToFreeCamera
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _autoSwitchToFreeCamera rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "autoSwitchToFreeCamera" "B@:" stub_0

  -- setAutoSwitchToFreeCamera:
  stub_1 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _setAutoSwitchToFreeCamera rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAutoSwitchToFreeCamera:" "v@:B" stub_1

  -- allowsTranslation
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _allowsTranslation rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "allowsTranslation" "B@:" stub_2

  -- setAllowsTranslation:
  stub_3 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _setAllowsTranslation rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAllowsTranslation:" "v@:B" stub_3

  -- flyModeVelocity
  stub_4 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _flyModeVelocity rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "flyModeVelocity" "d@:" stub_4

  -- setFlyModeVelocity:
  stub_5 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _setFlyModeVelocity rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setFlyModeVelocity:" "v@:d" stub_5

  -- panSensitivity
  stub_6 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _panSensitivity rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "panSensitivity" "d@:" stub_6

  -- setPanSensitivity:
  stub_7 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _setPanSensitivity rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setPanSensitivity:" "v@:d" stub_7

  -- truckSensitivity
  stub_8 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _truckSensitivity rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "truckSensitivity" "d@:" stub_8

  -- setTruckSensitivity:
  stub_9 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _setTruckSensitivity rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setTruckSensitivity:" "v@:d" stub_9

  -- rotationSensitivity
  stub_10 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _rotationSensitivity rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "rotationSensitivity" "d@:" stub_10

  -- setRotationSensitivity:
  stub_11 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    case _setRotationSensitivity rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setRotationSensitivity:" "v@:d" stub_11

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControlConfigurationOverrides
    if queriedSel == sel_autoSwitchToFreeCamera then pure (maybe 0 (const 1) (_autoSwitchToFreeCamera rec_))
    else if queriedSel == sel_setAutoSwitchToFreeCamera then pure (maybe 0 (const 1) (_setAutoSwitchToFreeCamera rec_))
    else if queriedSel == sel_allowsTranslation then pure (maybe 0 (const 1) (_allowsTranslation rec_))
    else if queriedSel == sel_setAllowsTranslation then pure (maybe 0 (const 1) (_setAllowsTranslation rec_))
    else if queriedSel == sel_flyModeVelocity then pure (maybe 0 (const 1) (_flyModeVelocity rec_))
    else if queriedSel == sel_setFlyModeVelocity then pure (maybe 0 (const 1) (_setFlyModeVelocity rec_))
    else if queriedSel == sel_panSensitivity then pure (maybe 0 (const 1) (_panSensitivity rec_))
    else if queriedSel == sel_setPanSensitivity then pure (maybe 0 (const 1) (_setPanSensitivity rec_))
    else if queriedSel == sel_truckSensitivity then pure (maybe 0 (const 1) (_truckSensitivity rec_))
    else if queriedSel == sel_setTruckSensitivity then pure (maybe 0 (const 1) (_setTruckSensitivity rec_))
    else if queriedSel == sel_rotationSensitivity then pure (maybe 0 (const 1) (_rotationSensitivity rec_))
    else if queriedSel == sel_setRotationSensitivity then pure (maybe 0 (const 1) (_setRotationSensitivity rec_))
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
newSCNCameraControlConfiguration :: SCNCameraControlConfigurationOverrides -> IO RawId
newSCNCameraControlConfiguration overrides = do
  inst <- class_createInstance scnCameraControlConfigurationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
