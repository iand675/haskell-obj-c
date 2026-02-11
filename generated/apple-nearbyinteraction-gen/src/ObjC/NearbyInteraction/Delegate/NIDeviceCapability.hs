{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NIDeviceCapability@.
--
-- Usage:
--
-- @
-- delegate <- newNIDeviceCapability defaultNIDeviceCapabilityOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.NearbyInteraction.Delegate.NIDeviceCapability
  ( NIDeviceCapabilityOverrides(..)
  , defaultNIDeviceCapabilityOverrides
  , newNIDeviceCapability
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

-- | Overrides record for @\@protocol NIDeviceCapability@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NIDeviceCapabilityOverrides = NIDeviceCapabilityOverrides
  { _supportsPreciseDistanceMeasurement :: !(Maybe (IO Bool))
  , _supportsDirectionMeasurement :: !(Maybe (IO Bool))
  , _supportsCameraAssistance :: !(Maybe (IO Bool))
  , _supportsExtendedDistanceMeasurement :: !(Maybe (IO Bool))
  , _supportsDLTDOAMeasurement :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNIDeviceCapabilityOverrides :: NIDeviceCapabilityOverrides
defaultNIDeviceCapabilityOverrides = NIDeviceCapabilityOverrides
  { _supportsPreciseDistanceMeasurement = Nothing
  , _supportsDirectionMeasurement = Nothing
  , _supportsCameraAssistance = Nothing
  , _supportsExtendedDistanceMeasurement = Nothing
  , _supportsDLTDOAMeasurement = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE niDeviceCapabilityDelegateClass #-}
niDeviceCapabilityDelegateClass :: Class
niDeviceCapabilityDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNIDeviceCapability" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_supportsPreciseDistanceMeasurement = unSelector (mkSelector "supportsPreciseDistanceMeasurement")
      sel_supportsDirectionMeasurement = unSelector (mkSelector "supportsDirectionMeasurement")
      sel_supportsCameraAssistance = unSelector (mkSelector "supportsCameraAssistance")
      sel_supportsExtendedDistanceMeasurement = unSelector (mkSelector "supportsExtendedDistanceMeasurement")
      sel_supportsDLTDOAMeasurement = unSelector (mkSelector "supportsDLTDOAMeasurement")
  -- supportsPreciseDistanceMeasurement
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NIDeviceCapabilityOverrides
    case _supportsPreciseDistanceMeasurement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsPreciseDistanceMeasurement" "B@:" stub_0

  -- supportsDirectionMeasurement
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NIDeviceCapabilityOverrides
    case _supportsDirectionMeasurement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsDirectionMeasurement" "B@:" stub_1

  -- supportsCameraAssistance
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NIDeviceCapabilityOverrides
    case _supportsCameraAssistance rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsCameraAssistance" "B@:" stub_2

  -- supportsExtendedDistanceMeasurement
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NIDeviceCapabilityOverrides
    case _supportsExtendedDistanceMeasurement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsExtendedDistanceMeasurement" "B@:" stub_3

  -- supportsDLTDOAMeasurement
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NIDeviceCapabilityOverrides
    case _supportsDLTDOAMeasurement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsDLTDOAMeasurement" "B@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NIDeviceCapabilityOverrides
    if queriedSel == sel_supportsPreciseDistanceMeasurement then pure (maybe 0 (const 1) (_supportsPreciseDistanceMeasurement rec_))
    else if queriedSel == sel_supportsDirectionMeasurement then pure (maybe 0 (const 1) (_supportsDirectionMeasurement rec_))
    else if queriedSel == sel_supportsCameraAssistance then pure (maybe 0 (const 1) (_supportsCameraAssistance rec_))
    else if queriedSel == sel_supportsExtendedDistanceMeasurement then pure (maybe 0 (const 1) (_supportsExtendedDistanceMeasurement rec_))
    else if queriedSel == sel_supportsDLTDOAMeasurement then pure (maybe 0 (const 1) (_supportsDLTDOAMeasurement rec_))
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
newNIDeviceCapability :: NIDeviceCapabilityOverrides -> IO RawId
newNIDeviceCapability overrides = do
  inst <- class_createInstance niDeviceCapabilityDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
