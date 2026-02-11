{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CMIOExtensionDeviceSource@.
--
-- Usage:
--
-- @
-- delegate <- newCMIOExtensionDeviceSource defaultCMIOExtensionDeviceSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreMediaIO.Delegate.CMIOExtensionDeviceSource
  ( CMIOExtensionDeviceSourceOverrides(..)
  , defaultCMIOExtensionDeviceSourceOverrides
  , newCMIOExtensionDeviceSource
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

-- | Overrides record for @\@protocol CMIOExtensionDeviceSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CMIOExtensionDeviceSourceOverrides = CMIOExtensionDeviceSourceOverrides
  { _devicePropertiesForProperties_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _setDeviceProperties_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _availableProperties :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultCMIOExtensionDeviceSourceOverrides :: CMIOExtensionDeviceSourceOverrides
defaultCMIOExtensionDeviceSourceOverrides = CMIOExtensionDeviceSourceOverrides
  { _devicePropertiesForProperties_error = Nothing
  , _setDeviceProperties_error = Nothing
  , _availableProperties = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cmioExtensionDeviceSourceDelegateClass #-}
cmioExtensionDeviceSourceDelegateClass :: Class
cmioExtensionDeviceSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCMIOExtensionDeviceSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_devicePropertiesForProperties_error = unSelector (mkSelector "devicePropertiesForProperties:error:")
      sel_setDeviceProperties_error = unSelector (mkSelector "setDeviceProperties:error:")
      sel_availableProperties = unSelector (mkSelector "availableProperties")
  -- devicePropertiesForProperties:error:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionDeviceSourceOverrides
    case _devicePropertiesForProperties_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "devicePropertiesForProperties:error:" "@@:@@" stub_0

  -- setDeviceProperties:error:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionDeviceSourceOverrides
    case _setDeviceProperties_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "setDeviceProperties:error:" "B@:@@" stub_1

  -- availableProperties
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionDeviceSourceOverrides
    case _availableProperties rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "availableProperties" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionDeviceSourceOverrides
    if queriedSel == sel_devicePropertiesForProperties_error then pure (maybe 0 (const 1) (_devicePropertiesForProperties_error rec_))
    else if queriedSel == sel_setDeviceProperties_error then pure (maybe 0 (const 1) (_setDeviceProperties_error rec_))
    else if queriedSel == sel_availableProperties then pure (maybe 0 (const 1) (_availableProperties rec_))
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
newCMIOExtensionDeviceSource :: CMIOExtensionDeviceSourceOverrides -> IO RawId
newCMIOExtensionDeviceSource overrides = do
  inst <- class_createInstance cmioExtensionDeviceSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
