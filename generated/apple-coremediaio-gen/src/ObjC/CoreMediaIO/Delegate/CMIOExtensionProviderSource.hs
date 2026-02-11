{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CMIOExtensionProviderSource@.
--
-- Usage:
--
-- @
-- delegate <- newCMIOExtensionProviderSource defaultCMIOExtensionProviderSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreMediaIO.Delegate.CMIOExtensionProviderSource
  ( CMIOExtensionProviderSourceOverrides(..)
  , defaultCMIOExtensionProviderSourceOverrides
  , newCMIOExtensionProviderSource
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

-- | Overrides record for @\@protocol CMIOExtensionProviderSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CMIOExtensionProviderSourceOverrides = CMIOExtensionProviderSourceOverrides
  { _connectClient_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _disconnectClient :: !(Maybe (RawId -> IO ()))
  , _providerPropertiesForProperties_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _setProviderProperties_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _availableProperties :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultCMIOExtensionProviderSourceOverrides :: CMIOExtensionProviderSourceOverrides
defaultCMIOExtensionProviderSourceOverrides = CMIOExtensionProviderSourceOverrides
  { _connectClient_error = Nothing
  , _disconnectClient = Nothing
  , _providerPropertiesForProperties_error = Nothing
  , _setProviderProperties_error = Nothing
  , _availableProperties = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cmioExtensionProviderSourceDelegateClass #-}
cmioExtensionProviderSourceDelegateClass :: Class
cmioExtensionProviderSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCMIOExtensionProviderSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_connectClient_error = unSelector (mkSelector "connectClient:error:")
      sel_disconnectClient = unSelector (mkSelector "disconnectClient:")
      sel_providerPropertiesForProperties_error = unSelector (mkSelector "providerPropertiesForProperties:error:")
      sel_setProviderProperties_error = unSelector (mkSelector "setProviderProperties:error:")
      sel_availableProperties = unSelector (mkSelector "availableProperties")
  -- connectClient:error:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionProviderSourceOverrides
    case _connectClient_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "connectClient:error:" "B@:@@" stub_0

  -- disconnectClient:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionProviderSourceOverrides
    case _disconnectClient rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "disconnectClient:" "v@:@" stub_1

  -- providerPropertiesForProperties:error:
  stub_2 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionProviderSourceOverrides
    case _providerPropertiesForProperties_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "providerPropertiesForProperties:error:" "@@:@@" stub_2

  -- setProviderProperties:error:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionProviderSourceOverrides
    case _setProviderProperties_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "setProviderProperties:error:" "B@:@@" stub_3

  -- availableProperties
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionProviderSourceOverrides
    case _availableProperties rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "availableProperties" "@@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionProviderSourceOverrides
    if queriedSel == sel_connectClient_error then pure (maybe 0 (const 1) (_connectClient_error rec_))
    else if queriedSel == sel_disconnectClient then pure (maybe 0 (const 1) (_disconnectClient rec_))
    else if queriedSel == sel_providerPropertiesForProperties_error then pure (maybe 0 (const 1) (_providerPropertiesForProperties_error rec_))
    else if queriedSel == sel_setProviderProperties_error then pure (maybe 0 (const 1) (_setProviderProperties_error rec_))
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
newCMIOExtensionProviderSource :: CMIOExtensionProviderSourceOverrides -> IO RawId
newCMIOExtensionProviderSource overrides = do
  inst <- class_createInstance cmioExtensionProviderSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
