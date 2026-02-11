{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CMIOExtensionStreamSource@.
--
-- Usage:
--
-- @
-- delegate <- newCMIOExtensionStreamSource defaultCMIOExtensionStreamSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreMediaIO.Delegate.CMIOExtensionStreamSource
  ( CMIOExtensionStreamSourceOverrides(..)
  , defaultCMIOExtensionStreamSourceOverrides
  , newCMIOExtensionStreamSource
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

-- | Overrides record for @\@protocol CMIOExtensionStreamSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CMIOExtensionStreamSourceOverrides = CMIOExtensionStreamSourceOverrides
  { _streamPropertiesForProperties_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _setStreamProperties_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _authorizedToStartStreamForClient :: !(Maybe (RawId -> IO Bool))
  , _startStreamAndReturnError :: !(Maybe (RawId -> IO Bool))
  , _stopStreamAndReturnError :: !(Maybe (RawId -> IO Bool))
  , _formats :: !(Maybe (IO RawId))
  , _availableProperties :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultCMIOExtensionStreamSourceOverrides :: CMIOExtensionStreamSourceOverrides
defaultCMIOExtensionStreamSourceOverrides = CMIOExtensionStreamSourceOverrides
  { _streamPropertiesForProperties_error = Nothing
  , _setStreamProperties_error = Nothing
  , _authorizedToStartStreamForClient = Nothing
  , _startStreamAndReturnError = Nothing
  , _stopStreamAndReturnError = Nothing
  , _formats = Nothing
  , _availableProperties = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

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
{-# NOINLINE cmioExtensionStreamSourceDelegateClass #-}
cmioExtensionStreamSourceDelegateClass :: Class
cmioExtensionStreamSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCMIOExtensionStreamSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_streamPropertiesForProperties_error = unSelector (mkSelector "streamPropertiesForProperties:error:")
      sel_setStreamProperties_error = unSelector (mkSelector "setStreamProperties:error:")
      sel_authorizedToStartStreamForClient = unSelector (mkSelector "authorizedToStartStreamForClient:")
      sel_startStreamAndReturnError = unSelector (mkSelector "startStreamAndReturnError:")
      sel_stopStreamAndReturnError = unSelector (mkSelector "stopStreamAndReturnError:")
      sel_formats = unSelector (mkSelector "formats")
      sel_availableProperties = unSelector (mkSelector "availableProperties")
  -- streamPropertiesForProperties:error:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _streamPropertiesForProperties_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "streamPropertiesForProperties:error:" "@@:@@" stub_0

  -- setStreamProperties:error:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _setStreamProperties_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "setStreamProperties:error:" "B@:@@" stub_1

  -- authorizedToStartStreamForClient:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _authorizedToStartStreamForClient rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "authorizedToStartStreamForClient:" "B@:@" stub_2

  -- startStreamAndReturnError:
  stub_3 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _startStreamAndReturnError rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "startStreamAndReturnError:" "B@:@" stub_3

  -- stopStreamAndReturnError:
  stub_4 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _stopStreamAndReturnError rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "stopStreamAndReturnError:" "B@:@" stub_4

  -- formats
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _formats rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "formats" "@@:" stub_5

  -- availableProperties
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    case _availableProperties rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "availableProperties" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMIOExtensionStreamSourceOverrides
    if queriedSel == sel_streamPropertiesForProperties_error then pure (maybe 0 (const 1) (_streamPropertiesForProperties_error rec_))
    else if queriedSel == sel_setStreamProperties_error then pure (maybe 0 (const 1) (_setStreamProperties_error rec_))
    else if queriedSel == sel_authorizedToStartStreamForClient then pure (maybe 0 (const 1) (_authorizedToStartStreamForClient rec_))
    else if queriedSel == sel_startStreamAndReturnError then pure (maybe 0 (const 1) (_startStreamAndReturnError rec_))
    else if queriedSel == sel_stopStreamAndReturnError then pure (maybe 0 (const 1) (_stopStreamAndReturnError rec_))
    else if queriedSel == sel_formats then pure (maybe 0 (const 1) (_formats rec_))
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
newCMIOExtensionStreamSource :: CMIOExtensionStreamSourceOverrides -> IO RawId
newCMIOExtensionStreamSource overrides = do
  inst <- class_createInstance cmioExtensionStreamSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
