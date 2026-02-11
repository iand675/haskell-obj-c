{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MERAWProcessorExtension@.
--
-- Usage:
--
-- @
-- delegate <- newMERAWProcessorExtension defaultMERAWProcessorExtensionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.MERAWProcessorExtension
  ( MERAWProcessorExtensionOverrides(..)
  , defaultMERAWProcessorExtensionOverrides
  , newMERAWProcessorExtension
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

-- | Overrides record for @\@protocol MERAWProcessorExtension@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MERAWProcessorExtensionOverrides = MERAWProcessorExtensionOverrides
  { _init :: !(Maybe (IO RawId))
  , _processorWithFormatDescription_extensionPixelBufferManager_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMERAWProcessorExtensionOverrides :: MERAWProcessorExtensionOverrides
defaultMERAWProcessorExtensionOverrides = MERAWProcessorExtensionOverrides
  { _init = Nothing
  , _processorWithFormatDescription_extensionPixelBufferManager_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE merawProcessorExtensionDelegateClass #-}
merawProcessorExtensionDelegateClass :: Class
merawProcessorExtensionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMERAWProcessorExtension" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_init = unSelector (mkSelector "init")
      sel_processorWithFormatDescription_extensionPixelBufferManager_error = unSelector (mkSelector "processorWithFormatDescription:extensionPixelBufferManager:error:")
  -- init
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorExtensionOverrides
    case _init rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "init" "@@:" stub_0

  -- processorWithFormatDescription:extensionPixelBufferManager:error:
  stub_1 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorExtensionOverrides
    case _processorWithFormatDescription_extensionPixelBufferManager_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "processorWithFormatDescription:extensionPixelBufferManager:error:" "@@:@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorExtensionOverrides
    if queriedSel == sel_init then pure (maybe 0 (const 1) (_init rec_))
    else if queriedSel == sel_processorWithFormatDescription_extensionPixelBufferManager_error then pure (maybe 0 (const 1) (_processorWithFormatDescription_extensionPixelBufferManager_error rec_))
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
newMERAWProcessorExtension :: MERAWProcessorExtensionOverrides -> IO RawId
newMERAWProcessorExtension overrides = do
  inst <- class_createInstance merawProcessorExtensionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
