{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MEVideoDecoderExtension@.
--
-- Usage:
--
-- @
-- delegate <- newMEVideoDecoderExtension defaultMEVideoDecoderExtensionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.MEVideoDecoderExtension
  ( MEVideoDecoderExtensionOverrides(..)
  , defaultMEVideoDecoderExtensionOverrides
  , newMEVideoDecoderExtension
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

-- | Overrides record for @\@protocol MEVideoDecoderExtension@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MEVideoDecoderExtensionOverrides = MEVideoDecoderExtensionOverrides
  { _init :: !(Maybe (IO RawId))
  , _videoDecoderWithCodecType_videoFormatDescription_videoDecoderSpecifications_extensionDecoderPixelBufferManager_error :: !(Maybe (Int -> RawId -> RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMEVideoDecoderExtensionOverrides :: MEVideoDecoderExtensionOverrides
defaultMEVideoDecoderExtensionOverrides = MEVideoDecoderExtensionOverrides
  { _init = Nothing
  , _videoDecoderWithCodecType_videoFormatDescription_videoDecoderSpecifications_extensionDecoderPixelBufferManager_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_I_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meVideoDecoderExtensionDelegateClass #-}
meVideoDecoderExtensionDelegateClass :: Class
meVideoDecoderExtensionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMEVideoDecoderExtension" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_init = unSelector (mkSelector "init")
      sel_videoDecoderWithCodecType_videoFormatDescription_videoDecoderSpecifications_extensionDecoderPixelBufferManager_error = unSelector (mkSelector "videoDecoderWithCodecType:videoFormatDescription:videoDecoderSpecifications:extensionDecoderPixelBufferManager:error:")
  -- init
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderExtensionOverrides
    case _init rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "init" "@@:" stub_0

  -- videoDecoderWithCodecType:videoFormatDescription:videoDecoderSpecifications:extensionDecoderPixelBufferManager:error:
  stub_1 <- wrap_I_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderExtensionOverrides
    case _videoDecoderWithCodecType_videoFormatDescription_videoDecoderSpecifications_extensionDecoderPixelBufferManager_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "videoDecoderWithCodecType:videoFormatDescription:videoDecoderSpecifications:extensionDecoderPixelBufferManager:error:" "@@:I@@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderExtensionOverrides
    if queriedSel == sel_init then pure (maybe 0 (const 1) (_init rec_))
    else if queriedSel == sel_videoDecoderWithCodecType_videoFormatDescription_videoDecoderSpecifications_extensionDecoderPixelBufferManager_error then pure (maybe 0 (const 1) (_videoDecoderWithCodecType_videoFormatDescription_videoDecoderSpecifications_extensionDecoderPixelBufferManager_error rec_))
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
newMEVideoDecoderExtension :: MEVideoDecoderExtensionOverrides -> IO RawId
newMEVideoDecoderExtension overrides = do
  inst <- class_createInstance meVideoDecoderExtensionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
