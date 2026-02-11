{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol VTFrameProcessorConfiguration@.
--
-- Usage:
--
-- @
-- delegate <- newVTFrameProcessorConfiguration defaultVTFrameProcessorConfigurationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.VideoToolbox.Delegate.VTFrameProcessorConfiguration
  ( VTFrameProcessorConfigurationOverrides(..)
  , defaultVTFrameProcessorConfigurationOverrides
  , newVTFrameProcessorConfiguration
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

-- | Overrides record for @\@protocol VTFrameProcessorConfiguration@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data VTFrameProcessorConfigurationOverrides = VTFrameProcessorConfigurationOverrides
  { _frameSupportedPixelFormats :: !(Maybe (IO RawId))
  , _sourcePixelBufferAttributes :: !(Maybe (IO RawId))
  , _destinationPixelBufferAttributes :: !(Maybe (IO RawId))
  , _nextFrameCount :: !(Maybe (IO Int))
  , _previousFrameCount :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultVTFrameProcessorConfigurationOverrides :: VTFrameProcessorConfigurationOverrides
defaultVTFrameProcessorConfigurationOverrides = VTFrameProcessorConfigurationOverrides
  { _frameSupportedPixelFormats = Nothing
  , _sourcePixelBufferAttributes = Nothing
  , _destinationPixelBufferAttributes = Nothing
  , _nextFrameCount = Nothing
  , _previousFrameCount = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE vtFrameProcessorConfigurationDelegateClass #-}
vtFrameProcessorConfigurationDelegateClass :: Class
vtFrameProcessorConfigurationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsVTFrameProcessorConfiguration" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_frameSupportedPixelFormats = unSelector (mkSelector "frameSupportedPixelFormats")
      sel_sourcePixelBufferAttributes = unSelector (mkSelector "sourcePixelBufferAttributes")
      sel_destinationPixelBufferAttributes = unSelector (mkSelector "destinationPixelBufferAttributes")
      sel_nextFrameCount = unSelector (mkSelector "nextFrameCount")
      sel_previousFrameCount = unSelector (mkSelector "previousFrameCount")
  -- frameSupportedPixelFormats
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorConfigurationOverrides
    case _frameSupportedPixelFormats rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "frameSupportedPixelFormats" "@@:" stub_0

  -- sourcePixelBufferAttributes
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorConfigurationOverrides
    case _sourcePixelBufferAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sourcePixelBufferAttributes" "@@:" stub_1

  -- destinationPixelBufferAttributes
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorConfigurationOverrides
    case _destinationPixelBufferAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "destinationPixelBufferAttributes" "@@:" stub_2

  -- nextFrameCount
  stub_3 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorConfigurationOverrides
    case _nextFrameCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "nextFrameCount" "q@:" stub_3

  -- previousFrameCount
  stub_4 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorConfigurationOverrides
    case _previousFrameCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "previousFrameCount" "q@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VTFrameProcessorConfigurationOverrides
    if queriedSel == sel_frameSupportedPixelFormats then pure (maybe 0 (const 1) (_frameSupportedPixelFormats rec_))
    else if queriedSel == sel_sourcePixelBufferAttributes then pure (maybe 0 (const 1) (_sourcePixelBufferAttributes rec_))
    else if queriedSel == sel_destinationPixelBufferAttributes then pure (maybe 0 (const 1) (_destinationPixelBufferAttributes rec_))
    else if queriedSel == sel_nextFrameCount then pure (maybe 0 (const 1) (_nextFrameCount rec_))
    else if queriedSel == sel_previousFrameCount then pure (maybe 0 (const 1) (_previousFrameCount rec_))
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
newVTFrameProcessorConfiguration :: VTFrameProcessorConfigurationOverrides -> IO RawId
newVTFrameProcessorConfiguration overrides = do
  inst <- class_createInstance vtFrameProcessorConfigurationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
