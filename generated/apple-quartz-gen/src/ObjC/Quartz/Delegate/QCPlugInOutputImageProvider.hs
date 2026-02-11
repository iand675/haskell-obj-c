{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol QCPlugInOutputImageProvider@.
--
-- Usage:
--
-- @
-- delegate <- newQCPlugInOutputImageProvider defaultQCPlugInOutputImageProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.QCPlugInOutputImageProvider
  ( QCPlugInOutputImageProviderOverrides(..)
  , defaultQCPlugInOutputImageProviderOverrides
  , newQCPlugInOutputImageProvider
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

-- | Overrides record for @\@protocol QCPlugInOutputImageProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data QCPlugInOutputImageProviderOverrides = QCPlugInOutputImageProviderOverrides
  { _shouldColorMatch :: !(Maybe (IO Bool))
  , _supportedBufferPixelFormats :: !(Maybe (IO RawId))
  , _supportedRenderedTexturePixelFormats :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultQCPlugInOutputImageProviderOverrides :: QCPlugInOutputImageProviderOverrides
defaultQCPlugInOutputImageProviderOverrides = QCPlugInOutputImageProviderOverrides
  { _shouldColorMatch = Nothing
  , _supportedBufferPixelFormats = Nothing
  , _supportedRenderedTexturePixelFormats = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE qcPlugInOutputImageProviderDelegateClass #-}
qcPlugInOutputImageProviderDelegateClass :: Class
qcPlugInOutputImageProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsQCPlugInOutputImageProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_shouldColorMatch = unSelector (mkSelector "shouldColorMatch")
      sel_supportedBufferPixelFormats = unSelector (mkSelector "supportedBufferPixelFormats")
      sel_supportedRenderedTexturePixelFormats = unSelector (mkSelector "supportedRenderedTexturePixelFormats")
  -- shouldColorMatch
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInOutputImageProviderOverrides
    case _shouldColorMatch rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldColorMatch" "B@:" stub_0

  -- supportedBufferPixelFormats
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInOutputImageProviderOverrides
    case _supportedBufferPixelFormats rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedBufferPixelFormats" "@@:" stub_1

  -- supportedRenderedTexturePixelFormats
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInOutputImageProviderOverrides
    case _supportedRenderedTexturePixelFormats rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedRenderedTexturePixelFormats" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInOutputImageProviderOverrides
    if queriedSel == sel_shouldColorMatch then pure (maybe 0 (const 1) (_shouldColorMatch rec_))
    else if queriedSel == sel_supportedBufferPixelFormats then pure (maybe 0 (const 1) (_supportedBufferPixelFormats rec_))
    else if queriedSel == sel_supportedRenderedTexturePixelFormats then pure (maybe 0 (const 1) (_supportedRenderedTexturePixelFormats rec_))
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
newQCPlugInOutputImageProvider :: QCPlugInOutputImageProviderOverrides -> IO RawId
newQCPlugInOutputImageProvider overrides = do
  inst <- class_createInstance qcPlugInOutputImageProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
