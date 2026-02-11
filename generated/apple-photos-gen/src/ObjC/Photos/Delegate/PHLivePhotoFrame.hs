{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PHLivePhotoFrame@.
--
-- Usage:
--
-- @
-- delegate <- newPHLivePhotoFrame defaultPHLivePhotoFrameOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Photos.Delegate.PHLivePhotoFrame
  ( PHLivePhotoFrameOverrides(..)
  , defaultPHLivePhotoFrameOverrides
  , newPHLivePhotoFrame
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

-- | Overrides record for @\@protocol PHLivePhotoFrame@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PHLivePhotoFrameOverrides = PHLivePhotoFrameOverrides
  { _image :: !(Maybe (IO RawId))
  , _renderScale :: !(Maybe (IO Double))
  }

-- | Default overrides with all methods unimplemented.
defaultPHLivePhotoFrameOverrides :: PHLivePhotoFrameOverrides
defaultPHLivePhotoFrameOverrides = PHLivePhotoFrameOverrides
  { _image = Nothing
  , _renderScale = Nothing
  }

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE phLivePhotoFrameDelegateClass #-}
phLivePhotoFrameDelegateClass :: Class
phLivePhotoFrameDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPHLivePhotoFrame" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_image = unSelector (mkSelector "image")
      sel_renderScale = unSelector (mkSelector "renderScale")
  -- image
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHLivePhotoFrameOverrides
    case _image rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "image" "@@:" stub_0

  -- renderScale
  stub_1 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHLivePhotoFrameOverrides
    case _renderScale rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "renderScale" "d@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHLivePhotoFrameOverrides
    if queriedSel == sel_image then pure (maybe 0 (const 1) (_image rec_))
    else if queriedSel == sel_renderScale then pure (maybe 0 (const 1) (_renderScale rec_))
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
newPHLivePhotoFrame :: PHLivePhotoFrameOverrides -> IO RawId
newPHLivePhotoFrame overrides = do
  inst <- class_createInstance phLivePhotoFrameDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
