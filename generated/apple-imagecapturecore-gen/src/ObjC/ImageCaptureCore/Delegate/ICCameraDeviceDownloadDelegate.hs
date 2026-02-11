{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ICCameraDeviceDownloadDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newICCameraDeviceDownloadDelegate defaultICCameraDeviceDownloadDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ImageCaptureCore.Delegate.ICCameraDeviceDownloadDelegate
  ( ICCameraDeviceDownloadDelegateOverrides(..)
  , defaultICCameraDeviceDownloadDelegateOverrides
  , newICCameraDeviceDownloadDelegate
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

-- | Overrides record for @\@protocol ICCameraDeviceDownloadDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ICCameraDeviceDownloadDelegateOverrides = ICCameraDeviceDownloadDelegateOverrides
  { _didReceiveDownloadProgressForFile_downloadedBytes_maxBytes :: !(Maybe (RawId -> Int -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultICCameraDeviceDownloadDelegateOverrides :: ICCameraDeviceDownloadDelegateOverrides
defaultICCameraDeviceDownloadDelegateOverrides = ICCameraDeviceDownloadDelegateOverrides
  { _didReceiveDownloadProgressForFile_downloadedBytes_maxBytes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE icCameraDeviceDownloadDelegateDelegateClass #-}
icCameraDeviceDownloadDelegateDelegateClass :: Class
icCameraDeviceDownloadDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsICCameraDeviceDownloadDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_didReceiveDownloadProgressForFile_downloadedBytes_maxBytes = unSelector (mkSelector "didReceiveDownloadProgressForFile:downloadedBytes:maxBytes:")
  -- didReceiveDownloadProgressForFile:downloadedBytes:maxBytes:
  stub_0 <- wrap_at_q_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDownloadDelegateOverrides
    case _didReceiveDownloadProgressForFile_downloadedBytes_maxBytes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "didReceiveDownloadProgressForFile:downloadedBytes:maxBytes:" "v@:@qq" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDownloadDelegateOverrides
    if queriedSel == sel_didReceiveDownloadProgressForFile_downloadedBytes_maxBytes then pure (maybe 0 (const 1) (_didReceiveDownloadProgressForFile_downloadedBytes_maxBytes rec_))
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
newICCameraDeviceDownloadDelegate :: ICCameraDeviceDownloadDelegateOverrides -> IO RawId
newICCameraDeviceDownloadDelegate overrides = do
  inst <- class_createInstance icCameraDeviceDownloadDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
