{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCloudSharingServiceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSCloudSharingServiceDelegate defaultNSCloudSharingServiceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCloudSharingServiceDelegate
  ( NSCloudSharingServiceDelegateOverrides(..)
  , defaultNSCloudSharingServiceDelegateOverrides
  , newNSCloudSharingServiceDelegate
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

-- | Overrides record for @\@protocol NSCloudSharingServiceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCloudSharingServiceDelegateOverrides = NSCloudSharingServiceDelegateOverrides
  { _sharingService_didCompleteForItems_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _sharingService_didSaveShare :: !(Maybe (RawId -> RawId -> IO ()))
  , _sharingService_didStopSharing :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCloudSharingServiceDelegateOverrides :: NSCloudSharingServiceDelegateOverrides
defaultNSCloudSharingServiceDelegateOverrides = NSCloudSharingServiceDelegateOverrides
  { _sharingService_didCompleteForItems_error = Nothing
  , _sharingService_didSaveShare = Nothing
  , _sharingService_didStopSharing = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCloudSharingServiceDelegateDelegateClass #-}
nsCloudSharingServiceDelegateDelegateClass :: Class
nsCloudSharingServiceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCloudSharingServiceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sharingService_didCompleteForItems_error = unSelector (mkSelector "sharingService:didCompleteForItems:error:")
      sel_sharingService_didSaveShare = unSelector (mkSelector "sharingService:didSaveShare:")
      sel_sharingService_didStopSharing = unSelector (mkSelector "sharingService:didStopSharing:")
  -- sharingService:didCompleteForItems:error:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCloudSharingServiceDelegateOverrides
    case _sharingService_didCompleteForItems_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "sharingService:didCompleteForItems:error:" "v@:@@@" stub_0

  -- sharingService:didSaveShare:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCloudSharingServiceDelegateOverrides
    case _sharingService_didSaveShare rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sharingService:didSaveShare:" "v@:@@" stub_1

  -- sharingService:didStopSharing:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCloudSharingServiceDelegateOverrides
    case _sharingService_didStopSharing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sharingService:didStopSharing:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCloudSharingServiceDelegateOverrides
    if queriedSel == sel_sharingService_didCompleteForItems_error then pure (maybe 0 (const 1) (_sharingService_didCompleteForItems_error rec_))
    else if queriedSel == sel_sharingService_didSaveShare then pure (maybe 0 (const 1) (_sharingService_didSaveShare rec_))
    else if queriedSel == sel_sharingService_didStopSharing then pure (maybe 0 (const 1) (_sharingService_didStopSharing rec_))
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
newNSCloudSharingServiceDelegate :: NSCloudSharingServiceDelegateOverrides -> IO RawId
newNSCloudSharingServiceDelegate overrides = do
  inst <- class_createInstance nsCloudSharingServiceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
