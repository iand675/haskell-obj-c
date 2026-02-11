{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSharingServicePickerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSSharingServicePickerDelegate defaultNSSharingServicePickerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSSharingServicePickerDelegate
  ( NSSharingServicePickerDelegateOverrides(..)
  , defaultNSSharingServicePickerDelegateOverrides
  , newNSSharingServicePickerDelegate
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

-- | Overrides record for @\@protocol NSSharingServicePickerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSharingServicePickerDelegateOverrides = NSSharingServicePickerDelegateOverrides
  { _sharingServicePicker_sharingServicesForItems_proposedSharingServices :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _sharingServicePicker_delegateForSharingService :: !(Maybe (RawId -> RawId -> IO RawId))
  , _sharingServicePicker_didChooseSharingService :: !(Maybe (RawId -> RawId -> IO ()))
  , _sharingServicePickerCollaborationModeRestrictions :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSharingServicePickerDelegateOverrides :: NSSharingServicePickerDelegateOverrides
defaultNSSharingServicePickerDelegateOverrides = NSSharingServicePickerDelegateOverrides
  { _sharingServicePicker_sharingServicesForItems_proposedSharingServices = Nothing
  , _sharingServicePicker_delegateForSharingService = Nothing
  , _sharingServicePicker_didChooseSharingService = Nothing
  , _sharingServicePickerCollaborationModeRestrictions = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsSharingServicePickerDelegateDelegateClass #-}
nsSharingServicePickerDelegateDelegateClass :: Class
nsSharingServicePickerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSharingServicePickerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sharingServicePicker_sharingServicesForItems_proposedSharingServices = unSelector (mkSelector "sharingServicePicker:sharingServicesForItems:proposedSharingServices:")
      sel_sharingServicePicker_delegateForSharingService = unSelector (mkSelector "sharingServicePicker:delegateForSharingService:")
      sel_sharingServicePicker_didChooseSharingService = unSelector (mkSelector "sharingServicePicker:didChooseSharingService:")
      sel_sharingServicePickerCollaborationModeRestrictions = unSelector (mkSelector "sharingServicePickerCollaborationModeRestrictions:")
  -- sharingServicePicker:sharingServicesForItems:proposedSharingServices:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServicePickerDelegateOverrides
    case _sharingServicePicker_sharingServicesForItems_proposedSharingServices rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sharingServicePicker:sharingServicesForItems:proposedSharingServices:" "@@:@@@" stub_0

  -- sharingServicePicker:delegateForSharingService:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServicePickerDelegateOverrides
    case _sharingServicePicker_delegateForSharingService rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sharingServicePicker:delegateForSharingService:" "@@:@@" stub_1

  -- sharingServicePicker:didChooseSharingService:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServicePickerDelegateOverrides
    case _sharingServicePicker_didChooseSharingService rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sharingServicePicker:didChooseSharingService:" "v@:@@" stub_2

  -- sharingServicePickerCollaborationModeRestrictions:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServicePickerDelegateOverrides
    case _sharingServicePickerCollaborationModeRestrictions rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sharingServicePickerCollaborationModeRestrictions:" "@@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServicePickerDelegateOverrides
    if queriedSel == sel_sharingServicePicker_sharingServicesForItems_proposedSharingServices then pure (maybe 0 (const 1) (_sharingServicePicker_sharingServicesForItems_proposedSharingServices rec_))
    else if queriedSel == sel_sharingServicePicker_delegateForSharingService then pure (maybe 0 (const 1) (_sharingServicePicker_delegateForSharingService rec_))
    else if queriedSel == sel_sharingServicePicker_didChooseSharingService then pure (maybe 0 (const 1) (_sharingServicePicker_didChooseSharingService rec_))
    else if queriedSel == sel_sharingServicePickerCollaborationModeRestrictions then pure (maybe 0 (const 1) (_sharingServicePickerCollaborationModeRestrictions rec_))
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
newNSSharingServicePickerDelegate :: NSSharingServicePickerDelegateOverrides -> IO RawId
newNSSharingServicePickerDelegate overrides = do
  inst <- class_createInstance nsSharingServicePickerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
