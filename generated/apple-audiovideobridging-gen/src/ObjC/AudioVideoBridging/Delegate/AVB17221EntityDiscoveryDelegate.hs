{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVB17221EntityDiscoveryDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVB17221EntityDiscoveryDelegate defaultAVB17221EntityDiscoveryDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AudioVideoBridging.Delegate.AVB17221EntityDiscoveryDelegate
  ( AVB17221EntityDiscoveryDelegateOverrides(..)
  , defaultAVB17221EntityDiscoveryDelegateOverrides
  , newAVB17221EntityDiscoveryDelegate
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

-- | Overrides record for @\@protocol AVB17221EntityDiscoveryDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVB17221EntityDiscoveryDelegateOverrides = AVB17221EntityDiscoveryDelegateOverrides
  { _didAddRemoteEntity_on17221EntityDiscovery :: !(Maybe (RawId -> RawId -> IO ()))
  , _didRemoveRemoteEntity_on17221EntityDiscovery :: !(Maybe (RawId -> RawId -> IO ()))
  , _didRediscoverRemoteEntity_on17221EntityDiscovery :: !(Maybe (RawId -> RawId -> IO ()))
  , _didAddLocalEntity_on17221EntityDiscovery :: !(Maybe (RawId -> RawId -> IO ()))
  , _didRemoveLocalEntity_on17221EntityDiscovery :: !(Maybe (RawId -> RawId -> IO ()))
  , _didRediscoverLocalEntity_on17221EntityDiscovery :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVB17221EntityDiscoveryDelegateOverrides :: AVB17221EntityDiscoveryDelegateOverrides
defaultAVB17221EntityDiscoveryDelegateOverrides = AVB17221EntityDiscoveryDelegateOverrides
  { _didAddRemoteEntity_on17221EntityDiscovery = Nothing
  , _didRemoveRemoteEntity_on17221EntityDiscovery = Nothing
  , _didRediscoverRemoteEntity_on17221EntityDiscovery = Nothing
  , _didAddLocalEntity_on17221EntityDiscovery = Nothing
  , _didRemoveLocalEntity_on17221EntityDiscovery = Nothing
  , _didRediscoverLocalEntity_on17221EntityDiscovery = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avB17221EntityDiscoveryDelegateDelegateClass #-}
avB17221EntityDiscoveryDelegateDelegateClass :: Class
avB17221EntityDiscoveryDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVB17221EntityDiscoveryDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_didAddRemoteEntity_on17221EntityDiscovery = unSelector (mkSelector "didAddRemoteEntity:on17221EntityDiscovery:")
      sel_didRemoveRemoteEntity_on17221EntityDiscovery = unSelector (mkSelector "didRemoveRemoteEntity:on17221EntityDiscovery:")
      sel_didRediscoverRemoteEntity_on17221EntityDiscovery = unSelector (mkSelector "didRediscoverRemoteEntity:on17221EntityDiscovery:")
      sel_didAddLocalEntity_on17221EntityDiscovery = unSelector (mkSelector "didAddLocalEntity:on17221EntityDiscovery:")
      sel_didRemoveLocalEntity_on17221EntityDiscovery = unSelector (mkSelector "didRemoveLocalEntity:on17221EntityDiscovery:")
      sel_didRediscoverLocalEntity_on17221EntityDiscovery = unSelector (mkSelector "didRediscoverLocalEntity:on17221EntityDiscovery:")
  -- didAddRemoteEntity:on17221EntityDiscovery:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    case _didAddRemoteEntity_on17221EntityDiscovery rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didAddRemoteEntity:on17221EntityDiscovery:" "v@:@@" stub_0

  -- didRemoveRemoteEntity:on17221EntityDiscovery:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    case _didRemoveRemoteEntity_on17221EntityDiscovery rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didRemoveRemoteEntity:on17221EntityDiscovery:" "v@:@@" stub_1

  -- didRediscoverRemoteEntity:on17221EntityDiscovery:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    case _didRediscoverRemoteEntity_on17221EntityDiscovery rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didRediscoverRemoteEntity:on17221EntityDiscovery:" "v@:@@" stub_2

  -- didAddLocalEntity:on17221EntityDiscovery:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    case _didAddLocalEntity_on17221EntityDiscovery rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didAddLocalEntity:on17221EntityDiscovery:" "v@:@@" stub_3

  -- didRemoveLocalEntity:on17221EntityDiscovery:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    case _didRemoveLocalEntity_on17221EntityDiscovery rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didRemoveLocalEntity:on17221EntityDiscovery:" "v@:@@" stub_4

  -- didRediscoverLocalEntity:on17221EntityDiscovery:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    case _didRediscoverLocalEntity_on17221EntityDiscovery rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didRediscoverLocalEntity:on17221EntityDiscovery:" "v@:@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221EntityDiscoveryDelegateOverrides
    if queriedSel == sel_didAddRemoteEntity_on17221EntityDiscovery then pure (maybe 0 (const 1) (_didAddRemoteEntity_on17221EntityDiscovery rec_))
    else if queriedSel == sel_didRemoveRemoteEntity_on17221EntityDiscovery then pure (maybe 0 (const 1) (_didRemoveRemoteEntity_on17221EntityDiscovery rec_))
    else if queriedSel == sel_didRediscoverRemoteEntity_on17221EntityDiscovery then pure (maybe 0 (const 1) (_didRediscoverRemoteEntity_on17221EntityDiscovery rec_))
    else if queriedSel == sel_didAddLocalEntity_on17221EntityDiscovery then pure (maybe 0 (const 1) (_didAddLocalEntity_on17221EntityDiscovery rec_))
    else if queriedSel == sel_didRemoveLocalEntity_on17221EntityDiscovery then pure (maybe 0 (const 1) (_didRemoveLocalEntity_on17221EntityDiscovery rec_))
    else if queriedSel == sel_didRediscoverLocalEntity_on17221EntityDiscovery then pure (maybe 0 (const 1) (_didRediscoverLocalEntity_on17221EntityDiscovery rec_))
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
newAVB17221EntityDiscoveryDelegate :: AVB17221EntityDiscoveryDelegateOverrides -> IO RawId
newAVB17221EntityDiscoveryDelegate overrides = do
  inst <- class_createInstance avB17221EntityDiscoveryDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
