{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothHandsFreeDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothHandsFreeDelegate defaultIOBluetoothHandsFreeDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothHandsFreeDelegate
  ( IOBluetoothHandsFreeDelegateOverrides(..)
  , defaultIOBluetoothHandsFreeDelegateOverrides
  , newIOBluetoothHandsFreeDelegate
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

-- | Overrides record for @\@protocol IOBluetoothHandsFreeDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothHandsFreeDelegateOverrides = IOBluetoothHandsFreeDelegateOverrides
  { _handsFree_connected :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_disconnected :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_scoConnectionOpened :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_scoConnectionClosed :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothHandsFreeDelegateOverrides :: IOBluetoothHandsFreeDelegateOverrides
defaultIOBluetoothHandsFreeDelegateOverrides = IOBluetoothHandsFreeDelegateOverrides
  { _handsFree_connected = Nothing
  , _handsFree_disconnected = Nothing
  , _handsFree_scoConnectionOpened = Nothing
  , _handsFree_scoConnectionClosed = Nothing
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
{-# NOINLINE ioBluetoothHandsFreeDelegateDelegateClass #-}
ioBluetoothHandsFreeDelegateDelegateClass :: Class
ioBluetoothHandsFreeDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothHandsFreeDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handsFree_connected = unSelector (mkSelector "handsFree:connected:")
      sel_handsFree_disconnected = unSelector (mkSelector "handsFree:disconnected:")
      sel_handsFree_scoConnectionOpened = unSelector (mkSelector "handsFree:scoConnectionOpened:")
      sel_handsFree_scoConnectionClosed = unSelector (mkSelector "handsFree:scoConnectionClosed:")
  -- handsFree:connected:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDelegateOverrides
    case _handsFree_connected rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:connected:" "v@:@@" stub_0

  -- handsFree:disconnected:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDelegateOverrides
    case _handsFree_disconnected rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:disconnected:" "v@:@@" stub_1

  -- handsFree:scoConnectionOpened:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDelegateOverrides
    case _handsFree_scoConnectionOpened rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:scoConnectionOpened:" "v@:@@" stub_2

  -- handsFree:scoConnectionClosed:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDelegateOverrides
    case _handsFree_scoConnectionClosed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:scoConnectionClosed:" "v@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeDelegateOverrides
    if queriedSel == sel_handsFree_connected then pure (maybe 0 (const 1) (_handsFree_connected rec_))
    else if queriedSel == sel_handsFree_disconnected then pure (maybe 0 (const 1) (_handsFree_disconnected rec_))
    else if queriedSel == sel_handsFree_scoConnectionOpened then pure (maybe 0 (const 1) (_handsFree_scoConnectionOpened rec_))
    else if queriedSel == sel_handsFree_scoConnectionClosed then pure (maybe 0 (const 1) (_handsFree_scoConnectionClosed rec_))
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
newIOBluetoothHandsFreeDelegate :: IOBluetoothHandsFreeDelegateOverrides -> IO RawId
newIOBluetoothHandsFreeDelegate overrides = do
  inst <- class_createInstance ioBluetoothHandsFreeDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
