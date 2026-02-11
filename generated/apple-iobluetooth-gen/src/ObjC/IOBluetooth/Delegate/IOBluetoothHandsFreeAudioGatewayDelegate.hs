{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IOBluetoothHandsFreeAudioGatewayDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIOBluetoothHandsFreeAudioGatewayDelegate defaultIOBluetoothHandsFreeAudioGatewayDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IOBluetooth.Delegate.IOBluetoothHandsFreeAudioGatewayDelegate
  ( IOBluetoothHandsFreeAudioGatewayDelegateOverrides(..)
  , defaultIOBluetoothHandsFreeAudioGatewayDelegateOverrides
  , newIOBluetoothHandsFreeAudioGatewayDelegate
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

-- | Overrides record for @\@protocol IOBluetoothHandsFreeAudioGatewayDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IOBluetoothHandsFreeAudioGatewayDelegateOverrides = IOBluetoothHandsFreeAudioGatewayDelegateOverrides
  { _handsFree_hangup :: !(Maybe (RawId -> RawId -> IO ()))
  , _handsFree_redial :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIOBluetoothHandsFreeAudioGatewayDelegateOverrides :: IOBluetoothHandsFreeAudioGatewayDelegateOverrides
defaultIOBluetoothHandsFreeAudioGatewayDelegateOverrides = IOBluetoothHandsFreeAudioGatewayDelegateOverrides
  { _handsFree_hangup = Nothing
  , _handsFree_redial = Nothing
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
{-# NOINLINE ioBluetoothHandsFreeAudioGatewayDelegateDelegateClass #-}
ioBluetoothHandsFreeAudioGatewayDelegateDelegateClass :: Class
ioBluetoothHandsFreeAudioGatewayDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIOBluetoothHandsFreeAudioGatewayDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handsFree_hangup = unSelector (mkSelector "handsFree:hangup:")
      sel_handsFree_redial = unSelector (mkSelector "handsFree:redial:")
  -- handsFree:hangup:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeAudioGatewayDelegateOverrides
    case _handsFree_hangup rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:hangup:" "v@:@@" stub_0

  -- handsFree:redial:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeAudioGatewayDelegateOverrides
    case _handsFree_redial rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "handsFree:redial:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IOBluetoothHandsFreeAudioGatewayDelegateOverrides
    if queriedSel == sel_handsFree_hangup then pure (maybe 0 (const 1) (_handsFree_hangup rec_))
    else if queriedSel == sel_handsFree_redial then pure (maybe 0 (const 1) (_handsFree_redial rec_))
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
newIOBluetoothHandsFreeAudioGatewayDelegate :: IOBluetoothHandsFreeAudioGatewayDelegateOverrides -> IO RawId
newIOBluetoothHandsFreeAudioGatewayDelegate overrides = do
  inst <- class_createInstance ioBluetoothHandsFreeAudioGatewayDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
