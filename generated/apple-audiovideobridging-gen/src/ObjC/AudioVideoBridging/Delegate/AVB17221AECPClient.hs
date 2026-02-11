{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVB17221AECPClient@.
--
-- Usage:
--
-- @
-- delegate <- newAVB17221AECPClient defaultAVB17221AECPClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AudioVideoBridging.Delegate.AVB17221AECPClient
  ( AVB17221AECPClientOverrides(..)
  , defaultAVB17221AECPClientOverrides
  , newAVB17221AECPClient
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

-- | Overrides record for @\@protocol AVB17221AECPClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVB17221AECPClientOverrides = AVB17221AECPClientOverrides
  { _aecpDidReceiveCommand_onInterface :: !(Maybe (RawId -> RawId -> IO Bool))
  , _aecpDidReceiveResponse_onInterface :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultAVB17221AECPClientOverrides :: AVB17221AECPClientOverrides
defaultAVB17221AECPClientOverrides = AVB17221AECPClientOverrides
  { _aecpDidReceiveCommand_onInterface = Nothing
  , _aecpDidReceiveResponse_onInterface = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avB17221AECPClientDelegateClass #-}
avB17221AECPClientDelegateClass :: Class
avB17221AECPClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVB17221AECPClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_aecpDidReceiveCommand_onInterface = unSelector (mkSelector "AECPDidReceiveCommand:onInterface:")
      sel_aecpDidReceiveResponse_onInterface = unSelector (mkSelector "AECPDidReceiveResponse:onInterface:")
  -- AECPDidReceiveCommand:onInterface:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221AECPClientOverrides
    case _aecpDidReceiveCommand_onInterface rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "AECPDidReceiveCommand:onInterface:" "B@:@@" stub_0

  -- AECPDidReceiveResponse:onInterface:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221AECPClientOverrides
    case _aecpDidReceiveResponse_onInterface rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "AECPDidReceiveResponse:onInterface:" "B@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVB17221AECPClientOverrides
    if queriedSel == sel_aecpDidReceiveCommand_onInterface then pure (maybe 0 (const 1) (_aecpDidReceiveCommand_onInterface rec_))
    else if queriedSel == sel_aecpDidReceiveResponse_onInterface then pure (maybe 0 (const 1) (_aecpDidReceiveResponse_onInterface rec_))
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
newAVB17221AECPClient :: AVB17221AECPClientOverrides -> IO RawId
newAVB17221AECPClient overrides = do
  inst <- class_createInstance avB17221AECPClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
