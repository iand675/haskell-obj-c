{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSXPCListenerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSXPCListenerDelegate defaultNSXPCListenerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSXPCListenerDelegate
  ( NSXPCListenerDelegateOverrides(..)
  , defaultNSXPCListenerDelegateOverrides
  , newNSXPCListenerDelegate
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

-- | Overrides record for @\@protocol NSXPCListenerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSXPCListenerDelegateOverrides = NSXPCListenerDelegateOverrides
  { _listener_shouldAcceptNewConnection :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSXPCListenerDelegateOverrides :: NSXPCListenerDelegateOverrides
defaultNSXPCListenerDelegateOverrides = NSXPCListenerDelegateOverrides
  { _listener_shouldAcceptNewConnection = Nothing
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
{-# NOINLINE nsxpcListenerDelegateDelegateClass #-}
nsxpcListenerDelegateDelegateClass :: Class
nsxpcListenerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSXPCListenerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_listener_shouldAcceptNewConnection = unSelector (mkSelector "listener:shouldAcceptNewConnection:")
  -- listener:shouldAcceptNewConnection:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXPCListenerDelegateOverrides
    case _listener_shouldAcceptNewConnection rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "listener:shouldAcceptNewConnection:" "B@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXPCListenerDelegateOverrides
    if queriedSel == sel_listener_shouldAcceptNewConnection then pure (maybe 0 (const 1) (_listener_shouldAcceptNewConnection rec_))
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
newNSXPCListenerDelegate :: NSXPCListenerDelegateOverrides -> IO RawId
newNSXPCListenerDelegate overrides = do
  inst <- class_createInstance nsxpcListenerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
