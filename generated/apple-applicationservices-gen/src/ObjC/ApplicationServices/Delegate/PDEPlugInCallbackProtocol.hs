{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PDEPlugInCallbackProtocol@.
--
-- Usage:
--
-- @
-- delegate <- newPDEPlugInCallbackProtocol defaultPDEPlugInCallbackProtocolOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ApplicationServices.Delegate.PDEPlugInCallbackProtocol
  ( PDEPlugInCallbackProtocolOverrides(..)
  , defaultPDEPlugInCallbackProtocolOverrides
  , newPDEPlugInCallbackProtocol
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

-- | Overrides record for @\@protocol PDEPlugInCallbackProtocol@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PDEPlugInCallbackProtocolOverrides = PDEPlugInCallbackProtocolOverrides
  { _willChangePPDOptionKeyValue_ppdChoice :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultPDEPlugInCallbackProtocolOverrides :: PDEPlugInCallbackProtocolOverrides
defaultPDEPlugInCallbackProtocolOverrides = PDEPlugInCallbackProtocolOverrides
  { _willChangePPDOptionKeyValue_ppdChoice = Nothing
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
{-# NOINLINE pdePlugInCallbackProtocolDelegateClass #-}
pdePlugInCallbackProtocolDelegateClass :: Class
pdePlugInCallbackProtocolDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPDEPlugInCallbackProtocol" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_willChangePPDOptionKeyValue_ppdChoice = unSelector (mkSelector "willChangePPDOptionKeyValue:ppdChoice:")
  -- willChangePPDOptionKeyValue:ppdChoice:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPlugInCallbackProtocolOverrides
    case _willChangePPDOptionKeyValue_ppdChoice rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "willChangePPDOptionKeyValue:ppdChoice:" "B@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPlugInCallbackProtocolOverrides
    if queriedSel == sel_willChangePPDOptionKeyValue_ppdChoice then pure (maybe 0 (const 1) (_willChangePPDOptionKeyValue_ppdChoice rec_))
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
newPDEPlugInCallbackProtocol :: PDEPlugInCallbackProtocolOverrides -> IO RawId
newPDEPlugInCallbackProtocol overrides = do
  inst <- class_createInstance pdePlugInCallbackProtocolDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
