{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CXCallDirectoryExtensionContextDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCXCallDirectoryExtensionContextDelegate defaultCXCallDirectoryExtensionContextDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CallKit.Delegate.CXCallDirectoryExtensionContextDelegate
  ( CXCallDirectoryExtensionContextDelegateOverrides(..)
  , defaultCXCallDirectoryExtensionContextDelegateOverrides
  , newCXCallDirectoryExtensionContextDelegate
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

-- | Overrides record for @\@protocol CXCallDirectoryExtensionContextDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CXCallDirectoryExtensionContextDelegateOverrides = CXCallDirectoryExtensionContextDelegateOverrides
  { _requestFailedForExtensionContext_withError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCXCallDirectoryExtensionContextDelegateOverrides :: CXCallDirectoryExtensionContextDelegateOverrides
defaultCXCallDirectoryExtensionContextDelegateOverrides = CXCallDirectoryExtensionContextDelegateOverrides
  { _requestFailedForExtensionContext_withError = Nothing
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
{-# NOINLINE cxCallDirectoryExtensionContextDelegateDelegateClass #-}
cxCallDirectoryExtensionContextDelegateDelegateClass :: Class
cxCallDirectoryExtensionContextDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCXCallDirectoryExtensionContextDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_requestFailedForExtensionContext_withError = unSelector (mkSelector "requestFailedForExtensionContext:withError:")
  -- requestFailedForExtensionContext:withError:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXCallDirectoryExtensionContextDelegateOverrides
    case _requestFailedForExtensionContext_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "requestFailedForExtensionContext:withError:" "v@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CXCallDirectoryExtensionContextDelegateOverrides
    if queriedSel == sel_requestFailedForExtensionContext_withError then pure (maybe 0 (const 1) (_requestFailedForExtensionContext_withError rec_))
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
newCXCallDirectoryExtensionContextDelegate :: CXCallDirectoryExtensionContextDelegateOverrides -> IO RawId
newCXCallDirectoryExtensionContextDelegate overrides = do
  inst <- class_createInstance cxCallDirectoryExtensionContextDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
