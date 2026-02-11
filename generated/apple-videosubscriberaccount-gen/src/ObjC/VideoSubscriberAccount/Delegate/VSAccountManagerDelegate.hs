{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol VSAccountManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newVSAccountManagerDelegate defaultVSAccountManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.VideoSubscriberAccount.Delegate.VSAccountManagerDelegate
  ( VSAccountManagerDelegateOverrides(..)
  , defaultVSAccountManagerDelegateOverrides
  , newVSAccountManagerDelegate
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

-- | Overrides record for @\@protocol VSAccountManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data VSAccountManagerDelegateOverrides = VSAccountManagerDelegateOverrides
  { _accountManager_presentViewController :: !(Maybe (RawId -> RawId -> IO ()))
  , _accountManager_dismissViewController :: !(Maybe (RawId -> RawId -> IO ()))
  , _accountManager_shouldAuthenticateAccountProviderWithIdentifier :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultVSAccountManagerDelegateOverrides :: VSAccountManagerDelegateOverrides
defaultVSAccountManagerDelegateOverrides = VSAccountManagerDelegateOverrides
  { _accountManager_presentViewController = Nothing
  , _accountManager_dismissViewController = Nothing
  , _accountManager_shouldAuthenticateAccountProviderWithIdentifier = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE vsAccountManagerDelegateDelegateClass #-}
vsAccountManagerDelegateDelegateClass :: Class
vsAccountManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsVSAccountManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accountManager_presentViewController = unSelector (mkSelector "accountManager:presentViewController:")
      sel_accountManager_dismissViewController = unSelector (mkSelector "accountManager:dismissViewController:")
      sel_accountManager_shouldAuthenticateAccountProviderWithIdentifier = unSelector (mkSelector "accountManager:shouldAuthenticateAccountProviderWithIdentifier:")
  -- accountManager:presentViewController:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VSAccountManagerDelegateOverrides
    case _accountManager_presentViewController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "accountManager:presentViewController:" "v@:@@" stub_0

  -- accountManager:dismissViewController:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VSAccountManagerDelegateOverrides
    case _accountManager_dismissViewController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "accountManager:dismissViewController:" "v@:@@" stub_1

  -- accountManager:shouldAuthenticateAccountProviderWithIdentifier:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VSAccountManagerDelegateOverrides
    case _accountManager_shouldAuthenticateAccountProviderWithIdentifier rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "accountManager:shouldAuthenticateAccountProviderWithIdentifier:" "B@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VSAccountManagerDelegateOverrides
    if queriedSel == sel_accountManager_presentViewController then pure (maybe 0 (const 1) (_accountManager_presentViewController rec_))
    else if queriedSel == sel_accountManager_dismissViewController then pure (maybe 0 (const 1) (_accountManager_dismissViewController rec_))
    else if queriedSel == sel_accountManager_shouldAuthenticateAccountProviderWithIdentifier then pure (maybe 0 (const 1) (_accountManager_shouldAuthenticateAccountProviderWithIdentifier rec_))
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
newVSAccountManagerDelegate :: VSAccountManagerDelegateOverrides -> IO RawId
newVSAccountManagerDelegate overrides = do
  inst <- class_createInstance vsAccountManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
