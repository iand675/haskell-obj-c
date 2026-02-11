{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ASAccountAuthenticationModificationControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newASAccountAuthenticationModificationControllerDelegate defaultASAccountAuthenticationModificationControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AuthenticationServices.Delegate.ASAccountAuthenticationModificationControllerDelegate
  ( ASAccountAuthenticationModificationControllerDelegateOverrides(..)
  , defaultASAccountAuthenticationModificationControllerDelegateOverrides
  , newASAccountAuthenticationModificationControllerDelegate
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

-- | Overrides record for @\@protocol ASAccountAuthenticationModificationControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ASAccountAuthenticationModificationControllerDelegateOverrides = ASAccountAuthenticationModificationControllerDelegateOverrides
  { _accountAuthenticationModificationController_didSuccessfullyCompleteRequest_withUserInfo :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _accountAuthenticationModificationController_didFailRequest_withError :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultASAccountAuthenticationModificationControllerDelegateOverrides :: ASAccountAuthenticationModificationControllerDelegateOverrides
defaultASAccountAuthenticationModificationControllerDelegateOverrides = ASAccountAuthenticationModificationControllerDelegateOverrides
  { _accountAuthenticationModificationController_didSuccessfullyCompleteRequest_withUserInfo = Nothing
  , _accountAuthenticationModificationController_didFailRequest_withError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE asAccountAuthenticationModificationControllerDelegateDelegateClass #-}
asAccountAuthenticationModificationControllerDelegateDelegateClass :: Class
asAccountAuthenticationModificationControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsASAccountAuthenticationModificationControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accountAuthenticationModificationController_didSuccessfullyCompleteRequest_withUserInfo = unSelector (mkSelector "accountAuthenticationModificationController:didSuccessfullyCompleteRequest:withUserInfo:")
      sel_accountAuthenticationModificationController_didFailRequest_withError = unSelector (mkSelector "accountAuthenticationModificationController:didFailRequest:withError:")
  -- accountAuthenticationModificationController:didSuccessfullyCompleteRequest:withUserInfo:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAccountAuthenticationModificationControllerDelegateOverrides
    case _accountAuthenticationModificationController_didSuccessfullyCompleteRequest_withUserInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "accountAuthenticationModificationController:didSuccessfullyCompleteRequest:withUserInfo:" "v@:@@@" stub_0

  -- accountAuthenticationModificationController:didFailRequest:withError:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAccountAuthenticationModificationControllerDelegateOverrides
    case _accountAuthenticationModificationController_didFailRequest_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "accountAuthenticationModificationController:didFailRequest:withError:" "v@:@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ASAccountAuthenticationModificationControllerDelegateOverrides
    if queriedSel == sel_accountAuthenticationModificationController_didSuccessfullyCompleteRequest_withUserInfo then pure (maybe 0 (const 1) (_accountAuthenticationModificationController_didSuccessfullyCompleteRequest_withUserInfo rec_))
    else if queriedSel == sel_accountAuthenticationModificationController_didFailRequest_withError then pure (maybe 0 (const 1) (_accountAuthenticationModificationController_didFailRequest_withError rec_))
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
newASAccountAuthenticationModificationControllerDelegate :: ASAccountAuthenticationModificationControllerDelegateOverrides -> IO RawId
newASAccountAuthenticationModificationControllerDelegate overrides = do
  inst <- class_createInstance asAccountAuthenticationModificationControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
