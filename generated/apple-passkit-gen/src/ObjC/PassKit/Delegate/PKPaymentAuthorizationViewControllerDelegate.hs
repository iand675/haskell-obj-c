{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PKPaymentAuthorizationViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newPKPaymentAuthorizationViewControllerDelegate defaultPKPaymentAuthorizationViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PassKit.Delegate.PKPaymentAuthorizationViewControllerDelegate
  ( PKPaymentAuthorizationViewControllerDelegateOverrides(..)
  , defaultPKPaymentAuthorizationViewControllerDelegateOverrides
  , newPKPaymentAuthorizationViewControllerDelegate
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

-- | Overrides record for @\@protocol PKPaymentAuthorizationViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PKPaymentAuthorizationViewControllerDelegateOverrides = PKPaymentAuthorizationViewControllerDelegateOverrides
  { _paymentAuthorizationViewControllerDidFinish :: !(Maybe (RawId -> IO ()))
  , _paymentAuthorizationViewControllerWillAuthorizePayment :: !(Maybe (RawId -> IO ()))
  , _paymentAuthorizationViewController_didSelectShippingMethod_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _paymentAuthorizationViewController_didSelectShippingContact_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _paymentAuthorizationViewController_didSelectPaymentMethod_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPKPaymentAuthorizationViewControllerDelegateOverrides :: PKPaymentAuthorizationViewControllerDelegateOverrides
defaultPKPaymentAuthorizationViewControllerDelegateOverrides = PKPaymentAuthorizationViewControllerDelegateOverrides
  { _paymentAuthorizationViewControllerDidFinish = Nothing
  , _paymentAuthorizationViewControllerWillAuthorizePayment = Nothing
  , _paymentAuthorizationViewController_didSelectShippingMethod_completion = Nothing
  , _paymentAuthorizationViewController_didSelectShippingContact_completion = Nothing
  , _paymentAuthorizationViewController_didSelectPaymentMethod_completion = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pkPaymentAuthorizationViewControllerDelegateDelegateClass #-}
pkPaymentAuthorizationViewControllerDelegateDelegateClass :: Class
pkPaymentAuthorizationViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPKPaymentAuthorizationViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_paymentAuthorizationViewControllerDidFinish = unSelector (mkSelector "paymentAuthorizationViewControllerDidFinish:")
      sel_paymentAuthorizationViewControllerWillAuthorizePayment = unSelector (mkSelector "paymentAuthorizationViewControllerWillAuthorizePayment:")
      sel_paymentAuthorizationViewController_didSelectShippingMethod_completion = unSelector (mkSelector "paymentAuthorizationViewController:didSelectShippingMethod:completion:")
      sel_paymentAuthorizationViewController_didSelectShippingContact_completion = unSelector (mkSelector "paymentAuthorizationViewController:didSelectShippingContact:completion:")
      sel_paymentAuthorizationViewController_didSelectPaymentMethod_completion = unSelector (mkSelector "paymentAuthorizationViewController:didSelectPaymentMethod:completion:")
  -- paymentAuthorizationViewControllerDidFinish:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationViewControllerDelegateOverrides
    case _paymentAuthorizationViewControllerDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "paymentAuthorizationViewControllerDidFinish:" "v@:@" stub_0

  -- paymentAuthorizationViewControllerWillAuthorizePayment:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationViewControllerDelegateOverrides
    case _paymentAuthorizationViewControllerWillAuthorizePayment rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "paymentAuthorizationViewControllerWillAuthorizePayment:" "v@:@" stub_1

  -- paymentAuthorizationViewController:didSelectShippingMethod:completion:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationViewControllerDelegateOverrides
    case _paymentAuthorizationViewController_didSelectShippingMethod_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "paymentAuthorizationViewController:didSelectShippingMethod:completion:" "v@:@@@" stub_2

  -- paymentAuthorizationViewController:didSelectShippingContact:completion:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationViewControllerDelegateOverrides
    case _paymentAuthorizationViewController_didSelectShippingContact_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "paymentAuthorizationViewController:didSelectShippingContact:completion:" "v@:@@@" stub_3

  -- paymentAuthorizationViewController:didSelectPaymentMethod:completion:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationViewControllerDelegateOverrides
    case _paymentAuthorizationViewController_didSelectPaymentMethod_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "paymentAuthorizationViewController:didSelectPaymentMethod:completion:" "v@:@@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationViewControllerDelegateOverrides
    if queriedSel == sel_paymentAuthorizationViewControllerDidFinish then pure (maybe 0 (const 1) (_paymentAuthorizationViewControllerDidFinish rec_))
    else if queriedSel == sel_paymentAuthorizationViewControllerWillAuthorizePayment then pure (maybe 0 (const 1) (_paymentAuthorizationViewControllerWillAuthorizePayment rec_))
    else if queriedSel == sel_paymentAuthorizationViewController_didSelectShippingMethod_completion then pure (maybe 0 (const 1) (_paymentAuthorizationViewController_didSelectShippingMethod_completion rec_))
    else if queriedSel == sel_paymentAuthorizationViewController_didSelectShippingContact_completion then pure (maybe 0 (const 1) (_paymentAuthorizationViewController_didSelectShippingContact_completion rec_))
    else if queriedSel == sel_paymentAuthorizationViewController_didSelectPaymentMethod_completion then pure (maybe 0 (const 1) (_paymentAuthorizationViewController_didSelectPaymentMethod_completion rec_))
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
newPKPaymentAuthorizationViewControllerDelegate :: PKPaymentAuthorizationViewControllerDelegateOverrides -> IO RawId
newPKPaymentAuthorizationViewControllerDelegate overrides = do
  inst <- class_createInstance pkPaymentAuthorizationViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
