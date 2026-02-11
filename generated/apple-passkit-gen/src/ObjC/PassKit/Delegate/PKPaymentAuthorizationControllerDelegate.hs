{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PKPaymentAuthorizationControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newPKPaymentAuthorizationControllerDelegate defaultPKPaymentAuthorizationControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PassKit.Delegate.PKPaymentAuthorizationControllerDelegate
  ( PKPaymentAuthorizationControllerDelegateOverrides(..)
  , defaultPKPaymentAuthorizationControllerDelegateOverrides
  , newPKPaymentAuthorizationControllerDelegate
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

-- | Overrides record for @\@protocol PKPaymentAuthorizationControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PKPaymentAuthorizationControllerDelegateOverrides = PKPaymentAuthorizationControllerDelegateOverrides
  { _paymentAuthorizationControllerDidFinish :: !(Maybe (RawId -> IO ()))
  , _paymentAuthorizationControllerWillAuthorizePayment :: !(Maybe (RawId -> IO ()))
  , _paymentAuthorizationController_didSelectShippingMethod_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _paymentAuthorizationController_didSelectShippingContact_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _paymentAuthorizationController_didSelectPaymentMethod_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _presentationWindowForPaymentAuthorizationController :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultPKPaymentAuthorizationControllerDelegateOverrides :: PKPaymentAuthorizationControllerDelegateOverrides
defaultPKPaymentAuthorizationControllerDelegateOverrides = PKPaymentAuthorizationControllerDelegateOverrides
  { _paymentAuthorizationControllerDidFinish = Nothing
  , _paymentAuthorizationControllerWillAuthorizePayment = Nothing
  , _paymentAuthorizationController_didSelectShippingMethod_completion = Nothing
  , _paymentAuthorizationController_didSelectShippingContact_completion = Nothing
  , _paymentAuthorizationController_didSelectPaymentMethod_completion = Nothing
  , _presentationWindowForPaymentAuthorizationController = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

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
{-# NOINLINE pkPaymentAuthorizationControllerDelegateDelegateClass #-}
pkPaymentAuthorizationControllerDelegateDelegateClass :: Class
pkPaymentAuthorizationControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPKPaymentAuthorizationControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_paymentAuthorizationControllerDidFinish = unSelector (mkSelector "paymentAuthorizationControllerDidFinish:")
      sel_paymentAuthorizationControllerWillAuthorizePayment = unSelector (mkSelector "paymentAuthorizationControllerWillAuthorizePayment:")
      sel_paymentAuthorizationController_didSelectShippingMethod_completion = unSelector (mkSelector "paymentAuthorizationController:didSelectShippingMethod:completion:")
      sel_paymentAuthorizationController_didSelectShippingContact_completion = unSelector (mkSelector "paymentAuthorizationController:didSelectShippingContact:completion:")
      sel_paymentAuthorizationController_didSelectPaymentMethod_completion = unSelector (mkSelector "paymentAuthorizationController:didSelectPaymentMethod:completion:")
      sel_presentationWindowForPaymentAuthorizationController = unSelector (mkSelector "presentationWindowForPaymentAuthorizationController:")
  -- paymentAuthorizationControllerDidFinish:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    case _paymentAuthorizationControllerDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "paymentAuthorizationControllerDidFinish:" "v@:@" stub_0

  -- paymentAuthorizationControllerWillAuthorizePayment:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    case _paymentAuthorizationControllerWillAuthorizePayment rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "paymentAuthorizationControllerWillAuthorizePayment:" "v@:@" stub_1

  -- paymentAuthorizationController:didSelectShippingMethod:completion:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    case _paymentAuthorizationController_didSelectShippingMethod_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "paymentAuthorizationController:didSelectShippingMethod:completion:" "v@:@@@" stub_2

  -- paymentAuthorizationController:didSelectShippingContact:completion:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    case _paymentAuthorizationController_didSelectShippingContact_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "paymentAuthorizationController:didSelectShippingContact:completion:" "v@:@@@" stub_3

  -- paymentAuthorizationController:didSelectPaymentMethod:completion:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    case _paymentAuthorizationController_didSelectPaymentMethod_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "paymentAuthorizationController:didSelectPaymentMethod:completion:" "v@:@@@" stub_4

  -- presentationWindowForPaymentAuthorizationController:
  stub_5 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    case _presentationWindowForPaymentAuthorizationController rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "presentationWindowForPaymentAuthorizationController:" "@@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PKPaymentAuthorizationControllerDelegateOverrides
    if queriedSel == sel_paymentAuthorizationControllerDidFinish then pure (maybe 0 (const 1) (_paymentAuthorizationControllerDidFinish rec_))
    else if queriedSel == sel_paymentAuthorizationControllerWillAuthorizePayment then pure (maybe 0 (const 1) (_paymentAuthorizationControllerWillAuthorizePayment rec_))
    else if queriedSel == sel_paymentAuthorizationController_didSelectShippingMethod_completion then pure (maybe 0 (const 1) (_paymentAuthorizationController_didSelectShippingMethod_completion rec_))
    else if queriedSel == sel_paymentAuthorizationController_didSelectShippingContact_completion then pure (maybe 0 (const 1) (_paymentAuthorizationController_didSelectShippingContact_completion rec_))
    else if queriedSel == sel_paymentAuthorizationController_didSelectPaymentMethod_completion then pure (maybe 0 (const 1) (_paymentAuthorizationController_didSelectPaymentMethod_completion rec_))
    else if queriedSel == sel_presentationWindowForPaymentAuthorizationController then pure (maybe 0 (const 1) (_presentationWindowForPaymentAuthorizationController rec_))
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
newPKPaymentAuthorizationControllerDelegate :: PKPaymentAuthorizationControllerDelegateOverrides -> IO RawId
newPKPaymentAuthorizationControllerDelegate overrides = do
  inst <- class_createInstance pkPaymentAuthorizationControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
