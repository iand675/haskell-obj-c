{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SKPaymentQueueDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSKPaymentQueueDelegate defaultSKPaymentQueueDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.StoreKit.Delegate.SKPaymentQueueDelegate
  ( SKPaymentQueueDelegateOverrides(..)
  , defaultSKPaymentQueueDelegateOverrides
  , newSKPaymentQueueDelegate
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

-- | Overrides record for @\@protocol SKPaymentQueueDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SKPaymentQueueDelegateOverrides = SKPaymentQueueDelegateOverrides
  { _paymentQueue_shouldContinueTransaction_inStorefront :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _paymentQueueShouldShowPriceConsent :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultSKPaymentQueueDelegateOverrides :: SKPaymentQueueDelegateOverrides
defaultSKPaymentQueueDelegateOverrides = SKPaymentQueueDelegateOverrides
  { _paymentQueue_shouldContinueTransaction_inStorefront = Nothing
  , _paymentQueueShouldShowPriceConsent = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE skPaymentQueueDelegateDelegateClass #-}
skPaymentQueueDelegateDelegateClass :: Class
skPaymentQueueDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSKPaymentQueueDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_paymentQueue_shouldContinueTransaction_inStorefront = unSelector (mkSelector "paymentQueue:shouldContinueTransaction:inStorefront:")
      sel_paymentQueueShouldShowPriceConsent = unSelector (mkSelector "paymentQueueShouldShowPriceConsent:")
  -- paymentQueue:shouldContinueTransaction:inStorefront:
  stub_0 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentQueueDelegateOverrides
    case _paymentQueue_shouldContinueTransaction_inStorefront rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "paymentQueue:shouldContinueTransaction:inStorefront:" "B@:@@@" stub_0

  -- paymentQueueShouldShowPriceConsent:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentQueueDelegateOverrides
    case _paymentQueueShouldShowPriceConsent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "paymentQueueShouldShowPriceConsent:" "B@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentQueueDelegateOverrides
    if queriedSel == sel_paymentQueue_shouldContinueTransaction_inStorefront then pure (maybe 0 (const 1) (_paymentQueue_shouldContinueTransaction_inStorefront rec_))
    else if queriedSel == sel_paymentQueueShouldShowPriceConsent then pure (maybe 0 (const 1) (_paymentQueueShouldShowPriceConsent rec_))
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
newSKPaymentQueueDelegate :: SKPaymentQueueDelegateOverrides -> IO RawId
newSKPaymentQueueDelegate overrides = do
  inst <- class_createInstance skPaymentQueueDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
