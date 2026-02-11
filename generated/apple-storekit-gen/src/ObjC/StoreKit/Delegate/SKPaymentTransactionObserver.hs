{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SKPaymentTransactionObserver@.
--
-- Usage:
--
-- @
-- delegate <- newSKPaymentTransactionObserver defaultSKPaymentTransactionObserverOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.StoreKit.Delegate.SKPaymentTransactionObserver
  ( SKPaymentTransactionObserverOverrides(..)
  , defaultSKPaymentTransactionObserverOverrides
  , newSKPaymentTransactionObserver
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

-- | Overrides record for @\@protocol SKPaymentTransactionObserver@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SKPaymentTransactionObserverOverrides = SKPaymentTransactionObserverOverrides
  { _paymentQueue_updatedTransactions :: !(Maybe (RawId -> RawId -> IO ()))
  , _paymentQueue_removedTransactions :: !(Maybe (RawId -> RawId -> IO ()))
  , _paymentQueue_restoreCompletedTransactionsFailedWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _paymentQueueRestoreCompletedTransactionsFinished :: !(Maybe (RawId -> IO ()))
  , _paymentQueue_updatedDownloads :: !(Maybe (RawId -> RawId -> IO ()))
  , _paymentQueue_shouldAddStorePayment_forProduct :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _paymentQueueDidChangeStorefront :: !(Maybe (RawId -> IO ()))
  , _paymentQueue_didRevokeEntitlementsForProductIdentifiers :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSKPaymentTransactionObserverOverrides :: SKPaymentTransactionObserverOverrides
defaultSKPaymentTransactionObserverOverrides = SKPaymentTransactionObserverOverrides
  { _paymentQueue_updatedTransactions = Nothing
  , _paymentQueue_removedTransactions = Nothing
  , _paymentQueue_restoreCompletedTransactionsFailedWithError = Nothing
  , _paymentQueueRestoreCompletedTransactionsFinished = Nothing
  , _paymentQueue_updatedDownloads = Nothing
  , _paymentQueue_shouldAddStorePayment_forProduct = Nothing
  , _paymentQueueDidChangeStorefront = Nothing
  , _paymentQueue_didRevokeEntitlementsForProductIdentifiers = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE skPaymentTransactionObserverDelegateClass #-}
skPaymentTransactionObserverDelegateClass :: Class
skPaymentTransactionObserverDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSKPaymentTransactionObserver" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_paymentQueue_updatedTransactions = unSelector (mkSelector "paymentQueue:updatedTransactions:")
      sel_paymentQueue_removedTransactions = unSelector (mkSelector "paymentQueue:removedTransactions:")
      sel_paymentQueue_restoreCompletedTransactionsFailedWithError = unSelector (mkSelector "paymentQueue:restoreCompletedTransactionsFailedWithError:")
      sel_paymentQueueRestoreCompletedTransactionsFinished = unSelector (mkSelector "paymentQueueRestoreCompletedTransactionsFinished:")
      sel_paymentQueue_updatedDownloads = unSelector (mkSelector "paymentQueue:updatedDownloads:")
      sel_paymentQueue_shouldAddStorePayment_forProduct = unSelector (mkSelector "paymentQueue:shouldAddStorePayment:forProduct:")
      sel_paymentQueueDidChangeStorefront = unSelector (mkSelector "paymentQueueDidChangeStorefront:")
      sel_paymentQueue_didRevokeEntitlementsForProductIdentifiers = unSelector (mkSelector "paymentQueue:didRevokeEntitlementsForProductIdentifiers:")
  -- paymentQueue:updatedTransactions:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueue_updatedTransactions rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "paymentQueue:updatedTransactions:" "v@:@@" stub_0

  -- paymentQueue:removedTransactions:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueue_removedTransactions rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "paymentQueue:removedTransactions:" "v@:@@" stub_1

  -- paymentQueue:restoreCompletedTransactionsFailedWithError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueue_restoreCompletedTransactionsFailedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "paymentQueue:restoreCompletedTransactionsFailedWithError:" "v@:@@" stub_2

  -- paymentQueueRestoreCompletedTransactionsFinished:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueueRestoreCompletedTransactionsFinished rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "paymentQueueRestoreCompletedTransactionsFinished:" "v@:@" stub_3

  -- paymentQueue:updatedDownloads:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueue_updatedDownloads rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "paymentQueue:updatedDownloads:" "v@:@@" stub_4

  -- paymentQueue:shouldAddStorePayment:forProduct:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueue_shouldAddStorePayment_forProduct rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "paymentQueue:shouldAddStorePayment:forProduct:" "B@:@@@" stub_5

  -- paymentQueueDidChangeStorefront:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueueDidChangeStorefront rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "paymentQueueDidChangeStorefront:" "v@:@" stub_6

  -- paymentQueue:didRevokeEntitlementsForProductIdentifiers:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    case _paymentQueue_didRevokeEntitlementsForProductIdentifiers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "paymentQueue:didRevokeEntitlementsForProductIdentifiers:" "v@:@@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKPaymentTransactionObserverOverrides
    if queriedSel == sel_paymentQueue_updatedTransactions then pure (maybe 0 (const 1) (_paymentQueue_updatedTransactions rec_))
    else if queriedSel == sel_paymentQueue_removedTransactions then pure (maybe 0 (const 1) (_paymentQueue_removedTransactions rec_))
    else if queriedSel == sel_paymentQueue_restoreCompletedTransactionsFailedWithError then pure (maybe 0 (const 1) (_paymentQueue_restoreCompletedTransactionsFailedWithError rec_))
    else if queriedSel == sel_paymentQueueRestoreCompletedTransactionsFinished then pure (maybe 0 (const 1) (_paymentQueueRestoreCompletedTransactionsFinished rec_))
    else if queriedSel == sel_paymentQueue_updatedDownloads then pure (maybe 0 (const 1) (_paymentQueue_updatedDownloads rec_))
    else if queriedSel == sel_paymentQueue_shouldAddStorePayment_forProduct then pure (maybe 0 (const 1) (_paymentQueue_shouldAddStorePayment_forProduct rec_))
    else if queriedSel == sel_paymentQueueDidChangeStorefront then pure (maybe 0 (const 1) (_paymentQueueDidChangeStorefront rec_))
    else if queriedSel == sel_paymentQueue_didRevokeEntitlementsForProductIdentifiers then pure (maybe 0 (const 1) (_paymentQueue_didRevokeEntitlementsForProductIdentifiers rec_))
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
newSKPaymentTransactionObserver :: SKPaymentTransactionObserverOverrides -> IO RawId
newSKPaymentTransactionObserver overrides = do
  inst <- class_createInstance skPaymentTransactionObserverDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
