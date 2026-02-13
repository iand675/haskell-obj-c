{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallController@.
module ObjC.CallKit.CXCallController
  ( CXCallController
  , IsCXCallController(..)
  , init_
  , initWithQueue
  , requestTransaction_completion
  , requestTransactionWithActions_completion
  , requestTransactionWithAction_completion
  , callObserver
  , callObserverSelector
  , initSelector
  , initWithQueueSelector
  , requestTransactionWithAction_completionSelector
  , requestTransactionWithActions_completionSelector
  , requestTransaction_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize call controller with a private, serial queue.
--
-- ObjC selector: @- init@
init_ :: IsCXCallController cxCallController => cxCallController -> IO (Id CXCallController)
init_ cxCallController =
  sendOwnedMessage cxCallController initSelector

-- | Initialize call controller with specified queue, which is used for calling completion blocks.
--
-- ObjC selector: @- initWithQueue:@
initWithQueue :: (IsCXCallController cxCallController, IsNSObject queue) => cxCallController -> queue -> IO (Id CXCallController)
initWithQueue cxCallController queue =
  sendOwnedMessage cxCallController initWithQueueSelector (toNSObject queue)

-- | Request a transaction to be performed by the in-app provider.
--
-- If the completion block is called with a nil error, then the transaction will be passed to the CXProvider's -provider:executeTransaction: delegate callback. A non-nil error indicates that the requested transaction could not be executed.
--
-- Completion block is performed on the queue supplied to designated initializer.
--
-- ObjC selector: @- requestTransaction:completion:@
requestTransaction_completion :: (IsCXCallController cxCallController, IsCXTransaction transaction) => cxCallController -> transaction -> Ptr () -> IO ()
requestTransaction_completion cxCallController transaction completion =
  sendMessage cxCallController requestTransaction_completionSelector (toCXTransaction transaction) completion

-- | Request a transaction containing the specified actions to be performed by the in-app provider.
--
-- If the completion block is called with a nil error, then the transaction will be passed to the CXProvider's -provider:executeTransaction: delegate callback. A non-nil error indicates that the requested transaction could not be executed.
--
-- Completion block is performed on the queue supplied to designated initializer.
--
-- ObjC selector: @- requestTransactionWithActions:completion:@
requestTransactionWithActions_completion :: (IsCXCallController cxCallController, IsNSArray actions) => cxCallController -> actions -> Ptr () -> IO ()
requestTransactionWithActions_completion cxCallController actions completion =
  sendMessage cxCallController requestTransactionWithActions_completionSelector (toNSArray actions) completion

-- | Request a transaction containing the specified action to be performed by the in-app provider.
--
-- If the completion block is called with a nil error, then the transaction will be passed to the CXProvider's -provider:executeTransaction: delegate callback. A non-nil error indicates that the requested transaction could not be executed.
--
-- Completion block is performed on the queue supplied to designated initializer.
--
-- ObjC selector: @- requestTransactionWithAction:completion:@
requestTransactionWithAction_completion :: (IsCXCallController cxCallController, IsCXAction action) => cxCallController -> action -> Ptr () -> IO ()
requestTransactionWithAction_completion cxCallController action completion =
  sendMessage cxCallController requestTransactionWithAction_completionSelector (toCXAction action) completion

-- | @- callObserver@
callObserver :: IsCXCallController cxCallController => cxCallController -> IO (Id CXCallObserver)
callObserver cxCallController =
  sendMessage cxCallController callObserverSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXCallController)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithQueue:@
initWithQueueSelector :: Selector '[Id NSObject] (Id CXCallController)
initWithQueueSelector = mkSelector "initWithQueue:"

-- | @Selector@ for @requestTransaction:completion:@
requestTransaction_completionSelector :: Selector '[Id CXTransaction, Ptr ()] ()
requestTransaction_completionSelector = mkSelector "requestTransaction:completion:"

-- | @Selector@ for @requestTransactionWithActions:completion:@
requestTransactionWithActions_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
requestTransactionWithActions_completionSelector = mkSelector "requestTransactionWithActions:completion:"

-- | @Selector@ for @requestTransactionWithAction:completion:@
requestTransactionWithAction_completionSelector :: Selector '[Id CXAction, Ptr ()] ()
requestTransactionWithAction_completionSelector = mkSelector "requestTransactionWithAction:completion:"

-- | @Selector@ for @callObserver@
callObserverSelector :: Selector '[] (Id CXCallObserver)
callObserverSelector = mkSelector "callObserver"

