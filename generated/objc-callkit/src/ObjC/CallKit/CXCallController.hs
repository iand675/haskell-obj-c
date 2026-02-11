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
  , initSelector
  , initWithQueueSelector
  , requestTransaction_completionSelector
  , requestTransactionWithActions_completionSelector
  , requestTransactionWithAction_completionSelector
  , callObserverSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize call controller with a private, serial queue.
--
-- ObjC selector: @- init@
init_ :: IsCXCallController cxCallController => cxCallController -> IO (Id CXCallController)
init_ cxCallController  =
  sendMsg cxCallController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize call controller with specified queue, which is used for calling completion blocks.
--
-- ObjC selector: @- initWithQueue:@
initWithQueue :: (IsCXCallController cxCallController, IsNSObject queue) => cxCallController -> queue -> IO (Id CXCallController)
initWithQueue cxCallController  queue =
withObjCPtr queue $ \raw_queue ->
    sendMsg cxCallController (mkSelector "initWithQueue:") (retPtr retVoid) [argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | Request a transaction to be performed by the in-app provider.
--
-- If the completion block is called with a nil error, then the transaction will be passed to the CXProvider's -provider:executeTransaction: delegate callback. A non-nil error indicates that the requested transaction could not be executed.
--
-- Completion block is performed on the queue supplied to designated initializer.
--
-- ObjC selector: @- requestTransaction:completion:@
requestTransaction_completion :: (IsCXCallController cxCallController, IsCXTransaction transaction) => cxCallController -> transaction -> Ptr () -> IO ()
requestTransaction_completion cxCallController  transaction completion =
withObjCPtr transaction $ \raw_transaction ->
    sendMsg cxCallController (mkSelector "requestTransaction:completion:") retVoid [argPtr (castPtr raw_transaction :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Request a transaction containing the specified actions to be performed by the in-app provider.
--
-- If the completion block is called with a nil error, then the transaction will be passed to the CXProvider's -provider:executeTransaction: delegate callback. A non-nil error indicates that the requested transaction could not be executed.
--
-- Completion block is performed on the queue supplied to designated initializer.
--
-- ObjC selector: @- requestTransactionWithActions:completion:@
requestTransactionWithActions_completion :: (IsCXCallController cxCallController, IsNSArray actions) => cxCallController -> actions -> Ptr () -> IO ()
requestTransactionWithActions_completion cxCallController  actions completion =
withObjCPtr actions $ \raw_actions ->
    sendMsg cxCallController (mkSelector "requestTransactionWithActions:completion:") retVoid [argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Request a transaction containing the specified action to be performed by the in-app provider.
--
-- If the completion block is called with a nil error, then the transaction will be passed to the CXProvider's -provider:executeTransaction: delegate callback. A non-nil error indicates that the requested transaction could not be executed.
--
-- Completion block is performed on the queue supplied to designated initializer.
--
-- ObjC selector: @- requestTransactionWithAction:completion:@
requestTransactionWithAction_completion :: (IsCXCallController cxCallController, IsCXAction action) => cxCallController -> action -> Ptr () -> IO ()
requestTransactionWithAction_completion cxCallController  action completion =
withObjCPtr action $ \raw_action ->
    sendMsg cxCallController (mkSelector "requestTransactionWithAction:completion:") retVoid [argPtr (castPtr raw_action :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- callObserver@
callObserver :: IsCXCallController cxCallController => cxCallController -> IO (Id CXCallObserver)
callObserver cxCallController  =
  sendMsg cxCallController (mkSelector "callObserver") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithQueue:@
initWithQueueSelector :: Selector
initWithQueueSelector = mkSelector "initWithQueue:"

-- | @Selector@ for @requestTransaction:completion:@
requestTransaction_completionSelector :: Selector
requestTransaction_completionSelector = mkSelector "requestTransaction:completion:"

-- | @Selector@ for @requestTransactionWithActions:completion:@
requestTransactionWithActions_completionSelector :: Selector
requestTransactionWithActions_completionSelector = mkSelector "requestTransactionWithActions:completion:"

-- | @Selector@ for @requestTransactionWithAction:completion:@
requestTransactionWithAction_completionSelector :: Selector
requestTransactionWithAction_completionSelector = mkSelector "requestTransactionWithAction:completion:"

-- | @Selector@ for @callObserver@
callObserverSelector :: Selector
callObserverSelector = mkSelector "callObserver"

