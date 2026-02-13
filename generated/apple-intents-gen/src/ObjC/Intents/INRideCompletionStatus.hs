{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideCompletionStatus@.
module ObjC.Intents.INRideCompletionStatus
  ( INRideCompletionStatus
  , IsINRideCompletionStatus(..)
  , init_
  , inRideCompletionStatusCompleted
  , completedWithSettledPaymentAmount
  , completedWithOutstandingPaymentAmount
  , completedWithOutstandingFeedbackType
  , canceledByService
  , canceledByUser
  , canceledMissedPickup
  , completionUserActivity
  , setCompletionUserActivity
  , completed
  , canceled
  , missedPickup
  , paymentAmount
  , feedbackType
  , outstanding
  , defaultTippingOptions
  , setDefaultTippingOptions
  , canceledByServiceSelector
  , canceledByUserSelector
  , canceledMissedPickupSelector
  , canceledSelector
  , completedSelector
  , completedWithOutstandingFeedbackTypeSelector
  , completedWithOutstandingPaymentAmountSelector
  , completedWithSettledPaymentAmountSelector
  , completionUserActivitySelector
  , defaultTippingOptionsSelector
  , feedbackTypeSelector
  , inRideCompletionStatusCompletedSelector
  , initSelector
  , missedPickupSelector
  , outstandingSelector
  , paymentAmountSelector
  , setCompletionUserActivitySelector
  , setDefaultTippingOptionsSelector

  -- * Enum types
  , INRideFeedbackTypeOptions(INRideFeedbackTypeOptions)
  , pattern INRideFeedbackTypeOptionRate
  , pattern INRideFeedbackTypeOptionTip

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id INRideCompletionStatus)
init_ inRideCompletionStatus =
  sendOwnedMessage inRideCompletionStatus initSelector

-- | @+ completed@
inRideCompletionStatusCompleted :: IO (Id INRideCompletionStatus)
inRideCompletionStatusCompleted  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' inRideCompletionStatusCompletedSelector

-- | @+ completedWithSettledPaymentAmount:@
completedWithSettledPaymentAmount :: IsINCurrencyAmount settledPaymentAmount => settledPaymentAmount -> IO (Id INRideCompletionStatus)
completedWithSettledPaymentAmount settledPaymentAmount =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' completedWithSettledPaymentAmountSelector (toINCurrencyAmount settledPaymentAmount)

-- | @+ completedWithOutstandingPaymentAmount:@
completedWithOutstandingPaymentAmount :: IsINCurrencyAmount outstandingPaymentAmount => outstandingPaymentAmount -> IO (Id INRideCompletionStatus)
completedWithOutstandingPaymentAmount outstandingPaymentAmount =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' completedWithOutstandingPaymentAmountSelector (toINCurrencyAmount outstandingPaymentAmount)

-- | @+ completedWithOutstandingFeedbackType:@
completedWithOutstandingFeedbackType :: INRideFeedbackTypeOptions -> IO (Id INRideCompletionStatus)
completedWithOutstandingFeedbackType feedbackType =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' completedWithOutstandingFeedbackTypeSelector feedbackType

-- | @+ canceledByService@
canceledByService :: IO (Id INRideCompletionStatus)
canceledByService  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' canceledByServiceSelector

-- | @+ canceledByUser@
canceledByUser :: IO (Id INRideCompletionStatus)
canceledByUser  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' canceledByUserSelector

-- | @+ canceledMissedPickup@
canceledMissedPickup :: IO (Id INRideCompletionStatus)
canceledMissedPickup  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMessage cls' canceledMissedPickupSelector

-- | @- completionUserActivity@
completionUserActivity :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id NSUserActivity)
completionUserActivity inRideCompletionStatus =
  sendMessage inRideCompletionStatus completionUserActivitySelector

-- | @- setCompletionUserActivity:@
setCompletionUserActivity :: (IsINRideCompletionStatus inRideCompletionStatus, IsNSUserActivity value) => inRideCompletionStatus -> value -> IO ()
setCompletionUserActivity inRideCompletionStatus value =
  sendMessage inRideCompletionStatus setCompletionUserActivitySelector (toNSUserActivity value)

-- | @- completed@
completed :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
completed inRideCompletionStatus =
  sendMessage inRideCompletionStatus completedSelector

-- | @- canceled@
canceled :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
canceled inRideCompletionStatus =
  sendMessage inRideCompletionStatus canceledSelector

-- | @- missedPickup@
missedPickup :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
missedPickup inRideCompletionStatus =
  sendMessage inRideCompletionStatus missedPickupSelector

-- | @- paymentAmount@
paymentAmount :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id INCurrencyAmount)
paymentAmount inRideCompletionStatus =
  sendMessage inRideCompletionStatus paymentAmountSelector

-- | @- feedbackType@
feedbackType :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO INRideFeedbackTypeOptions
feedbackType inRideCompletionStatus =
  sendMessage inRideCompletionStatus feedbackTypeSelector

-- | @- outstanding@
outstanding :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
outstanding inRideCompletionStatus =
  sendMessage inRideCompletionStatus outstandingSelector

-- | @- defaultTippingOptions@
defaultTippingOptions :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id NSSet)
defaultTippingOptions inRideCompletionStatus =
  sendMessage inRideCompletionStatus defaultTippingOptionsSelector

-- | @- setDefaultTippingOptions:@
setDefaultTippingOptions :: (IsINRideCompletionStatus inRideCompletionStatus, IsNSSet value) => inRideCompletionStatus -> value -> IO ()
setDefaultTippingOptions inRideCompletionStatus value =
  sendMessage inRideCompletionStatus setDefaultTippingOptionsSelector (toNSSet value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRideCompletionStatus)
initSelector = mkSelector "init"

-- | @Selector@ for @completed@
inRideCompletionStatusCompletedSelector :: Selector '[] (Id INRideCompletionStatus)
inRideCompletionStatusCompletedSelector = mkSelector "completed"

-- | @Selector@ for @completedWithSettledPaymentAmount:@
completedWithSettledPaymentAmountSelector :: Selector '[Id INCurrencyAmount] (Id INRideCompletionStatus)
completedWithSettledPaymentAmountSelector = mkSelector "completedWithSettledPaymentAmount:"

-- | @Selector@ for @completedWithOutstandingPaymentAmount:@
completedWithOutstandingPaymentAmountSelector :: Selector '[Id INCurrencyAmount] (Id INRideCompletionStatus)
completedWithOutstandingPaymentAmountSelector = mkSelector "completedWithOutstandingPaymentAmount:"

-- | @Selector@ for @completedWithOutstandingFeedbackType:@
completedWithOutstandingFeedbackTypeSelector :: Selector '[INRideFeedbackTypeOptions] (Id INRideCompletionStatus)
completedWithOutstandingFeedbackTypeSelector = mkSelector "completedWithOutstandingFeedbackType:"

-- | @Selector@ for @canceledByService@
canceledByServiceSelector :: Selector '[] (Id INRideCompletionStatus)
canceledByServiceSelector = mkSelector "canceledByService"

-- | @Selector@ for @canceledByUser@
canceledByUserSelector :: Selector '[] (Id INRideCompletionStatus)
canceledByUserSelector = mkSelector "canceledByUser"

-- | @Selector@ for @canceledMissedPickup@
canceledMissedPickupSelector :: Selector '[] (Id INRideCompletionStatus)
canceledMissedPickupSelector = mkSelector "canceledMissedPickup"

-- | @Selector@ for @completionUserActivity@
completionUserActivitySelector :: Selector '[] (Id NSUserActivity)
completionUserActivitySelector = mkSelector "completionUserActivity"

-- | @Selector@ for @setCompletionUserActivity:@
setCompletionUserActivitySelector :: Selector '[Id NSUserActivity] ()
setCompletionUserActivitySelector = mkSelector "setCompletionUserActivity:"

-- | @Selector@ for @completed@
completedSelector :: Selector '[] Bool
completedSelector = mkSelector "completed"

-- | @Selector@ for @canceled@
canceledSelector :: Selector '[] Bool
canceledSelector = mkSelector "canceled"

-- | @Selector@ for @missedPickup@
missedPickupSelector :: Selector '[] Bool
missedPickupSelector = mkSelector "missedPickup"

-- | @Selector@ for @paymentAmount@
paymentAmountSelector :: Selector '[] (Id INCurrencyAmount)
paymentAmountSelector = mkSelector "paymentAmount"

-- | @Selector@ for @feedbackType@
feedbackTypeSelector :: Selector '[] INRideFeedbackTypeOptions
feedbackTypeSelector = mkSelector "feedbackType"

-- | @Selector@ for @outstanding@
outstandingSelector :: Selector '[] Bool
outstandingSelector = mkSelector "outstanding"

-- | @Selector@ for @defaultTippingOptions@
defaultTippingOptionsSelector :: Selector '[] (Id NSSet)
defaultTippingOptionsSelector = mkSelector "defaultTippingOptions"

-- | @Selector@ for @setDefaultTippingOptions:@
setDefaultTippingOptionsSelector :: Selector '[Id NSSet] ()
setDefaultTippingOptionsSelector = mkSelector "setDefaultTippingOptions:"

