{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , completedSelector
  , completedWithSettledPaymentAmountSelector
  , completedWithOutstandingPaymentAmountSelector
  , completedWithOutstandingFeedbackTypeSelector
  , canceledByServiceSelector
  , canceledByUserSelector
  , canceledMissedPickupSelector
  , completionUserActivitySelector
  , setCompletionUserActivitySelector
  , canceledSelector
  , missedPickupSelector
  , paymentAmountSelector
  , feedbackTypeSelector
  , outstandingSelector
  , defaultTippingOptionsSelector
  , setDefaultTippingOptionsSelector

  -- * Enum types
  , INRideFeedbackTypeOptions(INRideFeedbackTypeOptions)
  , pattern INRideFeedbackTypeOptionRate
  , pattern INRideFeedbackTypeOptionTip

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id INRideCompletionStatus)
init_ inRideCompletionStatus  =
  sendMsg inRideCompletionStatus (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ completed@
inRideCompletionStatusCompleted :: IO (Id INRideCompletionStatus)
inRideCompletionStatusCompleted  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMsg cls' (mkSelector "completed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ completedWithSettledPaymentAmount:@
completedWithSettledPaymentAmount :: IsINCurrencyAmount settledPaymentAmount => settledPaymentAmount -> IO (Id INRideCompletionStatus)
completedWithSettledPaymentAmount settledPaymentAmount =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    withObjCPtr settledPaymentAmount $ \raw_settledPaymentAmount ->
      sendClassMsg cls' (mkSelector "completedWithSettledPaymentAmount:") (retPtr retVoid) [argPtr (castPtr raw_settledPaymentAmount :: Ptr ())] >>= retainedObject . castPtr

-- | @+ completedWithOutstandingPaymentAmount:@
completedWithOutstandingPaymentAmount :: IsINCurrencyAmount outstandingPaymentAmount => outstandingPaymentAmount -> IO (Id INRideCompletionStatus)
completedWithOutstandingPaymentAmount outstandingPaymentAmount =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    withObjCPtr outstandingPaymentAmount $ \raw_outstandingPaymentAmount ->
      sendClassMsg cls' (mkSelector "completedWithOutstandingPaymentAmount:") (retPtr retVoid) [argPtr (castPtr raw_outstandingPaymentAmount :: Ptr ())] >>= retainedObject . castPtr

-- | @+ completedWithOutstandingFeedbackType:@
completedWithOutstandingFeedbackType :: INRideFeedbackTypeOptions -> IO (Id INRideCompletionStatus)
completedWithOutstandingFeedbackType feedbackType =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMsg cls' (mkSelector "completedWithOutstandingFeedbackType:") (retPtr retVoid) [argCULong (coerce feedbackType)] >>= retainedObject . castPtr

-- | @+ canceledByService@
canceledByService :: IO (Id INRideCompletionStatus)
canceledByService  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMsg cls' (mkSelector "canceledByService") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ canceledByUser@
canceledByUser :: IO (Id INRideCompletionStatus)
canceledByUser  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMsg cls' (mkSelector "canceledByUser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ canceledMissedPickup@
canceledMissedPickup :: IO (Id INRideCompletionStatus)
canceledMissedPickup  =
  do
    cls' <- getRequiredClass "INRideCompletionStatus"
    sendClassMsg cls' (mkSelector "canceledMissedPickup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- completionUserActivity@
completionUserActivity :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id NSUserActivity)
completionUserActivity inRideCompletionStatus  =
  sendMsg inRideCompletionStatus (mkSelector "completionUserActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletionUserActivity:@
setCompletionUserActivity :: (IsINRideCompletionStatus inRideCompletionStatus, IsNSUserActivity value) => inRideCompletionStatus -> value -> IO ()
setCompletionUserActivity inRideCompletionStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideCompletionStatus (mkSelector "setCompletionUserActivity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- completed@
completed :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
completed inRideCompletionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRideCompletionStatus (mkSelector "completed") retCULong []

-- | @- canceled@
canceled :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
canceled inRideCompletionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRideCompletionStatus (mkSelector "canceled") retCULong []

-- | @- missedPickup@
missedPickup :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
missedPickup inRideCompletionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRideCompletionStatus (mkSelector "missedPickup") retCULong []

-- | @- paymentAmount@
paymentAmount :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id INCurrencyAmount)
paymentAmount inRideCompletionStatus  =
  sendMsg inRideCompletionStatus (mkSelector "paymentAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- feedbackType@
feedbackType :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO INRideFeedbackTypeOptions
feedbackType inRideCompletionStatus  =
  fmap (coerce :: CULong -> INRideFeedbackTypeOptions) $ sendMsg inRideCompletionStatus (mkSelector "feedbackType") retCULong []

-- | @- outstanding@
outstanding :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO Bool
outstanding inRideCompletionStatus  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRideCompletionStatus (mkSelector "outstanding") retCULong []

-- | @- defaultTippingOptions@
defaultTippingOptions :: IsINRideCompletionStatus inRideCompletionStatus => inRideCompletionStatus -> IO (Id NSSet)
defaultTippingOptions inRideCompletionStatus  =
  sendMsg inRideCompletionStatus (mkSelector "defaultTippingOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultTippingOptions:@
setDefaultTippingOptions :: (IsINRideCompletionStatus inRideCompletionStatus, IsNSSet value) => inRideCompletionStatus -> value -> IO ()
setDefaultTippingOptions inRideCompletionStatus  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideCompletionStatus (mkSelector "setDefaultTippingOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @completed@
completedSelector :: Selector
completedSelector = mkSelector "completed"

-- | @Selector@ for @completedWithSettledPaymentAmount:@
completedWithSettledPaymentAmountSelector :: Selector
completedWithSettledPaymentAmountSelector = mkSelector "completedWithSettledPaymentAmount:"

-- | @Selector@ for @completedWithOutstandingPaymentAmount:@
completedWithOutstandingPaymentAmountSelector :: Selector
completedWithOutstandingPaymentAmountSelector = mkSelector "completedWithOutstandingPaymentAmount:"

-- | @Selector@ for @completedWithOutstandingFeedbackType:@
completedWithOutstandingFeedbackTypeSelector :: Selector
completedWithOutstandingFeedbackTypeSelector = mkSelector "completedWithOutstandingFeedbackType:"

-- | @Selector@ for @canceledByService@
canceledByServiceSelector :: Selector
canceledByServiceSelector = mkSelector "canceledByService"

-- | @Selector@ for @canceledByUser@
canceledByUserSelector :: Selector
canceledByUserSelector = mkSelector "canceledByUser"

-- | @Selector@ for @canceledMissedPickup@
canceledMissedPickupSelector :: Selector
canceledMissedPickupSelector = mkSelector "canceledMissedPickup"

-- | @Selector@ for @completionUserActivity@
completionUserActivitySelector :: Selector
completionUserActivitySelector = mkSelector "completionUserActivity"

-- | @Selector@ for @setCompletionUserActivity:@
setCompletionUserActivitySelector :: Selector
setCompletionUserActivitySelector = mkSelector "setCompletionUserActivity:"

-- | @Selector@ for @canceled@
canceledSelector :: Selector
canceledSelector = mkSelector "canceled"

-- | @Selector@ for @missedPickup@
missedPickupSelector :: Selector
missedPickupSelector = mkSelector "missedPickup"

-- | @Selector@ for @paymentAmount@
paymentAmountSelector :: Selector
paymentAmountSelector = mkSelector "paymentAmount"

-- | @Selector@ for @feedbackType@
feedbackTypeSelector :: Selector
feedbackTypeSelector = mkSelector "feedbackType"

-- | @Selector@ for @outstanding@
outstandingSelector :: Selector
outstandingSelector = mkSelector "outstanding"

-- | @Selector@ for @defaultTippingOptions@
defaultTippingOptionsSelector :: Selector
defaultTippingOptionsSelector = mkSelector "defaultTippingOptions"

-- | @Selector@ for @setDefaultTippingOptions:@
setDefaultTippingOptionsSelector :: Selector
setDefaultTippingOptionsSelector = mkSelector "setDefaultTippingOptions:"

