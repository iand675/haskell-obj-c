{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterOperationCompletionEvent@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterOperationCompletionEvent
  ( MTROvenCavityOperationalStateClusterOperationCompletionEvent
  , IsMTROvenCavityOperationalStateClusterOperationCompletionEvent(..)
  , completionErrorCode
  , setCompletionErrorCode
  , totalOperationalTime
  , setTotalOperationalTime
  , pausedTime
  , setPausedTime
  , completionErrorCodeSelector
  , pausedTimeSelector
  , setCompletionErrorCodeSelector
  , setPausedTimeSelector
  , setTotalOperationalTimeSelector
  , totalOperationalTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- completionErrorCode@
completionErrorCode :: IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
completionErrorCode mtrOvenCavityOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrOvenCavityOperationalStateClusterOperationCompletionEvent completionErrorCodeSelector

-- | @- setCompletionErrorCode:@
setCompletionErrorCode :: (IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setCompletionErrorCode mtrOvenCavityOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationCompletionEvent setCompletionErrorCodeSelector (toNSNumber value)

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
totalOperationalTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrOvenCavityOperationalStateClusterOperationCompletionEvent totalOperationalTimeSelector

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setTotalOperationalTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationCompletionEvent setTotalOperationalTimeSelector (toNSNumber value)

-- | @- pausedTime@
pausedTime :: IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
pausedTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrOvenCavityOperationalStateClusterOperationCompletionEvent pausedTimeSelector

-- | @- setPausedTime:@
setPausedTime :: (IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setPausedTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationCompletionEvent setPausedTimeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completionErrorCode@
completionErrorCodeSelector :: Selector '[] (Id NSNumber)
completionErrorCodeSelector = mkSelector "completionErrorCode"

-- | @Selector@ for @setCompletionErrorCode:@
setCompletionErrorCodeSelector :: Selector '[Id NSNumber] ()
setCompletionErrorCodeSelector = mkSelector "setCompletionErrorCode:"

-- | @Selector@ for @totalOperationalTime@
totalOperationalTimeSelector :: Selector '[] (Id NSNumber)
totalOperationalTimeSelector = mkSelector "totalOperationalTime"

-- | @Selector@ for @setTotalOperationalTime:@
setTotalOperationalTimeSelector :: Selector '[Id NSNumber] ()
setTotalOperationalTimeSelector = mkSelector "setTotalOperationalTime:"

-- | @Selector@ for @pausedTime@
pausedTimeSelector :: Selector '[] (Id NSNumber)
pausedTimeSelector = mkSelector "pausedTime"

-- | @Selector@ for @setPausedTime:@
setPausedTimeSelector :: Selector '[Id NSNumber] ()
setPausedTimeSelector = mkSelector "setPausedTime:"

