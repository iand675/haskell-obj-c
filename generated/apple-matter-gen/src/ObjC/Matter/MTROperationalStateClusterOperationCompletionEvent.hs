{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationCompletionEvent@.
module ObjC.Matter.MTROperationalStateClusterOperationCompletionEvent
  ( MTROperationalStateClusterOperationCompletionEvent
  , IsMTROperationalStateClusterOperationCompletionEvent(..)
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
completionErrorCode :: IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent => mtrOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
completionErrorCode mtrOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrOperationalStateClusterOperationCompletionEvent completionErrorCodeSelector

-- | @- setCompletionErrorCode:@
setCompletionErrorCode :: (IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setCompletionErrorCode mtrOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrOperationalStateClusterOperationCompletionEvent setCompletionErrorCodeSelector (toNSNumber value)

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent => mtrOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
totalOperationalTime mtrOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrOperationalStateClusterOperationCompletionEvent totalOperationalTimeSelector

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setTotalOperationalTime mtrOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrOperationalStateClusterOperationCompletionEvent setTotalOperationalTimeSelector (toNSNumber value)

-- | @- pausedTime@
pausedTime :: IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent => mtrOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
pausedTime mtrOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrOperationalStateClusterOperationCompletionEvent pausedTimeSelector

-- | @- setPausedTime:@
setPausedTime :: (IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setPausedTime mtrOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrOperationalStateClusterOperationCompletionEvent setPausedTimeSelector (toNSNumber value)

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

