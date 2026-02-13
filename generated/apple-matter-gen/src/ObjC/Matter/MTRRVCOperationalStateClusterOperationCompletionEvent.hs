{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterOperationCompletionEvent@.
module ObjC.Matter.MTRRVCOperationalStateClusterOperationCompletionEvent
  ( MTRRVCOperationalStateClusterOperationCompletionEvent
  , IsMTRRVCOperationalStateClusterOperationCompletionEvent(..)
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
completionErrorCode :: IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent => mtrrvcOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
completionErrorCode mtrrvcOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrrvcOperationalStateClusterOperationCompletionEvent completionErrorCodeSelector

-- | @- setCompletionErrorCode:@
setCompletionErrorCode :: (IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrrvcOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setCompletionErrorCode mtrrvcOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrrvcOperationalStateClusterOperationCompletionEvent setCompletionErrorCodeSelector (toNSNumber value)

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent => mtrrvcOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
totalOperationalTime mtrrvcOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrrvcOperationalStateClusterOperationCompletionEvent totalOperationalTimeSelector

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrrvcOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setTotalOperationalTime mtrrvcOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrrvcOperationalStateClusterOperationCompletionEvent setTotalOperationalTimeSelector (toNSNumber value)

-- | @- pausedTime@
pausedTime :: IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent => mtrrvcOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
pausedTime mtrrvcOperationalStateClusterOperationCompletionEvent =
  sendMessage mtrrvcOperationalStateClusterOperationCompletionEvent pausedTimeSelector

-- | @- setPausedTime:@
setPausedTime :: (IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrrvcOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setPausedTime mtrrvcOperationalStateClusterOperationCompletionEvent value =
  sendMessage mtrrvcOperationalStateClusterOperationCompletionEvent setPausedTimeSelector (toNSNumber value)

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

