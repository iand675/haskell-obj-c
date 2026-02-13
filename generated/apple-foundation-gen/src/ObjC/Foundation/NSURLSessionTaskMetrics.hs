{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionTaskMetrics@.
module ObjC.Foundation.NSURLSessionTaskMetrics
  ( NSURLSessionTaskMetrics
  , IsNSURLSessionTaskMetrics(..)
  , init_
  , new
  , transactionMetrics
  , taskInterval
  , redirectCount
  , initSelector
  , newSelector
  , redirectCountSelector
  , taskIntervalSelector
  , transactionMetricsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO (Id NSURLSessionTaskMetrics)
init_ nsurlSessionTaskMetrics =
  sendOwnedMessage nsurlSessionTaskMetrics initSelector

-- | @+ new@
new :: IO (Id NSURLSessionTaskMetrics)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionTaskMetrics"
    sendOwnedClassMessage cls' newSelector

-- | @- transactionMetrics@
transactionMetrics :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO (Id NSArray)
transactionMetrics nsurlSessionTaskMetrics =
  sendMessage nsurlSessionTaskMetrics transactionMetricsSelector

-- | @- taskInterval@
taskInterval :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO (Id NSDateInterval)
taskInterval nsurlSessionTaskMetrics =
  sendMessage nsurlSessionTaskMetrics taskIntervalSelector

-- | @- redirectCount@
redirectCount :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO CULong
redirectCount nsurlSessionTaskMetrics =
  sendMessage nsurlSessionTaskMetrics redirectCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionTaskMetrics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionTaskMetrics)
newSelector = mkSelector "new"

-- | @Selector@ for @transactionMetrics@
transactionMetricsSelector :: Selector '[] (Id NSArray)
transactionMetricsSelector = mkSelector "transactionMetrics"

-- | @Selector@ for @taskInterval@
taskIntervalSelector :: Selector '[] (Id NSDateInterval)
taskIntervalSelector = mkSelector "taskInterval"

-- | @Selector@ for @redirectCount@
redirectCountSelector :: Selector '[] CULong
redirectCountSelector = mkSelector "redirectCount"

