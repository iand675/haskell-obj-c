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
  , transactionMetricsSelector
  , taskIntervalSelector
  , redirectCountSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO (Id NSURLSessionTaskMetrics)
init_ nsurlSessionTaskMetrics  =
  sendMsg nsurlSessionTaskMetrics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionTaskMetrics)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionTaskMetrics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- transactionMetrics@
transactionMetrics :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO (Id NSArray)
transactionMetrics nsurlSessionTaskMetrics  =
  sendMsg nsurlSessionTaskMetrics (mkSelector "transactionMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- taskInterval@
taskInterval :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO (Id NSDateInterval)
taskInterval nsurlSessionTaskMetrics  =
  sendMsg nsurlSessionTaskMetrics (mkSelector "taskInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- redirectCount@
redirectCount :: IsNSURLSessionTaskMetrics nsurlSessionTaskMetrics => nsurlSessionTaskMetrics -> IO CULong
redirectCount nsurlSessionTaskMetrics  =
  sendMsg nsurlSessionTaskMetrics (mkSelector "redirectCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @transactionMetrics@
transactionMetricsSelector :: Selector
transactionMetricsSelector = mkSelector "transactionMetrics"

-- | @Selector@ for @taskInterval@
taskIntervalSelector :: Selector
taskIntervalSelector = mkSelector "taskInterval"

-- | @Selector@ for @redirectCount@
redirectCountSelector :: Selector
redirectCountSelector = mkSelector "redirectCount"

