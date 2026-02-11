{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXHistogramBucket
--
-- A class that represents a bucket within an MXHistogram
--
-- Histogram buckets are sorted in ascending order.
--
-- Histogram bucket start and end values are exclusive.
--
-- Generated bindings for @MXHistogramBucket@.
module ObjC.MetricKit.MXHistogramBucket
  ( MXHistogramBucket
  , IsMXHistogramBucket(..)
  , bucketStart
  , bucketEnd
  , bucketCount
  , bucketStartSelector
  , bucketEndSelector
  , bucketCountSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | bucketStart
--
-- An NSMeasurement representing the start of a histogram bucket.
--
-- ObjC selector: @- bucketStart@
bucketStart :: IsMXHistogramBucket mxHistogramBucket => mxHistogramBucket -> IO (Id NSMeasurement)
bucketStart mxHistogramBucket  =
  sendMsg mxHistogramBucket (mkSelector "bucketStart") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | bucketEnd
--
-- An NSMeasurement representing the end of a histogram bucket.
--
-- ObjC selector: @- bucketEnd@
bucketEnd :: IsMXHistogramBucket mxHistogramBucket => mxHistogramBucket -> IO (Id NSMeasurement)
bucketEnd mxHistogramBucket  =
  sendMsg mxHistogramBucket (mkSelector "bucketEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | bucketCount
--
-- An NSUInteger representing the number of samples in this histogram bucket.
--
-- ObjC selector: @- bucketCount@
bucketCount :: IsMXHistogramBucket mxHistogramBucket => mxHistogramBucket -> IO CULong
bucketCount mxHistogramBucket  =
  sendMsg mxHistogramBucket (mkSelector "bucketCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bucketStart@
bucketStartSelector :: Selector
bucketStartSelector = mkSelector "bucketStart"

-- | @Selector@ for @bucketEnd@
bucketEndSelector :: Selector
bucketEndSelector = mkSelector "bucketEnd"

-- | @Selector@ for @bucketCount@
bucketCountSelector :: Selector
bucketCountSelector = mkSelector "bucketCount"

