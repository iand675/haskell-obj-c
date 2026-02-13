{-# LANGUAGE DataKinds #-}
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
  , bucketCountSelector
  , bucketEndSelector
  , bucketStartSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
bucketStart mxHistogramBucket =
  sendMessage mxHistogramBucket bucketStartSelector

-- | bucketEnd
--
-- An NSMeasurement representing the end of a histogram bucket.
--
-- ObjC selector: @- bucketEnd@
bucketEnd :: IsMXHistogramBucket mxHistogramBucket => mxHistogramBucket -> IO (Id NSMeasurement)
bucketEnd mxHistogramBucket =
  sendMessage mxHistogramBucket bucketEndSelector

-- | bucketCount
--
-- An NSUInteger representing the number of samples in this histogram bucket.
--
-- ObjC selector: @- bucketCount@
bucketCount :: IsMXHistogramBucket mxHistogramBucket => mxHistogramBucket -> IO CULong
bucketCount mxHistogramBucket =
  sendMessage mxHistogramBucket bucketCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bucketStart@
bucketStartSelector :: Selector '[] (Id NSMeasurement)
bucketStartSelector = mkSelector "bucketStart"

-- | @Selector@ for @bucketEnd@
bucketEndSelector :: Selector '[] (Id NSMeasurement)
bucketEndSelector = mkSelector "bucketEnd"

-- | @Selector@ for @bucketCount@
bucketCountSelector :: Selector '[] CULong
bucketCountSelector = mkSelector "bucketCount"

