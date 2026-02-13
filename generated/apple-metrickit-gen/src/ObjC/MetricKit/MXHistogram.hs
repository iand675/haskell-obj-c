{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXHistogram
--
-- A class representing bucketized histogram data.
--
-- Generated bindings for @MXHistogram@.
module ObjC.MetricKit.MXHistogram
  ( MXHistogram
  , IsMXHistogram(..)
  , totalBucketCount
  , bucketEnumerator
  , bucketEnumeratorSelector
  , totalBucketCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | totalBucketCount
--
-- The number of buckets contained within this histogram.
--
-- This value can never be negative.
--
-- ObjC selector: @- totalBucketCount@
totalBucketCount :: IsMXHistogram mxHistogram => mxHistogram -> IO CULong
totalBucketCount mxHistogram =
  sendMessage mxHistogram totalBucketCountSelector

-- | bucketEnumerator
--
-- An NSEnumerator that can be used to enumerate the buckets of this histogram.
--
-- ObjC selector: @- bucketEnumerator@
bucketEnumerator :: IsMXHistogram mxHistogram => mxHistogram -> IO (Id NSEnumerator)
bucketEnumerator mxHistogram =
  sendMessage mxHistogram bucketEnumeratorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @totalBucketCount@
totalBucketCountSelector :: Selector '[] CULong
totalBucketCountSelector = mkSelector "totalBucketCount"

-- | @Selector@ for @bucketEnumerator@
bucketEnumeratorSelector :: Selector '[] (Id NSEnumerator)
bucketEnumeratorSelector = mkSelector "bucketEnumerator"

