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
  , totalBucketCountSelector
  , bucketEnumeratorSelector


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

-- | totalBucketCount
--
-- The number of buckets contained within this histogram.
--
-- This value can never be negative.
--
-- ObjC selector: @- totalBucketCount@
totalBucketCount :: IsMXHistogram mxHistogram => mxHistogram -> IO CULong
totalBucketCount mxHistogram  =
  sendMsg mxHistogram (mkSelector "totalBucketCount") retCULong []

-- | bucketEnumerator
--
-- An NSEnumerator that can be used to enumerate the buckets of this histogram.
--
-- ObjC selector: @- bucketEnumerator@
bucketEnumerator :: IsMXHistogram mxHistogram => mxHistogram -> IO (Id NSEnumerator)
bucketEnumerator mxHistogram  =
  sendMsg mxHistogram (mkSelector "bucketEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @totalBucketCount@
totalBucketCountSelector :: Selector
totalBucketCountSelector = mkSelector "totalBucketCount"

-- | @Selector@ for @bucketEnumerator@
bucketEnumeratorSelector :: Selector
bucketEnumeratorSelector = mkSelector "bucketEnumerator"

