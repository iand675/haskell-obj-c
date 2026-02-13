{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLRasterizationRateSampleArray
--
-- A helper object for convient access to samples stored in an array.
--
-- Generated bindings for @MTLRasterizationRateSampleArray@.
module ObjC.Metal.MTLRasterizationRateSampleArray
  ( MTLRasterizationRateSampleArray
  , IsMTLRasterizationRateSampleArray(..)
  , objectAtIndexedSubscript
  , setObject_atIndexedSubscript
  , objectAtIndexedSubscriptSelector
  , setObject_atIndexedSubscriptSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | objectAtIndexedSubscript:
--
-- Retrieves the sample value at the specified index.
--
-- Returns: NSNumber instance describing the value of the sample at the specified index, or 0 if the index is out of range.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMTLRasterizationRateSampleArray mtlRasterizationRateSampleArray => mtlRasterizationRateSampleArray -> CULong -> IO (Id NSNumber)
objectAtIndexedSubscript mtlRasterizationRateSampleArray index =
  sendMessage mtlRasterizationRateSampleArray objectAtIndexedSubscriptSelector index

-- | setObject:atIndexedSubscript:
--
-- Stores a sample value at the specified index.
--
-- The value will be converted to a single precision floating point value.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRasterizationRateSampleArray mtlRasterizationRateSampleArray, IsNSNumber value) => mtlRasterizationRateSampleArray -> value -> CULong -> IO ()
setObject_atIndexedSubscript mtlRasterizationRateSampleArray value index =
  sendMessage mtlRasterizationRateSampleArray setObject_atIndexedSubscriptSelector (toNSNumber value) index

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id NSNumber)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id NSNumber, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

