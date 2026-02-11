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
objectAtIndexedSubscript mtlRasterizationRateSampleArray  index =
  sendMsg mtlRasterizationRateSampleArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | setObject:atIndexedSubscript:
--
-- Stores a sample value at the specified index.
--
-- The value will be converted to a single precision floating point value.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMTLRasterizationRateSampleArray mtlRasterizationRateSampleArray, IsNSNumber value) => mtlRasterizationRateSampleArray -> value -> CULong -> IO ()
setObject_atIndexedSubscript mtlRasterizationRateSampleArray  value index =
withObjCPtr value $ \raw_value ->
    sendMsg mtlRasterizationRateSampleArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argCULong (fromIntegral index)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

