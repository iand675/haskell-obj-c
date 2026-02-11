{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNCompare
--
-- This depends on Metal.framework.
--
-- Specifies the elementwise comparison operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = (abs(x-y)) <= threshold
--
-- Generated bindings for @MPSNNCompare@.
module ObjC.MetalPerformanceShaders.MPSNNCompare
  ( MPSNNCompare
  , IsMPSNNCompare(..)
  , initWithDevice
  , comparisonType
  , setComparisonType
  , threshold
  , setThreshold
  , initWithDeviceSelector
  , comparisonTypeSelector
  , setComparisonTypeSelector
  , thresholdSelector
  , setThresholdSelector

  -- * Enum types
  , MPSNNComparisonType(MPSNNComparisonType)
  , pattern MPSNNComparisonTypeEqual
  , pattern MPSNNComparisonTypeNotEqual
  , pattern MPSNNComparisonTypeLess
  , pattern MPSNNComparisonTypeLessOrEqual
  , pattern MPSNNComparisonTypeGreater
  , pattern MPSNNComparisonTypeGreaterOrEqual

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize the comparison operator
--
-- @device@ — The device the filter will run on.
--
-- Returns: A valid MPSNNCompare object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNCompare mpsnnCompare => mpsnnCompare -> RawId -> IO (Id MPSNNCompare)
initWithDevice mpsnnCompare  device =
  sendMsg mpsnnCompare (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | comparisonType
--
-- The comparison type to use
--
-- ObjC selector: @- comparisonType@
comparisonType :: IsMPSNNCompare mpsnnCompare => mpsnnCompare -> IO MPSNNComparisonType
comparisonType mpsnnCompare  =
  fmap (coerce :: CULong -> MPSNNComparisonType) $ sendMsg mpsnnCompare (mkSelector "comparisonType") retCULong []

-- | comparisonType
--
-- The comparison type to use
--
-- ObjC selector: @- setComparisonType:@
setComparisonType :: IsMPSNNCompare mpsnnCompare => mpsnnCompare -> MPSNNComparisonType -> IO ()
setComparisonType mpsnnCompare  value =
  sendMsg mpsnnCompare (mkSelector "setComparisonType:") retVoid [argCULong (coerce value)]

-- | threshold
--
-- The threshold to use when comparing for equality.  Two values will              be considered to be equal if the absolute value of their difference              is less than, or equal, to the specified threshold:                  result = |b - a| <= threshold
--
-- ObjC selector: @- threshold@
threshold :: IsMPSNNCompare mpsnnCompare => mpsnnCompare -> IO CFloat
threshold mpsnnCompare  =
  sendMsg mpsnnCompare (mkSelector "threshold") retCFloat []

-- | threshold
--
-- The threshold to use when comparing for equality.  Two values will              be considered to be equal if the absolute value of their difference              is less than, or equal, to the specified threshold:                  result = |b - a| <= threshold
--
-- ObjC selector: @- setThreshold:@
setThreshold :: IsMPSNNCompare mpsnnCompare => mpsnnCompare -> CFloat -> IO ()
setThreshold mpsnnCompare  value =
  sendMsg mpsnnCompare (mkSelector "setThreshold:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @comparisonType@
comparisonTypeSelector :: Selector
comparisonTypeSelector = mkSelector "comparisonType"

-- | @Selector@ for @setComparisonType:@
setComparisonTypeSelector :: Selector
setComparisonTypeSelector = mkSelector "setComparisonType:"

-- | @Selector@ for @threshold@
thresholdSelector :: Selector
thresholdSelector = mkSelector "threshold"

-- | @Selector@ for @setThreshold:@
setThresholdSelector :: Selector
setThresholdSelector = mkSelector "setThreshold:"

