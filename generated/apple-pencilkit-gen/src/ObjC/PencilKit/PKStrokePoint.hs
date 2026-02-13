{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A point value stores all the attributes of a PKStroke at a specific point. @PKStrokePoint@ stores its properties compressed, the value read for a property may not exactly equal the value set for a property.
--
-- Generated bindings for @PKStrokePoint@.
module ObjC.PencilKit.PKStrokePoint
  ( PKStrokePoint
  , IsPKStrokePoint(..)
  , init_
  , timeOffset
  , opacity
  , azimuth
  , force
  , altitude
  , secondaryScale
  , threshold
  , altitudeSelector
  , azimuthSelector
  , forceSelector
  , initSelector
  , opacitySelector
  , secondaryScaleSelector
  , thresholdSelector
  , timeOffsetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO (Id PKStrokePoint)
init_ pkStrokePoint =
  sendOwnedMessage pkStrokePoint initSelector

-- | Time offset since the start of the stroke path in seconds.
--
-- ObjC selector: @- timeOffset@
timeOffset :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
timeOffset pkStrokePoint =
  sendMessage pkStrokePoint timeOffsetSelector

-- | Opacity of the point 0-2.
--
-- ObjC selector: @- opacity@
opacity :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
opacity pkStrokePoint =
  sendMessage pkStrokePoint opacitySelector

-- | Azimuth of the point in radians, 0.0-2π radians
--
-- ObjC selector: @- azimuth@
azimuth :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
azimuth pkStrokePoint =
  sendMessage pkStrokePoint azimuthSelector

-- | Force used to create this point.
--
-- ObjC selector: @- force@
force :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
force pkStrokePoint =
  sendMessage pkStrokePoint forceSelector

-- | Altitude used to create this point in radians, 0.0-π/2 radians
--
-- ObjC selector: @- altitude@
altitude :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
altitude pkStrokePoint =
  sendMessage pkStrokePoint altitudeSelector

-- | The scaling of the point for secondary effects.
--
-- For example the scaling of the pigment in the watercolor ink.
--
-- ObjC selector: @- secondaryScale@
secondaryScale :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
secondaryScale pkStrokePoint =
  sendMessage pkStrokePoint secondaryScaleSelector

-- | The threshold for clipping the stroke rendering.
--
-- When rendering only pixels with an alpha greater than the threshold are drawn. A threshold of 0 has no affect on rendering, a threshold of 1 does not draw anything. Thresholds are only used for some inks, eg. @PKInkIdentifierReed@.
--
-- ObjC selector: @- threshold@
threshold :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
threshold pkStrokePoint =
  sendMessage pkStrokePoint thresholdSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKStrokePoint)
initSelector = mkSelector "init"

-- | @Selector@ for @timeOffset@
timeOffsetSelector :: Selector '[] CDouble
timeOffsetSelector = mkSelector "timeOffset"

-- | @Selector@ for @opacity@
opacitySelector :: Selector '[] CDouble
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @azimuth@
azimuthSelector :: Selector '[] CDouble
azimuthSelector = mkSelector "azimuth"

-- | @Selector@ for @force@
forceSelector :: Selector '[] CDouble
forceSelector = mkSelector "force"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector '[] CDouble
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @secondaryScale@
secondaryScaleSelector :: Selector '[] CDouble
secondaryScaleSelector = mkSelector "secondaryScale"

-- | @Selector@ for @threshold@
thresholdSelector :: Selector '[] CDouble
thresholdSelector = mkSelector "threshold"

