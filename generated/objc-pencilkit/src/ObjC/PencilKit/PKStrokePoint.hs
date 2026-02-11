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
  , initSelector
  , timeOffsetSelector
  , opacitySelector
  , azimuthSelector
  , forceSelector
  , altitudeSelector
  , secondaryScaleSelector
  , thresholdSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO (Id PKStrokePoint)
init_ pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Time offset since the start of the stroke path in seconds.
--
-- ObjC selector: @- timeOffset@
timeOffset :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
timeOffset pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "timeOffset") retCDouble []

-- | Opacity of the point 0-2.
--
-- ObjC selector: @- opacity@
opacity :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
opacity pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "opacity") retCDouble []

-- | Azimuth of the point in radians, 0.0-2π radians
--
-- ObjC selector: @- azimuth@
azimuth :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
azimuth pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "azimuth") retCDouble []

-- | Force used to create this point.
--
-- ObjC selector: @- force@
force :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
force pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "force") retCDouble []

-- | Altitude used to create this point in radians, 0.0-π/2 radians
--
-- ObjC selector: @- altitude@
altitude :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
altitude pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "altitude") retCDouble []

-- | The scaling of the point for secondary effects.
--
-- For example the scaling of the pigment in the watercolor ink.
--
-- ObjC selector: @- secondaryScale@
secondaryScale :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
secondaryScale pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "secondaryScale") retCDouble []

-- | The threshold for clipping the stroke rendering.
--
-- When rendering only pixels with an alpha greater than the threshold are drawn. A threshold of 0 has no affect on rendering, a threshold of 1 does not draw anything. Thresholds are only used for some inks, eg. @PKInkIdentifierReed@.
--
-- ObjC selector: @- threshold@
threshold :: IsPKStrokePoint pkStrokePoint => pkStrokePoint -> IO CDouble
threshold pkStrokePoint  =
  sendMsg pkStrokePoint (mkSelector "threshold") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @timeOffset@
timeOffsetSelector :: Selector
timeOffsetSelector = mkSelector "timeOffset"

-- | @Selector@ for @opacity@
opacitySelector :: Selector
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @azimuth@
azimuthSelector :: Selector
azimuthSelector = mkSelector "azimuth"

-- | @Selector@ for @force@
forceSelector :: Selector
forceSelector = mkSelector "force"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @secondaryScale@
secondaryScaleSelector :: Selector
secondaryScaleSelector = mkSelector "secondaryScale"

-- | @Selector@ for @threshold@
thresholdSelector :: Selector
thresholdSelector = mkSelector "threshold"

