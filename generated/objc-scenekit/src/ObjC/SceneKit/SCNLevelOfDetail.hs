{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNLevelOfDetail
--
-- SCNLevelOfDetail represents a level of detail of a geometry.
--
-- Generated bindings for @SCNLevelOfDetail@.
module ObjC.SceneKit.SCNLevelOfDetail
  ( SCNLevelOfDetail
  , IsSCNLevelOfDetail(..)
  , levelOfDetailWithGeometry_screenSpaceRadius
  , levelOfDetailWithGeometry_worldSpaceDistance
  , geometry
  , screenSpaceRadius
  , worldSpaceDistance
  , levelOfDetailWithGeometry_screenSpaceRadiusSelector
  , levelOfDetailWithGeometry_worldSpaceDistanceSelector
  , geometrySelector
  , screenSpaceRadiusSelector
  , worldSpaceDistanceSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | levelOfDetailWithGeometry:screenSpaceRadius:
--
-- This is a convenience method to create a level of detail with a coverage radius threshold mode.
--
-- @geometry@ — The geometry for this level of detail. nil is supported and indicates that no geometry should be rendered for this level of detail.
--
-- @radius@ — The maximum radius in screen-space that this level of detail is suitable for. The coverage radius is calculated from the projected bounding sphere and expressed in pixels.
--
-- ObjC selector: @+ levelOfDetailWithGeometry:screenSpaceRadius:@
levelOfDetailWithGeometry_screenSpaceRadius :: IsSCNGeometry geometry => geometry -> CDouble -> IO (Id SCNLevelOfDetail)
levelOfDetailWithGeometry_screenSpaceRadius geometry radius =
  do
    cls' <- getRequiredClass "SCNLevelOfDetail"
    withObjCPtr geometry $ \raw_geometry ->
      sendClassMsg cls' (mkSelector "levelOfDetailWithGeometry:screenSpaceRadius:") (retPtr retVoid) [argPtr (castPtr raw_geometry :: Ptr ()), argCDouble (fromIntegral radius)] >>= retainedObject . castPtr

-- | levelOfDetailWithGeometry:worldSpaceDistance:
--
-- This is a convenience method to create a level of detail with a distance threshold mode.
--
-- @geometry@ — The geometry for this level of detail. nil is supported and indicates that no geometry should be rendered for this level of detail.
--
-- @distance@ — The minimum distance to the current point of view that this level of detail is suitable for.
--
-- ObjC selector: @+ levelOfDetailWithGeometry:worldSpaceDistance:@
levelOfDetailWithGeometry_worldSpaceDistance :: IsSCNGeometry geometry => geometry -> CDouble -> IO (Id SCNLevelOfDetail)
levelOfDetailWithGeometry_worldSpaceDistance geometry distance =
  do
    cls' <- getRequiredClass "SCNLevelOfDetail"
    withObjCPtr geometry $ \raw_geometry ->
      sendClassMsg cls' (mkSelector "levelOfDetailWithGeometry:worldSpaceDistance:") (retPtr retVoid) [argPtr (castPtr raw_geometry :: Ptr ()), argCDouble (fromIntegral distance)] >>= retainedObject . castPtr

-- | geometry
--
-- Returns the geometry of the receiver.
--
-- ObjC selector: @- geometry@
geometry :: IsSCNLevelOfDetail scnLevelOfDetail => scnLevelOfDetail -> IO (Id SCNGeometry)
geometry scnLevelOfDetail  =
  sendMsg scnLevelOfDetail (mkSelector "geometry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | screenSpaceRadius
--
-- Returns the screen space radius of the receiver if any, 0 otherwise.
--
-- ObjC selector: @- screenSpaceRadius@
screenSpaceRadius :: IsSCNLevelOfDetail scnLevelOfDetail => scnLevelOfDetail -> IO CDouble
screenSpaceRadius scnLevelOfDetail  =
  sendMsg scnLevelOfDetail (mkSelector "screenSpaceRadius") retCDouble []

-- | worldSpaceDistance
--
-- Returns the world space distance of the receiver if any, 0 otherwise.
--
-- ObjC selector: @- worldSpaceDistance@
worldSpaceDistance :: IsSCNLevelOfDetail scnLevelOfDetail => scnLevelOfDetail -> IO CDouble
worldSpaceDistance scnLevelOfDetail  =
  sendMsg scnLevelOfDetail (mkSelector "worldSpaceDistance") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @levelOfDetailWithGeometry:screenSpaceRadius:@
levelOfDetailWithGeometry_screenSpaceRadiusSelector :: Selector
levelOfDetailWithGeometry_screenSpaceRadiusSelector = mkSelector "levelOfDetailWithGeometry:screenSpaceRadius:"

-- | @Selector@ for @levelOfDetailWithGeometry:worldSpaceDistance:@
levelOfDetailWithGeometry_worldSpaceDistanceSelector :: Selector
levelOfDetailWithGeometry_worldSpaceDistanceSelector = mkSelector "levelOfDetailWithGeometry:worldSpaceDistance:"

-- | @Selector@ for @geometry@
geometrySelector :: Selector
geometrySelector = mkSelector "geometry"

-- | @Selector@ for @screenSpaceRadius@
screenSpaceRadiusSelector :: Selector
screenSpaceRadiusSelector = mkSelector "screenSpaceRadius"

-- | @Selector@ for @worldSpaceDistance@
worldSpaceDistanceSelector :: Selector
worldSpaceDistanceSelector = mkSelector "worldSpaceDistance"

