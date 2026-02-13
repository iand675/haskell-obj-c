{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNShape
--
-- SCNShape represents a 2D shape (cubic Bezier spline) than can be extruded.
--
-- Generated bindings for @SCNShape@.
module ObjC.SceneKit.SCNShape
  ( SCNShape
  , IsSCNShape(..)
  , shapeWithPath_extrusionDepth
  , path
  , setPath
  , extrusionDepth
  , setExtrusionDepth
  , chamferMode
  , setChamferMode
  , chamferRadius
  , setChamferRadius
  , chamferProfile
  , setChamferProfile
  , chamferModeSelector
  , chamferProfileSelector
  , chamferRadiusSelector
  , extrusionDepthSelector
  , pathSelector
  , setChamferModeSelector
  , setChamferProfileSelector
  , setChamferRadiusSelector
  , setExtrusionDepthSelector
  , setPathSelector
  , shapeWithPath_extrusionDepthSelector

  -- * Enum types
  , SCNChamferMode(SCNChamferMode)
  , pattern SCNChamferModeBoth
  , pattern SCNChamferModeFront
  , pattern SCNChamferModeBack

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | shapeWithPath:extrusionDepth:
--
-- Creates and returns a 3D representation of the given shape with the given extrusion depth.
--
-- @path@ — The cubic Bezier spline to extrude.
--
-- @extrusionDepth@ — The extrusion depth.
--
-- ObjC selector: @+ shapeWithPath:extrusionDepth:@
shapeWithPath_extrusionDepth :: IsNSBezierPath path => path -> CDouble -> IO (Id SCNShape)
shapeWithPath_extrusionDepth path extrusionDepth =
  do
    cls' <- getRequiredClass "SCNShape"
    sendClassMessage cls' shapeWithPath_extrusionDepthSelector (toNSBezierPath path) extrusionDepth

-- | path
--
-- The path defining the shape to be rendered.
--
-- The path defines the outline of the shape. The path is filled using the even-odd rule. If the path is self-intersecting, the behavior is undefined.
--
-- ObjC selector: @- path@
path :: IsSCNShape scnShape => scnShape -> IO (Id NSBezierPath)
path scnShape =
  sendMessage scnShape pathSelector

-- | path
--
-- The path defining the shape to be rendered.
--
-- The path defines the outline of the shape. The path is filled using the even-odd rule. If the path is self-intersecting, the behavior is undefined.
--
-- ObjC selector: @- setPath:@
setPath :: (IsSCNShape scnShape, IsNSBezierPath value) => scnShape -> value -> IO ()
setPath scnShape value =
  sendMessage scnShape setPathSelector (toNSBezierPath value)

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the shape.
--
-- ObjC selector: @- extrusionDepth@
extrusionDepth :: IsSCNShape scnShape => scnShape -> IO CDouble
extrusionDepth scnShape =
  sendMessage scnShape extrusionDepthSelector

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the shape.
--
-- ObjC selector: @- setExtrusionDepth:@
setExtrusionDepth :: IsSCNShape scnShape => scnShape -> CDouble -> IO ()
setExtrusionDepth scnShape value =
  sendMessage scnShape setExtrusionDepthSelector value

-- | chamferMode
--
-- The sides of the text that are chamfered.
--
-- The default value is SCNChamferModeBoth.
--
-- ObjC selector: @- chamferMode@
chamferMode :: IsSCNShape scnShape => scnShape -> IO SCNChamferMode
chamferMode scnShape =
  sendMessage scnShape chamferModeSelector

-- | chamferMode
--
-- The sides of the text that are chamfered.
--
-- The default value is SCNChamferModeBoth.
--
-- ObjC selector: @- setChamferMode:@
setChamferMode :: IsSCNShape scnShape => scnShape -> SCNChamferMode -> IO ()
setChamferMode scnShape value =
  sendMessage scnShape setChamferModeSelector value

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The default value is 0.
--
-- ObjC selector: @- chamferRadius@
chamferRadius :: IsSCNShape scnShape => scnShape -> IO CDouble
chamferRadius scnShape =
  sendMessage scnShape chamferRadiusSelector

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The default value is 0.
--
-- ObjC selector: @- setChamferRadius:@
setChamferRadius :: IsSCNShape scnShape => scnShape -> CDouble -> IO ()
setChamferRadius scnShape value =
  sendMessage scnShape setChamferRadiusSelector value

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- chamferProfile@
chamferProfile :: IsSCNShape scnShape => scnShape -> IO (Id NSBezierPath)
chamferProfile scnShape =
  sendMessage scnShape chamferProfileSelector

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- setChamferProfile:@
setChamferProfile :: (IsSCNShape scnShape, IsNSBezierPath value) => scnShape -> value -> IO ()
setChamferProfile scnShape value =
  sendMessage scnShape setChamferProfileSelector (toNSBezierPath value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shapeWithPath:extrusionDepth:@
shapeWithPath_extrusionDepthSelector :: Selector '[Id NSBezierPath, CDouble] (Id SCNShape)
shapeWithPath_extrusionDepthSelector = mkSelector "shapeWithPath:extrusionDepth:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSBezierPath)
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id NSBezierPath] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @extrusionDepth@
extrusionDepthSelector :: Selector '[] CDouble
extrusionDepthSelector = mkSelector "extrusionDepth"

-- | @Selector@ for @setExtrusionDepth:@
setExtrusionDepthSelector :: Selector '[CDouble] ()
setExtrusionDepthSelector = mkSelector "setExtrusionDepth:"

-- | @Selector@ for @chamferMode@
chamferModeSelector :: Selector '[] SCNChamferMode
chamferModeSelector = mkSelector "chamferMode"

-- | @Selector@ for @setChamferMode:@
setChamferModeSelector :: Selector '[SCNChamferMode] ()
setChamferModeSelector = mkSelector "setChamferMode:"

-- | @Selector@ for @chamferRadius@
chamferRadiusSelector :: Selector '[] CDouble
chamferRadiusSelector = mkSelector "chamferRadius"

-- | @Selector@ for @setChamferRadius:@
setChamferRadiusSelector :: Selector '[CDouble] ()
setChamferRadiusSelector = mkSelector "setChamferRadius:"

-- | @Selector@ for @chamferProfile@
chamferProfileSelector :: Selector '[] (Id NSBezierPath)
chamferProfileSelector = mkSelector "chamferProfile"

-- | @Selector@ for @setChamferProfile:@
setChamferProfileSelector :: Selector '[Id NSBezierPath] ()
setChamferProfileSelector = mkSelector "setChamferProfile:"

