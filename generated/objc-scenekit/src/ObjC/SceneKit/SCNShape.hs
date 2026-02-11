{-# LANGUAGE PatternSynonyms #-}
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
  , shapeWithPath_extrusionDepthSelector
  , pathSelector
  , setPathSelector
  , extrusionDepthSelector
  , setExtrusionDepthSelector
  , chamferModeSelector
  , setChamferModeSelector
  , chamferRadiusSelector
  , setChamferRadiusSelector
  , chamferProfileSelector
  , setChamferProfileSelector

  -- * Enum types
  , SCNChamferMode(SCNChamferMode)
  , pattern SCNChamferModeBoth
  , pattern SCNChamferModeFront
  , pattern SCNChamferModeBack

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
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "shapeWithPath:extrusionDepth:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCDouble (fromIntegral extrusionDepth)] >>= retainedObject . castPtr

-- | path
--
-- The path defining the shape to be rendered.
--
-- The path defines the outline of the shape. The path is filled using the even-odd rule. If the path is self-intersecting, the behavior is undefined.
--
-- ObjC selector: @- path@
path :: IsSCNShape scnShape => scnShape -> IO (Id NSBezierPath)
path scnShape  =
  sendMsg scnShape (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | path
--
-- The path defining the shape to be rendered.
--
-- The path defines the outline of the shape. The path is filled using the even-odd rule. If the path is self-intersecting, the behavior is undefined.
--
-- ObjC selector: @- setPath:@
setPath :: (IsSCNShape scnShape, IsNSBezierPath value) => scnShape -> value -> IO ()
setPath scnShape  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnShape (mkSelector "setPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the shape.
--
-- ObjC selector: @- extrusionDepth@
extrusionDepth :: IsSCNShape scnShape => scnShape -> IO CDouble
extrusionDepth scnShape  =
  sendMsg scnShape (mkSelector "extrusionDepth") retCDouble []

-- | extrusionDepth
--
-- The extrusion depth. Animatable.
--
-- If the value is 0, we get a mono-sided, 2D version of the shape.
--
-- ObjC selector: @- setExtrusionDepth:@
setExtrusionDepth :: IsSCNShape scnShape => scnShape -> CDouble -> IO ()
setExtrusionDepth scnShape  value =
  sendMsg scnShape (mkSelector "setExtrusionDepth:") retVoid [argCDouble (fromIntegral value)]

-- | chamferMode
--
-- The sides of the text that are chamfered.
--
-- The default value is SCNChamferModeBoth.
--
-- ObjC selector: @- chamferMode@
chamferMode :: IsSCNShape scnShape => scnShape -> IO SCNChamferMode
chamferMode scnShape  =
  fmap (coerce :: CLong -> SCNChamferMode) $ sendMsg scnShape (mkSelector "chamferMode") retCLong []

-- | chamferMode
--
-- The sides of the text that are chamfered.
--
-- The default value is SCNChamferModeBoth.
--
-- ObjC selector: @- setChamferMode:@
setChamferMode :: IsSCNShape scnShape => scnShape -> SCNChamferMode -> IO ()
setChamferMode scnShape  value =
  sendMsg scnShape (mkSelector "setChamferMode:") retVoid [argCLong (coerce value)]

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The default value is 0.
--
-- ObjC selector: @- chamferRadius@
chamferRadius :: IsSCNShape scnShape => scnShape -> IO CDouble
chamferRadius scnShape  =
  sendMsg scnShape (mkSelector "chamferRadius") retCDouble []

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- Values are clamped to the range [0, extrusionDepth / 2]. The default value is 0.
--
-- ObjC selector: @- setChamferRadius:@
setChamferRadius :: IsSCNShape scnShape => scnShape -> CDouble -> IO ()
setChamferRadius scnShape  value =
  sendMsg scnShape (mkSelector "setChamferRadius:") retVoid [argCDouble (fromIntegral value)]

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- chamferProfile@
chamferProfile :: IsSCNShape scnShape => scnShape -> IO (Id NSBezierPath)
chamferProfile scnShape  =
  sendMsg scnShape (mkSelector "chamferProfile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | chamferProfile
--
-- Describes the profile used to when "chamferRadius" is not nil. When "chamferProfile" is nil we fallback on a path representing a quadrant.
--
-- The profile should be a 2D curve beginning at (0,1) and ending at (1,0). The "flatness" property is also used to flatten this path. The default value is nil.
--
-- ObjC selector: @- setChamferProfile:@
setChamferProfile :: (IsSCNShape scnShape, IsNSBezierPath value) => scnShape -> value -> IO ()
setChamferProfile scnShape  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnShape (mkSelector "setChamferProfile:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shapeWithPath:extrusionDepth:@
shapeWithPath_extrusionDepthSelector :: Selector
shapeWithPath_extrusionDepthSelector = mkSelector "shapeWithPath:extrusionDepth:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @extrusionDepth@
extrusionDepthSelector :: Selector
extrusionDepthSelector = mkSelector "extrusionDepth"

-- | @Selector@ for @setExtrusionDepth:@
setExtrusionDepthSelector :: Selector
setExtrusionDepthSelector = mkSelector "setExtrusionDepth:"

-- | @Selector@ for @chamferMode@
chamferModeSelector :: Selector
chamferModeSelector = mkSelector "chamferMode"

-- | @Selector@ for @setChamferMode:@
setChamferModeSelector :: Selector
setChamferModeSelector = mkSelector "setChamferMode:"

-- | @Selector@ for @chamferRadius@
chamferRadiusSelector :: Selector
chamferRadiusSelector = mkSelector "chamferRadius"

-- | @Selector@ for @setChamferRadius:@
setChamferRadiusSelector :: Selector
setChamferRadiusSelector = mkSelector "setChamferRadius:"

-- | @Selector@ for @chamferProfile@
chamferProfileSelector :: Selector
chamferProfileSelector = mkSelector "chamferProfile"

-- | @Selector@ for @setChamferProfile:@
setChamferProfileSelector :: Selector
setChamferProfileSelector = mkSelector "setChamferProfile:"

