{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGradient@.
module ObjC.AppKit.NSGradient
  ( NSGradient
  , IsNSGradient(..)
  , initWithStartingColor_endingColor
  , initWithColors
  , initWithColorsAndLocations
  , initWithColors_atLocations_colorSpace
  , initWithCoder
  , drawFromPoint_toPoint_options
  , drawInRect_angle
  , drawInBezierPath_angle
  , drawFromCenter_radius_toCenter_radius_options
  , drawInRect_relativeCenterPosition
  , drawInBezierPath_relativeCenterPosition
  , getColor_location_atIndex
  , interpolatedColorAtLocation
  , colorSpace
  , numberOfColorStops
  , initWithStartingColor_endingColorSelector
  , initWithColorsSelector
  , initWithColorsAndLocationsSelector
  , initWithColors_atLocations_colorSpaceSelector
  , initWithCoderSelector
  , drawFromPoint_toPoint_optionsSelector
  , drawInRect_angleSelector
  , drawInBezierPath_angleSelector
  , drawFromCenter_radius_toCenter_radius_optionsSelector
  , drawInRect_relativeCenterPositionSelector
  , drawInBezierPath_relativeCenterPositionSelector
  , getColor_location_atIndexSelector
  , interpolatedColorAtLocationSelector
  , colorSpaceSelector
  , numberOfColorStopsSelector

  -- * Enum types
  , NSGradientDrawingOptions(NSGradientDrawingOptions)
  , pattern NSGradientDrawsBeforeStartingLocation
  , pattern NSGradientDrawsAfterEndingLocation

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithStartingColor:endingColor:@
initWithStartingColor_endingColor :: (IsNSGradient nsGradient, IsNSColor startingColor, IsNSColor endingColor) => nsGradient -> startingColor -> endingColor -> IO (Id NSGradient)
initWithStartingColor_endingColor nsGradient  startingColor endingColor =
withObjCPtr startingColor $ \raw_startingColor ->
  withObjCPtr endingColor $ \raw_endingColor ->
      sendMsg nsGradient (mkSelector "initWithStartingColor:endingColor:") (retPtr retVoid) [argPtr (castPtr raw_startingColor :: Ptr ()), argPtr (castPtr raw_endingColor :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithColors:@
initWithColors :: (IsNSGradient nsGradient, IsNSArray colorArray) => nsGradient -> colorArray -> IO (Id NSGradient)
initWithColors nsGradient  colorArray =
withObjCPtr colorArray $ \raw_colorArray ->
    sendMsg nsGradient (mkSelector "initWithColors:") (retPtr retVoid) [argPtr (castPtr raw_colorArray :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithColorsAndLocations:@
initWithColorsAndLocations :: (IsNSGradient nsGradient, IsNSColor firstColor) => nsGradient -> firstColor -> IO (Id NSGradient)
initWithColorsAndLocations nsGradient  firstColor =
withObjCPtr firstColor $ \raw_firstColor ->
    sendMsg nsGradient (mkSelector "initWithColorsAndLocations:") (retPtr retVoid) [argPtr (castPtr raw_firstColor :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithColors:atLocations:colorSpace:@
initWithColors_atLocations_colorSpace :: (IsNSGradient nsGradient, IsNSArray colorArray, IsNSColorSpace colorSpace) => nsGradient -> colorArray -> Const (Ptr CDouble) -> colorSpace -> IO (Id NSGradient)
initWithColors_atLocations_colorSpace nsGradient  colorArray locations colorSpace =
withObjCPtr colorArray $ \raw_colorArray ->
  withObjCPtr colorSpace $ \raw_colorSpace ->
      sendMsg nsGradient (mkSelector "initWithColors:atLocations:colorSpace:") (retPtr retVoid) [argPtr (castPtr raw_colorArray :: Ptr ()), argPtr (unConst locations), argPtr (castPtr raw_colorSpace :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSGradient nsGradient, IsNSCoder coder) => nsGradient -> coder -> IO (Id NSGradient)
initWithCoder nsGradient  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsGradient (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- drawFromPoint:toPoint:options:@
drawFromPoint_toPoint_options :: IsNSGradient nsGradient => nsGradient -> NSPoint -> NSPoint -> NSGradientDrawingOptions -> IO ()
drawFromPoint_toPoint_options nsGradient  startingPoint endingPoint options =
  sendMsg nsGradient (mkSelector "drawFromPoint:toPoint:options:") retVoid [argNSPoint startingPoint, argNSPoint endingPoint, argCULong (coerce options)]

-- | @- drawInRect:angle:@
drawInRect_angle :: IsNSGradient nsGradient => nsGradient -> NSRect -> CDouble -> IO ()
drawInRect_angle nsGradient  rect angle =
  sendMsg nsGradient (mkSelector "drawInRect:angle:") retVoid [argNSRect rect, argCDouble (fromIntegral angle)]

-- | @- drawInBezierPath:angle:@
drawInBezierPath_angle :: (IsNSGradient nsGradient, IsNSBezierPath path) => nsGradient -> path -> CDouble -> IO ()
drawInBezierPath_angle nsGradient  path angle =
withObjCPtr path $ \raw_path ->
    sendMsg nsGradient (mkSelector "drawInBezierPath:angle:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argCDouble (fromIntegral angle)]

-- | @- drawFromCenter:radius:toCenter:radius:options:@
drawFromCenter_radius_toCenter_radius_options :: IsNSGradient nsGradient => nsGradient -> NSPoint -> CDouble -> NSPoint -> CDouble -> NSGradientDrawingOptions -> IO ()
drawFromCenter_radius_toCenter_radius_options nsGradient  startCenter startRadius endCenter endRadius options =
  sendMsg nsGradient (mkSelector "drawFromCenter:radius:toCenter:radius:options:") retVoid [argNSPoint startCenter, argCDouble (fromIntegral startRadius), argNSPoint endCenter, argCDouble (fromIntegral endRadius), argCULong (coerce options)]

-- | @- drawInRect:relativeCenterPosition:@
drawInRect_relativeCenterPosition :: IsNSGradient nsGradient => nsGradient -> NSRect -> NSPoint -> IO ()
drawInRect_relativeCenterPosition nsGradient  rect relativeCenterPosition =
  sendMsg nsGradient (mkSelector "drawInRect:relativeCenterPosition:") retVoid [argNSRect rect, argNSPoint relativeCenterPosition]

-- | @- drawInBezierPath:relativeCenterPosition:@
drawInBezierPath_relativeCenterPosition :: (IsNSGradient nsGradient, IsNSBezierPath path) => nsGradient -> path -> NSPoint -> IO ()
drawInBezierPath_relativeCenterPosition nsGradient  path relativeCenterPosition =
withObjCPtr path $ \raw_path ->
    sendMsg nsGradient (mkSelector "drawInBezierPath:relativeCenterPosition:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argNSPoint relativeCenterPosition]

-- | @- getColor:location:atIndex:@
getColor_location_atIndex :: (IsNSGradient nsGradient, IsNSColor color) => nsGradient -> color -> Ptr CDouble -> CLong -> IO ()
getColor_location_atIndex nsGradient  color location index =
withObjCPtr color $ \raw_color ->
    sendMsg nsGradient (mkSelector "getColor:location:atIndex:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argPtr location, argCLong (fromIntegral index)]

-- | @- interpolatedColorAtLocation:@
interpolatedColorAtLocation :: IsNSGradient nsGradient => nsGradient -> CDouble -> IO (Id NSColor)
interpolatedColorAtLocation nsGradient  location =
  sendMsg nsGradient (mkSelector "interpolatedColorAtLocation:") (retPtr retVoid) [argCDouble (fromIntegral location)] >>= retainedObject . castPtr

-- | @- colorSpace@
colorSpace :: IsNSGradient nsGradient => nsGradient -> IO (Id NSColorSpace)
colorSpace nsGradient  =
  sendMsg nsGradient (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfColorStops@
numberOfColorStops :: IsNSGradient nsGradient => nsGradient -> IO CLong
numberOfColorStops nsGradient  =
  sendMsg nsGradient (mkSelector "numberOfColorStops") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStartingColor:endingColor:@
initWithStartingColor_endingColorSelector :: Selector
initWithStartingColor_endingColorSelector = mkSelector "initWithStartingColor:endingColor:"

-- | @Selector@ for @initWithColors:@
initWithColorsSelector :: Selector
initWithColorsSelector = mkSelector "initWithColors:"

-- | @Selector@ for @initWithColorsAndLocations:@
initWithColorsAndLocationsSelector :: Selector
initWithColorsAndLocationsSelector = mkSelector "initWithColorsAndLocations:"

-- | @Selector@ for @initWithColors:atLocations:colorSpace:@
initWithColors_atLocations_colorSpaceSelector :: Selector
initWithColors_atLocations_colorSpaceSelector = mkSelector "initWithColors:atLocations:colorSpace:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @drawFromPoint:toPoint:options:@
drawFromPoint_toPoint_optionsSelector :: Selector
drawFromPoint_toPoint_optionsSelector = mkSelector "drawFromPoint:toPoint:options:"

-- | @Selector@ for @drawInRect:angle:@
drawInRect_angleSelector :: Selector
drawInRect_angleSelector = mkSelector "drawInRect:angle:"

-- | @Selector@ for @drawInBezierPath:angle:@
drawInBezierPath_angleSelector :: Selector
drawInBezierPath_angleSelector = mkSelector "drawInBezierPath:angle:"

-- | @Selector@ for @drawFromCenter:radius:toCenter:radius:options:@
drawFromCenter_radius_toCenter_radius_optionsSelector :: Selector
drawFromCenter_radius_toCenter_radius_optionsSelector = mkSelector "drawFromCenter:radius:toCenter:radius:options:"

-- | @Selector@ for @drawInRect:relativeCenterPosition:@
drawInRect_relativeCenterPositionSelector :: Selector
drawInRect_relativeCenterPositionSelector = mkSelector "drawInRect:relativeCenterPosition:"

-- | @Selector@ for @drawInBezierPath:relativeCenterPosition:@
drawInBezierPath_relativeCenterPositionSelector :: Selector
drawInBezierPath_relativeCenterPositionSelector = mkSelector "drawInBezierPath:relativeCenterPosition:"

-- | @Selector@ for @getColor:location:atIndex:@
getColor_location_atIndexSelector :: Selector
getColor_location_atIndexSelector = mkSelector "getColor:location:atIndex:"

-- | @Selector@ for @interpolatedColorAtLocation:@
interpolatedColorAtLocationSelector :: Selector
interpolatedColorAtLocationSelector = mkSelector "interpolatedColorAtLocation:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @numberOfColorStops@
numberOfColorStopsSelector :: Selector
numberOfColorStopsSelector = mkSelector "numberOfColorStops"

