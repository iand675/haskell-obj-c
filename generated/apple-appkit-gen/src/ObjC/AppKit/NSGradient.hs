{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , colorSpaceSelector
  , drawFromCenter_radius_toCenter_radius_optionsSelector
  , drawFromPoint_toPoint_optionsSelector
  , drawInBezierPath_angleSelector
  , drawInBezierPath_relativeCenterPositionSelector
  , drawInRect_angleSelector
  , drawInRect_relativeCenterPositionSelector
  , getColor_location_atIndexSelector
  , initWithCoderSelector
  , initWithColorsAndLocationsSelector
  , initWithColorsSelector
  , initWithColors_atLocations_colorSpaceSelector
  , initWithStartingColor_endingColorSelector
  , interpolatedColorAtLocationSelector
  , numberOfColorStopsSelector

  -- * Enum types
  , NSGradientDrawingOptions(NSGradientDrawingOptions)
  , pattern NSGradientDrawsBeforeStartingLocation
  , pattern NSGradientDrawsAfterEndingLocation

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithStartingColor:endingColor:@
initWithStartingColor_endingColor :: (IsNSGradient nsGradient, IsNSColor startingColor, IsNSColor endingColor) => nsGradient -> startingColor -> endingColor -> IO (Id NSGradient)
initWithStartingColor_endingColor nsGradient startingColor endingColor =
  sendOwnedMessage nsGradient initWithStartingColor_endingColorSelector (toNSColor startingColor) (toNSColor endingColor)

-- | @- initWithColors:@
initWithColors :: (IsNSGradient nsGradient, IsNSArray colorArray) => nsGradient -> colorArray -> IO (Id NSGradient)
initWithColors nsGradient colorArray =
  sendOwnedMessage nsGradient initWithColorsSelector (toNSArray colorArray)

-- | @- initWithColorsAndLocations:@
initWithColorsAndLocations :: (IsNSGradient nsGradient, IsNSColor firstColor) => nsGradient -> firstColor -> IO (Id NSGradient)
initWithColorsAndLocations nsGradient firstColor =
  sendOwnedMessage nsGradient initWithColorsAndLocationsSelector (toNSColor firstColor)

-- | @- initWithColors:atLocations:colorSpace:@
initWithColors_atLocations_colorSpace :: (IsNSGradient nsGradient, IsNSArray colorArray, IsNSColorSpace colorSpace) => nsGradient -> colorArray -> Const (Ptr CDouble) -> colorSpace -> IO (Id NSGradient)
initWithColors_atLocations_colorSpace nsGradient colorArray locations colorSpace =
  sendOwnedMessage nsGradient initWithColors_atLocations_colorSpaceSelector (toNSArray colorArray) locations (toNSColorSpace colorSpace)

-- | @- initWithCoder:@
initWithCoder :: (IsNSGradient nsGradient, IsNSCoder coder) => nsGradient -> coder -> IO (Id NSGradient)
initWithCoder nsGradient coder =
  sendOwnedMessage nsGradient initWithCoderSelector (toNSCoder coder)

-- | @- drawFromPoint:toPoint:options:@
drawFromPoint_toPoint_options :: IsNSGradient nsGradient => nsGradient -> NSPoint -> NSPoint -> NSGradientDrawingOptions -> IO ()
drawFromPoint_toPoint_options nsGradient startingPoint endingPoint options =
  sendMessage nsGradient drawFromPoint_toPoint_optionsSelector startingPoint endingPoint options

-- | @- drawInRect:angle:@
drawInRect_angle :: IsNSGradient nsGradient => nsGradient -> NSRect -> CDouble -> IO ()
drawInRect_angle nsGradient rect angle =
  sendMessage nsGradient drawInRect_angleSelector rect angle

-- | @- drawInBezierPath:angle:@
drawInBezierPath_angle :: (IsNSGradient nsGradient, IsNSBezierPath path) => nsGradient -> path -> CDouble -> IO ()
drawInBezierPath_angle nsGradient path angle =
  sendMessage nsGradient drawInBezierPath_angleSelector (toNSBezierPath path) angle

-- | @- drawFromCenter:radius:toCenter:radius:options:@
drawFromCenter_radius_toCenter_radius_options :: IsNSGradient nsGradient => nsGradient -> NSPoint -> CDouble -> NSPoint -> CDouble -> NSGradientDrawingOptions -> IO ()
drawFromCenter_radius_toCenter_radius_options nsGradient startCenter startRadius endCenter endRadius options =
  sendMessage nsGradient drawFromCenter_radius_toCenter_radius_optionsSelector startCenter startRadius endCenter endRadius options

-- | @- drawInRect:relativeCenterPosition:@
drawInRect_relativeCenterPosition :: IsNSGradient nsGradient => nsGradient -> NSRect -> NSPoint -> IO ()
drawInRect_relativeCenterPosition nsGradient rect relativeCenterPosition =
  sendMessage nsGradient drawInRect_relativeCenterPositionSelector rect relativeCenterPosition

-- | @- drawInBezierPath:relativeCenterPosition:@
drawInBezierPath_relativeCenterPosition :: (IsNSGradient nsGradient, IsNSBezierPath path) => nsGradient -> path -> NSPoint -> IO ()
drawInBezierPath_relativeCenterPosition nsGradient path relativeCenterPosition =
  sendMessage nsGradient drawInBezierPath_relativeCenterPositionSelector (toNSBezierPath path) relativeCenterPosition

-- | @- getColor:location:atIndex:@
getColor_location_atIndex :: (IsNSGradient nsGradient, IsNSColor color) => nsGradient -> color -> Ptr CDouble -> CLong -> IO ()
getColor_location_atIndex nsGradient color location index =
  sendMessage nsGradient getColor_location_atIndexSelector (toNSColor color) location index

-- | @- interpolatedColorAtLocation:@
interpolatedColorAtLocation :: IsNSGradient nsGradient => nsGradient -> CDouble -> IO (Id NSColor)
interpolatedColorAtLocation nsGradient location =
  sendMessage nsGradient interpolatedColorAtLocationSelector location

-- | @- colorSpace@
colorSpace :: IsNSGradient nsGradient => nsGradient -> IO (Id NSColorSpace)
colorSpace nsGradient =
  sendMessage nsGradient colorSpaceSelector

-- | @- numberOfColorStops@
numberOfColorStops :: IsNSGradient nsGradient => nsGradient -> IO CLong
numberOfColorStops nsGradient =
  sendMessage nsGradient numberOfColorStopsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStartingColor:endingColor:@
initWithStartingColor_endingColorSelector :: Selector '[Id NSColor, Id NSColor] (Id NSGradient)
initWithStartingColor_endingColorSelector = mkSelector "initWithStartingColor:endingColor:"

-- | @Selector@ for @initWithColors:@
initWithColorsSelector :: Selector '[Id NSArray] (Id NSGradient)
initWithColorsSelector = mkSelector "initWithColors:"

-- | @Selector@ for @initWithColorsAndLocations:@
initWithColorsAndLocationsSelector :: Selector '[Id NSColor] (Id NSGradient)
initWithColorsAndLocationsSelector = mkSelector "initWithColorsAndLocations:"

-- | @Selector@ for @initWithColors:atLocations:colorSpace:@
initWithColors_atLocations_colorSpaceSelector :: Selector '[Id NSArray, Const (Ptr CDouble), Id NSColorSpace] (Id NSGradient)
initWithColors_atLocations_colorSpaceSelector = mkSelector "initWithColors:atLocations:colorSpace:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSGradient)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @drawFromPoint:toPoint:options:@
drawFromPoint_toPoint_optionsSelector :: Selector '[NSPoint, NSPoint, NSGradientDrawingOptions] ()
drawFromPoint_toPoint_optionsSelector = mkSelector "drawFromPoint:toPoint:options:"

-- | @Selector@ for @drawInRect:angle:@
drawInRect_angleSelector :: Selector '[NSRect, CDouble] ()
drawInRect_angleSelector = mkSelector "drawInRect:angle:"

-- | @Selector@ for @drawInBezierPath:angle:@
drawInBezierPath_angleSelector :: Selector '[Id NSBezierPath, CDouble] ()
drawInBezierPath_angleSelector = mkSelector "drawInBezierPath:angle:"

-- | @Selector@ for @drawFromCenter:radius:toCenter:radius:options:@
drawFromCenter_radius_toCenter_radius_optionsSelector :: Selector '[NSPoint, CDouble, NSPoint, CDouble, NSGradientDrawingOptions] ()
drawFromCenter_radius_toCenter_radius_optionsSelector = mkSelector "drawFromCenter:radius:toCenter:radius:options:"

-- | @Selector@ for @drawInRect:relativeCenterPosition:@
drawInRect_relativeCenterPositionSelector :: Selector '[NSRect, NSPoint] ()
drawInRect_relativeCenterPositionSelector = mkSelector "drawInRect:relativeCenterPosition:"

-- | @Selector@ for @drawInBezierPath:relativeCenterPosition:@
drawInBezierPath_relativeCenterPositionSelector :: Selector '[Id NSBezierPath, NSPoint] ()
drawInBezierPath_relativeCenterPositionSelector = mkSelector "drawInBezierPath:relativeCenterPosition:"

-- | @Selector@ for @getColor:location:atIndex:@
getColor_location_atIndexSelector :: Selector '[Id NSColor, Ptr CDouble, CLong] ()
getColor_location_atIndexSelector = mkSelector "getColor:location:atIndex:"

-- | @Selector@ for @interpolatedColorAtLocation:@
interpolatedColorAtLocationSelector :: Selector '[CDouble] (Id NSColor)
interpolatedColorAtLocationSelector = mkSelector "interpolatedColorAtLocation:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSColorSpace)
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @numberOfColorStops@
numberOfColorStopsSelector :: Selector '[] CLong
numberOfColorStopsSelector = mkSelector "numberOfColorStops"

