{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The Core Image class that defines a color object.
--
-- Use @CIColor@ instances in conjunction with other Core Image classes, such as ``CIFilter-class`` and ``CIKernel``. Many of the built-in Core Image filters have one or more @CIColor@ inputs that you can set to affect the filter's behavior.
--
-- ### Color Model
--
-- A color is defined as a N-dimensional model where each dimension's color component is represented by intensity values. A color component may also be referred to as a color channel. An RGB color model, for example,  is three-dimensional and the red, green, and blue component intensities define each unique color.
--
-- ### Color Space
--
-- A color is also defined by a color space that locates the axes of N-dimensional model within the greater volume of human perceivable colors.  Core Image uses @CGColorSpace@ instances to specify a variety of different color spaces such as sRGB, P3, BT.2020, etc. The @CGColorSpace@ also defines if the color space is coded linearly or in a non-linear perceptual curve. (For more information on @CGColorSpace@ see <doc://com.apple.documentation/documentation/coregraphics/cgcolorspace>)
--
-- ### Color Range
--
-- Standard dynamic range (SDR) color color component values range from @0.0@ to @1.0@, with @0.0@  representing an 0% of that component and @1.0@ representing 100%. In contrast, high dynamic range (HDR) color values  can be less than @0.0@ (for more saturation) or greater than @1.0@ (for more brightness).
--
-- ### Color Opacity
--
-- @CIColor@ instances also have an alpha component, which represents the opacity of the color, with 0.0 meaning completely  transparent and 1.0 meaning completely opaque. If a color does not have an explicit alpha component, Core Image  assumes that the alpha component equals 1.0. With @CIColor@ that color components values are not premultiplied.  So for example, a semi-transparent pure red @CIColor@ is represented by RGB @1.0,0.0,0.0@ and A @0.5@.  In contrast  color components values in ``CIImage`` buffers or read in ``CIKernel`` samplers are premultiplied by default.
--
-- Generated bindings for @CIColor@.
module ObjC.AppKit.CIColor
  ( CIColor
  , IsCIColor(..)
  , initWithColor
  , initWithColorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes

-- | @- initWithColor:@
initWithColor :: (IsCIColor ciColor, IsNSColor color) => ciColor -> color -> IO (Id CIColor)
initWithColor ciColor color =
  sendOwnedMessage ciColor initWithColorSelector (toNSColor color)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithColor:@
initWithColorSelector :: Selector '[Id NSColor] (Id CIColor)
initWithColorSelector = mkSelector "initWithColor:"

