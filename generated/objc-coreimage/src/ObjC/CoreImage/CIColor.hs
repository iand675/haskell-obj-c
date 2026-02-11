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
module ObjC.CoreImage.CIColor
  ( CIColor
  , IsCIColor(..)
  , colorWithCGColor
  , colorWithRed_green_blue_alpha
  , colorWithRed_green_blue
  , colorWithRed_green_blue_alpha_colorSpace
  , colorWithRed_green_blue_colorSpace
  , colorWithString
  , initWithCGColor
  , initWithRed_green_blue_alpha
  , initWithRed_green_blue
  , initWithRed_green_blue_alpha_colorSpace
  , initWithRed_green_blue_colorSpace
  , numberOfComponents
  , alpha
  , colorSpace
  , red
  , green
  , blue
  , stringRepresentation
  , colorWithCGColorSelector
  , colorWithRed_green_blue_alphaSelector
  , colorWithRed_green_blueSelector
  , colorWithRed_green_blue_alpha_colorSpaceSelector
  , colorWithRed_green_blue_colorSpaceSelector
  , colorWithStringSelector
  , initWithCGColorSelector
  , initWithRed_green_blue_alphaSelector
  , initWithRed_green_blueSelector
  , initWithRed_green_blue_alpha_colorSpaceSelector
  , initWithRed_green_blue_colorSpaceSelector
  , numberOfComponentsSelector
  , alphaSelector
  , colorSpaceSelector
  , redSelector
  , greenSelector
  , blueSelector
  , stringRepresentationSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a Core Image color object with a Core Graphics color object. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithCGColor:@
colorWithCGColor :: Ptr () -> IO (Id CIColor)
colorWithCGColor color =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMsg cls' (mkSelector "colorWithCGColor:") (retPtr retVoid) [argPtr color] >>= retainedObject . castPtr

-- | Create a Core Image color object in the sRGB color space  with the specified red, green, blue, and alpha component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB. - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1.   - alpha: The color's alpha (opacity) value between 0 and 1. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIColor)
colorWithRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | Create a Core Image color object in the sRGB color space  with the specified red, green, and blue component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.  - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithRed:green:blue:@
colorWithRed_green_blue :: CDouble -> CDouble -> CDouble -> IO (Id CIColor)
colorWithRed_green_blue red green blue =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue)] >>= retainedObject . castPtr

-- | Create a Core Image color object  with the specified red, green, blue, and alpha component values as measured in the specified color space.
--
-- This will return @null@ if the @CGColorSpace@ is not @kCGColorSpaceModelRGB@.
--
-- The RGB values can be outside the @0...1@ range if the @CGColorSpace@ is unclamped. - Parameters:   - red: The color's unpremultiplied red component value.   - green: The color's unpremultiplied green component value.   - blue: The color's unpremultiplied blue component value.   - alpha: The color's alpha (opacity) value between 0 and 1.   - colorSpace: The color's @CGColorSpace@ which must have @kCGColorSpaceModelRGB@. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:colorSpace:@
colorWithRed_green_blue_alpha_colorSpace :: CDouble -> CDouble -> CDouble -> CDouble -> Ptr () -> IO (Id CIColor)
colorWithRed_green_blue_alpha_colorSpace red green blue alpha colorSpace =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:colorSpace:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha), argPtr colorSpace] >>= retainedObject . castPtr

-- | Create a Core Image color object  with the specified red, green, and blue component values as measured in the specified color space.
--
-- This will return @null@ if the @CGColorSpace@ is not @kCGColorSpaceModelRGB@.
--
-- The RGB values can be outside the @0...1@ range if the @CGColorSpace@ is unclamped. - Parameters:   - red: The color's unpremultiplied red component value.   - green: The color's unpremultiplied green component value.   - blue: The color's unpremultiplied blue component value.   - colorSpace: The color's @CGColorSpace@ which must have @kCGColorSpaceModelRGB@. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithRed:green:blue:colorSpace:@
colorWithRed_green_blue_colorSpace :: CDouble -> CDouble -> CDouble -> Ptr () -> IO (Id CIColor)
colorWithRed_green_blue_colorSpace red green blue colorSpace =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:colorSpace:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argPtr colorSpace] >>= retainedObject . castPtr

-- | Create a Core Image color object in the sRGB color space using a string containing the RGBA color component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.
--
-- - Parameters:   - representation: A string that contains color and alpha float values.    For example, the string: @"0.5 0.7 0.3 1.0"@ indicates an RGB color whose components    are 50% red, 70% green, 30% blue, and 100% opaque.    If the string contains only 3 float values, the alpha component will be @1.0@   If the string contains no float values, then ``/CIColor/clearColor`` will be returned. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithString:@
colorWithString :: IsNSString representation => representation -> IO (Id CIColor)
colorWithString representation =
  do
    cls' <- getRequiredClass "CIColor"
    withObjCPtr representation $ \raw_representation ->
      sendClassMsg cls' (mkSelector "colorWithString:") (retPtr retVoid) [argPtr (castPtr raw_representation :: Ptr ())] >>= retainedObject . castPtr

-- | Create a Core Image color object with a Core Graphics color object. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithCGColor:@
initWithCGColor :: IsCIColor ciColor => ciColor -> Ptr () -> IO (Id CIColor)
initWithCGColor ciColor  color =
  sendMsg ciColor (mkSelector "initWithCGColor:") (retPtr retVoid) [argPtr color] >>= ownedObject . castPtr

-- | Initialize a Core Image color object in the sRGB color space  with the specified red, green, blue, and alpha component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.  - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1.   - alpha: The color's alpha (opacity) value between 0 and 1. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:alpha:@
initWithRed_green_blue_alpha :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIColor)
initWithRed_green_blue_alpha ciColor  red green blue alpha =
  sendMsg ciColor (mkSelector "initWithRed:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= ownedObject . castPtr

-- | Initialize a Core Image color object in the sRGB color space  with the specified red, green, and blue component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.  - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:@
initWithRed_green_blue :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> IO (Id CIColor)
initWithRed_green_blue ciColor  red green blue =
  sendMsg ciColor (mkSelector "initWithRed:green:blue:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue)] >>= ownedObject . castPtr

-- | Initialize a Core Image color object  with the specified red, green, and blue component values as measured in the specified color space.
--
-- This will return null if the @CGColorSpace@ is not @kCGColorSpaceModelRGB@.  The RGB values can be outside the @0...1@ range if the @CGColorSpace@ is unclamped. - Parameters:   - red: The color's unpremultiplied red component value.   - green: The color's unpremultiplied green component value.   - blue: The color's unpremultiplied blue component value.   - alpha: The color's alpha (opacity) value between 0 and 1.   - colorSpace: The color's @CGColorSpace@ which must have @kCGColorSpaceModelRGB@. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:alpha:colorSpace:@
initWithRed_green_blue_alpha_colorSpace :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr () -> IO (Id CIColor)
initWithRed_green_blue_alpha_colorSpace ciColor  red green blue alpha colorSpace =
  sendMsg ciColor (mkSelector "initWithRed:green:blue:alpha:colorSpace:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha), argPtr colorSpace] >>= ownedObject . castPtr

-- | Initialize a Core Image color object  with the specified red, green, and blue component values as measured in the specified color space.
--
-- This will return null if the @CGColorSpace@ is not @kCGColorSpaceModelRGB@.  The RGB values can be outside the @0...1@ range if the @CGColorSpace@ is unclamped. - Parameters:   - red: The color's unpremultiplied red component value.   - green: The color's unpremultiplied green component value.   - blue: The color's unpremultiplied blue component value.   - colorSpace: The color's @CGColorSpace@ which must have @kCGColorSpaceModelRGB@. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:colorSpace:@
initWithRed_green_blue_colorSpace :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> Ptr () -> IO (Id CIColor)
initWithRed_green_blue_colorSpace ciColor  red green blue colorSpace =
  sendMsg ciColor (mkSelector "initWithRed:green:blue:colorSpace:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argPtr colorSpace] >>= ownedObject . castPtr

-- | Returns the color components of the color including alpha.
--
-- This number includes the alpha component if the color contains one.
--
-- Typically this number will be @4@ for red, green, blue, and alpha. If the ``CIColor`` was initialized with a @CGColor@ then the number  will be the same as calling @CGColorGetNumberOfComponents()@
--
-- ObjC selector: @- numberOfComponents@
numberOfComponents :: IsCIColor ciColor => ciColor -> IO CULong
numberOfComponents ciColor  =
  sendMsg ciColor (mkSelector "numberOfComponents") retCULong []

-- | Returns the alpha value of the color.
--
-- ObjC selector: @- alpha@
alpha :: IsCIColor ciColor => ciColor -> IO CDouble
alpha ciColor  =
  sendMsg ciColor (mkSelector "alpha") retCDouble []

-- | Returns the @CGColorSpace@ associated with the color
--
-- ObjC selector: @- colorSpace@
colorSpace :: IsCIColor ciColor => ciColor -> IO (Ptr ())
colorSpace ciColor  =
  fmap castPtr $ sendMsg ciColor (mkSelector "colorSpace") (retPtr retVoid) []

-- | Returns the unpremultiplied red component of the color.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the red component.
--
-- ObjC selector: @- red@
red :: IsCIColor ciColor => ciColor -> IO CDouble
red ciColor  =
  sendMsg ciColor (mkSelector "red") retCDouble []

-- | Returns the unpremultiplied green component of the color.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the green component.
--
-- ObjC selector: @- green@
green :: IsCIColor ciColor => ciColor -> IO CDouble
green ciColor  =
  sendMsg ciColor (mkSelector "green") retCDouble []

-- | Returns the unpremultiplied blue component of the color.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the green component.
--
-- ObjC selector: @- blue@
blue :: IsCIColor ciColor => ciColor -> IO CDouble
blue ciColor  =
  sendMsg ciColor (mkSelector "blue") retCDouble []

-- | Returns a formatted string with the unpremultiplied color and alpha components of the color.
--
-- The string representation always has four components: red, green, blue, and alpha.
--
-- Some example string representations of colors:
--
-- @CIColor@                                       | @stringRepresentation@ ----------------------------------------------- | -------------- @[CIColor colorWithRed:0.2 green:0.4 blue:0.6]@ | @"0.2 0.4 0.6 1.0"@ ``/CIColor/yellowColor``                        | @"1.0 1.0 0.0 1.0"@
--
-- To create a ``CIColor`` instance from a string representation, use the ``colorWithString:`` method.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the red, green, and blue components.
--
-- This property is not KVO-safe because it returns a new @NSString@ instance each time. The value of the @NSString@ will be the same each time it is called.
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsCIColor ciColor => ciColor -> IO (Id NSString)
stringRepresentation ciColor  =
  sendMsg ciColor (mkSelector "stringRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorWithCGColor:@
colorWithCGColorSelector :: Selector
colorWithCGColorSelector = mkSelector "colorWithCGColor:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alphaSelector :: Selector
colorWithRed_green_blue_alphaSelector = mkSelector "colorWithRed:green:blue:alpha:"

-- | @Selector@ for @colorWithRed:green:blue:@
colorWithRed_green_blueSelector :: Selector
colorWithRed_green_blueSelector = mkSelector "colorWithRed:green:blue:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:colorSpace:@
colorWithRed_green_blue_alpha_colorSpaceSelector :: Selector
colorWithRed_green_blue_alpha_colorSpaceSelector = mkSelector "colorWithRed:green:blue:alpha:colorSpace:"

-- | @Selector@ for @colorWithRed:green:blue:colorSpace:@
colorWithRed_green_blue_colorSpaceSelector :: Selector
colorWithRed_green_blue_colorSpaceSelector = mkSelector "colorWithRed:green:blue:colorSpace:"

-- | @Selector@ for @colorWithString:@
colorWithStringSelector :: Selector
colorWithStringSelector = mkSelector "colorWithString:"

-- | @Selector@ for @initWithCGColor:@
initWithCGColorSelector :: Selector
initWithCGColorSelector = mkSelector "initWithCGColor:"

-- | @Selector@ for @initWithRed:green:blue:alpha:@
initWithRed_green_blue_alphaSelector :: Selector
initWithRed_green_blue_alphaSelector = mkSelector "initWithRed:green:blue:alpha:"

-- | @Selector@ for @initWithRed:green:blue:@
initWithRed_green_blueSelector :: Selector
initWithRed_green_blueSelector = mkSelector "initWithRed:green:blue:"

-- | @Selector@ for @initWithRed:green:blue:alpha:colorSpace:@
initWithRed_green_blue_alpha_colorSpaceSelector :: Selector
initWithRed_green_blue_alpha_colorSpaceSelector = mkSelector "initWithRed:green:blue:alpha:colorSpace:"

-- | @Selector@ for @initWithRed:green:blue:colorSpace:@
initWithRed_green_blue_colorSpaceSelector :: Selector
initWithRed_green_blue_colorSpaceSelector = mkSelector "initWithRed:green:blue:colorSpace:"

-- | @Selector@ for @numberOfComponents@
numberOfComponentsSelector :: Selector
numberOfComponentsSelector = mkSelector "numberOfComponents"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @red@
redSelector :: Selector
redSelector = mkSelector "red"

-- | @Selector@ for @green@
greenSelector :: Selector
greenSelector = mkSelector "green"

-- | @Selector@ for @blue@
blueSelector :: Selector
blueSelector = mkSelector "blue"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector
stringRepresentationSelector = mkSelector "stringRepresentation"

