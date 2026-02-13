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
  , components
  , alpha
  , colorSpace
  , red
  , green
  , blue
  , stringRepresentation
  , blackColor
  , whiteColor
  , grayColor
  , redColor
  , greenColor
  , blueColor
  , cyanColor
  , magentaColor
  , yellowColor
  , clearColor
  , alphaSelector
  , blackColorSelector
  , blueColorSelector
  , blueSelector
  , clearColorSelector
  , colorSpaceSelector
  , colorWithCGColorSelector
  , colorWithRed_green_blueSelector
  , colorWithRed_green_blue_alphaSelector
  , colorWithRed_green_blue_alpha_colorSpaceSelector
  , colorWithRed_green_blue_colorSpaceSelector
  , colorWithStringSelector
  , componentsSelector
  , cyanColorSelector
  , grayColorSelector
  , greenColorSelector
  , greenSelector
  , initWithCGColorSelector
  , initWithRed_green_blueSelector
  , initWithRed_green_blue_alphaSelector
  , initWithRed_green_blue_alpha_colorSpaceSelector
  , initWithRed_green_blue_colorSpaceSelector
  , magentaColorSelector
  , numberOfComponentsSelector
  , redColorSelector
  , redSelector
  , stringRepresentationSelector
  , whiteColorSelector
  , yellowColorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' colorWithCGColorSelector color

-- | Create a Core Image color object in the sRGB color space  with the specified red, green, blue, and alpha component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB. - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1.   - alpha: The color's alpha (opacity) value between 0 and 1. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIColor)
colorWithRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' colorWithRed_green_blue_alphaSelector red green blue alpha

-- | Create a Core Image color object in the sRGB color space  with the specified red, green, and blue component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.  - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1. - Returns:    An autoreleased ``CIColor`` instance.
--
-- ObjC selector: @+ colorWithRed:green:blue:@
colorWithRed_green_blue :: CDouble -> CDouble -> CDouble -> IO (Id CIColor)
colorWithRed_green_blue red green blue =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' colorWithRed_green_blueSelector red green blue

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
    sendClassMessage cls' colorWithRed_green_blue_alpha_colorSpaceSelector red green blue alpha colorSpace

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
    sendClassMessage cls' colorWithRed_green_blue_colorSpaceSelector red green blue colorSpace

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
    sendClassMessage cls' colorWithStringSelector (toNSString representation)

-- | Create a Core Image color object with a Core Graphics color object. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithCGColor:@
initWithCGColor :: IsCIColor ciColor => ciColor -> Ptr () -> IO (Id CIColor)
initWithCGColor ciColor color =
  sendOwnedMessage ciColor initWithCGColorSelector color

-- | Initialize a Core Image color object in the sRGB color space  with the specified red, green, blue, and alpha component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.  - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1.   - alpha: The color's alpha (opacity) value between 0 and 1. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:alpha:@
initWithRed_green_blue_alpha :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIColor)
initWithRed_green_blue_alpha ciColor red green blue alpha =
  sendOwnedMessage ciColor initWithRed_green_blue_alphaSelector red green blue alpha

-- | Initialize a Core Image color object in the sRGB color space  with the specified red, green, and blue component values.
--
-- On macOS before 10.10, the CIColor's color space will be Generic RGB.  - Parameters:   - red: The color's unpremultiplied red component value between 0 and 1.   - green: The color's unpremultiplied green component value between 0 and 1.   - blue: The color's unpremultiplied blue component value between 0 and 1. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:@
initWithRed_green_blue :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> IO (Id CIColor)
initWithRed_green_blue ciColor red green blue =
  sendOwnedMessage ciColor initWithRed_green_blueSelector red green blue

-- | Initialize a Core Image color object  with the specified red, green, and blue component values as measured in the specified color space.
--
-- This will return null if the @CGColorSpace@ is not @kCGColorSpaceModelRGB@.  The RGB values can be outside the @0...1@ range if the @CGColorSpace@ is unclamped. - Parameters:   - red: The color's unpremultiplied red component value.   - green: The color's unpremultiplied green component value.   - blue: The color's unpremultiplied blue component value.   - alpha: The color's alpha (opacity) value between 0 and 1.   - colorSpace: The color's @CGColorSpace@ which must have @kCGColorSpaceModelRGB@. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:alpha:colorSpace:@
initWithRed_green_blue_alpha_colorSpace :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr () -> IO (Id CIColor)
initWithRed_green_blue_alpha_colorSpace ciColor red green blue alpha colorSpace =
  sendOwnedMessage ciColor initWithRed_green_blue_alpha_colorSpaceSelector red green blue alpha colorSpace

-- | Initialize a Core Image color object  with the specified red, green, and blue component values as measured in the specified color space.
--
-- This will return null if the @CGColorSpace@ is not @kCGColorSpaceModelRGB@.  The RGB values can be outside the @0...1@ range if the @CGColorSpace@ is unclamped. - Parameters:   - red: The color's unpremultiplied red component value.   - green: The color's unpremultiplied green component value.   - blue: The color's unpremultiplied blue component value.   - colorSpace: The color's @CGColorSpace@ which must have @kCGColorSpaceModelRGB@. - Returns:    An initialized ``CIColor`` instance.
--
-- ObjC selector: @- initWithRed:green:blue:colorSpace:@
initWithRed_green_blue_colorSpace :: IsCIColor ciColor => ciColor -> CDouble -> CDouble -> CDouble -> Ptr () -> IO (Id CIColor)
initWithRed_green_blue_colorSpace ciColor red green blue colorSpace =
  sendOwnedMessage ciColor initWithRed_green_blue_colorSpaceSelector red green blue colorSpace

-- | Returns the color components of the color including alpha.
--
-- This number includes the alpha component if the color contains one.
--
-- Typically this number will be @4@ for red, green, blue, and alpha. If the ``CIColor`` was initialized with a @CGColor@ then the number  will be the same as calling @CGColorGetNumberOfComponents()@
--
-- ObjC selector: @- numberOfComponents@
numberOfComponents :: IsCIColor ciColor => ciColor -> IO CULong
numberOfComponents ciColor =
  sendMessage ciColor numberOfComponentsSelector

-- | Return a pointer to an array of @CGFloat@ values including alpha.
--
-- Typically this array will contain @4@ @CGFloat@ values for red, green, blue, and alpha.  If the ``CIColor`` was initialized with a @CGColor@ then returned pointer  will be the same as calling @CGColorGetComponents()@
--
-- ObjC selector: @- components@
components :: IsCIColor ciColor => ciColor -> IO RawId
components ciColor =
  sendMessage ciColor componentsSelector

-- | Returns the alpha value of the color.
--
-- ObjC selector: @- alpha@
alpha :: IsCIColor ciColor => ciColor -> IO CDouble
alpha ciColor =
  sendMessage ciColor alphaSelector

-- | Returns the @CGColorSpace@ associated with the color
--
-- ObjC selector: @- colorSpace@
colorSpace :: IsCIColor ciColor => ciColor -> IO (Ptr ())
colorSpace ciColor =
  sendMessage ciColor colorSpaceSelector

-- | Returns the unpremultiplied red component of the color.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the red component.
--
-- ObjC selector: @- red@
red :: IsCIColor ciColor => ciColor -> IO CDouble
red ciColor =
  sendMessage ciColor redSelector

-- | Returns the unpremultiplied green component of the color.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the green component.
--
-- ObjC selector: @- green@
green :: IsCIColor ciColor => ciColor -> IO CDouble
green ciColor =
  sendMessage ciColor greenSelector

-- | Returns the unpremultiplied blue component of the color.
--
-- If the ``CIColor`` was initialized with a @CGColor@ in a non-RGB @CGColorSpace@ then it will be converted to sRGB to get the green component.
--
-- ObjC selector: @- blue@
blue :: IsCIColor ciColor => ciColor -> IO CDouble
blue ciColor =
  sendMessage ciColor blueSelector

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
stringRepresentation ciColor =
  sendMessage ciColor stringRepresentationSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @0,0,0@ and alpha value @1@.
--
-- ObjC selector: @+ blackColor@
blackColor :: IO RawId
blackColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' blackColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @1,1,1@ and alpha value @1@.
--
-- ObjC selector: @+ whiteColor@
whiteColor :: IO RawId
whiteColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' whiteColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @0.5,0.5,0.5@ and alpha value @1@.
--
-- ObjC selector: @+ grayColor@
grayColor :: IO RawId
grayColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' grayColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @1,0,0@ and alpha value @1@.
--
-- ObjC selector: @+ redColor@
redColor :: IO RawId
redColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' redColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @0,1,0@ and alpha value @1@.
--
-- ObjC selector: @+ greenColor@
greenColor :: IO RawId
greenColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' greenColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @0,0,1@ and alpha value @1@.
--
-- ObjC selector: @+ blueColor@
blueColor :: IO RawId
blueColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' blueColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @0,1,1@ and alpha value @1@.
--
-- ObjC selector: @+ cyanColor@
cyanColor :: IO RawId
cyanColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' cyanColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @1,0,1@ and alpha value @1@.
--
-- ObjC selector: @+ magentaColor@
magentaColor :: IO RawId
magentaColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' magentaColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @1,1,0@ and alpha value @1@.
--
-- ObjC selector: @+ yellowColor@
yellowColor :: IO RawId
yellowColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' yellowColorSelector

-- | Returns a singleton Core Image color instance in the sRGB color space with RGB values @0,0,0@ and alpha value @0@.
--
-- ObjC selector: @+ clearColor@
clearColor :: IO RawId
clearColor  =
  do
    cls' <- getRequiredClass "CIColor"
    sendClassMessage cls' clearColorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorWithCGColor:@
colorWithCGColorSelector :: Selector '[Ptr ()] (Id CIColor)
colorWithCGColorSelector = mkSelector "colorWithCGColor:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id CIColor)
colorWithRed_green_blue_alphaSelector = mkSelector "colorWithRed:green:blue:alpha:"

-- | @Selector@ for @colorWithRed:green:blue:@
colorWithRed_green_blueSelector :: Selector '[CDouble, CDouble, CDouble] (Id CIColor)
colorWithRed_green_blueSelector = mkSelector "colorWithRed:green:blue:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:colorSpace:@
colorWithRed_green_blue_alpha_colorSpaceSelector :: Selector '[CDouble, CDouble, CDouble, CDouble, Ptr ()] (Id CIColor)
colorWithRed_green_blue_alpha_colorSpaceSelector = mkSelector "colorWithRed:green:blue:alpha:colorSpace:"

-- | @Selector@ for @colorWithRed:green:blue:colorSpace:@
colorWithRed_green_blue_colorSpaceSelector :: Selector '[CDouble, CDouble, CDouble, Ptr ()] (Id CIColor)
colorWithRed_green_blue_colorSpaceSelector = mkSelector "colorWithRed:green:blue:colorSpace:"

-- | @Selector@ for @colorWithString:@
colorWithStringSelector :: Selector '[Id NSString] (Id CIColor)
colorWithStringSelector = mkSelector "colorWithString:"

-- | @Selector@ for @initWithCGColor:@
initWithCGColorSelector :: Selector '[Ptr ()] (Id CIColor)
initWithCGColorSelector = mkSelector "initWithCGColor:"

-- | @Selector@ for @initWithRed:green:blue:alpha:@
initWithRed_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id CIColor)
initWithRed_green_blue_alphaSelector = mkSelector "initWithRed:green:blue:alpha:"

-- | @Selector@ for @initWithRed:green:blue:@
initWithRed_green_blueSelector :: Selector '[CDouble, CDouble, CDouble] (Id CIColor)
initWithRed_green_blueSelector = mkSelector "initWithRed:green:blue:"

-- | @Selector@ for @initWithRed:green:blue:alpha:colorSpace:@
initWithRed_green_blue_alpha_colorSpaceSelector :: Selector '[CDouble, CDouble, CDouble, CDouble, Ptr ()] (Id CIColor)
initWithRed_green_blue_alpha_colorSpaceSelector = mkSelector "initWithRed:green:blue:alpha:colorSpace:"

-- | @Selector@ for @initWithRed:green:blue:colorSpace:@
initWithRed_green_blue_colorSpaceSelector :: Selector '[CDouble, CDouble, CDouble, Ptr ()] (Id CIColor)
initWithRed_green_blue_colorSpaceSelector = mkSelector "initWithRed:green:blue:colorSpace:"

-- | @Selector@ for @numberOfComponents@
numberOfComponentsSelector :: Selector '[] CULong
numberOfComponentsSelector = mkSelector "numberOfComponents"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] RawId
componentsSelector = mkSelector "components"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Ptr ())
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @red@
redSelector :: Selector '[] CDouble
redSelector = mkSelector "red"

-- | @Selector@ for @green@
greenSelector :: Selector '[] CDouble
greenSelector = mkSelector "green"

-- | @Selector@ for @blue@
blueSelector :: Selector '[] CDouble
blueSelector = mkSelector "blue"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector '[] (Id NSString)
stringRepresentationSelector = mkSelector "stringRepresentation"

-- | @Selector@ for @blackColor@
blackColorSelector :: Selector '[] RawId
blackColorSelector = mkSelector "blackColor"

-- | @Selector@ for @whiteColor@
whiteColorSelector :: Selector '[] RawId
whiteColorSelector = mkSelector "whiteColor"

-- | @Selector@ for @grayColor@
grayColorSelector :: Selector '[] RawId
grayColorSelector = mkSelector "grayColor"

-- | @Selector@ for @redColor@
redColorSelector :: Selector '[] RawId
redColorSelector = mkSelector "redColor"

-- | @Selector@ for @greenColor@
greenColorSelector :: Selector '[] RawId
greenColorSelector = mkSelector "greenColor"

-- | @Selector@ for @blueColor@
blueColorSelector :: Selector '[] RawId
blueColorSelector = mkSelector "blueColor"

-- | @Selector@ for @cyanColor@
cyanColorSelector :: Selector '[] RawId
cyanColorSelector = mkSelector "cyanColor"

-- | @Selector@ for @magentaColor@
magentaColorSelector :: Selector '[] RawId
magentaColorSelector = mkSelector "magentaColor"

-- | @Selector@ for @yellowColor@
yellowColorSelector :: Selector '[] RawId
yellowColorSelector = mkSelector "yellowColor"

-- | @Selector@ for @clearColor@
clearColorSelector :: Selector '[] RawId
clearColorSelector = mkSelector "clearColor"

