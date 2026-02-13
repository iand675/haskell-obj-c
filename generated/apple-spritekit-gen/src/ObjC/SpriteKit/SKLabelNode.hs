{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node that displays a text label with a given font.
--
-- Generated bindings for @SKLabelNode@.
module ObjC.SpriteKit.SKLabelNode
  ( SKLabelNode
  , IsSKLabelNode(..)
  , labelNodeWithText
  , labelNodeWithAttributedText
  , labelNodeWithFontNamed
  , initWithFontNamed
  , verticalAlignmentMode
  , setVerticalAlignmentMode
  , horizontalAlignmentMode
  , setHorizontalAlignmentMode
  , numberOfLines
  , setNumberOfLines
  , lineBreakMode
  , setLineBreakMode
  , preferredMaxLayoutWidth
  , setPreferredMaxLayoutWidth
  , fontName
  , setFontName
  , text
  , setText
  , attributedText
  , setAttributedText
  , fontSize
  , setFontSize
  , fontColor
  , setFontColor
  , colorBlendFactor
  , setColorBlendFactor
  , color
  , setColor
  , blendMode
  , setBlendMode
  , attributedTextSelector
  , blendModeSelector
  , colorBlendFactorSelector
  , colorSelector
  , fontColorSelector
  , fontNameSelector
  , fontSizeSelector
  , horizontalAlignmentModeSelector
  , initWithFontNamedSelector
  , labelNodeWithAttributedTextSelector
  , labelNodeWithFontNamedSelector
  , labelNodeWithTextSelector
  , lineBreakModeSelector
  , numberOfLinesSelector
  , preferredMaxLayoutWidthSelector
  , setAttributedTextSelector
  , setBlendModeSelector
  , setColorBlendFactorSelector
  , setColorSelector
  , setFontColorSelector
  , setFontNameSelector
  , setFontSizeSelector
  , setHorizontalAlignmentModeSelector
  , setLineBreakModeSelector
  , setNumberOfLinesSelector
  , setPreferredMaxLayoutWidthSelector
  , setTextSelector
  , setVerticalAlignmentModeSelector
  , textSelector
  , verticalAlignmentModeSelector

  -- * Enum types
  , NSLineBreakMode(NSLineBreakMode)
  , pattern NSLineBreakByWordWrapping
  , pattern NSLineBreakByCharWrapping
  , pattern NSLineBreakByClipping
  , pattern NSLineBreakByTruncatingHead
  , pattern NSLineBreakByTruncatingTail
  , pattern NSLineBreakByTruncatingMiddle
  , SKBlendMode(SKBlendMode)
  , pattern SKBlendModeAlpha
  , pattern SKBlendModeAdd
  , pattern SKBlendModeSubtract
  , pattern SKBlendModeMultiply
  , pattern SKBlendModeMultiplyX2
  , pattern SKBlendModeScreen
  , pattern SKBlendModeReplace
  , pattern SKBlendModeMultiplyAlpha
  , SKLabelHorizontalAlignmentMode(SKLabelHorizontalAlignmentMode)
  , pattern SKLabelHorizontalAlignmentModeCenter
  , pattern SKLabelHorizontalAlignmentModeLeft
  , pattern SKLabelHorizontalAlignmentModeRight
  , SKLabelVerticalAlignmentMode(SKLabelVerticalAlignmentMode)
  , pattern SKLabelVerticalAlignmentModeBaseline
  , pattern SKLabelVerticalAlignmentModeCenter
  , pattern SKLabelVerticalAlignmentModeTop
  , pattern SKLabelVerticalAlignmentModeBottom

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ labelNodeWithText:@
labelNodeWithText :: IsNSString text => text -> IO (Id SKLabelNode)
labelNodeWithText text =
  do
    cls' <- getRequiredClass "SKLabelNode"
    sendClassMessage cls' labelNodeWithTextSelector (toNSString text)

-- | @+ labelNodeWithAttributedText:@
labelNodeWithAttributedText :: IsNSAttributedString attributedText => attributedText -> IO (Id SKLabelNode)
labelNodeWithAttributedText attributedText =
  do
    cls' <- getRequiredClass "SKLabelNode"
    sendClassMessage cls' labelNodeWithAttributedTextSelector (toNSAttributedString attributedText)

-- | @+ labelNodeWithFontNamed:@
labelNodeWithFontNamed :: IsNSString fontName => fontName -> IO (Id SKLabelNode)
labelNodeWithFontNamed fontName =
  do
    cls' <- getRequiredClass "SKLabelNode"
    sendClassMessage cls' labelNodeWithFontNamedSelector (toNSString fontName)

-- | @- initWithFontNamed:@
initWithFontNamed :: (IsSKLabelNode skLabelNode, IsNSString fontName) => skLabelNode -> fontName -> IO (Id SKLabelNode)
initWithFontNamed skLabelNode fontName =
  sendOwnedMessage skLabelNode initWithFontNamedSelector (toNSString fontName)

-- | @- verticalAlignmentMode@
verticalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO SKLabelVerticalAlignmentMode
verticalAlignmentMode skLabelNode =
  sendMessage skLabelNode verticalAlignmentModeSelector

-- | @- setVerticalAlignmentMode:@
setVerticalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> SKLabelVerticalAlignmentMode -> IO ()
setVerticalAlignmentMode skLabelNode value =
  sendMessage skLabelNode setVerticalAlignmentModeSelector value

-- | @- horizontalAlignmentMode@
horizontalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO SKLabelHorizontalAlignmentMode
horizontalAlignmentMode skLabelNode =
  sendMessage skLabelNode horizontalAlignmentModeSelector

-- | @- setHorizontalAlignmentMode:@
setHorizontalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> SKLabelHorizontalAlignmentMode -> IO ()
setHorizontalAlignmentMode skLabelNode value =
  sendMessage skLabelNode setHorizontalAlignmentModeSelector value

-- | Determines the number of lines to draw. The default value is 1 (single line). A value of 0 means no limit.   If the height of the text reaches the # of lines the text will be truncated using the line break mode.
--
-- ObjC selector: @- numberOfLines@
numberOfLines :: IsSKLabelNode skLabelNode => skLabelNode -> IO CLong
numberOfLines skLabelNode =
  sendMessage skLabelNode numberOfLinesSelector

-- | Determines the number of lines to draw. The default value is 1 (single line). A value of 0 means no limit.   If the height of the text reaches the # of lines the text will be truncated using the line break mode.
--
-- ObjC selector: @- setNumberOfLines:@
setNumberOfLines :: IsSKLabelNode skLabelNode => skLabelNode -> CLong -> IO ()
setNumberOfLines skLabelNode value =
  sendMessage skLabelNode setNumberOfLinesSelector value

-- | Determines the line break mode for multiple lines.   Default is NSLineBreakByTruncatingTail
--
-- ObjC selector: @- lineBreakMode@
lineBreakMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO NSLineBreakMode
lineBreakMode skLabelNode =
  sendMessage skLabelNode lineBreakModeSelector

-- | Determines the line break mode for multiple lines.   Default is NSLineBreakByTruncatingTail
--
-- ObjC selector: @- setLineBreakMode:@
setLineBreakMode :: IsSKLabelNode skLabelNode => skLabelNode -> NSLineBreakMode -> IO ()
setLineBreakMode skLabelNode value =
  sendMessage skLabelNode setLineBreakModeSelector value

-- | If nonzero, this is used when determining layout width for multiline labels.   Default is zero.
--
-- ObjC selector: @- preferredMaxLayoutWidth@
preferredMaxLayoutWidth :: IsSKLabelNode skLabelNode => skLabelNode -> IO CDouble
preferredMaxLayoutWidth skLabelNode =
  sendMessage skLabelNode preferredMaxLayoutWidthSelector

-- | If nonzero, this is used when determining layout width for multiline labels.   Default is zero.
--
-- ObjC selector: @- setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidth :: IsSKLabelNode skLabelNode => skLabelNode -> CDouble -> IO ()
setPreferredMaxLayoutWidth skLabelNode value =
  sendMessage skLabelNode setPreferredMaxLayoutWidthSelector value

-- | @- fontName@
fontName :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSString)
fontName skLabelNode =
  sendMessage skLabelNode fontNameSelector

-- | @- setFontName:@
setFontName :: (IsSKLabelNode skLabelNode, IsNSString value) => skLabelNode -> value -> IO ()
setFontName skLabelNode value =
  sendMessage skLabelNode setFontNameSelector (toNSString value)

-- | @- text@
text :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSString)
text skLabelNode =
  sendMessage skLabelNode textSelector

-- | @- setText:@
setText :: (IsSKLabelNode skLabelNode, IsNSString value) => skLabelNode -> value -> IO ()
setText skLabelNode value =
  sendMessage skLabelNode setTextSelector (toNSString value)

-- | @- attributedText@
attributedText :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSAttributedString)
attributedText skLabelNode =
  sendMessage skLabelNode attributedTextSelector

-- | @- setAttributedText:@
setAttributedText :: (IsSKLabelNode skLabelNode, IsNSAttributedString value) => skLabelNode -> value -> IO ()
setAttributedText skLabelNode value =
  sendMessage skLabelNode setAttributedTextSelector (toNSAttributedString value)

-- | @- fontSize@
fontSize :: IsSKLabelNode skLabelNode => skLabelNode -> IO CDouble
fontSize skLabelNode =
  sendMessage skLabelNode fontSizeSelector

-- | @- setFontSize:@
setFontSize :: IsSKLabelNode skLabelNode => skLabelNode -> CDouble -> IO ()
setFontSize skLabelNode value =
  sendMessage skLabelNode setFontSizeSelector value

-- | Base color that the text is rendered with (if supported by the font)
--
-- ObjC selector: @- fontColor@
fontColor :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSColor)
fontColor skLabelNode =
  sendMessage skLabelNode fontColorSelector

-- | Base color that the text is rendered with (if supported by the font)
--
-- ObjC selector: @- setFontColor:@
setFontColor :: (IsSKLabelNode skLabelNode, IsNSColor value) => skLabelNode -> value -> IO ()
setFontColor skLabelNode value =
  sendMessage skLabelNode setFontColorSelector (toNSColor value)

-- | Controls the blending between the rendered text and a color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- colorBlendFactor@
colorBlendFactor :: IsSKLabelNode skLabelNode => skLabelNode -> IO CDouble
colorBlendFactor skLabelNode =
  sendMessage skLabelNode colorBlendFactorSelector

-- | Controls the blending between the rendered text and a color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- setColorBlendFactor:@
setColorBlendFactor :: IsSKLabelNode skLabelNode => skLabelNode -> CDouble -> IO ()
setColorBlendFactor skLabelNode value =
  sendMessage skLabelNode setColorBlendFactorSelector value

-- | Color to be blended with the text based on the colorBlendFactor
--
-- ObjC selector: @- color@
color :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSColor)
color skLabelNode =
  sendMessage skLabelNode colorSelector

-- | Color to be blended with the text based on the colorBlendFactor
--
-- ObjC selector: @- setColor:@
setColor :: (IsSKLabelNode skLabelNode, IsNSColor value) => skLabelNode -> value -> IO ()
setColor skLabelNode value =
  sendMessage skLabelNode setColorSelector (toNSColor value)

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO SKBlendMode
blendMode skLabelNode =
  sendMessage skLabelNode blendModeSelector

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKLabelNode skLabelNode => skLabelNode -> SKBlendMode -> IO ()
setBlendMode skLabelNode value =
  sendMessage skLabelNode setBlendModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @labelNodeWithText:@
labelNodeWithTextSelector :: Selector '[Id NSString] (Id SKLabelNode)
labelNodeWithTextSelector = mkSelector "labelNodeWithText:"

-- | @Selector@ for @labelNodeWithAttributedText:@
labelNodeWithAttributedTextSelector :: Selector '[Id NSAttributedString] (Id SKLabelNode)
labelNodeWithAttributedTextSelector = mkSelector "labelNodeWithAttributedText:"

-- | @Selector@ for @labelNodeWithFontNamed:@
labelNodeWithFontNamedSelector :: Selector '[Id NSString] (Id SKLabelNode)
labelNodeWithFontNamedSelector = mkSelector "labelNodeWithFontNamed:"

-- | @Selector@ for @initWithFontNamed:@
initWithFontNamedSelector :: Selector '[Id NSString] (Id SKLabelNode)
initWithFontNamedSelector = mkSelector "initWithFontNamed:"

-- | @Selector@ for @verticalAlignmentMode@
verticalAlignmentModeSelector :: Selector '[] SKLabelVerticalAlignmentMode
verticalAlignmentModeSelector = mkSelector "verticalAlignmentMode"

-- | @Selector@ for @setVerticalAlignmentMode:@
setVerticalAlignmentModeSelector :: Selector '[SKLabelVerticalAlignmentMode] ()
setVerticalAlignmentModeSelector = mkSelector "setVerticalAlignmentMode:"

-- | @Selector@ for @horizontalAlignmentMode@
horizontalAlignmentModeSelector :: Selector '[] SKLabelHorizontalAlignmentMode
horizontalAlignmentModeSelector = mkSelector "horizontalAlignmentMode"

-- | @Selector@ for @setHorizontalAlignmentMode:@
setHorizontalAlignmentModeSelector :: Selector '[SKLabelHorizontalAlignmentMode] ()
setHorizontalAlignmentModeSelector = mkSelector "setHorizontalAlignmentMode:"

-- | @Selector@ for @numberOfLines@
numberOfLinesSelector :: Selector '[] CLong
numberOfLinesSelector = mkSelector "numberOfLines"

-- | @Selector@ for @setNumberOfLines:@
setNumberOfLinesSelector :: Selector '[CLong] ()
setNumberOfLinesSelector = mkSelector "setNumberOfLines:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector '[] NSLineBreakMode
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector '[NSLineBreakMode] ()
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @preferredMaxLayoutWidth@
preferredMaxLayoutWidthSelector :: Selector '[] CDouble
preferredMaxLayoutWidthSelector = mkSelector "preferredMaxLayoutWidth"

-- | @Selector@ for @setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidthSelector :: Selector '[CDouble] ()
setPreferredMaxLayoutWidthSelector = mkSelector "setPreferredMaxLayoutWidth:"

-- | @Selector@ for @fontName@
fontNameSelector :: Selector '[] (Id NSString)
fontNameSelector = mkSelector "fontName"

-- | @Selector@ for @setFontName:@
setFontNameSelector :: Selector '[Id NSString] ()
setFontNameSelector = mkSelector "setFontName:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector '[Id NSString] ()
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @attributedText@
attributedTextSelector :: Selector '[] (Id NSAttributedString)
attributedTextSelector = mkSelector "attributedText"

-- | @Selector@ for @setAttributedText:@
setAttributedTextSelector :: Selector '[Id NSAttributedString] ()
setAttributedTextSelector = mkSelector "setAttributedText:"

-- | @Selector@ for @fontSize@
fontSizeSelector :: Selector '[] CDouble
fontSizeSelector = mkSelector "fontSize"

-- | @Selector@ for @setFontSize:@
setFontSizeSelector :: Selector '[CDouble] ()
setFontSizeSelector = mkSelector "setFontSize:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector '[] (Id NSColor)
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector '[Id NSColor] ()
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @colorBlendFactor@
colorBlendFactorSelector :: Selector '[] CDouble
colorBlendFactorSelector = mkSelector "colorBlendFactor"

-- | @Selector@ for @setColorBlendFactor:@
setColorBlendFactorSelector :: Selector '[CDouble] ()
setColorBlendFactorSelector = mkSelector "setColorBlendFactor:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] SKBlendMode
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[SKBlendMode] ()
setBlendModeSelector = mkSelector "setBlendMode:"

