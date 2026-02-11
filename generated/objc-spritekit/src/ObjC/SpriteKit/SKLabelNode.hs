{-# LANGUAGE PatternSynonyms #-}
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
  , labelNodeWithTextSelector
  , labelNodeWithAttributedTextSelector
  , labelNodeWithFontNamedSelector
  , initWithFontNamedSelector
  , verticalAlignmentModeSelector
  , setVerticalAlignmentModeSelector
  , horizontalAlignmentModeSelector
  , setHorizontalAlignmentModeSelector
  , numberOfLinesSelector
  , setNumberOfLinesSelector
  , lineBreakModeSelector
  , setLineBreakModeSelector
  , preferredMaxLayoutWidthSelector
  , setPreferredMaxLayoutWidthSelector
  , fontNameSelector
  , setFontNameSelector
  , textSelector
  , setTextSelector
  , attributedTextSelector
  , setAttributedTextSelector
  , fontSizeSelector
  , setFontSizeSelector
  , fontColorSelector
  , setFontColorSelector
  , colorBlendFactorSelector
  , setColorBlendFactorSelector
  , colorSelector
  , setColorSelector
  , blendModeSelector
  , setBlendModeSelector

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
    withObjCPtr text $ \raw_text ->
      sendClassMsg cls' (mkSelector "labelNodeWithText:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ())] >>= retainedObject . castPtr

-- | @+ labelNodeWithAttributedText:@
labelNodeWithAttributedText :: IsNSAttributedString attributedText => attributedText -> IO (Id SKLabelNode)
labelNodeWithAttributedText attributedText =
  do
    cls' <- getRequiredClass "SKLabelNode"
    withObjCPtr attributedText $ \raw_attributedText ->
      sendClassMsg cls' (mkSelector "labelNodeWithAttributedText:") (retPtr retVoid) [argPtr (castPtr raw_attributedText :: Ptr ())] >>= retainedObject . castPtr

-- | @+ labelNodeWithFontNamed:@
labelNodeWithFontNamed :: IsNSString fontName => fontName -> IO (Id SKLabelNode)
labelNodeWithFontNamed fontName =
  do
    cls' <- getRequiredClass "SKLabelNode"
    withObjCPtr fontName $ \raw_fontName ->
      sendClassMsg cls' (mkSelector "labelNodeWithFontNamed:") (retPtr retVoid) [argPtr (castPtr raw_fontName :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithFontNamed:@
initWithFontNamed :: (IsSKLabelNode skLabelNode, IsNSString fontName) => skLabelNode -> fontName -> IO (Id SKLabelNode)
initWithFontNamed skLabelNode  fontName =
withObjCPtr fontName $ \raw_fontName ->
    sendMsg skLabelNode (mkSelector "initWithFontNamed:") (retPtr retVoid) [argPtr (castPtr raw_fontName :: Ptr ())] >>= ownedObject . castPtr

-- | @- verticalAlignmentMode@
verticalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO SKLabelVerticalAlignmentMode
verticalAlignmentMode skLabelNode  =
  fmap (coerce :: CLong -> SKLabelVerticalAlignmentMode) $ sendMsg skLabelNode (mkSelector "verticalAlignmentMode") retCLong []

-- | @- setVerticalAlignmentMode:@
setVerticalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> SKLabelVerticalAlignmentMode -> IO ()
setVerticalAlignmentMode skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setVerticalAlignmentMode:") retVoid [argCLong (coerce value)]

-- | @- horizontalAlignmentMode@
horizontalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO SKLabelHorizontalAlignmentMode
horizontalAlignmentMode skLabelNode  =
  fmap (coerce :: CLong -> SKLabelHorizontalAlignmentMode) $ sendMsg skLabelNode (mkSelector "horizontalAlignmentMode") retCLong []

-- | @- setHorizontalAlignmentMode:@
setHorizontalAlignmentMode :: IsSKLabelNode skLabelNode => skLabelNode -> SKLabelHorizontalAlignmentMode -> IO ()
setHorizontalAlignmentMode skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setHorizontalAlignmentMode:") retVoid [argCLong (coerce value)]

-- | Determines the number of lines to draw. The default value is 1 (single line). A value of 0 means no limit.   If the height of the text reaches the # of lines the text will be truncated using the line break mode.
--
-- ObjC selector: @- numberOfLines@
numberOfLines :: IsSKLabelNode skLabelNode => skLabelNode -> IO CLong
numberOfLines skLabelNode  =
  sendMsg skLabelNode (mkSelector "numberOfLines") retCLong []

-- | Determines the number of lines to draw. The default value is 1 (single line). A value of 0 means no limit.   If the height of the text reaches the # of lines the text will be truncated using the line break mode.
--
-- ObjC selector: @- setNumberOfLines:@
setNumberOfLines :: IsSKLabelNode skLabelNode => skLabelNode -> CLong -> IO ()
setNumberOfLines skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setNumberOfLines:") retVoid [argCLong (fromIntegral value)]

-- | Determines the line break mode for multiple lines.   Default is NSLineBreakByTruncatingTail
--
-- ObjC selector: @- lineBreakMode@
lineBreakMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO NSLineBreakMode
lineBreakMode skLabelNode  =
  fmap (coerce :: CULong -> NSLineBreakMode) $ sendMsg skLabelNode (mkSelector "lineBreakMode") retCULong []

-- | Determines the line break mode for multiple lines.   Default is NSLineBreakByTruncatingTail
--
-- ObjC selector: @- setLineBreakMode:@
setLineBreakMode :: IsSKLabelNode skLabelNode => skLabelNode -> NSLineBreakMode -> IO ()
setLineBreakMode skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setLineBreakMode:") retVoid [argCULong (coerce value)]

-- | If nonzero, this is used when determining layout width for multiline labels.   Default is zero.
--
-- ObjC selector: @- preferredMaxLayoutWidth@
preferredMaxLayoutWidth :: IsSKLabelNode skLabelNode => skLabelNode -> IO CDouble
preferredMaxLayoutWidth skLabelNode  =
  sendMsg skLabelNode (mkSelector "preferredMaxLayoutWidth") retCDouble []

-- | If nonzero, this is used when determining layout width for multiline labels.   Default is zero.
--
-- ObjC selector: @- setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidth :: IsSKLabelNode skLabelNode => skLabelNode -> CDouble -> IO ()
setPreferredMaxLayoutWidth skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setPreferredMaxLayoutWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- fontName@
fontName :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSString)
fontName skLabelNode  =
  sendMsg skLabelNode (mkSelector "fontName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontName:@
setFontName :: (IsSKLabelNode skLabelNode, IsNSString value) => skLabelNode -> value -> IO ()
setFontName skLabelNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLabelNode (mkSelector "setFontName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- text@
text :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSString)
text skLabelNode  =
  sendMsg skLabelNode (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setText:@
setText :: (IsSKLabelNode skLabelNode, IsNSString value) => skLabelNode -> value -> IO ()
setText skLabelNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLabelNode (mkSelector "setText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedText@
attributedText :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSAttributedString)
attributedText skLabelNode  =
  sendMsg skLabelNode (mkSelector "attributedText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedText:@
setAttributedText :: (IsSKLabelNode skLabelNode, IsNSAttributedString value) => skLabelNode -> value -> IO ()
setAttributedText skLabelNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLabelNode (mkSelector "setAttributedText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fontSize@
fontSize :: IsSKLabelNode skLabelNode => skLabelNode -> IO CDouble
fontSize skLabelNode  =
  sendMsg skLabelNode (mkSelector "fontSize") retCDouble []

-- | @- setFontSize:@
setFontSize :: IsSKLabelNode skLabelNode => skLabelNode -> CDouble -> IO ()
setFontSize skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setFontSize:") retVoid [argCDouble (fromIntegral value)]

-- | Base color that the text is rendered with (if supported by the font)
--
-- ObjC selector: @- fontColor@
fontColor :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSColor)
fontColor skLabelNode  =
  sendMsg skLabelNode (mkSelector "fontColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Base color that the text is rendered with (if supported by the font)
--
-- ObjC selector: @- setFontColor:@
setFontColor :: (IsSKLabelNode skLabelNode, IsNSColor value) => skLabelNode -> value -> IO ()
setFontColor skLabelNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLabelNode (mkSelector "setFontColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls the blending between the rendered text and a color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- colorBlendFactor@
colorBlendFactor :: IsSKLabelNode skLabelNode => skLabelNode -> IO CDouble
colorBlendFactor skLabelNode  =
  sendMsg skLabelNode (mkSelector "colorBlendFactor") retCDouble []

-- | Controls the blending between the rendered text and a color. The valid interval of values is from 0.0 up to and including 1.0. A value above or below that interval is clamped to the minimum (0.0) if below or the maximum (1.0) if above.
--
-- ObjC selector: @- setColorBlendFactor:@
setColorBlendFactor :: IsSKLabelNode skLabelNode => skLabelNode -> CDouble -> IO ()
setColorBlendFactor skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setColorBlendFactor:") retVoid [argCDouble (fromIntegral value)]

-- | Color to be blended with the text based on the colorBlendFactor
--
-- ObjC selector: @- color@
color :: IsSKLabelNode skLabelNode => skLabelNode -> IO (Id NSColor)
color skLabelNode  =
  sendMsg skLabelNode (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Color to be blended with the text based on the colorBlendFactor
--
-- ObjC selector: @- setColor:@
setColor :: (IsSKLabelNode skLabelNode, IsNSColor value) => skLabelNode -> value -> IO ()
setColor skLabelNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skLabelNode (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- blendMode@
blendMode :: IsSKLabelNode skLabelNode => skLabelNode -> IO SKBlendMode
blendMode skLabelNode  =
  fmap (coerce :: CLong -> SKBlendMode) $ sendMsg skLabelNode (mkSelector "blendMode") retCLong []

-- | Sets the blend mode to use when composing the sprite with the final framebuffer.
--
-- See: SKNode.SKBlendMode
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSKLabelNode skLabelNode => skLabelNode -> SKBlendMode -> IO ()
setBlendMode skLabelNode  value =
  sendMsg skLabelNode (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @labelNodeWithText:@
labelNodeWithTextSelector :: Selector
labelNodeWithTextSelector = mkSelector "labelNodeWithText:"

-- | @Selector@ for @labelNodeWithAttributedText:@
labelNodeWithAttributedTextSelector :: Selector
labelNodeWithAttributedTextSelector = mkSelector "labelNodeWithAttributedText:"

-- | @Selector@ for @labelNodeWithFontNamed:@
labelNodeWithFontNamedSelector :: Selector
labelNodeWithFontNamedSelector = mkSelector "labelNodeWithFontNamed:"

-- | @Selector@ for @initWithFontNamed:@
initWithFontNamedSelector :: Selector
initWithFontNamedSelector = mkSelector "initWithFontNamed:"

-- | @Selector@ for @verticalAlignmentMode@
verticalAlignmentModeSelector :: Selector
verticalAlignmentModeSelector = mkSelector "verticalAlignmentMode"

-- | @Selector@ for @setVerticalAlignmentMode:@
setVerticalAlignmentModeSelector :: Selector
setVerticalAlignmentModeSelector = mkSelector "setVerticalAlignmentMode:"

-- | @Selector@ for @horizontalAlignmentMode@
horizontalAlignmentModeSelector :: Selector
horizontalAlignmentModeSelector = mkSelector "horizontalAlignmentMode"

-- | @Selector@ for @setHorizontalAlignmentMode:@
setHorizontalAlignmentModeSelector :: Selector
setHorizontalAlignmentModeSelector = mkSelector "setHorizontalAlignmentMode:"

-- | @Selector@ for @numberOfLines@
numberOfLinesSelector :: Selector
numberOfLinesSelector = mkSelector "numberOfLines"

-- | @Selector@ for @setNumberOfLines:@
setNumberOfLinesSelector :: Selector
setNumberOfLinesSelector = mkSelector "setNumberOfLines:"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @preferredMaxLayoutWidth@
preferredMaxLayoutWidthSelector :: Selector
preferredMaxLayoutWidthSelector = mkSelector "preferredMaxLayoutWidth"

-- | @Selector@ for @setPreferredMaxLayoutWidth:@
setPreferredMaxLayoutWidthSelector :: Selector
setPreferredMaxLayoutWidthSelector = mkSelector "setPreferredMaxLayoutWidth:"

-- | @Selector@ for @fontName@
fontNameSelector :: Selector
fontNameSelector = mkSelector "fontName"

-- | @Selector@ for @setFontName:@
setFontNameSelector :: Selector
setFontNameSelector = mkSelector "setFontName:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @attributedText@
attributedTextSelector :: Selector
attributedTextSelector = mkSelector "attributedText"

-- | @Selector@ for @setAttributedText:@
setAttributedTextSelector :: Selector
setAttributedTextSelector = mkSelector "setAttributedText:"

-- | @Selector@ for @fontSize@
fontSizeSelector :: Selector
fontSizeSelector = mkSelector "fontSize"

-- | @Selector@ for @setFontSize:@
setFontSizeSelector :: Selector
setFontSizeSelector = mkSelector "setFontSize:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @colorBlendFactor@
colorBlendFactorSelector :: Selector
colorBlendFactorSelector = mkSelector "colorBlendFactor"

-- | @Selector@ for @setColorBlendFactor:@
setColorBlendFactorSelector :: Selector
setColorBlendFactorSelector = mkSelector "setColorBlendFactor:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

