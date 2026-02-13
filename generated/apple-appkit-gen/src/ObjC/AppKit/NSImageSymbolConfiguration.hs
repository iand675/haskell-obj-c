{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSImageSymbolConfiguration@.
module ObjC.AppKit.NSImageSymbolConfiguration
  ( NSImageSymbolConfiguration
  , IsNSImageSymbolConfiguration(..)
  , configurationWithPointSize_weight_scale
  , configurationWithPointSize_weight
  , configurationWithTextStyle_scale
  , configurationWithTextStyle
  , configurationWithScale
  , configurationPreferringMonochrome
  , configurationPreferringHierarchical
  , configurationWithHierarchicalColor
  , configurationWithPaletteColors
  , configurationPreferringMulticolor
  , configurationWithVariableValueMode
  , configurationWithColorRenderingMode
  , configurationByApplyingConfiguration
  , configurationByApplyingConfigurationSelector
  , configurationPreferringHierarchicalSelector
  , configurationPreferringMonochromeSelector
  , configurationPreferringMulticolorSelector
  , configurationWithColorRenderingModeSelector
  , configurationWithHierarchicalColorSelector
  , configurationWithPaletteColorsSelector
  , configurationWithPointSize_weightSelector
  , configurationWithPointSize_weight_scaleSelector
  , configurationWithScaleSelector
  , configurationWithTextStyleSelector
  , configurationWithTextStyle_scaleSelector
  , configurationWithVariableValueModeSelector

  -- * Enum types
  , NSImageSymbolColorRenderingMode(NSImageSymbolColorRenderingMode)
  , pattern NSImageSymbolColorRenderingModeAutomatic
  , pattern NSImageSymbolColorRenderingModeFlat
  , pattern NSImageSymbolColorRenderingModeGradient
  , NSImageSymbolScale(NSImageSymbolScale)
  , pattern NSImageSymbolScaleSmall
  , pattern NSImageSymbolScaleMedium
  , pattern NSImageSymbolScaleLarge
  , NSImageSymbolVariableValueMode(NSImageSymbolVariableValueMode)
  , pattern NSImageSymbolVariableValueModeAutomatic
  , pattern NSImageSymbolVariableValueModeColor
  , pattern NSImageSymbolVariableValueModeDraw

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ configurationWithPointSize:weight:scale:@
configurationWithPointSize_weight_scale :: CDouble -> CDouble -> NSImageSymbolScale -> IO (Id NSImageSymbolConfiguration)
configurationWithPointSize_weight_scale pointSize weight scale =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithPointSize_weight_scaleSelector pointSize weight scale

-- | @+ configurationWithPointSize:weight:@
configurationWithPointSize_weight :: CDouble -> CDouble -> IO (Id NSImageSymbolConfiguration)
configurationWithPointSize_weight pointSize weight =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithPointSize_weightSelector pointSize weight

-- | @+ configurationWithTextStyle:scale:@
configurationWithTextStyle_scale :: IsNSString style => style -> NSImageSymbolScale -> IO (Id NSImageSymbolConfiguration)
configurationWithTextStyle_scale style scale =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithTextStyle_scaleSelector (toNSString style) scale

-- | @+ configurationWithTextStyle:@
configurationWithTextStyle :: IsNSString style => style -> IO (Id NSImageSymbolConfiguration)
configurationWithTextStyle style =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithTextStyleSelector (toNSString style)

-- | @+ configurationWithScale:@
configurationWithScale :: NSImageSymbolScale -> IO (Id NSImageSymbolConfiguration)
configurationWithScale scale =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithScaleSelector scale

-- | Create a configuration that specifies that the symbol should prefer its monochrome variant.
--
-- ObjC selector: @+ configurationPreferringMonochrome@
configurationPreferringMonochrome :: IO (Id NSImageSymbolConfiguration)
configurationPreferringMonochrome  =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationPreferringMonochromeSelector

-- | Create a configuration that specifies that the symbol should prefer its hierarchical variant, if one exists.
--
-- If the symbol doesn’t support hierarchical, the result will be a monochrome (templated) symbol.
--
-- ObjC selector: @+ configurationPreferringHierarchical@
configurationPreferringHierarchical :: IO (Id NSImageSymbolConfiguration)
configurationPreferringHierarchical  =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationPreferringHierarchicalSelector

-- | Create a color configuration with a palette derived from one color.
--
-- A color scheme will be created based on the provided color, deriving secondary and tertiary colors by reducing the intensity of the base color. This is typically (but not only) accomplished by reducing opacity of the primary color.
--
-- When combined with another configuration creating a palette, the last specified configuration will win, overwriting the other color configuration.
--
-- If the symbol doesn’t have a palette-based variant, the configuration will have no effect and the result will be a monochrome (templated) symbol.
--
-- ObjC selector: @+ configurationWithHierarchicalColor:@
configurationWithHierarchicalColor :: IsNSColor hierarchicalColor => hierarchicalColor -> IO (Id NSImageSymbolConfiguration)
configurationWithHierarchicalColor hierarchicalColor =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithHierarchicalColorSelector (toNSColor hierarchicalColor)

-- | Create a color configuration by specifying a palette of colors. The colors are used sequentially per layer: the first color for the first layer, the second color for the second layer etc. This is independent of the hierarchy level of the layer.
--
-- When combined with another configuration creating a palette, the last specified configuration will win, overwriting the other color configuration.
--
-- If the symbol doesn’t have a palette-based variant, the configuration will have no effect and the result will be a monochrome (templated) symbol.
--
-- ObjC selector: @+ configurationWithPaletteColors:@
configurationWithPaletteColors :: IsNSArray paletteColors => paletteColors -> IO (Id NSImageSymbolConfiguration)
configurationWithPaletteColors paletteColors =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithPaletteColorsSelector (toNSArray paletteColors)

-- | Create a configuration that specifies that the symbol should prefer its multicolor variant, if one exists.
--
-- This configuration can be combined with one of the palette-based configurations; in that case, the symbol will use the multicolor variant if one exists, or the palette variant otherwise.
--
-- If the symbol supports neither, the result will be a monochrome (templated) symbol.
--
-- ObjC selector: @+ configurationPreferringMulticolor@
configurationPreferringMulticolor :: IO (Id NSImageSymbolConfiguration)
configurationPreferringMulticolor  =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationPreferringMulticolorSelector

-- | Create a configuration with a specified variable value mode.
--
-- ObjC selector: @+ configurationWithVariableValueMode:@
configurationWithVariableValueMode :: NSImageSymbolVariableValueMode -> IO (Id NSImageSymbolConfiguration)
configurationWithVariableValueMode variableValueMode =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithVariableValueModeSelector variableValueMode

-- | Create a configuration with a specific color rendering mode.
--
-- ObjC selector: @+ configurationWithColorRenderingMode:@
configurationWithColorRenderingMode :: NSImageSymbolColorRenderingMode -> IO (Id NSImageSymbolConfiguration)
configurationWithColorRenderingMode mode =
  do
    cls' <- getRequiredClass "NSImageSymbolConfiguration"
    sendClassMessage cls' configurationWithColorRenderingModeSelector mode

-- | Returns a new configuration object whose values are defined by applying values from the provided configuration and the receiver.
--
-- Values defined by both configurations will use the provided configuration’s values.
--
-- ObjC selector: @- configurationByApplyingConfiguration:@
configurationByApplyingConfiguration :: (IsNSImageSymbolConfiguration nsImageSymbolConfiguration, IsNSImageSymbolConfiguration configuration) => nsImageSymbolConfiguration -> configuration -> IO (Id NSImageSymbolConfiguration)
configurationByApplyingConfiguration nsImageSymbolConfiguration configuration =
  sendMessage nsImageSymbolConfiguration configurationByApplyingConfigurationSelector (toNSImageSymbolConfiguration configuration)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurationWithPointSize:weight:scale:@
configurationWithPointSize_weight_scaleSelector :: Selector '[CDouble, CDouble, NSImageSymbolScale] (Id NSImageSymbolConfiguration)
configurationWithPointSize_weight_scaleSelector = mkSelector "configurationWithPointSize:weight:scale:"

-- | @Selector@ for @configurationWithPointSize:weight:@
configurationWithPointSize_weightSelector :: Selector '[CDouble, CDouble] (Id NSImageSymbolConfiguration)
configurationWithPointSize_weightSelector = mkSelector "configurationWithPointSize:weight:"

-- | @Selector@ for @configurationWithTextStyle:scale:@
configurationWithTextStyle_scaleSelector :: Selector '[Id NSString, NSImageSymbolScale] (Id NSImageSymbolConfiguration)
configurationWithTextStyle_scaleSelector = mkSelector "configurationWithTextStyle:scale:"

-- | @Selector@ for @configurationWithTextStyle:@
configurationWithTextStyleSelector :: Selector '[Id NSString] (Id NSImageSymbolConfiguration)
configurationWithTextStyleSelector = mkSelector "configurationWithTextStyle:"

-- | @Selector@ for @configurationWithScale:@
configurationWithScaleSelector :: Selector '[NSImageSymbolScale] (Id NSImageSymbolConfiguration)
configurationWithScaleSelector = mkSelector "configurationWithScale:"

-- | @Selector@ for @configurationPreferringMonochrome@
configurationPreferringMonochromeSelector :: Selector '[] (Id NSImageSymbolConfiguration)
configurationPreferringMonochromeSelector = mkSelector "configurationPreferringMonochrome"

-- | @Selector@ for @configurationPreferringHierarchical@
configurationPreferringHierarchicalSelector :: Selector '[] (Id NSImageSymbolConfiguration)
configurationPreferringHierarchicalSelector = mkSelector "configurationPreferringHierarchical"

-- | @Selector@ for @configurationWithHierarchicalColor:@
configurationWithHierarchicalColorSelector :: Selector '[Id NSColor] (Id NSImageSymbolConfiguration)
configurationWithHierarchicalColorSelector = mkSelector "configurationWithHierarchicalColor:"

-- | @Selector@ for @configurationWithPaletteColors:@
configurationWithPaletteColorsSelector :: Selector '[Id NSArray] (Id NSImageSymbolConfiguration)
configurationWithPaletteColorsSelector = mkSelector "configurationWithPaletteColors:"

-- | @Selector@ for @configurationPreferringMulticolor@
configurationPreferringMulticolorSelector :: Selector '[] (Id NSImageSymbolConfiguration)
configurationPreferringMulticolorSelector = mkSelector "configurationPreferringMulticolor"

-- | @Selector@ for @configurationWithVariableValueMode:@
configurationWithVariableValueModeSelector :: Selector '[NSImageSymbolVariableValueMode] (Id NSImageSymbolConfiguration)
configurationWithVariableValueModeSelector = mkSelector "configurationWithVariableValueMode:"

-- | @Selector@ for @configurationWithColorRenderingMode:@
configurationWithColorRenderingModeSelector :: Selector '[NSImageSymbolColorRenderingMode] (Id NSImageSymbolConfiguration)
configurationWithColorRenderingModeSelector = mkSelector "configurationWithColorRenderingMode:"

-- | @Selector@ for @configurationByApplyingConfiguration:@
configurationByApplyingConfigurationSelector :: Selector '[Id NSImageSymbolConfiguration] (Id NSImageSymbolConfiguration)
configurationByApplyingConfigurationSelector = mkSelector "configurationByApplyingConfiguration:"

