{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLevelIndicator@.
module ObjC.AppKit.NSLevelIndicator
  ( NSLevelIndicator
  , IsNSLevelIndicator(..)
  , tickMarkValueAtIndex
  , rectOfTickMarkAtIndex
  , levelIndicatorStyle
  , setLevelIndicatorStyle
  , editable
  , setEditable
  , minValue
  , setMinValue
  , maxValue
  , setMaxValue
  , warningValue
  , setWarningValue
  , criticalValue
  , setCriticalValue
  , tickMarkPosition
  , setTickMarkPosition
  , numberOfTickMarks
  , setNumberOfTickMarks
  , numberOfMajorTickMarks
  , setNumberOfMajorTickMarks
  , fillColor
  , setFillColor
  , warningFillColor
  , setWarningFillColor
  , criticalFillColor
  , setCriticalFillColor
  , drawsTieredCapacityLevels
  , setDrawsTieredCapacityLevels
  , placeholderVisibility
  , setPlaceholderVisibility
  , ratingImage
  , setRatingImage
  , ratingPlaceholderImage
  , setRatingPlaceholderImage
  , criticalFillColorSelector
  , criticalValueSelector
  , drawsTieredCapacityLevelsSelector
  , editableSelector
  , fillColorSelector
  , levelIndicatorStyleSelector
  , maxValueSelector
  , minValueSelector
  , numberOfMajorTickMarksSelector
  , numberOfTickMarksSelector
  , placeholderVisibilitySelector
  , ratingImageSelector
  , ratingPlaceholderImageSelector
  , rectOfTickMarkAtIndexSelector
  , setCriticalFillColorSelector
  , setCriticalValueSelector
  , setDrawsTieredCapacityLevelsSelector
  , setEditableSelector
  , setFillColorSelector
  , setLevelIndicatorStyleSelector
  , setMaxValueSelector
  , setMinValueSelector
  , setNumberOfMajorTickMarksSelector
  , setNumberOfTickMarksSelector
  , setPlaceholderVisibilitySelector
  , setRatingImageSelector
  , setRatingPlaceholderImageSelector
  , setTickMarkPositionSelector
  , setWarningFillColorSelector
  , setWarningValueSelector
  , tickMarkPositionSelector
  , tickMarkValueAtIndexSelector
  , warningFillColorSelector
  , warningValueSelector

  -- * Enum types
  , NSLevelIndicatorPlaceholderVisibility(NSLevelIndicatorPlaceholderVisibility)
  , pattern NSLevelIndicatorPlaceholderVisibilityAutomatic
  , pattern NSLevelIndicatorPlaceholderVisibilityAlways
  , pattern NSLevelIndicatorPlaceholderVisibilityWhileEditing
  , NSLevelIndicatorStyle(NSLevelIndicatorStyle)
  , pattern NSLevelIndicatorStyleRelevancy
  , pattern NSLevelIndicatorStyleContinuousCapacity
  , pattern NSLevelIndicatorStyleDiscreteCapacity
  , pattern NSLevelIndicatorStyleRating
  , NSTickMarkPosition(NSTickMarkPosition)
  , pattern NSTickMarkPositionBelow
  , pattern NSTickMarkPositionAbove
  , pattern NSTickMarkPositionLeading
  , pattern NSTickMarkPositionTrailing

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

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO CDouble
tickMarkValueAtIndex nsLevelIndicator index =
  sendMessage nsLevelIndicator tickMarkValueAtIndexSelector index

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsLevelIndicator index =
  sendMessage nsLevelIndicator rectOfTickMarkAtIndexSelector index

-- | @- levelIndicatorStyle@
levelIndicatorStyle :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO NSLevelIndicatorStyle
levelIndicatorStyle nsLevelIndicator =
  sendMessage nsLevelIndicator levelIndicatorStyleSelector

-- | @- setLevelIndicatorStyle:@
setLevelIndicatorStyle :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> NSLevelIndicatorStyle -> IO ()
setLevelIndicatorStyle nsLevelIndicator value =
  sendMessage nsLevelIndicator setLevelIndicatorStyleSelector value

-- | @- editable@
editable :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO Bool
editable nsLevelIndicator =
  sendMessage nsLevelIndicator editableSelector

-- | @- setEditable:@
setEditable :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> Bool -> IO ()
setEditable nsLevelIndicator value =
  sendMessage nsLevelIndicator setEditableSelector value

-- | @- minValue@
minValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
minValue nsLevelIndicator =
  sendMessage nsLevelIndicator minValueSelector

-- | @- setMinValue:@
setMinValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setMinValue nsLevelIndicator value =
  sendMessage nsLevelIndicator setMinValueSelector value

-- | @- maxValue@
maxValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
maxValue nsLevelIndicator =
  sendMessage nsLevelIndicator maxValueSelector

-- | @- setMaxValue:@
setMaxValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setMaxValue nsLevelIndicator value =
  sendMessage nsLevelIndicator setMaxValueSelector value

-- | @- warningValue@
warningValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
warningValue nsLevelIndicator =
  sendMessage nsLevelIndicator warningValueSelector

-- | @- setWarningValue:@
setWarningValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setWarningValue nsLevelIndicator value =
  sendMessage nsLevelIndicator setWarningValueSelector value

-- | @- criticalValue@
criticalValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
criticalValue nsLevelIndicator =
  sendMessage nsLevelIndicator criticalValueSelector

-- | @- setCriticalValue:@
setCriticalValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setCriticalValue nsLevelIndicator value =
  sendMessage nsLevelIndicator setCriticalValueSelector value

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO NSTickMarkPosition
tickMarkPosition nsLevelIndicator =
  sendMessage nsLevelIndicator tickMarkPositionSelector

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsLevelIndicator value =
  sendMessage nsLevelIndicator setTickMarkPositionSelector value

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CLong
numberOfTickMarks nsLevelIndicator =
  sendMessage nsLevelIndicator numberOfTickMarksSelector

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO ()
setNumberOfTickMarks nsLevelIndicator value =
  sendMessage nsLevelIndicator setNumberOfTickMarksSelector value

-- | @- numberOfMajorTickMarks@
numberOfMajorTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CLong
numberOfMajorTickMarks nsLevelIndicator =
  sendMessage nsLevelIndicator numberOfMajorTickMarksSelector

-- | @- setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO ()
setNumberOfMajorTickMarks nsLevelIndicator value =
  sendMessage nsLevelIndicator setNumberOfMajorTickMarksSelector value

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing the "normal" state, and by the Rating indicator when drawing stars. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- fillColor@
fillColor :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSColor)
fillColor nsLevelIndicator =
  sendMessage nsLevelIndicator fillColorSelector

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing the "normal" state, and by the Rating indicator when drawing stars. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- setFillColor:@
setFillColor :: (IsNSLevelIndicator nsLevelIndicator, IsNSColor value) => nsLevelIndicator -> value -> IO ()
setFillColor nsLevelIndicator value =
  sendMessage nsLevelIndicator setFillColorSelector (toNSColor value)

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "warning" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- warningFillColor@
warningFillColor :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSColor)
warningFillColor nsLevelIndicator =
  sendMessage nsLevelIndicator warningFillColorSelector

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "warning" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- setWarningFillColor:@
setWarningFillColor :: (IsNSLevelIndicator nsLevelIndicator, IsNSColor value) => nsLevelIndicator -> value -> IO ()
setWarningFillColor nsLevelIndicator value =
  sendMessage nsLevelIndicator setWarningFillColorSelector (toNSColor value)

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "critical" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- criticalFillColor@
criticalFillColor :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSColor)
criticalFillColor nsLevelIndicator =
  sendMessage nsLevelIndicator criticalFillColorSelector

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "critical" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- setCriticalFillColor:@
setCriticalFillColor :: (IsNSLevelIndicator nsLevelIndicator, IsNSColor value) => nsLevelIndicator -> value -> IO ()
setCriticalFillColor nsLevelIndicator value =
  sendMessage nsLevelIndicator setCriticalFillColorSelector (toNSColor value)

-- | @- drawsTieredCapacityLevels@
drawsTieredCapacityLevels :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO Bool
drawsTieredCapacityLevels nsLevelIndicator =
  sendMessage nsLevelIndicator drawsTieredCapacityLevelsSelector

-- | @- setDrawsTieredCapacityLevels:@
setDrawsTieredCapacityLevels :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> Bool -> IO ()
setDrawsTieredCapacityLevels nsLevelIndicator value =
  sendMessage nsLevelIndicator setDrawsTieredCapacityLevelsSelector value

-- | For a Rating-style indicator, sets the conditions under which Rating placeholders are displayed. This property currently has no effect for other indicator styles. The default value is @NSLevelIndicatorPlaceholderVisibilityAutomatic.@
--
-- ObjC selector: @- placeholderVisibility@
placeholderVisibility :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO NSLevelIndicatorPlaceholderVisibility
placeholderVisibility nsLevelIndicator =
  sendMessage nsLevelIndicator placeholderVisibilitySelector

-- | For a Rating-style indicator, sets the conditions under which Rating placeholders are displayed. This property currently has no effect for other indicator styles. The default value is @NSLevelIndicatorPlaceholderVisibilityAutomatic.@
--
-- ObjC selector: @- setPlaceholderVisibility:@
setPlaceholderVisibility :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> NSLevelIndicatorPlaceholderVisibility -> IO ()
setPlaceholderVisibility nsLevelIndicator value =
  sendMessage nsLevelIndicator setPlaceholderVisibilitySelector value

-- | If non-nil, sets the image used by the Rating indicator style in place of the default star image. The default value is nil.
--
-- ObjC selector: @- ratingImage@
ratingImage :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSImage)
ratingImage nsLevelIndicator =
  sendMessage nsLevelIndicator ratingImageSelector

-- | If non-nil, sets the image used by the Rating indicator style in place of the default star image. The default value is nil.
--
-- ObjC selector: @- setRatingImage:@
setRatingImage :: (IsNSLevelIndicator nsLevelIndicator, IsNSImage value) => nsLevelIndicator -> value -> IO ()
setRatingImage nsLevelIndicator value =
  sendMessage nsLevelIndicator setRatingImageSelector (toNSImage value)

-- | If non-nil, sets the image used by the Rating indicator style in place of the default faded placeholder image. The default value is nil.
--
-- If the custom placeholder is a template image, its fill opacity can be adjusted by modifying the opacity of the template image.
--
-- If both a ratingImage and ratingPlaceholderImage are set, each rating position is sized such that either image will fit without scaling (i.e. sized to the maximum width and height of both images).
--
-- ObjC selector: @- ratingPlaceholderImage@
ratingPlaceholderImage :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSImage)
ratingPlaceholderImage nsLevelIndicator =
  sendMessage nsLevelIndicator ratingPlaceholderImageSelector

-- | If non-nil, sets the image used by the Rating indicator style in place of the default faded placeholder image. The default value is nil.
--
-- If the custom placeholder is a template image, its fill opacity can be adjusted by modifying the opacity of the template image.
--
-- If both a ratingImage and ratingPlaceholderImage are set, each rating position is sized such that either image will fit without scaling (i.e. sized to the maximum width and height of both images).
--
-- ObjC selector: @- setRatingPlaceholderImage:@
setRatingPlaceholderImage :: (IsNSLevelIndicator nsLevelIndicator, IsNSImage value) => nsLevelIndicator -> value -> IO ()
setRatingPlaceholderImage nsLevelIndicator value =
  sendMessage nsLevelIndicator setRatingPlaceholderImageSelector (toNSImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tickMarkValueAtIndex:@
tickMarkValueAtIndexSelector :: Selector '[CLong] CDouble
tickMarkValueAtIndexSelector = mkSelector "tickMarkValueAtIndex:"

-- | @Selector@ for @rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndexSelector :: Selector '[CLong] NSRect
rectOfTickMarkAtIndexSelector = mkSelector "rectOfTickMarkAtIndex:"

-- | @Selector@ for @levelIndicatorStyle@
levelIndicatorStyleSelector :: Selector '[] NSLevelIndicatorStyle
levelIndicatorStyleSelector = mkSelector "levelIndicatorStyle"

-- | @Selector@ for @setLevelIndicatorStyle:@
setLevelIndicatorStyleSelector :: Selector '[NSLevelIndicatorStyle] ()
setLevelIndicatorStyleSelector = mkSelector "setLevelIndicatorStyle:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CDouble
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector '[CDouble] ()
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CDouble
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector '[CDouble] ()
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @warningValue@
warningValueSelector :: Selector '[] CDouble
warningValueSelector = mkSelector "warningValue"

-- | @Selector@ for @setWarningValue:@
setWarningValueSelector :: Selector '[CDouble] ()
setWarningValueSelector = mkSelector "setWarningValue:"

-- | @Selector@ for @criticalValue@
criticalValueSelector :: Selector '[] CDouble
criticalValueSelector = mkSelector "criticalValue"

-- | @Selector@ for @setCriticalValue:@
setCriticalValueSelector :: Selector '[CDouble] ()
setCriticalValueSelector = mkSelector "setCriticalValue:"

-- | @Selector@ for @tickMarkPosition@
tickMarkPositionSelector :: Selector '[] NSTickMarkPosition
tickMarkPositionSelector = mkSelector "tickMarkPosition"

-- | @Selector@ for @setTickMarkPosition:@
setTickMarkPositionSelector :: Selector '[NSTickMarkPosition] ()
setTickMarkPositionSelector = mkSelector "setTickMarkPosition:"

-- | @Selector@ for @numberOfTickMarks@
numberOfTickMarksSelector :: Selector '[] CLong
numberOfTickMarksSelector = mkSelector "numberOfTickMarks"

-- | @Selector@ for @setNumberOfTickMarks:@
setNumberOfTickMarksSelector :: Selector '[CLong] ()
setNumberOfTickMarksSelector = mkSelector "setNumberOfTickMarks:"

-- | @Selector@ for @numberOfMajorTickMarks@
numberOfMajorTickMarksSelector :: Selector '[] CLong
numberOfMajorTickMarksSelector = mkSelector "numberOfMajorTickMarks"

-- | @Selector@ for @setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarksSelector :: Selector '[CLong] ()
setNumberOfMajorTickMarksSelector = mkSelector "setNumberOfMajorTickMarks:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector '[] (Id NSColor)
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector '[Id NSColor] ()
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @warningFillColor@
warningFillColorSelector :: Selector '[] (Id NSColor)
warningFillColorSelector = mkSelector "warningFillColor"

-- | @Selector@ for @setWarningFillColor:@
setWarningFillColorSelector :: Selector '[Id NSColor] ()
setWarningFillColorSelector = mkSelector "setWarningFillColor:"

-- | @Selector@ for @criticalFillColor@
criticalFillColorSelector :: Selector '[] (Id NSColor)
criticalFillColorSelector = mkSelector "criticalFillColor"

-- | @Selector@ for @setCriticalFillColor:@
setCriticalFillColorSelector :: Selector '[Id NSColor] ()
setCriticalFillColorSelector = mkSelector "setCriticalFillColor:"

-- | @Selector@ for @drawsTieredCapacityLevels@
drawsTieredCapacityLevelsSelector :: Selector '[] Bool
drawsTieredCapacityLevelsSelector = mkSelector "drawsTieredCapacityLevels"

-- | @Selector@ for @setDrawsTieredCapacityLevels:@
setDrawsTieredCapacityLevelsSelector :: Selector '[Bool] ()
setDrawsTieredCapacityLevelsSelector = mkSelector "setDrawsTieredCapacityLevels:"

-- | @Selector@ for @placeholderVisibility@
placeholderVisibilitySelector :: Selector '[] NSLevelIndicatorPlaceholderVisibility
placeholderVisibilitySelector = mkSelector "placeholderVisibility"

-- | @Selector@ for @setPlaceholderVisibility:@
setPlaceholderVisibilitySelector :: Selector '[NSLevelIndicatorPlaceholderVisibility] ()
setPlaceholderVisibilitySelector = mkSelector "setPlaceholderVisibility:"

-- | @Selector@ for @ratingImage@
ratingImageSelector :: Selector '[] (Id NSImage)
ratingImageSelector = mkSelector "ratingImage"

-- | @Selector@ for @setRatingImage:@
setRatingImageSelector :: Selector '[Id NSImage] ()
setRatingImageSelector = mkSelector "setRatingImage:"

-- | @Selector@ for @ratingPlaceholderImage@
ratingPlaceholderImageSelector :: Selector '[] (Id NSImage)
ratingPlaceholderImageSelector = mkSelector "ratingPlaceholderImage"

-- | @Selector@ for @setRatingPlaceholderImage:@
setRatingPlaceholderImageSelector :: Selector '[Id NSImage] ()
setRatingPlaceholderImageSelector = mkSelector "setRatingPlaceholderImage:"

