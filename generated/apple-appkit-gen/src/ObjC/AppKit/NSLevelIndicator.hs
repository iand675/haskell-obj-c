{-# LANGUAGE PatternSynonyms #-}
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
  , tickMarkValueAtIndexSelector
  , rectOfTickMarkAtIndexSelector
  , levelIndicatorStyleSelector
  , setLevelIndicatorStyleSelector
  , editableSelector
  , setEditableSelector
  , minValueSelector
  , setMinValueSelector
  , maxValueSelector
  , setMaxValueSelector
  , warningValueSelector
  , setWarningValueSelector
  , criticalValueSelector
  , setCriticalValueSelector
  , tickMarkPositionSelector
  , setTickMarkPositionSelector
  , numberOfTickMarksSelector
  , setNumberOfTickMarksSelector
  , numberOfMajorTickMarksSelector
  , setNumberOfMajorTickMarksSelector
  , fillColorSelector
  , setFillColorSelector
  , warningFillColorSelector
  , setWarningFillColorSelector
  , criticalFillColorSelector
  , setCriticalFillColorSelector
  , drawsTieredCapacityLevelsSelector
  , setDrawsTieredCapacityLevelsSelector
  , placeholderVisibilitySelector
  , setPlaceholderVisibilitySelector
  , ratingImageSelector
  , setRatingImageSelector
  , ratingPlaceholderImageSelector
  , setRatingPlaceholderImageSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO CDouble
tickMarkValueAtIndex nsLevelIndicator  index =
    sendMsg nsLevelIndicator (mkSelector "tickMarkValueAtIndex:") retCDouble [argCLong index]

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsLevelIndicator  index =
    sendMsgStret nsLevelIndicator (mkSelector "rectOfTickMarkAtIndex:") retNSRect [argCLong index]

-- | @- levelIndicatorStyle@
levelIndicatorStyle :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO NSLevelIndicatorStyle
levelIndicatorStyle nsLevelIndicator  =
    fmap (coerce :: CULong -> NSLevelIndicatorStyle) $ sendMsg nsLevelIndicator (mkSelector "levelIndicatorStyle") retCULong []

-- | @- setLevelIndicatorStyle:@
setLevelIndicatorStyle :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> NSLevelIndicatorStyle -> IO ()
setLevelIndicatorStyle nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setLevelIndicatorStyle:") retVoid [argCULong (coerce value)]

-- | @- editable@
editable :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO Bool
editable nsLevelIndicator  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLevelIndicator (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> Bool -> IO ()
setEditable nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minValue@
minValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
minValue nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setMinValue nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setMinValue:") retVoid [argCDouble value]

-- | @- maxValue@
maxValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
maxValue nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setMaxValue nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setMaxValue:") retVoid [argCDouble value]

-- | @- warningValue@
warningValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
warningValue nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "warningValue") retCDouble []

-- | @- setWarningValue:@
setWarningValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setWarningValue nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setWarningValue:") retVoid [argCDouble value]

-- | @- criticalValue@
criticalValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CDouble
criticalValue nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "criticalValue") retCDouble []

-- | @- setCriticalValue:@
setCriticalValue :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CDouble -> IO ()
setCriticalValue nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setCriticalValue:") retVoid [argCDouble value]

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO NSTickMarkPosition
tickMarkPosition nsLevelIndicator  =
    fmap (coerce :: CULong -> NSTickMarkPosition) $ sendMsg nsLevelIndicator (mkSelector "tickMarkPosition") retCULong []

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setTickMarkPosition:") retVoid [argCULong (coerce value)]

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CLong
numberOfTickMarks nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "numberOfTickMarks") retCLong []

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO ()
setNumberOfTickMarks nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setNumberOfTickMarks:") retVoid [argCLong value]

-- | @- numberOfMajorTickMarks@
numberOfMajorTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO CLong
numberOfMajorTickMarks nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "numberOfMajorTickMarks") retCLong []

-- | @- setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarks :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> CLong -> IO ()
setNumberOfMajorTickMarks nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setNumberOfMajorTickMarks:") retVoid [argCLong value]

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing the "normal" state, and by the Rating indicator when drawing stars. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- fillColor@
fillColor :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSColor)
fillColor nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "fillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing the "normal" state, and by the Rating indicator when drawing stars. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- setFillColor:@
setFillColor :: (IsNSLevelIndicator nsLevelIndicator, IsNSColor value) => nsLevelIndicator -> value -> IO ()
setFillColor nsLevelIndicator  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsLevelIndicator (mkSelector "setFillColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "warning" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- warningFillColor@
warningFillColor :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSColor)
warningFillColor nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "warningFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "warning" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- setWarningFillColor:@
setWarningFillColor :: (IsNSLevelIndicator nsLevelIndicator, IsNSColor value) => nsLevelIndicator -> value -> IO ()
setWarningFillColor nsLevelIndicator  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsLevelIndicator (mkSelector "setWarningFillColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "critical" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- criticalFillColor@
criticalFillColor :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSColor)
criticalFillColor nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "criticalFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the fill color used by Continuous and Discrete Capacity indicators when drawing values above the "critical" threshold. The default value is a system-defined color which may vary between level indicator styles and OS releases.
--
-- ObjC selector: @- setCriticalFillColor:@
setCriticalFillColor :: (IsNSLevelIndicator nsLevelIndicator, IsNSColor value) => nsLevelIndicator -> value -> IO ()
setCriticalFillColor nsLevelIndicator  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsLevelIndicator (mkSelector "setCriticalFillColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- drawsTieredCapacityLevels@
drawsTieredCapacityLevels :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO Bool
drawsTieredCapacityLevels nsLevelIndicator  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLevelIndicator (mkSelector "drawsTieredCapacityLevels") retCULong []

-- | @- setDrawsTieredCapacityLevels:@
setDrawsTieredCapacityLevels :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> Bool -> IO ()
setDrawsTieredCapacityLevels nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setDrawsTieredCapacityLevels:") retVoid [argCULong (if value then 1 else 0)]

-- | For a Rating-style indicator, sets the conditions under which Rating placeholders are displayed. This property currently has no effect for other indicator styles. The default value is @NSLevelIndicatorPlaceholderVisibilityAutomatic.@
--
-- ObjC selector: @- placeholderVisibility@
placeholderVisibility :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO NSLevelIndicatorPlaceholderVisibility
placeholderVisibility nsLevelIndicator  =
    fmap (coerce :: CLong -> NSLevelIndicatorPlaceholderVisibility) $ sendMsg nsLevelIndicator (mkSelector "placeholderVisibility") retCLong []

-- | For a Rating-style indicator, sets the conditions under which Rating placeholders are displayed. This property currently has no effect for other indicator styles. The default value is @NSLevelIndicatorPlaceholderVisibilityAutomatic.@
--
-- ObjC selector: @- setPlaceholderVisibility:@
setPlaceholderVisibility :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> NSLevelIndicatorPlaceholderVisibility -> IO ()
setPlaceholderVisibility nsLevelIndicator  value =
    sendMsg nsLevelIndicator (mkSelector "setPlaceholderVisibility:") retVoid [argCLong (coerce value)]

-- | If non-nil, sets the image used by the Rating indicator style in place of the default star image. The default value is nil.
--
-- ObjC selector: @- ratingImage@
ratingImage :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSImage)
ratingImage nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "ratingImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If non-nil, sets the image used by the Rating indicator style in place of the default star image. The default value is nil.
--
-- ObjC selector: @- setRatingImage:@
setRatingImage :: (IsNSLevelIndicator nsLevelIndicator, IsNSImage value) => nsLevelIndicator -> value -> IO ()
setRatingImage nsLevelIndicator  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsLevelIndicator (mkSelector "setRatingImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If non-nil, sets the image used by the Rating indicator style in place of the default faded placeholder image. The default value is nil.
--
-- If the custom placeholder is a template image, its fill opacity can be adjusted by modifying the opacity of the template image.
--
-- If both a ratingImage and ratingPlaceholderImage are set, each rating position is sized such that either image will fit without scaling (i.e. sized to the maximum width and height of both images).
--
-- ObjC selector: @- ratingPlaceholderImage@
ratingPlaceholderImage :: IsNSLevelIndicator nsLevelIndicator => nsLevelIndicator -> IO (Id NSImage)
ratingPlaceholderImage nsLevelIndicator  =
    sendMsg nsLevelIndicator (mkSelector "ratingPlaceholderImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If non-nil, sets the image used by the Rating indicator style in place of the default faded placeholder image. The default value is nil.
--
-- If the custom placeholder is a template image, its fill opacity can be adjusted by modifying the opacity of the template image.
--
-- If both a ratingImage and ratingPlaceholderImage are set, each rating position is sized such that either image will fit without scaling (i.e. sized to the maximum width and height of both images).
--
-- ObjC selector: @- setRatingPlaceholderImage:@
setRatingPlaceholderImage :: (IsNSLevelIndicator nsLevelIndicator, IsNSImage value) => nsLevelIndicator -> value -> IO ()
setRatingPlaceholderImage nsLevelIndicator  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsLevelIndicator (mkSelector "setRatingPlaceholderImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tickMarkValueAtIndex:@
tickMarkValueAtIndexSelector :: Selector
tickMarkValueAtIndexSelector = mkSelector "tickMarkValueAtIndex:"

-- | @Selector@ for @rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndexSelector :: Selector
rectOfTickMarkAtIndexSelector = mkSelector "rectOfTickMarkAtIndex:"

-- | @Selector@ for @levelIndicatorStyle@
levelIndicatorStyleSelector :: Selector
levelIndicatorStyleSelector = mkSelector "levelIndicatorStyle"

-- | @Selector@ for @setLevelIndicatorStyle:@
setLevelIndicatorStyleSelector :: Selector
setLevelIndicatorStyleSelector = mkSelector "setLevelIndicatorStyle:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @warningValue@
warningValueSelector :: Selector
warningValueSelector = mkSelector "warningValue"

-- | @Selector@ for @setWarningValue:@
setWarningValueSelector :: Selector
setWarningValueSelector = mkSelector "setWarningValue:"

-- | @Selector@ for @criticalValue@
criticalValueSelector :: Selector
criticalValueSelector = mkSelector "criticalValue"

-- | @Selector@ for @setCriticalValue:@
setCriticalValueSelector :: Selector
setCriticalValueSelector = mkSelector "setCriticalValue:"

-- | @Selector@ for @tickMarkPosition@
tickMarkPositionSelector :: Selector
tickMarkPositionSelector = mkSelector "tickMarkPosition"

-- | @Selector@ for @setTickMarkPosition:@
setTickMarkPositionSelector :: Selector
setTickMarkPositionSelector = mkSelector "setTickMarkPosition:"

-- | @Selector@ for @numberOfTickMarks@
numberOfTickMarksSelector :: Selector
numberOfTickMarksSelector = mkSelector "numberOfTickMarks"

-- | @Selector@ for @setNumberOfTickMarks:@
setNumberOfTickMarksSelector :: Selector
setNumberOfTickMarksSelector = mkSelector "setNumberOfTickMarks:"

-- | @Selector@ for @numberOfMajorTickMarks@
numberOfMajorTickMarksSelector :: Selector
numberOfMajorTickMarksSelector = mkSelector "numberOfMajorTickMarks"

-- | @Selector@ for @setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarksSelector :: Selector
setNumberOfMajorTickMarksSelector = mkSelector "setNumberOfMajorTickMarks:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @warningFillColor@
warningFillColorSelector :: Selector
warningFillColorSelector = mkSelector "warningFillColor"

-- | @Selector@ for @setWarningFillColor:@
setWarningFillColorSelector :: Selector
setWarningFillColorSelector = mkSelector "setWarningFillColor:"

-- | @Selector@ for @criticalFillColor@
criticalFillColorSelector :: Selector
criticalFillColorSelector = mkSelector "criticalFillColor"

-- | @Selector@ for @setCriticalFillColor:@
setCriticalFillColorSelector :: Selector
setCriticalFillColorSelector = mkSelector "setCriticalFillColor:"

-- | @Selector@ for @drawsTieredCapacityLevels@
drawsTieredCapacityLevelsSelector :: Selector
drawsTieredCapacityLevelsSelector = mkSelector "drawsTieredCapacityLevels"

-- | @Selector@ for @setDrawsTieredCapacityLevels:@
setDrawsTieredCapacityLevelsSelector :: Selector
setDrawsTieredCapacityLevelsSelector = mkSelector "setDrawsTieredCapacityLevels:"

-- | @Selector@ for @placeholderVisibility@
placeholderVisibilitySelector :: Selector
placeholderVisibilitySelector = mkSelector "placeholderVisibility"

-- | @Selector@ for @setPlaceholderVisibility:@
setPlaceholderVisibilitySelector :: Selector
setPlaceholderVisibilitySelector = mkSelector "setPlaceholderVisibility:"

-- | @Selector@ for @ratingImage@
ratingImageSelector :: Selector
ratingImageSelector = mkSelector "ratingImage"

-- | @Selector@ for @setRatingImage:@
setRatingImageSelector :: Selector
setRatingImageSelector = mkSelector "setRatingImage:"

-- | @Selector@ for @ratingPlaceholderImage@
ratingPlaceholderImageSelector :: Selector
ratingPlaceholderImageSelector = mkSelector "ratingPlaceholderImage"

-- | @Selector@ for @setRatingPlaceholderImage:@
setRatingPlaceholderImageSelector :: Selector
setRatingPlaceholderImageSelector = mkSelector "setRatingPlaceholderImage:"

