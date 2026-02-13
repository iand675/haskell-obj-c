{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSegmentedControl@.
module ObjC.AppKit.NSSegmentedControl
  ( NSSegmentedControl
  , IsNSSegmentedControl(..)
  , selectSegmentWithTag
  , setWidth_forSegment
  , widthForSegment
  , setImage_forSegment
  , imageForSegment
  , setImageScaling_forSegment
  , imageScalingForSegment
  , setLabel_forSegment
  , labelForSegment
  , setMenu_forSegment
  , menuForSegment
  , setSelected_forSegment
  , isSelectedForSegment
  , setEnabled_forSegment
  , isEnabledForSegment
  , setToolTip_forSegment
  , toolTipForSegment
  , setTag_forSegment
  , tagForSegment
  , setShowsMenuIndicator_forSegment
  , showsMenuIndicatorForSegment
  , setAlignment_forSegment
  , alignmentForSegment
  , compressWithPrioritizedCompressionOptions
  , minimumSizeWithPrioritizedCompressionOptions
  , segmentedControlWithLabels_trackingMode_target_action
  , segmentedControlWithImages_trackingMode_target_action
  , segmentCount
  , setSegmentCount
  , selectedSegment
  , setSelectedSegment
  , segmentStyle
  , setSegmentStyle
  , springLoaded
  , setSpringLoaded
  , trackingMode
  , setTrackingMode
  , doubleValueForSelectedSegment
  , selectedSegmentBezelColor
  , setSelectedSegmentBezelColor
  , indexOfSelectedItem
  , segmentDistribution
  , setSegmentDistribution
  , activeCompressionOptions
  , borderShape
  , setBorderShape
  , activeCompressionOptionsSelector
  , alignmentForSegmentSelector
  , borderShapeSelector
  , compressWithPrioritizedCompressionOptionsSelector
  , doubleValueForSelectedSegmentSelector
  , imageForSegmentSelector
  , imageScalingForSegmentSelector
  , indexOfSelectedItemSelector
  , isEnabledForSegmentSelector
  , isSelectedForSegmentSelector
  , labelForSegmentSelector
  , menuForSegmentSelector
  , minimumSizeWithPrioritizedCompressionOptionsSelector
  , segmentCountSelector
  , segmentDistributionSelector
  , segmentStyleSelector
  , segmentedControlWithImages_trackingMode_target_actionSelector
  , segmentedControlWithLabels_trackingMode_target_actionSelector
  , selectSegmentWithTagSelector
  , selectedSegmentBezelColorSelector
  , selectedSegmentSelector
  , setAlignment_forSegmentSelector
  , setBorderShapeSelector
  , setEnabled_forSegmentSelector
  , setImageScaling_forSegmentSelector
  , setImage_forSegmentSelector
  , setLabel_forSegmentSelector
  , setMenu_forSegmentSelector
  , setSegmentCountSelector
  , setSegmentDistributionSelector
  , setSegmentStyleSelector
  , setSelectedSegmentBezelColorSelector
  , setSelectedSegmentSelector
  , setSelected_forSegmentSelector
  , setShowsMenuIndicator_forSegmentSelector
  , setSpringLoadedSelector
  , setTag_forSegmentSelector
  , setToolTip_forSegmentSelector
  , setTrackingModeSelector
  , setWidth_forSegmentSelector
  , showsMenuIndicatorForSegmentSelector
  , springLoadedSelector
  , tagForSegmentSelector
  , toolTipForSegmentSelector
  , trackingModeSelector
  , widthForSegmentSelector

  -- * Enum types
  , NSControlBorderShape(NSControlBorderShape)
  , pattern NSControlBorderShapeAutomatic
  , pattern NSControlBorderShapeCapsule
  , pattern NSControlBorderShapeRoundedRectangle
  , pattern NSControlBorderShapeCircle
  , NSImageScaling(NSImageScaling)
  , pattern NSImageScaleProportionallyDown
  , pattern NSImageScaleAxesIndependently
  , pattern NSImageScaleNone
  , pattern NSImageScaleProportionallyUpOrDown
  , pattern NSScaleProportionally
  , pattern NSScaleToFit
  , pattern NSScaleNone
  , NSSegmentDistribution(NSSegmentDistribution)
  , pattern NSSegmentDistributionFit
  , pattern NSSegmentDistributionFill
  , pattern NSSegmentDistributionFillEqually
  , pattern NSSegmentDistributionFillProportionally
  , NSSegmentStyle(NSSegmentStyle)
  , pattern NSSegmentStyleAutomatic
  , pattern NSSegmentStyleRounded
  , pattern NSSegmentStyleRoundRect
  , pattern NSSegmentStyleTexturedSquare
  , pattern NSSegmentStyleSmallSquare
  , pattern NSSegmentStyleSeparated
  , pattern NSSegmentStyleTexturedRounded
  , pattern NSSegmentStyleCapsule
  , NSSegmentSwitchTracking(NSSegmentSwitchTracking)
  , pattern NSSegmentSwitchTrackingSelectOne
  , pattern NSSegmentSwitchTrackingSelectAny
  , pattern NSSegmentSwitchTrackingMomentary
  , pattern NSSegmentSwitchTrackingMomentaryAccelerator
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural

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

-- | @- selectSegmentWithTag:@
selectSegmentWithTag :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
selectSegmentWithTag nsSegmentedControl tag =
  sendMessage nsSegmentedControl selectSegmentWithTagSelector tag

-- | @- setWidth:forSegment:@
setWidth_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CDouble -> CLong -> IO ()
setWidth_forSegment nsSegmentedControl width segment =
  sendMessage nsSegmentedControl setWidth_forSegmentSelector width segment

-- | @- widthForSegment:@
widthForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO CDouble
widthForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl widthForSegmentSelector segment

-- | @- setImage:forSegment:@
setImage_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSImage image) => nsSegmentedControl -> image -> CLong -> IO ()
setImage_forSegment nsSegmentedControl image segment =
  sendMessage nsSegmentedControl setImage_forSegmentSelector (toNSImage image) segment

-- | @- imageForSegment:@
imageForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSImage)
imageForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl imageForSegmentSelector segment

-- | @- setImageScaling:forSegment:@
setImageScaling_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSImageScaling -> CLong -> IO ()
setImageScaling_forSegment nsSegmentedControl scaling segment =
  sendMessage nsSegmentedControl setImageScaling_forSegmentSelector scaling segment

-- | @- imageScalingForSegment:@
imageScalingForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO NSImageScaling
imageScalingForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl imageScalingForSegmentSelector segment

-- | @- setLabel:forSegment:@
setLabel_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSString label) => nsSegmentedControl -> label -> CLong -> IO ()
setLabel_forSegment nsSegmentedControl label segment =
  sendMessage nsSegmentedControl setLabel_forSegmentSelector (toNSString label) segment

-- | @- labelForSegment:@
labelForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSString)
labelForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl labelForSegmentSelector segment

-- | @- setMenu:forSegment:@
setMenu_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSMenu menu) => nsSegmentedControl -> menu -> CLong -> IO ()
setMenu_forSegment nsSegmentedControl menu segment =
  sendMessage nsSegmentedControl setMenu_forSegmentSelector (toNSMenu menu) segment

-- | @- menuForSegment:@
menuForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSMenu)
menuForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl menuForSegmentSelector segment

-- | @- setSelected:forSegment:@
setSelected_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> CLong -> IO ()
setSelected_forSegment nsSegmentedControl selected segment =
  sendMessage nsSegmentedControl setSelected_forSegmentSelector selected segment

-- | @- isSelectedForSegment:@
isSelectedForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
isSelectedForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl isSelectedForSegmentSelector segment

-- | @- setEnabled:forSegment:@
setEnabled_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> CLong -> IO ()
setEnabled_forSegment nsSegmentedControl enabled segment =
  sendMessage nsSegmentedControl setEnabled_forSegmentSelector enabled segment

-- | @- isEnabledForSegment:@
isEnabledForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
isEnabledForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl isEnabledForSegmentSelector segment

-- | @- setToolTip:forSegment:@
setToolTip_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSString toolTip) => nsSegmentedControl -> toolTip -> CLong -> IO ()
setToolTip_forSegment nsSegmentedControl toolTip segment =
  sendMessage nsSegmentedControl setToolTip_forSegmentSelector (toNSString toolTip) segment

-- | @- toolTipForSegment:@
toolTipForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSString)
toolTipForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl toolTipForSegmentSelector segment

-- | @- setTag:forSegment:@
setTag_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> CLong -> IO ()
setTag_forSegment nsSegmentedControl tag segment =
  sendMessage nsSegmentedControl setTag_forSegmentSelector tag segment

-- | @- tagForSegment:@
tagForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO CLong
tagForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl tagForSegmentSelector segment

-- | @- setShowsMenuIndicator:forSegment:@
setShowsMenuIndicator_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> CLong -> IO ()
setShowsMenuIndicator_forSegment nsSegmentedControl showsMenuIndicator segment =
  sendMessage nsSegmentedControl setShowsMenuIndicator_forSegmentSelector showsMenuIndicator segment

-- | @- showsMenuIndicatorForSegment:@
showsMenuIndicatorForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
showsMenuIndicatorForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl showsMenuIndicatorForSegmentSelector segment

-- | @- setAlignment:forSegment:@
setAlignment_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSTextAlignment -> CLong -> IO ()
setAlignment_forSegment nsSegmentedControl alignment segment =
  sendMessage nsSegmentedControl setAlignment_forSegmentSelector alignment segment

-- | @- alignmentForSegment:@
alignmentForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO NSTextAlignment
alignmentForSegment nsSegmentedControl segment =
  sendMessage nsSegmentedControl alignmentForSegmentSelector segment

-- | @- compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptions :: (IsNSSegmentedControl nsSegmentedControl, IsNSArray prioritizedOptions) => nsSegmentedControl -> prioritizedOptions -> IO ()
compressWithPrioritizedCompressionOptions nsSegmentedControl prioritizedOptions =
  sendMessage nsSegmentedControl compressWithPrioritizedCompressionOptionsSelector (toNSArray prioritizedOptions)

-- | @- minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptions :: (IsNSSegmentedControl nsSegmentedControl, IsNSArray prioritizedOptions) => nsSegmentedControl -> prioritizedOptions -> IO NSSize
minimumSizeWithPrioritizedCompressionOptions nsSegmentedControl prioritizedOptions =
  sendMessage nsSegmentedControl minimumSizeWithPrioritizedCompressionOptionsSelector (toNSArray prioritizedOptions)

-- | Creates a standard segmented control containing one segment for each of the provided labels.
--
-- @labels@ — An array of localized label strings to use for the control's segments.
--
-- @trackingMode@ — The selection mode for the control. The NSSegmentSwitchTracking enum describes the possible values and their effects.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized segmented control.
--
-- ObjC selector: @+ segmentedControlWithLabels:trackingMode:target:action:@
segmentedControlWithLabels_trackingMode_target_action :: IsNSArray labels => labels -> NSSegmentSwitchTracking -> RawId -> Sel -> IO (Id NSSegmentedControl)
segmentedControlWithLabels_trackingMode_target_action labels trackingMode target action =
  do
    cls' <- getRequiredClass "NSSegmentedControl"
    sendClassMessage cls' segmentedControlWithLabels_trackingMode_target_actionSelector (toNSArray labels) trackingMode target action

-- | Creates a standard segmented control containing one segment for each of the provided images. To ensure accessibility for this control, set the accessibilityDescription property on each of the provided images.
--
-- @images@ — An array of image objects to use for the control's segments.
--
-- @trackingMode@ — The selection mode for the control. The NSSegmentSwitchTracking enum describes the possible values and their effects.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized segmented control.
--
-- ObjC selector: @+ segmentedControlWithImages:trackingMode:target:action:@
segmentedControlWithImages_trackingMode_target_action :: IsNSArray images => images -> NSSegmentSwitchTracking -> RawId -> Sel -> IO (Id NSSegmentedControl)
segmentedControlWithImages_trackingMode_target_action images trackingMode target action =
  do
    cls' <- getRequiredClass "NSSegmentedControl"
    sendClassMessage cls' segmentedControlWithImages_trackingMode_target_actionSelector (toNSArray images) trackingMode target action

-- | @- segmentCount@
segmentCount :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CLong
segmentCount nsSegmentedControl =
  sendMessage nsSegmentedControl segmentCountSelector

-- | @- setSegmentCount:@
setSegmentCount :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO ()
setSegmentCount nsSegmentedControl value =
  sendMessage nsSegmentedControl setSegmentCountSelector value

-- | @- selectedSegment@
selectedSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CLong
selectedSegment nsSegmentedControl =
  sendMessage nsSegmentedControl selectedSegmentSelector

-- | @- setSelectedSegment:@
setSelectedSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO ()
setSelectedSegment nsSegmentedControl value =
  sendMessage nsSegmentedControl setSelectedSegmentSelector value

-- | @- segmentStyle@
segmentStyle :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSSegmentStyle
segmentStyle nsSegmentedControl =
  sendMessage nsSegmentedControl segmentStyleSelector

-- | @- setSegmentStyle:@
setSegmentStyle :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSSegmentStyle -> IO ()
setSegmentStyle nsSegmentedControl value =
  sendMessage nsSegmentedControl setSegmentStyleSelector value

-- | @- springLoaded@
springLoaded :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO Bool
springLoaded nsSegmentedControl =
  sendMessage nsSegmentedControl springLoadedSelector

-- | @- setSpringLoaded:@
setSpringLoaded :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> IO ()
setSpringLoaded nsSegmentedControl value =
  sendMessage nsSegmentedControl setSpringLoadedSelector value

-- | @- trackingMode@
trackingMode :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSSegmentSwitchTracking
trackingMode nsSegmentedControl =
  sendMessage nsSegmentedControl trackingModeSelector

-- | @- setTrackingMode:@
setTrackingMode :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSSegmentSwitchTracking -> IO ()
setTrackingMode nsSegmentedControl value =
  sendMessage nsSegmentedControl setTrackingModeSelector value

-- | @- doubleValueForSelectedSegment@
doubleValueForSelectedSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CDouble
doubleValueForSelectedSegment nsSegmentedControl =
  sendMessage nsSegmentedControl doubleValueForSelectedSegmentSelector

-- | @- selectedSegmentBezelColor@
selectedSegmentBezelColor :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO (Id NSColor)
selectedSegmentBezelColor nsSegmentedControl =
  sendMessage nsSegmentedControl selectedSegmentBezelColorSelector

-- | @- setSelectedSegmentBezelColor:@
setSelectedSegmentBezelColor :: (IsNSSegmentedControl nsSegmentedControl, IsNSColor value) => nsSegmentedControl -> value -> IO ()
setSelectedSegmentBezelColor nsSegmentedControl value =
  sendMessage nsSegmentedControl setSelectedSegmentBezelColorSelector (toNSColor value)

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CLong
indexOfSelectedItem nsSegmentedControl =
  sendMessage nsSegmentedControl indexOfSelectedItemSelector

-- | @- segmentDistribution@
segmentDistribution :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSSegmentDistribution
segmentDistribution nsSegmentedControl =
  sendMessage nsSegmentedControl segmentDistributionSelector

-- | @- setSegmentDistribution:@
setSegmentDistribution :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSSegmentDistribution -> IO ()
setSegmentDistribution nsSegmentedControl value =
  sendMessage nsSegmentedControl setSegmentDistributionSelector value

-- | @- activeCompressionOptions@
activeCompressionOptions :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO (Id NSUserInterfaceCompressionOptions)
activeCompressionOptions nsSegmentedControl =
  sendMessage nsSegmentedControl activeCompressionOptionsSelector

-- | @- borderShape@
borderShape :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSControlBorderShape
borderShape nsSegmentedControl =
  sendMessage nsSegmentedControl borderShapeSelector

-- | @- setBorderShape:@
setBorderShape :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSControlBorderShape -> IO ()
setBorderShape nsSegmentedControl value =
  sendMessage nsSegmentedControl setBorderShapeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectSegmentWithTag:@
selectSegmentWithTagSelector :: Selector '[CLong] Bool
selectSegmentWithTagSelector = mkSelector "selectSegmentWithTag:"

-- | @Selector@ for @setWidth:forSegment:@
setWidth_forSegmentSelector :: Selector '[CDouble, CLong] ()
setWidth_forSegmentSelector = mkSelector "setWidth:forSegment:"

-- | @Selector@ for @widthForSegment:@
widthForSegmentSelector :: Selector '[CLong] CDouble
widthForSegmentSelector = mkSelector "widthForSegment:"

-- | @Selector@ for @setImage:forSegment:@
setImage_forSegmentSelector :: Selector '[Id NSImage, CLong] ()
setImage_forSegmentSelector = mkSelector "setImage:forSegment:"

-- | @Selector@ for @imageForSegment:@
imageForSegmentSelector :: Selector '[CLong] (Id NSImage)
imageForSegmentSelector = mkSelector "imageForSegment:"

-- | @Selector@ for @setImageScaling:forSegment:@
setImageScaling_forSegmentSelector :: Selector '[NSImageScaling, CLong] ()
setImageScaling_forSegmentSelector = mkSelector "setImageScaling:forSegment:"

-- | @Selector@ for @imageScalingForSegment:@
imageScalingForSegmentSelector :: Selector '[CLong] NSImageScaling
imageScalingForSegmentSelector = mkSelector "imageScalingForSegment:"

-- | @Selector@ for @setLabel:forSegment:@
setLabel_forSegmentSelector :: Selector '[Id NSString, CLong] ()
setLabel_forSegmentSelector = mkSelector "setLabel:forSegment:"

-- | @Selector@ for @labelForSegment:@
labelForSegmentSelector :: Selector '[CLong] (Id NSString)
labelForSegmentSelector = mkSelector "labelForSegment:"

-- | @Selector@ for @setMenu:forSegment:@
setMenu_forSegmentSelector :: Selector '[Id NSMenu, CLong] ()
setMenu_forSegmentSelector = mkSelector "setMenu:forSegment:"

-- | @Selector@ for @menuForSegment:@
menuForSegmentSelector :: Selector '[CLong] (Id NSMenu)
menuForSegmentSelector = mkSelector "menuForSegment:"

-- | @Selector@ for @setSelected:forSegment:@
setSelected_forSegmentSelector :: Selector '[Bool, CLong] ()
setSelected_forSegmentSelector = mkSelector "setSelected:forSegment:"

-- | @Selector@ for @isSelectedForSegment:@
isSelectedForSegmentSelector :: Selector '[CLong] Bool
isSelectedForSegmentSelector = mkSelector "isSelectedForSegment:"

-- | @Selector@ for @setEnabled:forSegment:@
setEnabled_forSegmentSelector :: Selector '[Bool, CLong] ()
setEnabled_forSegmentSelector = mkSelector "setEnabled:forSegment:"

-- | @Selector@ for @isEnabledForSegment:@
isEnabledForSegmentSelector :: Selector '[CLong] Bool
isEnabledForSegmentSelector = mkSelector "isEnabledForSegment:"

-- | @Selector@ for @setToolTip:forSegment:@
setToolTip_forSegmentSelector :: Selector '[Id NSString, CLong] ()
setToolTip_forSegmentSelector = mkSelector "setToolTip:forSegment:"

-- | @Selector@ for @toolTipForSegment:@
toolTipForSegmentSelector :: Selector '[CLong] (Id NSString)
toolTipForSegmentSelector = mkSelector "toolTipForSegment:"

-- | @Selector@ for @setTag:forSegment:@
setTag_forSegmentSelector :: Selector '[CLong, CLong] ()
setTag_forSegmentSelector = mkSelector "setTag:forSegment:"

-- | @Selector@ for @tagForSegment:@
tagForSegmentSelector :: Selector '[CLong] CLong
tagForSegmentSelector = mkSelector "tagForSegment:"

-- | @Selector@ for @setShowsMenuIndicator:forSegment:@
setShowsMenuIndicator_forSegmentSelector :: Selector '[Bool, CLong] ()
setShowsMenuIndicator_forSegmentSelector = mkSelector "setShowsMenuIndicator:forSegment:"

-- | @Selector@ for @showsMenuIndicatorForSegment:@
showsMenuIndicatorForSegmentSelector :: Selector '[CLong] Bool
showsMenuIndicatorForSegmentSelector = mkSelector "showsMenuIndicatorForSegment:"

-- | @Selector@ for @setAlignment:forSegment:@
setAlignment_forSegmentSelector :: Selector '[NSTextAlignment, CLong] ()
setAlignment_forSegmentSelector = mkSelector "setAlignment:forSegment:"

-- | @Selector@ for @alignmentForSegment:@
alignmentForSegmentSelector :: Selector '[CLong] NSTextAlignment
alignmentForSegmentSelector = mkSelector "alignmentForSegment:"

-- | @Selector@ for @compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptionsSelector :: Selector '[Id NSArray] ()
compressWithPrioritizedCompressionOptionsSelector = mkSelector "compressWithPrioritizedCompressionOptions:"

-- | @Selector@ for @minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptionsSelector :: Selector '[Id NSArray] NSSize
minimumSizeWithPrioritizedCompressionOptionsSelector = mkSelector "minimumSizeWithPrioritizedCompressionOptions:"

-- | @Selector@ for @segmentedControlWithLabels:trackingMode:target:action:@
segmentedControlWithLabels_trackingMode_target_actionSelector :: Selector '[Id NSArray, NSSegmentSwitchTracking, RawId, Sel] (Id NSSegmentedControl)
segmentedControlWithLabels_trackingMode_target_actionSelector = mkSelector "segmentedControlWithLabels:trackingMode:target:action:"

-- | @Selector@ for @segmentedControlWithImages:trackingMode:target:action:@
segmentedControlWithImages_trackingMode_target_actionSelector :: Selector '[Id NSArray, NSSegmentSwitchTracking, RawId, Sel] (Id NSSegmentedControl)
segmentedControlWithImages_trackingMode_target_actionSelector = mkSelector "segmentedControlWithImages:trackingMode:target:action:"

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector '[] CLong
segmentCountSelector = mkSelector "segmentCount"

-- | @Selector@ for @setSegmentCount:@
setSegmentCountSelector :: Selector '[CLong] ()
setSegmentCountSelector = mkSelector "setSegmentCount:"

-- | @Selector@ for @selectedSegment@
selectedSegmentSelector :: Selector '[] CLong
selectedSegmentSelector = mkSelector "selectedSegment"

-- | @Selector@ for @setSelectedSegment:@
setSelectedSegmentSelector :: Selector '[CLong] ()
setSelectedSegmentSelector = mkSelector "setSelectedSegment:"

-- | @Selector@ for @segmentStyle@
segmentStyleSelector :: Selector '[] NSSegmentStyle
segmentStyleSelector = mkSelector "segmentStyle"

-- | @Selector@ for @setSegmentStyle:@
setSegmentStyleSelector :: Selector '[NSSegmentStyle] ()
setSegmentStyleSelector = mkSelector "setSegmentStyle:"

-- | @Selector@ for @springLoaded@
springLoadedSelector :: Selector '[] Bool
springLoadedSelector = mkSelector "springLoaded"

-- | @Selector@ for @setSpringLoaded:@
setSpringLoadedSelector :: Selector '[Bool] ()
setSpringLoadedSelector = mkSelector "setSpringLoaded:"

-- | @Selector@ for @trackingMode@
trackingModeSelector :: Selector '[] NSSegmentSwitchTracking
trackingModeSelector = mkSelector "trackingMode"

-- | @Selector@ for @setTrackingMode:@
setTrackingModeSelector :: Selector '[NSSegmentSwitchTracking] ()
setTrackingModeSelector = mkSelector "setTrackingMode:"

-- | @Selector@ for @doubleValueForSelectedSegment@
doubleValueForSelectedSegmentSelector :: Selector '[] CDouble
doubleValueForSelectedSegmentSelector = mkSelector "doubleValueForSelectedSegment"

-- | @Selector@ for @selectedSegmentBezelColor@
selectedSegmentBezelColorSelector :: Selector '[] (Id NSColor)
selectedSegmentBezelColorSelector = mkSelector "selectedSegmentBezelColor"

-- | @Selector@ for @setSelectedSegmentBezelColor:@
setSelectedSegmentBezelColorSelector :: Selector '[Id NSColor] ()
setSelectedSegmentBezelColorSelector = mkSelector "setSelectedSegmentBezelColor:"

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector '[] CLong
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @segmentDistribution@
segmentDistributionSelector :: Selector '[] NSSegmentDistribution
segmentDistributionSelector = mkSelector "segmentDistribution"

-- | @Selector@ for @setSegmentDistribution:@
setSegmentDistributionSelector :: Selector '[NSSegmentDistribution] ()
setSegmentDistributionSelector = mkSelector "setSegmentDistribution:"

-- | @Selector@ for @activeCompressionOptions@
activeCompressionOptionsSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
activeCompressionOptionsSelector = mkSelector "activeCompressionOptions"

-- | @Selector@ for @borderShape@
borderShapeSelector :: Selector '[] NSControlBorderShape
borderShapeSelector = mkSelector "borderShape"

-- | @Selector@ for @setBorderShape:@
setBorderShapeSelector :: Selector '[NSControlBorderShape] ()
setBorderShapeSelector = mkSelector "setBorderShape:"

