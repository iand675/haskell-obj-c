{-# LANGUAGE PatternSynonyms #-}
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
  , indexOfSelectedItem
  , segmentDistribution
  , setSegmentDistribution
  , borderShape
  , setBorderShape
  , selectSegmentWithTagSelector
  , setWidth_forSegmentSelector
  , widthForSegmentSelector
  , setImage_forSegmentSelector
  , imageForSegmentSelector
  , setImageScaling_forSegmentSelector
  , imageScalingForSegmentSelector
  , setLabel_forSegmentSelector
  , labelForSegmentSelector
  , setMenu_forSegmentSelector
  , menuForSegmentSelector
  , setSelected_forSegmentSelector
  , isSelectedForSegmentSelector
  , setEnabled_forSegmentSelector
  , isEnabledForSegmentSelector
  , setToolTip_forSegmentSelector
  , toolTipForSegmentSelector
  , setTag_forSegmentSelector
  , tagForSegmentSelector
  , setShowsMenuIndicator_forSegmentSelector
  , showsMenuIndicatorForSegmentSelector
  , setAlignment_forSegmentSelector
  , alignmentForSegmentSelector
  , compressWithPrioritizedCompressionOptionsSelector
  , minimumSizeWithPrioritizedCompressionOptionsSelector
  , segmentedControlWithLabels_trackingMode_target_actionSelector
  , segmentedControlWithImages_trackingMode_target_actionSelector
  , segmentCountSelector
  , setSegmentCountSelector
  , selectedSegmentSelector
  , setSelectedSegmentSelector
  , segmentStyleSelector
  , setSegmentStyleSelector
  , springLoadedSelector
  , setSpringLoadedSelector
  , trackingModeSelector
  , setTrackingModeSelector
  , doubleValueForSelectedSegmentSelector
  , indexOfSelectedItemSelector
  , segmentDistributionSelector
  , setSegmentDistributionSelector
  , borderShapeSelector
  , setBorderShapeSelector

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

-- | @- selectSegmentWithTag:@
selectSegmentWithTag :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
selectSegmentWithTag nsSegmentedControl  tag =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedControl (mkSelector "selectSegmentWithTag:") retCULong [argCLong (fromIntegral tag)]

-- | @- setWidth:forSegment:@
setWidth_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CDouble -> CLong -> IO ()
setWidth_forSegment nsSegmentedControl  width segment =
  sendMsg nsSegmentedControl (mkSelector "setWidth:forSegment:") retVoid [argCDouble (fromIntegral width), argCLong (fromIntegral segment)]

-- | @- widthForSegment:@
widthForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO CDouble
widthForSegment nsSegmentedControl  segment =
  sendMsg nsSegmentedControl (mkSelector "widthForSegment:") retCDouble [argCLong (fromIntegral segment)]

-- | @- setImage:forSegment:@
setImage_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSImage image) => nsSegmentedControl -> image -> CLong -> IO ()
setImage_forSegment nsSegmentedControl  image segment =
withObjCPtr image $ \raw_image ->
    sendMsg nsSegmentedControl (mkSelector "setImage:forSegment:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- imageForSegment:@
imageForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSImage)
imageForSegment nsSegmentedControl  segment =
  sendMsg nsSegmentedControl (mkSelector "imageForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setImageScaling:forSegment:@
setImageScaling_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSImageScaling -> CLong -> IO ()
setImageScaling_forSegment nsSegmentedControl  scaling segment =
  sendMsg nsSegmentedControl (mkSelector "setImageScaling:forSegment:") retVoid [argCULong (coerce scaling), argCLong (fromIntegral segment)]

-- | @- imageScalingForSegment:@
imageScalingForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO NSImageScaling
imageScalingForSegment nsSegmentedControl  segment =
  fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsSegmentedControl (mkSelector "imageScalingForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setLabel:forSegment:@
setLabel_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSString label) => nsSegmentedControl -> label -> CLong -> IO ()
setLabel_forSegment nsSegmentedControl  label segment =
withObjCPtr label $ \raw_label ->
    sendMsg nsSegmentedControl (mkSelector "setLabel:forSegment:") retVoid [argPtr (castPtr raw_label :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- labelForSegment:@
labelForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSString)
labelForSegment nsSegmentedControl  segment =
  sendMsg nsSegmentedControl (mkSelector "labelForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setMenu:forSegment:@
setMenu_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSMenu menu) => nsSegmentedControl -> menu -> CLong -> IO ()
setMenu_forSegment nsSegmentedControl  menu segment =
withObjCPtr menu $ \raw_menu ->
    sendMsg nsSegmentedControl (mkSelector "setMenu:forSegment:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- menuForSegment:@
menuForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSMenu)
menuForSegment nsSegmentedControl  segment =
  sendMsg nsSegmentedControl (mkSelector "menuForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setSelected:forSegment:@
setSelected_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> CLong -> IO ()
setSelected_forSegment nsSegmentedControl  selected segment =
  sendMsg nsSegmentedControl (mkSelector "setSelected:forSegment:") retVoid [argCULong (if selected then 1 else 0), argCLong (fromIntegral segment)]

-- | @- isSelectedForSegment:@
isSelectedForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
isSelectedForSegment nsSegmentedControl  segment =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedControl (mkSelector "isSelectedForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setEnabled:forSegment:@
setEnabled_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> CLong -> IO ()
setEnabled_forSegment nsSegmentedControl  enabled segment =
  sendMsg nsSegmentedControl (mkSelector "setEnabled:forSegment:") retVoid [argCULong (if enabled then 1 else 0), argCLong (fromIntegral segment)]

-- | @- isEnabledForSegment:@
isEnabledForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
isEnabledForSegment nsSegmentedControl  segment =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedControl (mkSelector "isEnabledForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setToolTip:forSegment:@
setToolTip_forSegment :: (IsNSSegmentedControl nsSegmentedControl, IsNSString toolTip) => nsSegmentedControl -> toolTip -> CLong -> IO ()
setToolTip_forSegment nsSegmentedControl  toolTip segment =
withObjCPtr toolTip $ \raw_toolTip ->
    sendMsg nsSegmentedControl (mkSelector "setToolTip:forSegment:") retVoid [argPtr (castPtr raw_toolTip :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- toolTipForSegment:@
toolTipForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO (Id NSString)
toolTipForSegment nsSegmentedControl  segment =
  sendMsg nsSegmentedControl (mkSelector "toolTipForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setTag:forSegment:@
setTag_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> CLong -> IO ()
setTag_forSegment nsSegmentedControl  tag segment =
  sendMsg nsSegmentedControl (mkSelector "setTag:forSegment:") retVoid [argCLong (fromIntegral tag), argCLong (fromIntegral segment)]

-- | @- tagForSegment:@
tagForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO CLong
tagForSegment nsSegmentedControl  segment =
  sendMsg nsSegmentedControl (mkSelector "tagForSegment:") retCLong [argCLong (fromIntegral segment)]

-- | @- setShowsMenuIndicator:forSegment:@
setShowsMenuIndicator_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> CLong -> IO ()
setShowsMenuIndicator_forSegment nsSegmentedControl  showsMenuIndicator segment =
  sendMsg nsSegmentedControl (mkSelector "setShowsMenuIndicator:forSegment:") retVoid [argCULong (if showsMenuIndicator then 1 else 0), argCLong (fromIntegral segment)]

-- | @- showsMenuIndicatorForSegment:@
showsMenuIndicatorForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO Bool
showsMenuIndicatorForSegment nsSegmentedControl  segment =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedControl (mkSelector "showsMenuIndicatorForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setAlignment:forSegment:@
setAlignment_forSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSTextAlignment -> CLong -> IO ()
setAlignment_forSegment nsSegmentedControl  alignment segment =
  sendMsg nsSegmentedControl (mkSelector "setAlignment:forSegment:") retVoid [argCLong (coerce alignment), argCLong (fromIntegral segment)]

-- | @- alignmentForSegment:@
alignmentForSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO NSTextAlignment
alignmentForSegment nsSegmentedControl  segment =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsSegmentedControl (mkSelector "alignmentForSegment:") retCLong [argCLong (fromIntegral segment)]

-- | @- compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptions :: (IsNSSegmentedControl nsSegmentedControl, IsNSArray prioritizedOptions) => nsSegmentedControl -> prioritizedOptions -> IO ()
compressWithPrioritizedCompressionOptions nsSegmentedControl  prioritizedOptions =
withObjCPtr prioritizedOptions $ \raw_prioritizedOptions ->
    sendMsg nsSegmentedControl (mkSelector "compressWithPrioritizedCompressionOptions:") retVoid [argPtr (castPtr raw_prioritizedOptions :: Ptr ())]

-- | @- minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptions :: (IsNSSegmentedControl nsSegmentedControl, IsNSArray prioritizedOptions) => nsSegmentedControl -> prioritizedOptions -> IO NSSize
minimumSizeWithPrioritizedCompressionOptions nsSegmentedControl  prioritizedOptions =
withObjCPtr prioritizedOptions $ \raw_prioritizedOptions ->
    sendMsgStret nsSegmentedControl (mkSelector "minimumSizeWithPrioritizedCompressionOptions:") retNSSize [argPtr (castPtr raw_prioritizedOptions :: Ptr ())]

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
segmentedControlWithLabels_trackingMode_target_action :: IsNSArray labels => labels -> NSSegmentSwitchTracking -> RawId -> Selector -> IO (Id NSSegmentedControl)
segmentedControlWithLabels_trackingMode_target_action labels trackingMode target action =
  do
    cls' <- getRequiredClass "NSSegmentedControl"
    withObjCPtr labels $ \raw_labels ->
      sendClassMsg cls' (mkSelector "segmentedControlWithLabels:trackingMode:target:action:") (retPtr retVoid) [argPtr (castPtr raw_labels :: Ptr ()), argCULong (coerce trackingMode), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

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
segmentedControlWithImages_trackingMode_target_action :: IsNSArray images => images -> NSSegmentSwitchTracking -> RawId -> Selector -> IO (Id NSSegmentedControl)
segmentedControlWithImages_trackingMode_target_action images trackingMode target action =
  do
    cls' <- getRequiredClass "NSSegmentedControl"
    withObjCPtr images $ \raw_images ->
      sendClassMsg cls' (mkSelector "segmentedControlWithImages:trackingMode:target:action:") (retPtr retVoid) [argPtr (castPtr raw_images :: Ptr ()), argCULong (coerce trackingMode), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @- segmentCount@
segmentCount :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CLong
segmentCount nsSegmentedControl  =
  sendMsg nsSegmentedControl (mkSelector "segmentCount") retCLong []

-- | @- setSegmentCount:@
setSegmentCount :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO ()
setSegmentCount nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | @- selectedSegment@
selectedSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CLong
selectedSegment nsSegmentedControl  =
  sendMsg nsSegmentedControl (mkSelector "selectedSegment") retCLong []

-- | @- setSelectedSegment:@
setSelectedSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> CLong -> IO ()
setSelectedSegment nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setSelectedSegment:") retVoid [argCLong (fromIntegral value)]

-- | @- segmentStyle@
segmentStyle :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSSegmentStyle
segmentStyle nsSegmentedControl  =
  fmap (coerce :: CLong -> NSSegmentStyle) $ sendMsg nsSegmentedControl (mkSelector "segmentStyle") retCLong []

-- | @- setSegmentStyle:@
setSegmentStyle :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSSegmentStyle -> IO ()
setSegmentStyle nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setSegmentStyle:") retVoid [argCLong (coerce value)]

-- | @- springLoaded@
springLoaded :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO Bool
springLoaded nsSegmentedControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedControl (mkSelector "springLoaded") retCULong []

-- | @- setSpringLoaded:@
setSpringLoaded :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> Bool -> IO ()
setSpringLoaded nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setSpringLoaded:") retVoid [argCULong (if value then 1 else 0)]

-- | @- trackingMode@
trackingMode :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSSegmentSwitchTracking
trackingMode nsSegmentedControl  =
  fmap (coerce :: CULong -> NSSegmentSwitchTracking) $ sendMsg nsSegmentedControl (mkSelector "trackingMode") retCULong []

-- | @- setTrackingMode:@
setTrackingMode :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSSegmentSwitchTracking -> IO ()
setTrackingMode nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setTrackingMode:") retVoid [argCULong (coerce value)]

-- | @- doubleValueForSelectedSegment@
doubleValueForSelectedSegment :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CDouble
doubleValueForSelectedSegment nsSegmentedControl  =
  sendMsg nsSegmentedControl (mkSelector "doubleValueForSelectedSegment") retCDouble []

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO CLong
indexOfSelectedItem nsSegmentedControl  =
  sendMsg nsSegmentedControl (mkSelector "indexOfSelectedItem") retCLong []

-- | @- segmentDistribution@
segmentDistribution :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSSegmentDistribution
segmentDistribution nsSegmentedControl  =
  fmap (coerce :: CLong -> NSSegmentDistribution) $ sendMsg nsSegmentedControl (mkSelector "segmentDistribution") retCLong []

-- | @- setSegmentDistribution:@
setSegmentDistribution :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSSegmentDistribution -> IO ()
setSegmentDistribution nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setSegmentDistribution:") retVoid [argCLong (coerce value)]

-- | @- borderShape@
borderShape :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> IO NSControlBorderShape
borderShape nsSegmentedControl  =
  fmap (coerce :: CLong -> NSControlBorderShape) $ sendMsg nsSegmentedControl (mkSelector "borderShape") retCLong []

-- | @- setBorderShape:@
setBorderShape :: IsNSSegmentedControl nsSegmentedControl => nsSegmentedControl -> NSControlBorderShape -> IO ()
setBorderShape nsSegmentedControl  value =
  sendMsg nsSegmentedControl (mkSelector "setBorderShape:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectSegmentWithTag:@
selectSegmentWithTagSelector :: Selector
selectSegmentWithTagSelector = mkSelector "selectSegmentWithTag:"

-- | @Selector@ for @setWidth:forSegment:@
setWidth_forSegmentSelector :: Selector
setWidth_forSegmentSelector = mkSelector "setWidth:forSegment:"

-- | @Selector@ for @widthForSegment:@
widthForSegmentSelector :: Selector
widthForSegmentSelector = mkSelector "widthForSegment:"

-- | @Selector@ for @setImage:forSegment:@
setImage_forSegmentSelector :: Selector
setImage_forSegmentSelector = mkSelector "setImage:forSegment:"

-- | @Selector@ for @imageForSegment:@
imageForSegmentSelector :: Selector
imageForSegmentSelector = mkSelector "imageForSegment:"

-- | @Selector@ for @setImageScaling:forSegment:@
setImageScaling_forSegmentSelector :: Selector
setImageScaling_forSegmentSelector = mkSelector "setImageScaling:forSegment:"

-- | @Selector@ for @imageScalingForSegment:@
imageScalingForSegmentSelector :: Selector
imageScalingForSegmentSelector = mkSelector "imageScalingForSegment:"

-- | @Selector@ for @setLabel:forSegment:@
setLabel_forSegmentSelector :: Selector
setLabel_forSegmentSelector = mkSelector "setLabel:forSegment:"

-- | @Selector@ for @labelForSegment:@
labelForSegmentSelector :: Selector
labelForSegmentSelector = mkSelector "labelForSegment:"

-- | @Selector@ for @setMenu:forSegment:@
setMenu_forSegmentSelector :: Selector
setMenu_forSegmentSelector = mkSelector "setMenu:forSegment:"

-- | @Selector@ for @menuForSegment:@
menuForSegmentSelector :: Selector
menuForSegmentSelector = mkSelector "menuForSegment:"

-- | @Selector@ for @setSelected:forSegment:@
setSelected_forSegmentSelector :: Selector
setSelected_forSegmentSelector = mkSelector "setSelected:forSegment:"

-- | @Selector@ for @isSelectedForSegment:@
isSelectedForSegmentSelector :: Selector
isSelectedForSegmentSelector = mkSelector "isSelectedForSegment:"

-- | @Selector@ for @setEnabled:forSegment:@
setEnabled_forSegmentSelector :: Selector
setEnabled_forSegmentSelector = mkSelector "setEnabled:forSegment:"

-- | @Selector@ for @isEnabledForSegment:@
isEnabledForSegmentSelector :: Selector
isEnabledForSegmentSelector = mkSelector "isEnabledForSegment:"

-- | @Selector@ for @setToolTip:forSegment:@
setToolTip_forSegmentSelector :: Selector
setToolTip_forSegmentSelector = mkSelector "setToolTip:forSegment:"

-- | @Selector@ for @toolTipForSegment:@
toolTipForSegmentSelector :: Selector
toolTipForSegmentSelector = mkSelector "toolTipForSegment:"

-- | @Selector@ for @setTag:forSegment:@
setTag_forSegmentSelector :: Selector
setTag_forSegmentSelector = mkSelector "setTag:forSegment:"

-- | @Selector@ for @tagForSegment:@
tagForSegmentSelector :: Selector
tagForSegmentSelector = mkSelector "tagForSegment:"

-- | @Selector@ for @setShowsMenuIndicator:forSegment:@
setShowsMenuIndicator_forSegmentSelector :: Selector
setShowsMenuIndicator_forSegmentSelector = mkSelector "setShowsMenuIndicator:forSegment:"

-- | @Selector@ for @showsMenuIndicatorForSegment:@
showsMenuIndicatorForSegmentSelector :: Selector
showsMenuIndicatorForSegmentSelector = mkSelector "showsMenuIndicatorForSegment:"

-- | @Selector@ for @setAlignment:forSegment:@
setAlignment_forSegmentSelector :: Selector
setAlignment_forSegmentSelector = mkSelector "setAlignment:forSegment:"

-- | @Selector@ for @alignmentForSegment:@
alignmentForSegmentSelector :: Selector
alignmentForSegmentSelector = mkSelector "alignmentForSegment:"

-- | @Selector@ for @compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptionsSelector :: Selector
compressWithPrioritizedCompressionOptionsSelector = mkSelector "compressWithPrioritizedCompressionOptions:"

-- | @Selector@ for @minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptionsSelector :: Selector
minimumSizeWithPrioritizedCompressionOptionsSelector = mkSelector "minimumSizeWithPrioritizedCompressionOptions:"

-- | @Selector@ for @segmentedControlWithLabels:trackingMode:target:action:@
segmentedControlWithLabels_trackingMode_target_actionSelector :: Selector
segmentedControlWithLabels_trackingMode_target_actionSelector = mkSelector "segmentedControlWithLabels:trackingMode:target:action:"

-- | @Selector@ for @segmentedControlWithImages:trackingMode:target:action:@
segmentedControlWithImages_trackingMode_target_actionSelector :: Selector
segmentedControlWithImages_trackingMode_target_actionSelector = mkSelector "segmentedControlWithImages:trackingMode:target:action:"

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector
segmentCountSelector = mkSelector "segmentCount"

-- | @Selector@ for @setSegmentCount:@
setSegmentCountSelector :: Selector
setSegmentCountSelector = mkSelector "setSegmentCount:"

-- | @Selector@ for @selectedSegment@
selectedSegmentSelector :: Selector
selectedSegmentSelector = mkSelector "selectedSegment"

-- | @Selector@ for @setSelectedSegment:@
setSelectedSegmentSelector :: Selector
setSelectedSegmentSelector = mkSelector "setSelectedSegment:"

-- | @Selector@ for @segmentStyle@
segmentStyleSelector :: Selector
segmentStyleSelector = mkSelector "segmentStyle"

-- | @Selector@ for @setSegmentStyle:@
setSegmentStyleSelector :: Selector
setSegmentStyleSelector = mkSelector "setSegmentStyle:"

-- | @Selector@ for @springLoaded@
springLoadedSelector :: Selector
springLoadedSelector = mkSelector "springLoaded"

-- | @Selector@ for @setSpringLoaded:@
setSpringLoadedSelector :: Selector
setSpringLoadedSelector = mkSelector "setSpringLoaded:"

-- | @Selector@ for @trackingMode@
trackingModeSelector :: Selector
trackingModeSelector = mkSelector "trackingMode"

-- | @Selector@ for @setTrackingMode:@
setTrackingModeSelector :: Selector
setTrackingModeSelector = mkSelector "setTrackingMode:"

-- | @Selector@ for @doubleValueForSelectedSegment@
doubleValueForSelectedSegmentSelector :: Selector
doubleValueForSelectedSegmentSelector = mkSelector "doubleValueForSelectedSegment"

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @segmentDistribution@
segmentDistributionSelector :: Selector
segmentDistributionSelector = mkSelector "segmentDistribution"

-- | @Selector@ for @setSegmentDistribution:@
setSegmentDistributionSelector :: Selector
setSegmentDistributionSelector = mkSelector "setSegmentDistribution:"

-- | @Selector@ for @borderShape@
borderShapeSelector :: Selector
borderShapeSelector = mkSelector "borderShape"

-- | @Selector@ for @setBorderShape:@
setBorderShapeSelector :: Selector
setBorderShapeSelector = mkSelector "setBorderShape:"

