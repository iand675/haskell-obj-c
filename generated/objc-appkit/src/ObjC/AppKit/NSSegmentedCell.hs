{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSegmentedCell@.
module ObjC.AppKit.NSSegmentedCell
  ( NSSegmentedCell
  , IsNSSegmentedCell(..)
  , selectSegmentWithTag
  , makeNextSegmentKey
  , makePreviousSegmentKey
  , setWidth_forSegment
  , widthForSegment
  , setImage_forSegment
  , imageForSegment
  , setImageScaling_forSegment
  , imageScalingForSegment
  , setLabel_forSegment
  , labelForSegment
  , setSelected_forSegment
  , isSelectedForSegment
  , setEnabled_forSegment
  , isEnabledForSegment
  , setMenu_forSegment
  , menuForSegment
  , setToolTip_forSegment
  , toolTipForSegment
  , setTag_forSegment
  , tagForSegment
  , drawSegment_inFrame_withView
  , interiorBackgroundStyleForSegment
  , segmentCount
  , setSegmentCount
  , selectedSegment
  , setSelectedSegment
  , trackingMode
  , setTrackingMode
  , segmentStyle
  , setSegmentStyle
  , selectSegmentWithTagSelector
  , makeNextSegmentKeySelector
  , makePreviousSegmentKeySelector
  , setWidth_forSegmentSelector
  , widthForSegmentSelector
  , setImage_forSegmentSelector
  , imageForSegmentSelector
  , setImageScaling_forSegmentSelector
  , imageScalingForSegmentSelector
  , setLabel_forSegmentSelector
  , labelForSegmentSelector
  , setSelected_forSegmentSelector
  , isSelectedForSegmentSelector
  , setEnabled_forSegmentSelector
  , isEnabledForSegmentSelector
  , setMenu_forSegmentSelector
  , menuForSegmentSelector
  , setToolTip_forSegmentSelector
  , toolTipForSegmentSelector
  , setTag_forSegmentSelector
  , tagForSegmentSelector
  , drawSegment_inFrame_withViewSelector
  , interiorBackgroundStyleForSegmentSelector
  , segmentCountSelector
  , setSegmentCountSelector
  , selectedSegmentSelector
  , setSelectedSegmentSelector
  , trackingModeSelector
  , setTrackingModeSelector
  , segmentStyleSelector
  , setSegmentStyleSelector

  -- * Enum types
  , NSBackgroundStyle(NSBackgroundStyle)
  , pattern NSBackgroundStyleNormal
  , pattern NSBackgroundStyleEmphasized
  , pattern NSBackgroundStyleRaised
  , pattern NSBackgroundStyleLowered
  , NSImageScaling(NSImageScaling)
  , pattern NSImageScaleProportionallyDown
  , pattern NSImageScaleAxesIndependently
  , pattern NSImageScaleNone
  , pattern NSImageScaleProportionallyUpOrDown
  , pattern NSScaleProportionally
  , pattern NSScaleToFit
  , pattern NSScaleNone
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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- selectSegmentWithTag:@
selectSegmentWithTag :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO Bool
selectSegmentWithTag nsSegmentedCell  tag =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedCell (mkSelector "selectSegmentWithTag:") retCULong [argCLong (fromIntegral tag)]

-- | @- makeNextSegmentKey@
makeNextSegmentKey :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO ()
makeNextSegmentKey nsSegmentedCell  =
  sendMsg nsSegmentedCell (mkSelector "makeNextSegmentKey") retVoid []

-- | @- makePreviousSegmentKey@
makePreviousSegmentKey :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO ()
makePreviousSegmentKey nsSegmentedCell  =
  sendMsg nsSegmentedCell (mkSelector "makePreviousSegmentKey") retVoid []

-- | @- setWidth:forSegment:@
setWidth_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CDouble -> CLong -> IO ()
setWidth_forSegment nsSegmentedCell  width segment =
  sendMsg nsSegmentedCell (mkSelector "setWidth:forSegment:") retVoid [argCDouble (fromIntegral width), argCLong (fromIntegral segment)]

-- | @- widthForSegment:@
widthForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO CDouble
widthForSegment nsSegmentedCell  segment =
  sendMsg nsSegmentedCell (mkSelector "widthForSegment:") retCDouble [argCLong (fromIntegral segment)]

-- | @- setImage:forSegment:@
setImage_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSImage image) => nsSegmentedCell -> image -> CLong -> IO ()
setImage_forSegment nsSegmentedCell  image segment =
withObjCPtr image $ \raw_image ->
    sendMsg nsSegmentedCell (mkSelector "setImage:forSegment:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- imageForSegment:@
imageForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSImage)
imageForSegment nsSegmentedCell  segment =
  sendMsg nsSegmentedCell (mkSelector "imageForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setImageScaling:forSegment:@
setImageScaling_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> NSImageScaling -> CLong -> IO ()
setImageScaling_forSegment nsSegmentedCell  scaling segment =
  sendMsg nsSegmentedCell (mkSelector "setImageScaling:forSegment:") retVoid [argCULong (coerce scaling), argCLong (fromIntegral segment)]

-- | @- imageScalingForSegment:@
imageScalingForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO NSImageScaling
imageScalingForSegment nsSegmentedCell  segment =
  fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsSegmentedCell (mkSelector "imageScalingForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setLabel:forSegment:@
setLabel_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSString label) => nsSegmentedCell -> label -> CLong -> IO ()
setLabel_forSegment nsSegmentedCell  label segment =
withObjCPtr label $ \raw_label ->
    sendMsg nsSegmentedCell (mkSelector "setLabel:forSegment:") retVoid [argPtr (castPtr raw_label :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- labelForSegment:@
labelForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSString)
labelForSegment nsSegmentedCell  segment =
  sendMsg nsSegmentedCell (mkSelector "labelForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setSelected:forSegment:@
setSelected_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> Bool -> CLong -> IO ()
setSelected_forSegment nsSegmentedCell  selected segment =
  sendMsg nsSegmentedCell (mkSelector "setSelected:forSegment:") retVoid [argCULong (if selected then 1 else 0), argCLong (fromIntegral segment)]

-- | @- isSelectedForSegment:@
isSelectedForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO Bool
isSelectedForSegment nsSegmentedCell  segment =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedCell (mkSelector "isSelectedForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setEnabled:forSegment:@
setEnabled_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> Bool -> CLong -> IO ()
setEnabled_forSegment nsSegmentedCell  enabled segment =
  sendMsg nsSegmentedCell (mkSelector "setEnabled:forSegment:") retVoid [argCULong (if enabled then 1 else 0), argCLong (fromIntegral segment)]

-- | @- isEnabledForSegment:@
isEnabledForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO Bool
isEnabledForSegment nsSegmentedCell  segment =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSegmentedCell (mkSelector "isEnabledForSegment:") retCULong [argCLong (fromIntegral segment)]

-- | @- setMenu:forSegment:@
setMenu_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSMenu menu) => nsSegmentedCell -> menu -> CLong -> IO ()
setMenu_forSegment nsSegmentedCell  menu segment =
withObjCPtr menu $ \raw_menu ->
    sendMsg nsSegmentedCell (mkSelector "setMenu:forSegment:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- menuForSegment:@
menuForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSMenu)
menuForSegment nsSegmentedCell  segment =
  sendMsg nsSegmentedCell (mkSelector "menuForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setToolTip:forSegment:@
setToolTip_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSString toolTip) => nsSegmentedCell -> toolTip -> CLong -> IO ()
setToolTip_forSegment nsSegmentedCell  toolTip segment =
withObjCPtr toolTip $ \raw_toolTip ->
    sendMsg nsSegmentedCell (mkSelector "setToolTip:forSegment:") retVoid [argPtr (castPtr raw_toolTip :: Ptr ()), argCLong (fromIntegral segment)]

-- | @- toolTipForSegment:@
toolTipForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSString)
toolTipForSegment nsSegmentedCell  segment =
  sendMsg nsSegmentedCell (mkSelector "toolTipForSegment:") (retPtr retVoid) [argCLong (fromIntegral segment)] >>= retainedObject . castPtr

-- | @- setTag:forSegment:@
setTag_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> CLong -> IO ()
setTag_forSegment nsSegmentedCell  tag segment =
  sendMsg nsSegmentedCell (mkSelector "setTag:forSegment:") retVoid [argCLong (fromIntegral tag), argCLong (fromIntegral segment)]

-- | @- tagForSegment:@
tagForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO CLong
tagForSegment nsSegmentedCell  segment =
  sendMsg nsSegmentedCell (mkSelector "tagForSegment:") retCLong [argCLong (fromIntegral segment)]

-- | @- drawSegment:inFrame:withView:@
drawSegment_inFrame_withView :: (IsNSSegmentedCell nsSegmentedCell, IsNSView controlView) => nsSegmentedCell -> CLong -> NSRect -> controlView -> IO ()
drawSegment_inFrame_withView nsSegmentedCell  segment frame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsSegmentedCell (mkSelector "drawSegment:inFrame:withView:") retVoid [argCLong (fromIntegral segment), argNSRect frame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- interiorBackgroundStyleForSegment:@
interiorBackgroundStyleForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO NSBackgroundStyle
interiorBackgroundStyleForSegment nsSegmentedCell  segment =
  fmap (coerce :: CLong -> NSBackgroundStyle) $ sendMsg nsSegmentedCell (mkSelector "interiorBackgroundStyleForSegment:") retCLong [argCLong (fromIntegral segment)]

-- | @- segmentCount@
segmentCount :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO CLong
segmentCount nsSegmentedCell  =
  sendMsg nsSegmentedCell (mkSelector "segmentCount") retCLong []

-- | @- setSegmentCount:@
setSegmentCount :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO ()
setSegmentCount nsSegmentedCell  value =
  sendMsg nsSegmentedCell (mkSelector "setSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | @- selectedSegment@
selectedSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO CLong
selectedSegment nsSegmentedCell  =
  sendMsg nsSegmentedCell (mkSelector "selectedSegment") retCLong []

-- | @- setSelectedSegment:@
setSelectedSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO ()
setSelectedSegment nsSegmentedCell  value =
  sendMsg nsSegmentedCell (mkSelector "setSelectedSegment:") retVoid [argCLong (fromIntegral value)]

-- | @- trackingMode@
trackingMode :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO NSSegmentSwitchTracking
trackingMode nsSegmentedCell  =
  fmap (coerce :: CULong -> NSSegmentSwitchTracking) $ sendMsg nsSegmentedCell (mkSelector "trackingMode") retCULong []

-- | @- setTrackingMode:@
setTrackingMode :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> NSSegmentSwitchTracking -> IO ()
setTrackingMode nsSegmentedCell  value =
  sendMsg nsSegmentedCell (mkSelector "setTrackingMode:") retVoid [argCULong (coerce value)]

-- | @- segmentStyle@
segmentStyle :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO NSSegmentStyle
segmentStyle nsSegmentedCell  =
  fmap (coerce :: CLong -> NSSegmentStyle) $ sendMsg nsSegmentedCell (mkSelector "segmentStyle") retCLong []

-- | @- setSegmentStyle:@
setSegmentStyle :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> NSSegmentStyle -> IO ()
setSegmentStyle nsSegmentedCell  value =
  sendMsg nsSegmentedCell (mkSelector "setSegmentStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectSegmentWithTag:@
selectSegmentWithTagSelector :: Selector
selectSegmentWithTagSelector = mkSelector "selectSegmentWithTag:"

-- | @Selector@ for @makeNextSegmentKey@
makeNextSegmentKeySelector :: Selector
makeNextSegmentKeySelector = mkSelector "makeNextSegmentKey"

-- | @Selector@ for @makePreviousSegmentKey@
makePreviousSegmentKeySelector :: Selector
makePreviousSegmentKeySelector = mkSelector "makePreviousSegmentKey"

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

-- | @Selector@ for @setMenu:forSegment:@
setMenu_forSegmentSelector :: Selector
setMenu_forSegmentSelector = mkSelector "setMenu:forSegment:"

-- | @Selector@ for @menuForSegment:@
menuForSegmentSelector :: Selector
menuForSegmentSelector = mkSelector "menuForSegment:"

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

-- | @Selector@ for @drawSegment:inFrame:withView:@
drawSegment_inFrame_withViewSelector :: Selector
drawSegment_inFrame_withViewSelector = mkSelector "drawSegment:inFrame:withView:"

-- | @Selector@ for @interiorBackgroundStyleForSegment:@
interiorBackgroundStyleForSegmentSelector :: Selector
interiorBackgroundStyleForSegmentSelector = mkSelector "interiorBackgroundStyleForSegment:"

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

-- | @Selector@ for @trackingMode@
trackingModeSelector :: Selector
trackingModeSelector = mkSelector "trackingMode"

-- | @Selector@ for @setTrackingMode:@
setTrackingModeSelector :: Selector
setTrackingModeSelector = mkSelector "setTrackingMode:"

-- | @Selector@ for @segmentStyle@
segmentStyleSelector :: Selector
segmentStyleSelector = mkSelector "segmentStyle"

-- | @Selector@ for @setSegmentStyle:@
setSegmentStyleSelector :: Selector
setSegmentStyleSelector = mkSelector "setSegmentStyle:"

