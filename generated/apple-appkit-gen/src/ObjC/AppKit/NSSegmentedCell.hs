{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , drawSegment_inFrame_withViewSelector
  , imageForSegmentSelector
  , imageScalingForSegmentSelector
  , interiorBackgroundStyleForSegmentSelector
  , isEnabledForSegmentSelector
  , isSelectedForSegmentSelector
  , labelForSegmentSelector
  , makeNextSegmentKeySelector
  , makePreviousSegmentKeySelector
  , menuForSegmentSelector
  , segmentCountSelector
  , segmentStyleSelector
  , selectSegmentWithTagSelector
  , selectedSegmentSelector
  , setEnabled_forSegmentSelector
  , setImageScaling_forSegmentSelector
  , setImage_forSegmentSelector
  , setLabel_forSegmentSelector
  , setMenu_forSegmentSelector
  , setSegmentCountSelector
  , setSegmentStyleSelector
  , setSelectedSegmentSelector
  , setSelected_forSegmentSelector
  , setTag_forSegmentSelector
  , setToolTip_forSegmentSelector
  , setTrackingModeSelector
  , setWidth_forSegmentSelector
  , tagForSegmentSelector
  , toolTipForSegmentSelector
  , trackingModeSelector
  , widthForSegmentSelector

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
selectSegmentWithTag :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO Bool
selectSegmentWithTag nsSegmentedCell tag =
  sendMessage nsSegmentedCell selectSegmentWithTagSelector tag

-- | @- makeNextSegmentKey@
makeNextSegmentKey :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO ()
makeNextSegmentKey nsSegmentedCell =
  sendMessage nsSegmentedCell makeNextSegmentKeySelector

-- | @- makePreviousSegmentKey@
makePreviousSegmentKey :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO ()
makePreviousSegmentKey nsSegmentedCell =
  sendMessage nsSegmentedCell makePreviousSegmentKeySelector

-- | @- setWidth:forSegment:@
setWidth_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CDouble -> CLong -> IO ()
setWidth_forSegment nsSegmentedCell width segment =
  sendMessage nsSegmentedCell setWidth_forSegmentSelector width segment

-- | @- widthForSegment:@
widthForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO CDouble
widthForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell widthForSegmentSelector segment

-- | @- setImage:forSegment:@
setImage_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSImage image) => nsSegmentedCell -> image -> CLong -> IO ()
setImage_forSegment nsSegmentedCell image segment =
  sendMessage nsSegmentedCell setImage_forSegmentSelector (toNSImage image) segment

-- | @- imageForSegment:@
imageForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSImage)
imageForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell imageForSegmentSelector segment

-- | @- setImageScaling:forSegment:@
setImageScaling_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> NSImageScaling -> CLong -> IO ()
setImageScaling_forSegment nsSegmentedCell scaling segment =
  sendMessage nsSegmentedCell setImageScaling_forSegmentSelector scaling segment

-- | @- imageScalingForSegment:@
imageScalingForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO NSImageScaling
imageScalingForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell imageScalingForSegmentSelector segment

-- | @- setLabel:forSegment:@
setLabel_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSString label) => nsSegmentedCell -> label -> CLong -> IO ()
setLabel_forSegment nsSegmentedCell label segment =
  sendMessage nsSegmentedCell setLabel_forSegmentSelector (toNSString label) segment

-- | @- labelForSegment:@
labelForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSString)
labelForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell labelForSegmentSelector segment

-- | @- setSelected:forSegment:@
setSelected_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> Bool -> CLong -> IO ()
setSelected_forSegment nsSegmentedCell selected segment =
  sendMessage nsSegmentedCell setSelected_forSegmentSelector selected segment

-- | @- isSelectedForSegment:@
isSelectedForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO Bool
isSelectedForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell isSelectedForSegmentSelector segment

-- | @- setEnabled:forSegment:@
setEnabled_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> Bool -> CLong -> IO ()
setEnabled_forSegment nsSegmentedCell enabled segment =
  sendMessage nsSegmentedCell setEnabled_forSegmentSelector enabled segment

-- | @- isEnabledForSegment:@
isEnabledForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO Bool
isEnabledForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell isEnabledForSegmentSelector segment

-- | @- setMenu:forSegment:@
setMenu_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSMenu menu) => nsSegmentedCell -> menu -> CLong -> IO ()
setMenu_forSegment nsSegmentedCell menu segment =
  sendMessage nsSegmentedCell setMenu_forSegmentSelector (toNSMenu menu) segment

-- | @- menuForSegment:@
menuForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSMenu)
menuForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell menuForSegmentSelector segment

-- | @- setToolTip:forSegment:@
setToolTip_forSegment :: (IsNSSegmentedCell nsSegmentedCell, IsNSString toolTip) => nsSegmentedCell -> toolTip -> CLong -> IO ()
setToolTip_forSegment nsSegmentedCell toolTip segment =
  sendMessage nsSegmentedCell setToolTip_forSegmentSelector (toNSString toolTip) segment

-- | @- toolTipForSegment:@
toolTipForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO (Id NSString)
toolTipForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell toolTipForSegmentSelector segment

-- | @- setTag:forSegment:@
setTag_forSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> CLong -> IO ()
setTag_forSegment nsSegmentedCell tag segment =
  sendMessage nsSegmentedCell setTag_forSegmentSelector tag segment

-- | @- tagForSegment:@
tagForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO CLong
tagForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell tagForSegmentSelector segment

-- | @- drawSegment:inFrame:withView:@
drawSegment_inFrame_withView :: (IsNSSegmentedCell nsSegmentedCell, IsNSView controlView) => nsSegmentedCell -> CLong -> NSRect -> controlView -> IO ()
drawSegment_inFrame_withView nsSegmentedCell segment frame controlView =
  sendMessage nsSegmentedCell drawSegment_inFrame_withViewSelector segment frame (toNSView controlView)

-- | @- interiorBackgroundStyleForSegment:@
interiorBackgroundStyleForSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO NSBackgroundStyle
interiorBackgroundStyleForSegment nsSegmentedCell segment =
  sendMessage nsSegmentedCell interiorBackgroundStyleForSegmentSelector segment

-- | @- segmentCount@
segmentCount :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO CLong
segmentCount nsSegmentedCell =
  sendMessage nsSegmentedCell segmentCountSelector

-- | @- setSegmentCount:@
setSegmentCount :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO ()
setSegmentCount nsSegmentedCell value =
  sendMessage nsSegmentedCell setSegmentCountSelector value

-- | @- selectedSegment@
selectedSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO CLong
selectedSegment nsSegmentedCell =
  sendMessage nsSegmentedCell selectedSegmentSelector

-- | @- setSelectedSegment:@
setSelectedSegment :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> CLong -> IO ()
setSelectedSegment nsSegmentedCell value =
  sendMessage nsSegmentedCell setSelectedSegmentSelector value

-- | @- trackingMode@
trackingMode :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO NSSegmentSwitchTracking
trackingMode nsSegmentedCell =
  sendMessage nsSegmentedCell trackingModeSelector

-- | @- setTrackingMode:@
setTrackingMode :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> NSSegmentSwitchTracking -> IO ()
setTrackingMode nsSegmentedCell value =
  sendMessage nsSegmentedCell setTrackingModeSelector value

-- | @- segmentStyle@
segmentStyle :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> IO NSSegmentStyle
segmentStyle nsSegmentedCell =
  sendMessage nsSegmentedCell segmentStyleSelector

-- | @- setSegmentStyle:@
setSegmentStyle :: IsNSSegmentedCell nsSegmentedCell => nsSegmentedCell -> NSSegmentStyle -> IO ()
setSegmentStyle nsSegmentedCell value =
  sendMessage nsSegmentedCell setSegmentStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectSegmentWithTag:@
selectSegmentWithTagSelector :: Selector '[CLong] Bool
selectSegmentWithTagSelector = mkSelector "selectSegmentWithTag:"

-- | @Selector@ for @makeNextSegmentKey@
makeNextSegmentKeySelector :: Selector '[] ()
makeNextSegmentKeySelector = mkSelector "makeNextSegmentKey"

-- | @Selector@ for @makePreviousSegmentKey@
makePreviousSegmentKeySelector :: Selector '[] ()
makePreviousSegmentKeySelector = mkSelector "makePreviousSegmentKey"

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

-- | @Selector@ for @setMenu:forSegment:@
setMenu_forSegmentSelector :: Selector '[Id NSMenu, CLong] ()
setMenu_forSegmentSelector = mkSelector "setMenu:forSegment:"

-- | @Selector@ for @menuForSegment:@
menuForSegmentSelector :: Selector '[CLong] (Id NSMenu)
menuForSegmentSelector = mkSelector "menuForSegment:"

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

-- | @Selector@ for @drawSegment:inFrame:withView:@
drawSegment_inFrame_withViewSelector :: Selector '[CLong, NSRect, Id NSView] ()
drawSegment_inFrame_withViewSelector = mkSelector "drawSegment:inFrame:withView:"

-- | @Selector@ for @interiorBackgroundStyleForSegment:@
interiorBackgroundStyleForSegmentSelector :: Selector '[CLong] NSBackgroundStyle
interiorBackgroundStyleForSegmentSelector = mkSelector "interiorBackgroundStyleForSegment:"

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

-- | @Selector@ for @trackingMode@
trackingModeSelector :: Selector '[] NSSegmentSwitchTracking
trackingModeSelector = mkSelector "trackingMode"

-- | @Selector@ for @setTrackingMode:@
setTrackingModeSelector :: Selector '[NSSegmentSwitchTracking] ()
setTrackingModeSelector = mkSelector "setTrackingMode:"

-- | @Selector@ for @segmentStyle@
segmentStyleSelector :: Selector '[] NSSegmentStyle
segmentStyleSelector = mkSelector "segmentStyle"

-- | @Selector@ for @setSegmentStyle:@
setSegmentStyleSelector :: Selector '[NSSegmentStyle] ()
setSegmentStyleSelector = mkSelector "setSegmentStyle:"

