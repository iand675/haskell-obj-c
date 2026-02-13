{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScreen@.
module ObjC.AppKit.NSScreen
  ( NSScreen
  , IsNSScreen(..)
  , canRepresentDisplayGamut
  , convertRectToBacking
  , convertRectFromBacking
  , backingAlignedRect_options
  , userSpaceScaleFactor
  , displayLinkWithTarget_selector
  , screens
  , mainScreen
  , deepestScreen
  , screensHaveSeparateSpaces
  , depth
  , frame
  , visibleFrame
  , deviceDescription
  , colorSpace
  , supportedWindowDepths
  , backingScaleFactor
  , localizedName
  , safeAreaInsets
  , auxiliaryTopLeftArea
  , auxiliaryTopRightArea
  , cgDirectDisplayID
  , maximumFramesPerSecond
  , minimumRefreshInterval
  , maximumRefreshInterval
  , displayUpdateGranularity
  , lastDisplayUpdateTimestamp
  , maximumExtendedDynamicRangeColorComponentValue
  , maximumPotentialExtendedDynamicRangeColorComponentValue
  , maximumReferenceExtendedDynamicRangeColorComponentValue
  , auxiliaryTopLeftAreaSelector
  , auxiliaryTopRightAreaSelector
  , backingAlignedRect_optionsSelector
  , backingScaleFactorSelector
  , canRepresentDisplayGamutSelector
  , cgDirectDisplayIDSelector
  , colorSpaceSelector
  , convertRectFromBackingSelector
  , convertRectToBackingSelector
  , deepestScreenSelector
  , depthSelector
  , deviceDescriptionSelector
  , displayLinkWithTarget_selectorSelector
  , displayUpdateGranularitySelector
  , frameSelector
  , lastDisplayUpdateTimestampSelector
  , localizedNameSelector
  , mainScreenSelector
  , maximumExtendedDynamicRangeColorComponentValueSelector
  , maximumFramesPerSecondSelector
  , maximumPotentialExtendedDynamicRangeColorComponentValueSelector
  , maximumReferenceExtendedDynamicRangeColorComponentValueSelector
  , maximumRefreshIntervalSelector
  , minimumRefreshIntervalSelector
  , safeAreaInsetsSelector
  , screensHaveSeparateSpacesSelector
  , screensSelector
  , supportedWindowDepthsSelector
  , userSpaceScaleFactorSelector
  , visibleFrameSelector

  -- * Enum types
  , NSAlignmentOptions(NSAlignmentOptions)
  , pattern NSAlignMinXInward
  , pattern NSAlignMinYInward
  , pattern NSAlignMaxXInward
  , pattern NSAlignMaxYInward
  , pattern NSAlignWidthInward
  , pattern NSAlignHeightInward
  , pattern NSAlignMinXOutward
  , pattern NSAlignMinYOutward
  , pattern NSAlignMaxXOutward
  , pattern NSAlignMaxYOutward
  , pattern NSAlignWidthOutward
  , pattern NSAlignHeightOutward
  , pattern NSAlignMinXNearest
  , pattern NSAlignMinYNearest
  , pattern NSAlignMaxXNearest
  , pattern NSAlignMaxYNearest
  , pattern NSAlignWidthNearest
  , pattern NSAlignHeightNearest
  , pattern NSAlignRectFlipped
  , pattern NSAlignAllEdgesInward
  , pattern NSAlignAllEdgesOutward
  , pattern NSAlignAllEdgesNearest
  , NSDisplayGamut(NSDisplayGamut)
  , pattern NSDisplayGamutSRGB
  , pattern NSDisplayGamutP3
  , NSWindowDepth(NSWindowDepth)
  , pattern NSWindowDepthTwentyfourBitRGB
  , pattern NSWindowDepthSixtyfourBitRGB
  , pattern NSWindowDepthOnehundredtwentyeightBitRGB

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- canRepresentDisplayGamut:@
canRepresentDisplayGamut :: IsNSScreen nsScreen => nsScreen -> NSDisplayGamut -> IO Bool
canRepresentDisplayGamut nsScreen displayGamut =
  sendMessage nsScreen canRepresentDisplayGamutSelector displayGamut

-- | @- convertRectToBacking:@
convertRectToBacking :: IsNSScreen nsScreen => nsScreen -> NSRect -> IO NSRect
convertRectToBacking nsScreen rect =
  sendMessage nsScreen convertRectToBackingSelector rect

-- | @- convertRectFromBacking:@
convertRectFromBacking :: IsNSScreen nsScreen => nsScreen -> NSRect -> IO NSRect
convertRectFromBacking nsScreen rect =
  sendMessage nsScreen convertRectFromBackingSelector rect

-- | @- backingAlignedRect:options:@
backingAlignedRect_options :: IsNSScreen nsScreen => nsScreen -> NSRect -> NSAlignmentOptions -> IO NSRect
backingAlignedRect_options nsScreen rect options =
  sendMessage nsScreen backingAlignedRect_optionsSelector rect options

-- | @- userSpaceScaleFactor@
userSpaceScaleFactor :: IsNSScreen nsScreen => nsScreen -> IO CDouble
userSpaceScaleFactor nsScreen =
  sendMessage nsScreen userSpaceScaleFactorSelector

-- | @- displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: IsNSScreen nsScreen => nsScreen -> RawId -> Sel -> IO (Id CADisplayLink)
displayLinkWithTarget_selector nsScreen target selector =
  sendMessage nsScreen displayLinkWithTarget_selectorSelector target selector

-- | @+ screens@
screens :: IO (Id NSArray)
screens  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMessage cls' screensSelector

-- | @+ mainScreen@
mainScreen :: IO (Id NSScreen)
mainScreen  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMessage cls' mainScreenSelector

-- | @+ deepestScreen@
deepestScreen :: IO (Id NSScreen)
deepestScreen  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMessage cls' deepestScreenSelector

-- | @+ screensHaveSeparateSpaces@
screensHaveSeparateSpaces :: IO Bool
screensHaveSeparateSpaces  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMessage cls' screensHaveSeparateSpacesSelector

-- | @- depth@
depth :: IsNSScreen nsScreen => nsScreen -> IO NSWindowDepth
depth nsScreen =
  sendMessage nsScreen depthSelector

-- | @- frame@
frame :: IsNSScreen nsScreen => nsScreen -> IO NSRect
frame nsScreen =
  sendMessage nsScreen frameSelector

-- | @- visibleFrame@
visibleFrame :: IsNSScreen nsScreen => nsScreen -> IO NSRect
visibleFrame nsScreen =
  sendMessage nsScreen visibleFrameSelector

-- | @- deviceDescription@
deviceDescription :: IsNSScreen nsScreen => nsScreen -> IO (Id NSDictionary)
deviceDescription nsScreen =
  sendMessage nsScreen deviceDescriptionSelector

-- | @- colorSpace@
colorSpace :: IsNSScreen nsScreen => nsScreen -> IO (Id NSColorSpace)
colorSpace nsScreen =
  sendMessage nsScreen colorSpaceSelector

-- | @- supportedWindowDepths@
supportedWindowDepths :: IsNSScreen nsScreen => nsScreen -> IO RawId
supportedWindowDepths nsScreen =
  sendMessage nsScreen supportedWindowDepthsSelector

-- | @- backingScaleFactor@
backingScaleFactor :: IsNSScreen nsScreen => nsScreen -> IO CDouble
backingScaleFactor nsScreen =
  sendMessage nsScreen backingScaleFactorSelector

-- | @- localizedName@
localizedName :: IsNSScreen nsScreen => nsScreen -> IO (Id NSString)
localizedName nsScreen =
  sendMessage nsScreen localizedNameSelector

-- | @- safeAreaInsets@
safeAreaInsets :: IsNSScreen nsScreen => nsScreen -> IO NSEdgeInsets
safeAreaInsets nsScreen =
  sendMessage nsScreen safeAreaInsetsSelector

-- | @- auxiliaryTopLeftArea@
auxiliaryTopLeftArea :: IsNSScreen nsScreen => nsScreen -> IO NSRect
auxiliaryTopLeftArea nsScreen =
  sendMessage nsScreen auxiliaryTopLeftAreaSelector

-- | @- auxiliaryTopRightArea@
auxiliaryTopRightArea :: IsNSScreen nsScreen => nsScreen -> IO NSRect
auxiliaryTopRightArea nsScreen =
  sendMessage nsScreen auxiliaryTopRightAreaSelector

-- | The CGDirectDisplayID for this screen. This will return kCGNullDirectDisplay if there isn't one.
--
-- ObjC selector: @- CGDirectDisplayID@
cgDirectDisplayID :: IsNSScreen nsScreen => nsScreen -> IO CUInt
cgDirectDisplayID nsScreen =
  sendMessage nsScreen cgDirectDisplayIDSelector

-- | The maximum frames per second this screen supports.
--
-- ObjC selector: @- maximumFramesPerSecond@
maximumFramesPerSecond :: IsNSScreen nsScreen => nsScreen -> IO CLong
maximumFramesPerSecond nsScreen =
  sendMessage nsScreen maximumFramesPerSecondSelector

-- | The minimum refresh interval this screen supports, in seconds.
--
-- This is the shortest amount of time a frame will be present on screen.    minimumRefreshInterval and maximumRefreshInterval will be the same for displays that do not support variable refresh rates.
--
-- ObjC selector: @- minimumRefreshInterval@
minimumRefreshInterval :: IsNSScreen nsScreen => nsScreen -> IO CDouble
minimumRefreshInterval nsScreen =
  sendMessage nsScreen minimumRefreshIntervalSelector

-- | The maximum refresh interval this screen supports, in seconds.
--
-- minimumRefreshInterval and maximumRefreshInterval will be the same for displays that do not support variable refresh rates.
--
-- ObjC selector: @- maximumRefreshInterval@
maximumRefreshInterval :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumRefreshInterval nsScreen =
  sendMessage nsScreen maximumRefreshIntervalSelector

-- | The update granularity of the screen's current mode, in seconds.
--
-- The display will update at the next boundary defined by the granularity, after the minimum refresh interval has been reached. When 0, the display can update at any time between the minimum and maximum refresh rate intervals of the screen. Fixed refresh rate screen modes will return the refresh interval as the update granularity (e.g. 16.66ms for 60Hz refresh rates), meaning updates only occur at refresh rate boundaries.
--
-- ObjC selector: @- displayUpdateGranularity@
displayUpdateGranularity :: IsNSScreen nsScreen => nsScreen -> IO CDouble
displayUpdateGranularity nsScreen =
  sendMessage nsScreen displayUpdateGranularitySelector

-- | The time at which the last framebuffer update occurred on the display, in seconds since startup that the system has been awake.
--
-- ObjC selector: @- lastDisplayUpdateTimestamp@
lastDisplayUpdateTimestamp :: IsNSScreen nsScreen => nsScreen -> IO CDouble
lastDisplayUpdateTimestamp nsScreen =
  sendMessage nsScreen lastDisplayUpdateTimestampSelector

-- | @- maximumExtendedDynamicRangeColorComponentValue@
maximumExtendedDynamicRangeColorComponentValue :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumExtendedDynamicRangeColorComponentValue nsScreen =
  sendMessage nsScreen maximumExtendedDynamicRangeColorComponentValueSelector

-- | @- maximumPotentialExtendedDynamicRangeColorComponentValue@
maximumPotentialExtendedDynamicRangeColorComponentValue :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumPotentialExtendedDynamicRangeColorComponentValue nsScreen =
  sendMessage nsScreen maximumPotentialExtendedDynamicRangeColorComponentValueSelector

-- | @- maximumReferenceExtendedDynamicRangeColorComponentValue@
maximumReferenceExtendedDynamicRangeColorComponentValue :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumReferenceExtendedDynamicRangeColorComponentValue nsScreen =
  sendMessage nsScreen maximumReferenceExtendedDynamicRangeColorComponentValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canRepresentDisplayGamut:@
canRepresentDisplayGamutSelector :: Selector '[NSDisplayGamut] Bool
canRepresentDisplayGamutSelector = mkSelector "canRepresentDisplayGamut:"

-- | @Selector@ for @convertRectToBacking:@
convertRectToBackingSelector :: Selector '[NSRect] NSRect
convertRectToBackingSelector = mkSelector "convertRectToBacking:"

-- | @Selector@ for @convertRectFromBacking:@
convertRectFromBackingSelector :: Selector '[NSRect] NSRect
convertRectFromBackingSelector = mkSelector "convertRectFromBacking:"

-- | @Selector@ for @backingAlignedRect:options:@
backingAlignedRect_optionsSelector :: Selector '[NSRect, NSAlignmentOptions] NSRect
backingAlignedRect_optionsSelector = mkSelector "backingAlignedRect:options:"

-- | @Selector@ for @userSpaceScaleFactor@
userSpaceScaleFactorSelector :: Selector '[] CDouble
userSpaceScaleFactorSelector = mkSelector "userSpaceScaleFactor"

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector '[RawId, Sel] (Id CADisplayLink)
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @screens@
screensSelector :: Selector '[] (Id NSArray)
screensSelector = mkSelector "screens"

-- | @Selector@ for @mainScreen@
mainScreenSelector :: Selector '[] (Id NSScreen)
mainScreenSelector = mkSelector "mainScreen"

-- | @Selector@ for @deepestScreen@
deepestScreenSelector :: Selector '[] (Id NSScreen)
deepestScreenSelector = mkSelector "deepestScreen"

-- | @Selector@ for @screensHaveSeparateSpaces@
screensHaveSeparateSpacesSelector :: Selector '[] Bool
screensHaveSeparateSpacesSelector = mkSelector "screensHaveSeparateSpaces"

-- | @Selector@ for @depth@
depthSelector :: Selector '[] NSWindowDepth
depthSelector = mkSelector "depth"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @visibleFrame@
visibleFrameSelector :: Selector '[] NSRect
visibleFrameSelector = mkSelector "visibleFrame"

-- | @Selector@ for @deviceDescription@
deviceDescriptionSelector :: Selector '[] (Id NSDictionary)
deviceDescriptionSelector = mkSelector "deviceDescription"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSColorSpace)
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @supportedWindowDepths@
supportedWindowDepthsSelector :: Selector '[] RawId
supportedWindowDepthsSelector = mkSelector "supportedWindowDepths"

-- | @Selector@ for @backingScaleFactor@
backingScaleFactorSelector :: Selector '[] CDouble
backingScaleFactorSelector = mkSelector "backingScaleFactor"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @safeAreaInsets@
safeAreaInsetsSelector :: Selector '[] NSEdgeInsets
safeAreaInsetsSelector = mkSelector "safeAreaInsets"

-- | @Selector@ for @auxiliaryTopLeftArea@
auxiliaryTopLeftAreaSelector :: Selector '[] NSRect
auxiliaryTopLeftAreaSelector = mkSelector "auxiliaryTopLeftArea"

-- | @Selector@ for @auxiliaryTopRightArea@
auxiliaryTopRightAreaSelector :: Selector '[] NSRect
auxiliaryTopRightAreaSelector = mkSelector "auxiliaryTopRightArea"

-- | @Selector@ for @CGDirectDisplayID@
cgDirectDisplayIDSelector :: Selector '[] CUInt
cgDirectDisplayIDSelector = mkSelector "CGDirectDisplayID"

-- | @Selector@ for @maximumFramesPerSecond@
maximumFramesPerSecondSelector :: Selector '[] CLong
maximumFramesPerSecondSelector = mkSelector "maximumFramesPerSecond"

-- | @Selector@ for @minimumRefreshInterval@
minimumRefreshIntervalSelector :: Selector '[] CDouble
minimumRefreshIntervalSelector = mkSelector "minimumRefreshInterval"

-- | @Selector@ for @maximumRefreshInterval@
maximumRefreshIntervalSelector :: Selector '[] CDouble
maximumRefreshIntervalSelector = mkSelector "maximumRefreshInterval"

-- | @Selector@ for @displayUpdateGranularity@
displayUpdateGranularitySelector :: Selector '[] CDouble
displayUpdateGranularitySelector = mkSelector "displayUpdateGranularity"

-- | @Selector@ for @lastDisplayUpdateTimestamp@
lastDisplayUpdateTimestampSelector :: Selector '[] CDouble
lastDisplayUpdateTimestampSelector = mkSelector "lastDisplayUpdateTimestamp"

-- | @Selector@ for @maximumExtendedDynamicRangeColorComponentValue@
maximumExtendedDynamicRangeColorComponentValueSelector :: Selector '[] CDouble
maximumExtendedDynamicRangeColorComponentValueSelector = mkSelector "maximumExtendedDynamicRangeColorComponentValue"

-- | @Selector@ for @maximumPotentialExtendedDynamicRangeColorComponentValue@
maximumPotentialExtendedDynamicRangeColorComponentValueSelector :: Selector '[] CDouble
maximumPotentialExtendedDynamicRangeColorComponentValueSelector = mkSelector "maximumPotentialExtendedDynamicRangeColorComponentValue"

-- | @Selector@ for @maximumReferenceExtendedDynamicRangeColorComponentValue@
maximumReferenceExtendedDynamicRangeColorComponentValueSelector :: Selector '[] CDouble
maximumReferenceExtendedDynamicRangeColorComponentValueSelector = mkSelector "maximumReferenceExtendedDynamicRangeColorComponentValue"

