{-# LANGUAGE PatternSynonyms #-}
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
  , canRepresentDisplayGamutSelector
  , convertRectToBackingSelector
  , convertRectFromBackingSelector
  , backingAlignedRect_optionsSelector
  , userSpaceScaleFactorSelector
  , displayLinkWithTarget_selectorSelector
  , screensSelector
  , mainScreenSelector
  , deepestScreenSelector
  , screensHaveSeparateSpacesSelector
  , depthSelector
  , frameSelector
  , visibleFrameSelector
  , deviceDescriptionSelector
  , colorSpaceSelector
  , supportedWindowDepthsSelector
  , backingScaleFactorSelector
  , localizedNameSelector
  , safeAreaInsetsSelector
  , auxiliaryTopLeftAreaSelector
  , auxiliaryTopRightAreaSelector
  , cgDirectDisplayIDSelector
  , maximumFramesPerSecondSelector
  , minimumRefreshIntervalSelector
  , maximumRefreshIntervalSelector
  , displayUpdateGranularitySelector
  , lastDisplayUpdateTimestampSelector
  , maximumExtendedDynamicRangeColorComponentValueSelector
  , maximumPotentialExtendedDynamicRangeColorComponentValueSelector
  , maximumReferenceExtendedDynamicRangeColorComponentValueSelector

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- canRepresentDisplayGamut:@
canRepresentDisplayGamut :: IsNSScreen nsScreen => nsScreen -> NSDisplayGamut -> IO Bool
canRepresentDisplayGamut nsScreen  displayGamut =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScreen (mkSelector "canRepresentDisplayGamut:") retCULong [argCLong (coerce displayGamut)]

-- | @- convertRectToBacking:@
convertRectToBacking :: IsNSScreen nsScreen => nsScreen -> NSRect -> IO NSRect
convertRectToBacking nsScreen  rect =
    sendMsgStret nsScreen (mkSelector "convertRectToBacking:") retNSRect [argNSRect rect]

-- | @- convertRectFromBacking:@
convertRectFromBacking :: IsNSScreen nsScreen => nsScreen -> NSRect -> IO NSRect
convertRectFromBacking nsScreen  rect =
    sendMsgStret nsScreen (mkSelector "convertRectFromBacking:") retNSRect [argNSRect rect]

-- | @- backingAlignedRect:options:@
backingAlignedRect_options :: IsNSScreen nsScreen => nsScreen -> NSRect -> NSAlignmentOptions -> IO NSRect
backingAlignedRect_options nsScreen  rect options =
    sendMsgStret nsScreen (mkSelector "backingAlignedRect:options:") retNSRect [argNSRect rect, argCULong (coerce options)]

-- | @- userSpaceScaleFactor@
userSpaceScaleFactor :: IsNSScreen nsScreen => nsScreen -> IO CDouble
userSpaceScaleFactor nsScreen  =
    sendMsg nsScreen (mkSelector "userSpaceScaleFactor") retCDouble []

-- | @- displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: IsNSScreen nsScreen => nsScreen -> RawId -> Selector -> IO (Id CADisplayLink)
displayLinkWithTarget_selector nsScreen  target selector =
    sendMsg nsScreen (mkSelector "displayLinkWithTarget:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | @+ screens@
screens :: IO (Id NSArray)
screens  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMsg cls' (mkSelector "screens") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mainScreen@
mainScreen :: IO (Id NSScreen)
mainScreen  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMsg cls' (mkSelector "mainScreen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ deepestScreen@
deepestScreen :: IO (Id NSScreen)
deepestScreen  =
  do
    cls' <- getRequiredClass "NSScreen"
    sendClassMsg cls' (mkSelector "deepestScreen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ screensHaveSeparateSpaces@
screensHaveSeparateSpaces :: IO Bool
screensHaveSeparateSpaces  =
  do
    cls' <- getRequiredClass "NSScreen"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "screensHaveSeparateSpaces") retCULong []

-- | @- depth@
depth :: IsNSScreen nsScreen => nsScreen -> IO NSWindowDepth
depth nsScreen  =
    fmap (coerce :: CInt -> NSWindowDepth) $ sendMsg nsScreen (mkSelector "depth") retCInt []

-- | @- frame@
frame :: IsNSScreen nsScreen => nsScreen -> IO NSRect
frame nsScreen  =
    sendMsgStret nsScreen (mkSelector "frame") retNSRect []

-- | @- visibleFrame@
visibleFrame :: IsNSScreen nsScreen => nsScreen -> IO NSRect
visibleFrame nsScreen  =
    sendMsgStret nsScreen (mkSelector "visibleFrame") retNSRect []

-- | @- deviceDescription@
deviceDescription :: IsNSScreen nsScreen => nsScreen -> IO (Id NSDictionary)
deviceDescription nsScreen  =
    sendMsg nsScreen (mkSelector "deviceDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- colorSpace@
colorSpace :: IsNSScreen nsScreen => nsScreen -> IO (Id NSColorSpace)
colorSpace nsScreen  =
    sendMsg nsScreen (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supportedWindowDepths@
supportedWindowDepths :: IsNSScreen nsScreen => nsScreen -> IO RawId
supportedWindowDepths nsScreen  =
    fmap (RawId . castPtr) $ sendMsg nsScreen (mkSelector "supportedWindowDepths") (retPtr retVoid) []

-- | @- backingScaleFactor@
backingScaleFactor :: IsNSScreen nsScreen => nsScreen -> IO CDouble
backingScaleFactor nsScreen  =
    sendMsg nsScreen (mkSelector "backingScaleFactor") retCDouble []

-- | @- localizedName@
localizedName :: IsNSScreen nsScreen => nsScreen -> IO (Id NSString)
localizedName nsScreen  =
    sendMsg nsScreen (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- safeAreaInsets@
safeAreaInsets :: IsNSScreen nsScreen => nsScreen -> IO NSEdgeInsets
safeAreaInsets nsScreen  =
    sendMsgStret nsScreen (mkSelector "safeAreaInsets") retNSEdgeInsets []

-- | @- auxiliaryTopLeftArea@
auxiliaryTopLeftArea :: IsNSScreen nsScreen => nsScreen -> IO NSRect
auxiliaryTopLeftArea nsScreen  =
    sendMsgStret nsScreen (mkSelector "auxiliaryTopLeftArea") retNSRect []

-- | @- auxiliaryTopRightArea@
auxiliaryTopRightArea :: IsNSScreen nsScreen => nsScreen -> IO NSRect
auxiliaryTopRightArea nsScreen  =
    sendMsgStret nsScreen (mkSelector "auxiliaryTopRightArea") retNSRect []

-- | The CGDirectDisplayID for this screen. This will return kCGNullDirectDisplay if there isn't one.
--
-- ObjC selector: @- CGDirectDisplayID@
cgDirectDisplayID :: IsNSScreen nsScreen => nsScreen -> IO CUInt
cgDirectDisplayID nsScreen  =
    sendMsg nsScreen (mkSelector "CGDirectDisplayID") retCUInt []

-- | The maximum frames per second this screen supports.
--
-- ObjC selector: @- maximumFramesPerSecond@
maximumFramesPerSecond :: IsNSScreen nsScreen => nsScreen -> IO CLong
maximumFramesPerSecond nsScreen  =
    sendMsg nsScreen (mkSelector "maximumFramesPerSecond") retCLong []

-- | The minimum refresh interval this screen supports, in seconds.
--
-- This is the shortest amount of time a frame will be present on screen.    minimumRefreshInterval and maximumRefreshInterval will be the same for displays that do not support variable refresh rates.
--
-- ObjC selector: @- minimumRefreshInterval@
minimumRefreshInterval :: IsNSScreen nsScreen => nsScreen -> IO CDouble
minimumRefreshInterval nsScreen  =
    sendMsg nsScreen (mkSelector "minimumRefreshInterval") retCDouble []

-- | The maximum refresh interval this screen supports, in seconds.
--
-- minimumRefreshInterval and maximumRefreshInterval will be the same for displays that do not support variable refresh rates.
--
-- ObjC selector: @- maximumRefreshInterval@
maximumRefreshInterval :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumRefreshInterval nsScreen  =
    sendMsg nsScreen (mkSelector "maximumRefreshInterval") retCDouble []

-- | The update granularity of the screen's current mode, in seconds.
--
-- The display will update at the next boundary defined by the granularity, after the minimum refresh interval has been reached. When 0, the display can update at any time between the minimum and maximum refresh rate intervals of the screen. Fixed refresh rate screen modes will return the refresh interval as the update granularity (e.g. 16.66ms for 60Hz refresh rates), meaning updates only occur at refresh rate boundaries.
--
-- ObjC selector: @- displayUpdateGranularity@
displayUpdateGranularity :: IsNSScreen nsScreen => nsScreen -> IO CDouble
displayUpdateGranularity nsScreen  =
    sendMsg nsScreen (mkSelector "displayUpdateGranularity") retCDouble []

-- | The time at which the last framebuffer update occurred on the display, in seconds since startup that the system has been awake.
--
-- ObjC selector: @- lastDisplayUpdateTimestamp@
lastDisplayUpdateTimestamp :: IsNSScreen nsScreen => nsScreen -> IO CDouble
lastDisplayUpdateTimestamp nsScreen  =
    sendMsg nsScreen (mkSelector "lastDisplayUpdateTimestamp") retCDouble []

-- | @- maximumExtendedDynamicRangeColorComponentValue@
maximumExtendedDynamicRangeColorComponentValue :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumExtendedDynamicRangeColorComponentValue nsScreen  =
    sendMsg nsScreen (mkSelector "maximumExtendedDynamicRangeColorComponentValue") retCDouble []

-- | @- maximumPotentialExtendedDynamicRangeColorComponentValue@
maximumPotentialExtendedDynamicRangeColorComponentValue :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumPotentialExtendedDynamicRangeColorComponentValue nsScreen  =
    sendMsg nsScreen (mkSelector "maximumPotentialExtendedDynamicRangeColorComponentValue") retCDouble []

-- | @- maximumReferenceExtendedDynamicRangeColorComponentValue@
maximumReferenceExtendedDynamicRangeColorComponentValue :: IsNSScreen nsScreen => nsScreen -> IO CDouble
maximumReferenceExtendedDynamicRangeColorComponentValue nsScreen  =
    sendMsg nsScreen (mkSelector "maximumReferenceExtendedDynamicRangeColorComponentValue") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canRepresentDisplayGamut:@
canRepresentDisplayGamutSelector :: Selector
canRepresentDisplayGamutSelector = mkSelector "canRepresentDisplayGamut:"

-- | @Selector@ for @convertRectToBacking:@
convertRectToBackingSelector :: Selector
convertRectToBackingSelector = mkSelector "convertRectToBacking:"

-- | @Selector@ for @convertRectFromBacking:@
convertRectFromBackingSelector :: Selector
convertRectFromBackingSelector = mkSelector "convertRectFromBacking:"

-- | @Selector@ for @backingAlignedRect:options:@
backingAlignedRect_optionsSelector :: Selector
backingAlignedRect_optionsSelector = mkSelector "backingAlignedRect:options:"

-- | @Selector@ for @userSpaceScaleFactor@
userSpaceScaleFactorSelector :: Selector
userSpaceScaleFactorSelector = mkSelector "userSpaceScaleFactor"

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @screens@
screensSelector :: Selector
screensSelector = mkSelector "screens"

-- | @Selector@ for @mainScreen@
mainScreenSelector :: Selector
mainScreenSelector = mkSelector "mainScreen"

-- | @Selector@ for @deepestScreen@
deepestScreenSelector :: Selector
deepestScreenSelector = mkSelector "deepestScreen"

-- | @Selector@ for @screensHaveSeparateSpaces@
screensHaveSeparateSpacesSelector :: Selector
screensHaveSeparateSpacesSelector = mkSelector "screensHaveSeparateSpaces"

-- | @Selector@ for @depth@
depthSelector :: Selector
depthSelector = mkSelector "depth"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @visibleFrame@
visibleFrameSelector :: Selector
visibleFrameSelector = mkSelector "visibleFrame"

-- | @Selector@ for @deviceDescription@
deviceDescriptionSelector :: Selector
deviceDescriptionSelector = mkSelector "deviceDescription"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @supportedWindowDepths@
supportedWindowDepthsSelector :: Selector
supportedWindowDepthsSelector = mkSelector "supportedWindowDepths"

-- | @Selector@ for @backingScaleFactor@
backingScaleFactorSelector :: Selector
backingScaleFactorSelector = mkSelector "backingScaleFactor"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @safeAreaInsets@
safeAreaInsetsSelector :: Selector
safeAreaInsetsSelector = mkSelector "safeAreaInsets"

-- | @Selector@ for @auxiliaryTopLeftArea@
auxiliaryTopLeftAreaSelector :: Selector
auxiliaryTopLeftAreaSelector = mkSelector "auxiliaryTopLeftArea"

-- | @Selector@ for @auxiliaryTopRightArea@
auxiliaryTopRightAreaSelector :: Selector
auxiliaryTopRightAreaSelector = mkSelector "auxiliaryTopRightArea"

-- | @Selector@ for @CGDirectDisplayID@
cgDirectDisplayIDSelector :: Selector
cgDirectDisplayIDSelector = mkSelector "CGDirectDisplayID"

-- | @Selector@ for @maximumFramesPerSecond@
maximumFramesPerSecondSelector :: Selector
maximumFramesPerSecondSelector = mkSelector "maximumFramesPerSecond"

-- | @Selector@ for @minimumRefreshInterval@
minimumRefreshIntervalSelector :: Selector
minimumRefreshIntervalSelector = mkSelector "minimumRefreshInterval"

-- | @Selector@ for @maximumRefreshInterval@
maximumRefreshIntervalSelector :: Selector
maximumRefreshIntervalSelector = mkSelector "maximumRefreshInterval"

-- | @Selector@ for @displayUpdateGranularity@
displayUpdateGranularitySelector :: Selector
displayUpdateGranularitySelector = mkSelector "displayUpdateGranularity"

-- | @Selector@ for @lastDisplayUpdateTimestamp@
lastDisplayUpdateTimestampSelector :: Selector
lastDisplayUpdateTimestampSelector = mkSelector "lastDisplayUpdateTimestamp"

-- | @Selector@ for @maximumExtendedDynamicRangeColorComponentValue@
maximumExtendedDynamicRangeColorComponentValueSelector :: Selector
maximumExtendedDynamicRangeColorComponentValueSelector = mkSelector "maximumExtendedDynamicRangeColorComponentValue"

-- | @Selector@ for @maximumPotentialExtendedDynamicRangeColorComponentValue@
maximumPotentialExtendedDynamicRangeColorComponentValueSelector :: Selector
maximumPotentialExtendedDynamicRangeColorComponentValueSelector = mkSelector "maximumPotentialExtendedDynamicRangeColorComponentValue"

-- | @Selector@ for @maximumReferenceExtendedDynamicRangeColorComponentValue@
maximumReferenceExtendedDynamicRangeColorComponentValueSelector :: Selector
maximumReferenceExtendedDynamicRangeColorComponentValueSelector = mkSelector "maximumReferenceExtendedDynamicRangeColorComponentValue"

