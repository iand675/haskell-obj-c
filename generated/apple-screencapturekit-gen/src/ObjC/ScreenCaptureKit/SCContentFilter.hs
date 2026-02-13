{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCContentFilter
--
-- SCContentFilter is a object that determines the exact content to be captured in the SCStream. It can be filtered through displays, windows, excluded windows or applications.
--
-- Generated bindings for @SCContentFilter@.
module ObjC.ScreenCaptureKit.SCContentFilter
  ( SCContentFilter
  , IsSCContentFilter(..)
  , initWithDesktopIndependentWindow
  , initWithDisplay_excludingWindows
  , initWithDisplay_includingWindows
  , initWithDisplay_includingApplications_exceptingWindows
  , initWithDisplay_excludingApplications_exceptingWindows
  , streamType
  , style
  , pointPixelScale
  , includeMenuBar
  , setIncludeMenuBar
  , includedDisplays
  , includedApplications
  , includedWindows
  , includeMenuBarSelector
  , includedApplicationsSelector
  , includedDisplaysSelector
  , includedWindowsSelector
  , initWithDesktopIndependentWindowSelector
  , initWithDisplay_excludingApplications_exceptingWindowsSelector
  , initWithDisplay_excludingWindowsSelector
  , initWithDisplay_includingApplications_exceptingWindowsSelector
  , initWithDisplay_includingWindowsSelector
  , pointPixelScaleSelector
  , setIncludeMenuBarSelector
  , streamTypeSelector
  , styleSelector

  -- * Enum types
  , SCShareableContentStyle(SCShareableContentStyle)
  , pattern SCShareableContentStyleNone
  , pattern SCShareableContentStyleWindow
  , pattern SCShareableContentStyleDisplay
  , pattern SCShareableContentStyleApplication
  , SCStreamType(SCStreamType)
  , pattern SCStreamTypeWindow
  , pattern SCStreamTypeDisplay

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithDesktopIndependentWindow:
--
-- @window@ — the independent SCWindow you wish to capture
--
-- this method will create a SCContentFilter that captures just the independent window passed in.
--
-- ObjC selector: @- initWithDesktopIndependentWindow:@
initWithDesktopIndependentWindow :: (IsSCContentFilter scContentFilter, IsSCWindow window) => scContentFilter -> window -> IO (Id SCContentFilter)
initWithDesktopIndependentWindow scContentFilter window =
  sendOwnedMessage scContentFilter initWithDesktopIndependentWindowSelector (toSCWindow window)

-- | initWithDisplay:excludingWindows
--
-- @display@ — the SCDisplay you wish to capture
--
-- @excluded@ — the SCWindow(s) you wish to exclude from the passed in SCDisplay
--
-- This method will create a SCContentFilter that captures the SCDisplay, excluding the passed in excluded SCWindow(s). The desktop background and dock will be included with this content filter.
--
-- ObjC selector: @- initWithDisplay:excludingWindows:@
initWithDisplay_excludingWindows :: (IsSCContentFilter scContentFilter, IsSCDisplay display, IsNSArray excluded) => scContentFilter -> display -> excluded -> IO (Id SCContentFilter)
initWithDisplay_excludingWindows scContentFilter display excluded =
  sendOwnedMessage scContentFilter initWithDisplay_excludingWindowsSelector (toSCDisplay display) (toNSArray excluded)

-- | initWithDisplay:includingWindows
--
-- @display@ — the SCDisplay you wish to capture
--
-- @includedWindows@ — a set of SCWindows you wish to capture
--
-- This method will create a SCContentFilter that captures a group of SCWindows. The desktop background and dock will be excluded with this content filter.
--
-- ObjC selector: @- initWithDisplay:includingWindows:@
initWithDisplay_includingWindows :: (IsSCContentFilter scContentFilter, IsSCDisplay display, IsNSArray includedWindows) => scContentFilter -> display -> includedWindows -> IO (Id SCContentFilter)
initWithDisplay_includingWindows scContentFilter display includedWindows =
  sendOwnedMessage scContentFilter initWithDisplay_includingWindowsSelector (toSCDisplay display) (toNSArray includedWindows)

-- | initWithDisplay:includingApplications:exceptingWindows
--
-- @display@ — the SCDisplay you wish to capture
--
-- @applications@ — the NSSet of SCRunningApplications that you wish to capture
--
-- @exceptingWindows@ — the NSSet of SCWindows that you wish to be an exception to the filter
--
-- This method creates a SCContentFilter that captures all windows owned by the passed in SCRunningApplications. Any windows that are an exception to the filter will not be shown if their owning application is in the provided list and will be shown otherwise. The desktop background and dock will be excluded with this content filter.
--
-- ObjC selector: @- initWithDisplay:includingApplications:exceptingWindows:@
initWithDisplay_includingApplications_exceptingWindows :: (IsSCContentFilter scContentFilter, IsSCDisplay display, IsNSArray applications, IsNSArray exceptingWindows) => scContentFilter -> display -> applications -> exceptingWindows -> IO (Id SCContentFilter)
initWithDisplay_includingApplications_exceptingWindows scContentFilter display applications exceptingWindows =
  sendOwnedMessage scContentFilter initWithDisplay_includingApplications_exceptingWindowsSelector (toSCDisplay display) (toNSArray applications) (toNSArray exceptingWindows)

-- | initWithDisplay:excludingApplications:exceptingWindows
--
-- @display@ — the SCDisplay you wish to capture
--
-- @applications@ — the NSSet of SCRunningApplications that you do not wish to capture
--
-- @exceptingWindows@ — the NSSet of SCWindows that you wish to be an exception to the filter
--
-- This method creates a SCContentFilter that captures all windows not owned by the passed in SCRunningApplications. Any windows that are an exception to the filter will be shown if their owning application is in the provided list and will not be shown otherwise. The desktop background and dock will be included with this content filter.
--
-- ObjC selector: @- initWithDisplay:excludingApplications:exceptingWindows:@
initWithDisplay_excludingApplications_exceptingWindows :: (IsSCContentFilter scContentFilter, IsSCDisplay display, IsNSArray applications, IsNSArray exceptingWindows) => scContentFilter -> display -> applications -> exceptingWindows -> IO (Id SCContentFilter)
initWithDisplay_excludingApplications_exceptingWindows scContentFilter display applications exceptingWindows =
  sendOwnedMessage scContentFilter initWithDisplay_excludingApplications_exceptingWindowsSelector (toSCDisplay display) (toNSArray applications) (toNSArray exceptingWindows)

-- | streamType type of stream
--
-- ObjC selector: @- streamType@
streamType :: IsSCContentFilter scContentFilter => scContentFilter -> IO SCStreamType
streamType scContentFilter =
  sendMessage scContentFilter streamTypeSelector

-- | style of stream
--
-- ObjC selector: @- style@
style :: IsSCContentFilter scContentFilter => scContentFilter -> IO SCShareableContentStyle
style scContentFilter =
  sendMessage scContentFilter styleSelector

-- | Pixel to points scaling factor
--
-- ObjC selector: @- pointPixelScale@
pointPixelScale :: IsSCContentFilter scContentFilter => scContentFilter -> IO CFloat
pointPixelScale scContentFilter =
  sendMessage scContentFilter pointPixelScaleSelector

-- | To include menu bar as part of the capture. This property has no effect for the desktop independent window filter. For content filters created with initWithDisplay:excluding, the default value is YES. Display excluding content filters contains the desktop and dock. For content filters created with initWithDisplay:including, the default value is NO. Display including content filters do not contain the desktop and dock
--
-- ObjC selector: @- includeMenuBar@
includeMenuBar :: IsSCContentFilter scContentFilter => scContentFilter -> IO Bool
includeMenuBar scContentFilter =
  sendMessage scContentFilter includeMenuBarSelector

-- | To include menu bar as part of the capture. This property has no effect for the desktop independent window filter. For content filters created with initWithDisplay:excluding, the default value is YES. Display excluding content filters contains the desktop and dock. For content filters created with initWithDisplay:including, the default value is NO. Display including content filters do not contain the desktop and dock
--
-- ObjC selector: @- setIncludeMenuBar:@
setIncludeMenuBar :: IsSCContentFilter scContentFilter => scContentFilter -> Bool -> IO ()
setIncludeMenuBar scContentFilter value =
  sendMessage scContentFilter setIncludeMenuBarSelector value

-- | SCDisplays that are included in the content filter
--
-- ObjC selector: @- includedDisplays@
includedDisplays :: IsSCContentFilter scContentFilter => scContentFilter -> IO (Id NSArray)
includedDisplays scContentFilter =
  sendMessage scContentFilter includedDisplaysSelector

-- | Applications that are included in the content filter
--
-- ObjC selector: @- includedApplications@
includedApplications :: IsSCContentFilter scContentFilter => scContentFilter -> IO (Id NSArray)
includedApplications scContentFilter =
  sendMessage scContentFilter includedApplicationsSelector

-- | Windows that are included in the content filter
--
-- ObjC selector: @- includedWindows@
includedWindows :: IsSCContentFilter scContentFilter => scContentFilter -> IO (Id NSArray)
includedWindows scContentFilter =
  sendMessage scContentFilter includedWindowsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDesktopIndependentWindow:@
initWithDesktopIndependentWindowSelector :: Selector '[Id SCWindow] (Id SCContentFilter)
initWithDesktopIndependentWindowSelector = mkSelector "initWithDesktopIndependentWindow:"

-- | @Selector@ for @initWithDisplay:excludingWindows:@
initWithDisplay_excludingWindowsSelector :: Selector '[Id SCDisplay, Id NSArray] (Id SCContentFilter)
initWithDisplay_excludingWindowsSelector = mkSelector "initWithDisplay:excludingWindows:"

-- | @Selector@ for @initWithDisplay:includingWindows:@
initWithDisplay_includingWindowsSelector :: Selector '[Id SCDisplay, Id NSArray] (Id SCContentFilter)
initWithDisplay_includingWindowsSelector = mkSelector "initWithDisplay:includingWindows:"

-- | @Selector@ for @initWithDisplay:includingApplications:exceptingWindows:@
initWithDisplay_includingApplications_exceptingWindowsSelector :: Selector '[Id SCDisplay, Id NSArray, Id NSArray] (Id SCContentFilter)
initWithDisplay_includingApplications_exceptingWindowsSelector = mkSelector "initWithDisplay:includingApplications:exceptingWindows:"

-- | @Selector@ for @initWithDisplay:excludingApplications:exceptingWindows:@
initWithDisplay_excludingApplications_exceptingWindowsSelector :: Selector '[Id SCDisplay, Id NSArray, Id NSArray] (Id SCContentFilter)
initWithDisplay_excludingApplications_exceptingWindowsSelector = mkSelector "initWithDisplay:excludingApplications:exceptingWindows:"

-- | @Selector@ for @streamType@
streamTypeSelector :: Selector '[] SCStreamType
streamTypeSelector = mkSelector "streamType"

-- | @Selector@ for @style@
styleSelector :: Selector '[] SCShareableContentStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @pointPixelScale@
pointPixelScaleSelector :: Selector '[] CFloat
pointPixelScaleSelector = mkSelector "pointPixelScale"

-- | @Selector@ for @includeMenuBar@
includeMenuBarSelector :: Selector '[] Bool
includeMenuBarSelector = mkSelector "includeMenuBar"

-- | @Selector@ for @setIncludeMenuBar:@
setIncludeMenuBarSelector :: Selector '[Bool] ()
setIncludeMenuBarSelector = mkSelector "setIncludeMenuBar:"

-- | @Selector@ for @includedDisplays@
includedDisplaysSelector :: Selector '[] (Id NSArray)
includedDisplaysSelector = mkSelector "includedDisplays"

-- | @Selector@ for @includedApplications@
includedApplicationsSelector :: Selector '[] (Id NSArray)
includedApplicationsSelector = mkSelector "includedApplications"

-- | @Selector@ for @includedWindows@
includedWindowsSelector :: Selector '[] (Id NSArray)
includedWindowsSelector = mkSelector "includedWindows"

