{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCScreenshotConfiguration@.
module ObjC.ScreenCaptureKit.SCScreenshotConfiguration
  ( SCScreenshotConfiguration
  , IsSCScreenshotConfiguration(..)
  , width
  , setWidth
  , height
  , setHeight
  , showsCursor
  , setShowsCursor
  , ignoreShadows
  , setIgnoreShadows
  , ignoreClipping
  , setIgnoreClipping
  , includeChildWindows
  , setIncludeChildWindows
  , displayIntent
  , setDisplayIntent
  , dynamicRange
  , setDynamicRange
  , fileURL
  , setFileURL
  , displayIntentSelector
  , dynamicRangeSelector
  , fileURLSelector
  , heightSelector
  , ignoreClippingSelector
  , ignoreShadowsSelector
  , includeChildWindowsSelector
  , setDisplayIntentSelector
  , setDynamicRangeSelector
  , setFileURLSelector
  , setHeightSelector
  , setIgnoreClippingSelector
  , setIgnoreShadowsSelector
  , setIncludeChildWindowsSelector
  , setShowsCursorSelector
  , setWidthSelector
  , showsCursorSelector
  , widthSelector

  -- * Enum types
  , SCScreenshotDisplayIntent(SCScreenshotDisplayIntent)
  , pattern SCScreenshotDisplayIntentCanonical
  , pattern SCScreenshotDisplayIntentLocal
  , SCScreenshotDynamicRange(SCScreenshotDynamicRange)
  , pattern SCScreenshotDynamicRangeSDR
  , pattern SCScreenshotDynamicRangeHDR
  , pattern SCScreenshotDynamicRangeSDRAndHDR

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

-- | SCScreenshotProperty for output width as measured in pixels. Default is the width of the content being captured.
--
-- ObjC selector: @- width@
width :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO CLong
width scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration widthSelector

-- | SCScreenshotProperty for output width as measured in pixels. Default is the width of the content being captured.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> CLong -> IO ()
setWidth scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setWidthSelector value

-- | SCScreenshotProperty for output height as measured in pixels. Default is the height of the content being captured.
--
-- ObjC selector: @- height@
height :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO CLong
height scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration heightSelector

-- | SCScreenshotProperty for output height as measured in pixels. Default is the height of the content being captured.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> CLong -> IO ()
setHeight scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setHeightSelector value

-- | SCScreenshotProperty that specifies whether the cursor should appear in the screenshot.  By default the cursor is visible.
--
-- ObjC selector: @- showsCursor@
showsCursor :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
showsCursor scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration showsCursorSelector

-- | SCScreenshotProperty that specifies whether the cursor should appear in the screenshot.  By default the cursor is visible.
--
-- ObjC selector: @- setShowsCursor:@
setShowsCursor :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setShowsCursor scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setShowsCursorSelector value

-- | SCScreenshotProperty to ignore framing on windows (will ignore shadows).
--
-- ObjC selector: @- ignoreShadows@
ignoreShadows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
ignoreShadows scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration ignoreShadowsSelector

-- | SCScreenshotProperty to ignore framing on windows (will ignore shadows).
--
-- ObjC selector: @- setIgnoreShadows:@
setIgnoreShadows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setIgnoreShadows scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setIgnoreShadowsSelector value

-- | SCScreenshotProperty to ignore framing on windows in the display bounded sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreClipping@
ignoreClipping :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
ignoreClipping scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration ignoreClippingSelector

-- | SCScreenshotProperty to ignore framing on windows in the display bounded sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreClipping:@
setIgnoreClipping :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setIgnoreClipping scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setIgnoreClippingSelector value

-- | SCScreenshotProperty to show the child windows of the applications and windows being captured.  Child windows are included by default.
--
-- ObjC selector: @- includeChildWindows@
includeChildWindows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
includeChildWindows scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration includeChildWindowsSelector

-- | SCScreenshotProperty to show the child windows of the applications and windows being captured.  Child windows are included by default.
--
-- ObjC selector: @- setIncludeChildWindows:@
setIncludeChildWindows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setIncludeChildWindows scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setIncludeChildWindowsSelector value

-- | Specifies the render type of the screenshot.
--
-- ObjC selector: @- displayIntent@
displayIntent :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO SCScreenshotDisplayIntent
displayIntent scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration displayIntentSelector

-- | Specifies the render type of the screenshot.
--
-- ObjC selector: @- setDisplayIntent:@
setDisplayIntent :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> SCScreenshotDisplayIntent -> IO ()
setDisplayIntent scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setDisplayIntentSelector value

-- | Specifies the CGImage to return to the client.
--
-- ObjC selector: @- dynamicRange@
dynamicRange :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO SCScreenshotDynamicRange
dynamicRange scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration dynamicRangeSelector

-- | Specifies the CGImage to return to the client.
--
-- ObjC selector: @- setDynamicRange:@
setDynamicRange :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> SCScreenshotDynamicRange -> IO ()
setDynamicRange scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setDynamicRangeSelector value

-- | Specifies output URL to save the screenshot.  If the imageOutputURL is nil, then the file will not be saved.
--
-- ObjC selector: @- fileURL@
fileURL :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO (Id NSURL)
fileURL scScreenshotConfiguration =
  sendMessage scScreenshotConfiguration fileURLSelector

-- | Specifies output URL to save the screenshot.  If the imageOutputURL is nil, then the file will not be saved.
--
-- ObjC selector: @- setFileURL:@
setFileURL :: (IsSCScreenshotConfiguration scScreenshotConfiguration, IsNSURL value) => scScreenshotConfiguration -> value -> IO ()
setFileURL scScreenshotConfiguration value =
  sendMessage scScreenshotConfiguration setFileURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector '[] CLong
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CLong] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CLong
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CLong] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @showsCursor@
showsCursorSelector :: Selector '[] Bool
showsCursorSelector = mkSelector "showsCursor"

-- | @Selector@ for @setShowsCursor:@
setShowsCursorSelector :: Selector '[Bool] ()
setShowsCursorSelector = mkSelector "setShowsCursor:"

-- | @Selector@ for @ignoreShadows@
ignoreShadowsSelector :: Selector '[] Bool
ignoreShadowsSelector = mkSelector "ignoreShadows"

-- | @Selector@ for @setIgnoreShadows:@
setIgnoreShadowsSelector :: Selector '[Bool] ()
setIgnoreShadowsSelector = mkSelector "setIgnoreShadows:"

-- | @Selector@ for @ignoreClipping@
ignoreClippingSelector :: Selector '[] Bool
ignoreClippingSelector = mkSelector "ignoreClipping"

-- | @Selector@ for @setIgnoreClipping:@
setIgnoreClippingSelector :: Selector '[Bool] ()
setIgnoreClippingSelector = mkSelector "setIgnoreClipping:"

-- | @Selector@ for @includeChildWindows@
includeChildWindowsSelector :: Selector '[] Bool
includeChildWindowsSelector = mkSelector "includeChildWindows"

-- | @Selector@ for @setIncludeChildWindows:@
setIncludeChildWindowsSelector :: Selector '[Bool] ()
setIncludeChildWindowsSelector = mkSelector "setIncludeChildWindows:"

-- | @Selector@ for @displayIntent@
displayIntentSelector :: Selector '[] SCScreenshotDisplayIntent
displayIntentSelector = mkSelector "displayIntent"

-- | @Selector@ for @setDisplayIntent:@
setDisplayIntentSelector :: Selector '[SCScreenshotDisplayIntent] ()
setDisplayIntentSelector = mkSelector "setDisplayIntent:"

-- | @Selector@ for @dynamicRange@
dynamicRangeSelector :: Selector '[] SCScreenshotDynamicRange
dynamicRangeSelector = mkSelector "dynamicRange"

-- | @Selector@ for @setDynamicRange:@
setDynamicRangeSelector :: Selector '[SCScreenshotDynamicRange] ()
setDynamicRangeSelector = mkSelector "setDynamicRange:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector '[Id NSURL] ()
setFileURLSelector = mkSelector "setFileURL:"

