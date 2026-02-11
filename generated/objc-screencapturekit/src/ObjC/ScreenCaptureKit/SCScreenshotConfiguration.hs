{-# LANGUAGE PatternSynonyms #-}
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
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , showsCursorSelector
  , setShowsCursorSelector
  , ignoreShadowsSelector
  , setIgnoreShadowsSelector
  , ignoreClippingSelector
  , setIgnoreClippingSelector
  , includeChildWindowsSelector
  , setIncludeChildWindowsSelector
  , displayIntentSelector
  , setDisplayIntentSelector
  , dynamicRangeSelector
  , setDynamicRangeSelector
  , fileURLSelector
  , setFileURLSelector

  -- * Enum types
  , SCScreenshotDisplayIntent(SCScreenshotDisplayIntent)
  , pattern SCScreenshotDisplayIntentCanonical
  , pattern SCScreenshotDisplayIntentLocal
  , SCScreenshotDynamicRange(SCScreenshotDynamicRange)
  , pattern SCScreenshotDynamicRangeSDR
  , pattern SCScreenshotDynamicRangeHDR
  , pattern SCScreenshotDynamicRangeSDRAndHDR

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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | SCScreenshotProperty for output width as measured in pixels. Default is the width of the content being captured.
--
-- ObjC selector: @- width@
width :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO CLong
width scScreenshotConfiguration  =
  sendMsg scScreenshotConfiguration (mkSelector "width") retCLong []

-- | SCScreenshotProperty for output width as measured in pixels. Default is the width of the content being captured.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> CLong -> IO ()
setWidth scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setWidth:") retVoid [argCLong (fromIntegral value)]

-- | SCScreenshotProperty for output height as measured in pixels. Default is the height of the content being captured.
--
-- ObjC selector: @- height@
height :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO CLong
height scScreenshotConfiguration  =
  sendMsg scScreenshotConfiguration (mkSelector "height") retCLong []

-- | SCScreenshotProperty for output height as measured in pixels. Default is the height of the content being captured.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> CLong -> IO ()
setHeight scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setHeight:") retVoid [argCLong (fromIntegral value)]

-- | SCScreenshotProperty that specifies whether the cursor should appear in the screenshot.  By default the cursor is visible.
--
-- ObjC selector: @- showsCursor@
showsCursor :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
showsCursor scScreenshotConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scScreenshotConfiguration (mkSelector "showsCursor") retCULong []

-- | SCScreenshotProperty that specifies whether the cursor should appear in the screenshot.  By default the cursor is visible.
--
-- ObjC selector: @- setShowsCursor:@
setShowsCursor :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setShowsCursor scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setShowsCursor:") retVoid [argCULong (if value then 1 else 0)]

-- | SCScreenshotProperty to ignore framing on windows (will ignore shadows).
--
-- ObjC selector: @- ignoreShadows@
ignoreShadows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
ignoreShadows scScreenshotConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scScreenshotConfiguration (mkSelector "ignoreShadows") retCULong []

-- | SCScreenshotProperty to ignore framing on windows (will ignore shadows).
--
-- ObjC selector: @- setIgnoreShadows:@
setIgnoreShadows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setIgnoreShadows scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setIgnoreShadows:") retVoid [argCULong (if value then 1 else 0)]

-- | SCScreenshotProperty to ignore framing on windows in the display bounded sharing case (will ignore shadows).
--
-- ObjC selector: @- ignoreClipping@
ignoreClipping :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
ignoreClipping scScreenshotConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scScreenshotConfiguration (mkSelector "ignoreClipping") retCULong []

-- | SCScreenshotProperty to ignore framing on windows in the display bounded sharing case (will ignore shadows).
--
-- ObjC selector: @- setIgnoreClipping:@
setIgnoreClipping :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setIgnoreClipping scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setIgnoreClipping:") retVoid [argCULong (if value then 1 else 0)]

-- | SCScreenshotProperty to show the child windows of the applications and windows being captured.  Child windows are included by default.
--
-- ObjC selector: @- includeChildWindows@
includeChildWindows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO Bool
includeChildWindows scScreenshotConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scScreenshotConfiguration (mkSelector "includeChildWindows") retCULong []

-- | SCScreenshotProperty to show the child windows of the applications and windows being captured.  Child windows are included by default.
--
-- ObjC selector: @- setIncludeChildWindows:@
setIncludeChildWindows :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> Bool -> IO ()
setIncludeChildWindows scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setIncludeChildWindows:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies the render type of the screenshot.
--
-- ObjC selector: @- displayIntent@
displayIntent :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO SCScreenshotDisplayIntent
displayIntent scScreenshotConfiguration  =
  fmap (coerce :: CLong -> SCScreenshotDisplayIntent) $ sendMsg scScreenshotConfiguration (mkSelector "displayIntent") retCLong []

-- | Specifies the render type of the screenshot.
--
-- ObjC selector: @- setDisplayIntent:@
setDisplayIntent :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> SCScreenshotDisplayIntent -> IO ()
setDisplayIntent scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setDisplayIntent:") retVoid [argCLong (coerce value)]

-- | Specifies the CGImage to return to the client.
--
-- ObjC selector: @- dynamicRange@
dynamicRange :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO SCScreenshotDynamicRange
dynamicRange scScreenshotConfiguration  =
  fmap (coerce :: CLong -> SCScreenshotDynamicRange) $ sendMsg scScreenshotConfiguration (mkSelector "dynamicRange") retCLong []

-- | Specifies the CGImage to return to the client.
--
-- ObjC selector: @- setDynamicRange:@
setDynamicRange :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> SCScreenshotDynamicRange -> IO ()
setDynamicRange scScreenshotConfiguration  value =
  sendMsg scScreenshotConfiguration (mkSelector "setDynamicRange:") retVoid [argCLong (coerce value)]

-- | Specifies output URL to save the screenshot.  If the imageOutputURL is nil, then the file will not be saved.
--
-- ObjC selector: @- fileURL@
fileURL :: IsSCScreenshotConfiguration scScreenshotConfiguration => scScreenshotConfiguration -> IO (Id NSURL)
fileURL scScreenshotConfiguration  =
  sendMsg scScreenshotConfiguration (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies output URL to save the screenshot.  If the imageOutputURL is nil, then the file will not be saved.
--
-- ObjC selector: @- setFileURL:@
setFileURL :: (IsSCScreenshotConfiguration scScreenshotConfiguration, IsNSURL value) => scScreenshotConfiguration -> value -> IO ()
setFileURL scScreenshotConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg scScreenshotConfiguration (mkSelector "setFileURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @showsCursor@
showsCursorSelector :: Selector
showsCursorSelector = mkSelector "showsCursor"

-- | @Selector@ for @setShowsCursor:@
setShowsCursorSelector :: Selector
setShowsCursorSelector = mkSelector "setShowsCursor:"

-- | @Selector@ for @ignoreShadows@
ignoreShadowsSelector :: Selector
ignoreShadowsSelector = mkSelector "ignoreShadows"

-- | @Selector@ for @setIgnoreShadows:@
setIgnoreShadowsSelector :: Selector
setIgnoreShadowsSelector = mkSelector "setIgnoreShadows:"

-- | @Selector@ for @ignoreClipping@
ignoreClippingSelector :: Selector
ignoreClippingSelector = mkSelector "ignoreClipping"

-- | @Selector@ for @setIgnoreClipping:@
setIgnoreClippingSelector :: Selector
setIgnoreClippingSelector = mkSelector "setIgnoreClipping:"

-- | @Selector@ for @includeChildWindows@
includeChildWindowsSelector :: Selector
includeChildWindowsSelector = mkSelector "includeChildWindows"

-- | @Selector@ for @setIncludeChildWindows:@
setIncludeChildWindowsSelector :: Selector
setIncludeChildWindowsSelector = mkSelector "setIncludeChildWindows:"

-- | @Selector@ for @displayIntent@
displayIntentSelector :: Selector
displayIntentSelector = mkSelector "displayIntent"

-- | @Selector@ for @setDisplayIntent:@
setDisplayIntentSelector :: Selector
setDisplayIntentSelector = mkSelector "setDisplayIntent:"

-- | @Selector@ for @dynamicRange@
dynamicRangeSelector :: Selector
dynamicRangeSelector = mkSelector "dynamicRange"

-- | @Selector@ for @setDynamicRange:@
setDynamicRangeSelector :: Selector
setDynamicRangeSelector = mkSelector "setDynamicRange:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector
setFileURLSelector = mkSelector "setFileURL:"

