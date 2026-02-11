{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKPreferences object encapsulates the preference settings for a web view. The preferences object associated with a web view is specified by its web view configuration.
--
-- Generated bindings for @WKPreferences@.
module ObjC.WebKit.WKPreferences
  ( WKPreferences
  , IsWKPreferences(..)
  , minimumFontSize
  , setMinimumFontSize
  , javaScriptCanOpenWindowsAutomatically
  , setJavaScriptCanOpenWindowsAutomatically
  , fraudulentWebsiteWarningEnabled
  , setFraudulentWebsiteWarningEnabled
  , shouldPrintBackgrounds
  , setShouldPrintBackgrounds
  , tabFocusesLinks
  , setTabFocusesLinks
  , textInteractionEnabled
  , setTextInteractionEnabled
  , siteSpecificQuirksModeEnabled
  , setSiteSpecificQuirksModeEnabled
  , elementFullscreenEnabled
  , setElementFullscreenEnabled
  , inactiveSchedulingPolicy
  , setInactiveSchedulingPolicy
  , javaEnabled
  , setJavaEnabled
  , plugInsEnabled
  , setPlugInsEnabled
  , javaScriptEnabled
  , setJavaScriptEnabled
  , isLookToScrollEnabled
  , setIsLookToScrollEnabled
  , minimumFontSizeSelector
  , setMinimumFontSizeSelector
  , javaScriptCanOpenWindowsAutomaticallySelector
  , setJavaScriptCanOpenWindowsAutomaticallySelector
  , fraudulentWebsiteWarningEnabledSelector
  , setFraudulentWebsiteWarningEnabledSelector
  , shouldPrintBackgroundsSelector
  , setShouldPrintBackgroundsSelector
  , tabFocusesLinksSelector
  , setTabFocusesLinksSelector
  , textInteractionEnabledSelector
  , setTextInteractionEnabledSelector
  , siteSpecificQuirksModeEnabledSelector
  , setSiteSpecificQuirksModeEnabledSelector
  , elementFullscreenEnabledSelector
  , setElementFullscreenEnabledSelector
  , inactiveSchedulingPolicySelector
  , setInactiveSchedulingPolicySelector
  , javaEnabledSelector
  , setJavaEnabledSelector
  , plugInsEnabledSelector
  , setPlugInsEnabledSelector
  , javaScriptEnabledSelector
  , setJavaScriptEnabledSelector
  , isLookToScrollEnabledSelector
  , setIsLookToScrollEnabledSelector

  -- * Enum types
  , WKInactiveSchedulingPolicy(WKInactiveSchedulingPolicy)
  , pattern WKInactiveSchedulingPolicySuspend
  , pattern WKInactiveSchedulingPolicyThrottle
  , pattern WKInactiveSchedulingPolicyNone

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

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The minimum font size in points.
--
-- The default value is 0.
--
-- ObjC selector: @- minimumFontSize@
minimumFontSize :: IsWKPreferences wkPreferences => wkPreferences -> IO CDouble
minimumFontSize wkPreferences  =
  sendMsg wkPreferences (mkSelector "minimumFontSize") retCDouble []

-- | The minimum font size in points.
--
-- The default value is 0.
--
-- ObjC selector: @- setMinimumFontSize:@
setMinimumFontSize :: IsWKPreferences wkPreferences => wkPreferences -> CDouble -> IO ()
setMinimumFontSize wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setMinimumFontSize:") retVoid [argCDouble (fromIntegral value)]

-- | A Boolean value indicating whether JavaScript can open windows without user interaction.
--
-- The default value is NO in iOS and YES in OS X.
--
-- ObjC selector: @- javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomatically :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
javaScriptCanOpenWindowsAutomatically wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "javaScriptCanOpenWindowsAutomatically") retCULong []

-- | A Boolean value indicating whether JavaScript can open windows without user interaction.
--
-- The default value is NO in iOS and YES in OS X.
--
-- ObjC selector: @- setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomatically :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setJavaScriptCanOpenWindowsAutomatically wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setJavaScriptCanOpenWindowsAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether warnings should be shown for suspected fraudulent content such as phishing or malware.
--
-- The default value is YES.
--
-- ObjC selector: @- fraudulentWebsiteWarningEnabled@
fraudulentWebsiteWarningEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
fraudulentWebsiteWarningEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "fraudulentWebsiteWarningEnabled") retCULong []

-- | A Boolean value indicating whether warnings should be shown for suspected fraudulent content such as phishing or malware.
--
-- The default value is YES.
--
-- ObjC selector: @- setFraudulentWebsiteWarningEnabled:@
setFraudulentWebsiteWarningEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setFraudulentWebsiteWarningEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setFraudulentWebsiteWarningEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether the web view should include backgrounds when printing.
--
-- The default value is @NO@.
--
-- ObjC selector: @- shouldPrintBackgrounds@
shouldPrintBackgrounds :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
shouldPrintBackgrounds wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "shouldPrintBackgrounds") retCULong []

-- | A Boolean value indicating whether the web view should include backgrounds when printing.
--
-- The default value is @NO@.
--
-- ObjC selector: @- setShouldPrintBackgrounds:@
setShouldPrintBackgrounds :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setShouldPrintBackgrounds wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setShouldPrintBackgrounds:") retVoid [argCULong (if value then 1 else 0)]

-- | tabFocusesLinks
--
-- If tabFocusesLinks is YES, the tab key will focus links and form controls. The Option key temporarily reverses this preference.
--
-- ObjC selector: @- tabFocusesLinks@
tabFocusesLinks :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
tabFocusesLinks wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "tabFocusesLinks") retCULong []

-- | tabFocusesLinks
--
-- If tabFocusesLinks is YES, the tab key will focus links and form controls. The Option key temporarily reverses this preference.
--
-- ObjC selector: @- setTabFocusesLinks:@
setTabFocusesLinks :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setTabFocusesLinks wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setTabFocusesLinks:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether text interaction is disabled.
--
-- ObjC selector: @- textInteractionEnabled@
textInteractionEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
textInteractionEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "textInteractionEnabled") retCULong []

-- | A Boolean value indicating whether text interaction is disabled.
--
-- ObjC selector: @- setTextInteractionEnabled:@
setTextInteractionEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setTextInteractionEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setTextInteractionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether WebKit will apply built-in workarounds (quirks) to improve compatibility with certain known websites. You can disable site-specific quirks to help test your website without these workarounds. Enabled by default.
--
-- ObjC selector: @- siteSpecificQuirksModeEnabled@
siteSpecificQuirksModeEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
siteSpecificQuirksModeEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "siteSpecificQuirksModeEnabled") retCULong []

-- | A Boolean value indicating whether WebKit will apply built-in workarounds (quirks) to improve compatibility with certain known websites. You can disable site-specific quirks to help test your website without these workarounds. Enabled by default.
--
-- ObjC selector: @- setSiteSpecificQuirksModeEnabled:@
setSiteSpecificQuirksModeEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setSiteSpecificQuirksModeEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setSiteSpecificQuirksModeEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether Fullscreen API is enabled.
--
-- The default value is NO. We can set it to YES to enable support for the fullscreen API.
--
-- ObjC selector: @- elementFullscreenEnabled@
elementFullscreenEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
elementFullscreenEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "elementFullscreenEnabled") retCULong []

-- | A Boolean value indicating whether Fullscreen API is enabled.
--
-- The default value is NO. We can set it to YES to enable support for the fullscreen API.
--
-- ObjC selector: @- setElementFullscreenEnabled:@
setElementFullscreenEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setElementFullscreenEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setElementFullscreenEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Specify the scheduling policy for the web view when it is inactive and detached from the view hierarchy. Web views are not considered idle when playing media or loading web pages. A suspended web view will pause JavaScript execution and page layout.
--
-- ObjC selector: @- inactiveSchedulingPolicy@
inactiveSchedulingPolicy :: IsWKPreferences wkPreferences => wkPreferences -> IO WKInactiveSchedulingPolicy
inactiveSchedulingPolicy wkPreferences  =
  fmap (coerce :: CLong -> WKInactiveSchedulingPolicy) $ sendMsg wkPreferences (mkSelector "inactiveSchedulingPolicy") retCLong []

-- | Specify the scheduling policy for the web view when it is inactive and detached from the view hierarchy. Web views are not considered idle when playing media or loading web pages. A suspended web view will pause JavaScript execution and page layout.
--
-- ObjC selector: @- setInactiveSchedulingPolicy:@
setInactiveSchedulingPolicy :: IsWKPreferences wkPreferences => wkPreferences -> WKInactiveSchedulingPolicy -> IO ()
setInactiveSchedulingPolicy wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setInactiveSchedulingPolicy:") retVoid [argCLong (coerce value)]

-- | @- javaEnabled@
javaEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
javaEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "javaEnabled") retCULong []

-- | @- setJavaEnabled:@
setJavaEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setJavaEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setJavaEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- plugInsEnabled@
plugInsEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
plugInsEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "plugInsEnabled") retCULong []

-- | @- setPlugInsEnabled:@
setPlugInsEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setPlugInsEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setPlugInsEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- javaScriptEnabled@
javaScriptEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
javaScriptEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "javaScriptEnabled") retCULong []

-- | @- setJavaScriptEnabled:@
setJavaScriptEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setJavaScriptEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setJavaScriptEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether LookToScroll is enabled.
--
-- The default value is @NO@.
--
-- ObjC selector: @- isLookToScrollEnabled@
isLookToScrollEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
isLookToScrollEnabled wkPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkPreferences (mkSelector "isLookToScrollEnabled") retCULong []

-- | A Boolean value indicating whether LookToScroll is enabled.
--
-- The default value is @NO@.
--
-- ObjC selector: @- setIsLookToScrollEnabled:@
setIsLookToScrollEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setIsLookToScrollEnabled wkPreferences  value =
  sendMsg wkPreferences (mkSelector "setIsLookToScrollEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumFontSize@
minimumFontSizeSelector :: Selector
minimumFontSizeSelector = mkSelector "minimumFontSize"

-- | @Selector@ for @setMinimumFontSize:@
setMinimumFontSizeSelector :: Selector
setMinimumFontSizeSelector = mkSelector "setMinimumFontSize:"

-- | @Selector@ for @javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomaticallySelector :: Selector
javaScriptCanOpenWindowsAutomaticallySelector = mkSelector "javaScriptCanOpenWindowsAutomatically"

-- | @Selector@ for @setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomaticallySelector :: Selector
setJavaScriptCanOpenWindowsAutomaticallySelector = mkSelector "setJavaScriptCanOpenWindowsAutomatically:"

-- | @Selector@ for @fraudulentWebsiteWarningEnabled@
fraudulentWebsiteWarningEnabledSelector :: Selector
fraudulentWebsiteWarningEnabledSelector = mkSelector "fraudulentWebsiteWarningEnabled"

-- | @Selector@ for @setFraudulentWebsiteWarningEnabled:@
setFraudulentWebsiteWarningEnabledSelector :: Selector
setFraudulentWebsiteWarningEnabledSelector = mkSelector "setFraudulentWebsiteWarningEnabled:"

-- | @Selector@ for @shouldPrintBackgrounds@
shouldPrintBackgroundsSelector :: Selector
shouldPrintBackgroundsSelector = mkSelector "shouldPrintBackgrounds"

-- | @Selector@ for @setShouldPrintBackgrounds:@
setShouldPrintBackgroundsSelector :: Selector
setShouldPrintBackgroundsSelector = mkSelector "setShouldPrintBackgrounds:"

-- | @Selector@ for @tabFocusesLinks@
tabFocusesLinksSelector :: Selector
tabFocusesLinksSelector = mkSelector "tabFocusesLinks"

-- | @Selector@ for @setTabFocusesLinks:@
setTabFocusesLinksSelector :: Selector
setTabFocusesLinksSelector = mkSelector "setTabFocusesLinks:"

-- | @Selector@ for @textInteractionEnabled@
textInteractionEnabledSelector :: Selector
textInteractionEnabledSelector = mkSelector "textInteractionEnabled"

-- | @Selector@ for @setTextInteractionEnabled:@
setTextInteractionEnabledSelector :: Selector
setTextInteractionEnabledSelector = mkSelector "setTextInteractionEnabled:"

-- | @Selector@ for @siteSpecificQuirksModeEnabled@
siteSpecificQuirksModeEnabledSelector :: Selector
siteSpecificQuirksModeEnabledSelector = mkSelector "siteSpecificQuirksModeEnabled"

-- | @Selector@ for @setSiteSpecificQuirksModeEnabled:@
setSiteSpecificQuirksModeEnabledSelector :: Selector
setSiteSpecificQuirksModeEnabledSelector = mkSelector "setSiteSpecificQuirksModeEnabled:"

-- | @Selector@ for @elementFullscreenEnabled@
elementFullscreenEnabledSelector :: Selector
elementFullscreenEnabledSelector = mkSelector "elementFullscreenEnabled"

-- | @Selector@ for @setElementFullscreenEnabled:@
setElementFullscreenEnabledSelector :: Selector
setElementFullscreenEnabledSelector = mkSelector "setElementFullscreenEnabled:"

-- | @Selector@ for @inactiveSchedulingPolicy@
inactiveSchedulingPolicySelector :: Selector
inactiveSchedulingPolicySelector = mkSelector "inactiveSchedulingPolicy"

-- | @Selector@ for @setInactiveSchedulingPolicy:@
setInactiveSchedulingPolicySelector :: Selector
setInactiveSchedulingPolicySelector = mkSelector "setInactiveSchedulingPolicy:"

-- | @Selector@ for @javaEnabled@
javaEnabledSelector :: Selector
javaEnabledSelector = mkSelector "javaEnabled"

-- | @Selector@ for @setJavaEnabled:@
setJavaEnabledSelector :: Selector
setJavaEnabledSelector = mkSelector "setJavaEnabled:"

-- | @Selector@ for @plugInsEnabled@
plugInsEnabledSelector :: Selector
plugInsEnabledSelector = mkSelector "plugInsEnabled"

-- | @Selector@ for @setPlugInsEnabled:@
setPlugInsEnabledSelector :: Selector
setPlugInsEnabledSelector = mkSelector "setPlugInsEnabled:"

-- | @Selector@ for @javaScriptEnabled@
javaScriptEnabledSelector :: Selector
javaScriptEnabledSelector = mkSelector "javaScriptEnabled"

-- | @Selector@ for @setJavaScriptEnabled:@
setJavaScriptEnabledSelector :: Selector
setJavaScriptEnabledSelector = mkSelector "setJavaScriptEnabled:"

-- | @Selector@ for @isLookToScrollEnabled@
isLookToScrollEnabledSelector :: Selector
isLookToScrollEnabledSelector = mkSelector "isLookToScrollEnabled"

-- | @Selector@ for @setIsLookToScrollEnabled:@
setIsLookToScrollEnabledSelector :: Selector
setIsLookToScrollEnabledSelector = mkSelector "setIsLookToScrollEnabled:"

