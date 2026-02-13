{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , elementFullscreenEnabledSelector
  , fraudulentWebsiteWarningEnabledSelector
  , inactiveSchedulingPolicySelector
  , isLookToScrollEnabledSelector
  , javaEnabledSelector
  , javaScriptCanOpenWindowsAutomaticallySelector
  , javaScriptEnabledSelector
  , minimumFontSizeSelector
  , plugInsEnabledSelector
  , setElementFullscreenEnabledSelector
  , setFraudulentWebsiteWarningEnabledSelector
  , setInactiveSchedulingPolicySelector
  , setIsLookToScrollEnabledSelector
  , setJavaEnabledSelector
  , setJavaScriptCanOpenWindowsAutomaticallySelector
  , setJavaScriptEnabledSelector
  , setMinimumFontSizeSelector
  , setPlugInsEnabledSelector
  , setShouldPrintBackgroundsSelector
  , setSiteSpecificQuirksModeEnabledSelector
  , setTabFocusesLinksSelector
  , setTextInteractionEnabledSelector
  , shouldPrintBackgroundsSelector
  , siteSpecificQuirksModeEnabledSelector
  , tabFocusesLinksSelector
  , textInteractionEnabledSelector

  -- * Enum types
  , WKInactiveSchedulingPolicy(WKInactiveSchedulingPolicy)
  , pattern WKInactiveSchedulingPolicySuspend
  , pattern WKInactiveSchedulingPolicyThrottle
  , pattern WKInactiveSchedulingPolicyNone

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
minimumFontSize wkPreferences =
  sendMessage wkPreferences minimumFontSizeSelector

-- | The minimum font size in points.
--
-- The default value is 0.
--
-- ObjC selector: @- setMinimumFontSize:@
setMinimumFontSize :: IsWKPreferences wkPreferences => wkPreferences -> CDouble -> IO ()
setMinimumFontSize wkPreferences value =
  sendMessage wkPreferences setMinimumFontSizeSelector value

-- | A Boolean value indicating whether JavaScript can open windows without user interaction.
--
-- The default value is NO in iOS and YES in OS X.
--
-- ObjC selector: @- javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomatically :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
javaScriptCanOpenWindowsAutomatically wkPreferences =
  sendMessage wkPreferences javaScriptCanOpenWindowsAutomaticallySelector

-- | A Boolean value indicating whether JavaScript can open windows without user interaction.
--
-- The default value is NO in iOS and YES in OS X.
--
-- ObjC selector: @- setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomatically :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setJavaScriptCanOpenWindowsAutomatically wkPreferences value =
  sendMessage wkPreferences setJavaScriptCanOpenWindowsAutomaticallySelector value

-- | A Boolean value indicating whether warnings should be shown for suspected fraudulent content such as phishing or malware.
--
-- The default value is YES.
--
-- ObjC selector: @- fraudulentWebsiteWarningEnabled@
fraudulentWebsiteWarningEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
fraudulentWebsiteWarningEnabled wkPreferences =
  sendMessage wkPreferences fraudulentWebsiteWarningEnabledSelector

-- | A Boolean value indicating whether warnings should be shown for suspected fraudulent content such as phishing or malware.
--
-- The default value is YES.
--
-- ObjC selector: @- setFraudulentWebsiteWarningEnabled:@
setFraudulentWebsiteWarningEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setFraudulentWebsiteWarningEnabled wkPreferences value =
  sendMessage wkPreferences setFraudulentWebsiteWarningEnabledSelector value

-- | A Boolean value indicating whether the web view should include backgrounds when printing.
--
-- The default value is @NO@.
--
-- ObjC selector: @- shouldPrintBackgrounds@
shouldPrintBackgrounds :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
shouldPrintBackgrounds wkPreferences =
  sendMessage wkPreferences shouldPrintBackgroundsSelector

-- | A Boolean value indicating whether the web view should include backgrounds when printing.
--
-- The default value is @NO@.
--
-- ObjC selector: @- setShouldPrintBackgrounds:@
setShouldPrintBackgrounds :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setShouldPrintBackgrounds wkPreferences value =
  sendMessage wkPreferences setShouldPrintBackgroundsSelector value

-- | tabFocusesLinks
--
-- If tabFocusesLinks is YES, the tab key will focus links and form controls. The Option key temporarily reverses this preference.
--
-- ObjC selector: @- tabFocusesLinks@
tabFocusesLinks :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
tabFocusesLinks wkPreferences =
  sendMessage wkPreferences tabFocusesLinksSelector

-- | tabFocusesLinks
--
-- If tabFocusesLinks is YES, the tab key will focus links and form controls. The Option key temporarily reverses this preference.
--
-- ObjC selector: @- setTabFocusesLinks:@
setTabFocusesLinks :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setTabFocusesLinks wkPreferences value =
  sendMessage wkPreferences setTabFocusesLinksSelector value

-- | A Boolean value indicating whether text interaction is disabled.
--
-- ObjC selector: @- textInteractionEnabled@
textInteractionEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
textInteractionEnabled wkPreferences =
  sendMessage wkPreferences textInteractionEnabledSelector

-- | A Boolean value indicating whether text interaction is disabled.
--
-- ObjC selector: @- setTextInteractionEnabled:@
setTextInteractionEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setTextInteractionEnabled wkPreferences value =
  sendMessage wkPreferences setTextInteractionEnabledSelector value

-- | A Boolean value indicating whether WebKit will apply built-in workarounds (quirks) to improve compatibility with certain known websites. You can disable site-specific quirks to help test your website without these workarounds. Enabled by default.
--
-- ObjC selector: @- siteSpecificQuirksModeEnabled@
siteSpecificQuirksModeEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
siteSpecificQuirksModeEnabled wkPreferences =
  sendMessage wkPreferences siteSpecificQuirksModeEnabledSelector

-- | A Boolean value indicating whether WebKit will apply built-in workarounds (quirks) to improve compatibility with certain known websites. You can disable site-specific quirks to help test your website without these workarounds. Enabled by default.
--
-- ObjC selector: @- setSiteSpecificQuirksModeEnabled:@
setSiteSpecificQuirksModeEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setSiteSpecificQuirksModeEnabled wkPreferences value =
  sendMessage wkPreferences setSiteSpecificQuirksModeEnabledSelector value

-- | A Boolean value indicating whether Fullscreen API is enabled.
--
-- The default value is NO. We can set it to YES to enable support for the fullscreen API.
--
-- ObjC selector: @- elementFullscreenEnabled@
elementFullscreenEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
elementFullscreenEnabled wkPreferences =
  sendMessage wkPreferences elementFullscreenEnabledSelector

-- | A Boolean value indicating whether Fullscreen API is enabled.
--
-- The default value is NO. We can set it to YES to enable support for the fullscreen API.
--
-- ObjC selector: @- setElementFullscreenEnabled:@
setElementFullscreenEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setElementFullscreenEnabled wkPreferences value =
  sendMessage wkPreferences setElementFullscreenEnabledSelector value

-- | Specify the scheduling policy for the web view when it is inactive and detached from the view hierarchy. Web views are not considered idle when playing media or loading web pages. A suspended web view will pause JavaScript execution and page layout.
--
-- ObjC selector: @- inactiveSchedulingPolicy@
inactiveSchedulingPolicy :: IsWKPreferences wkPreferences => wkPreferences -> IO WKInactiveSchedulingPolicy
inactiveSchedulingPolicy wkPreferences =
  sendMessage wkPreferences inactiveSchedulingPolicySelector

-- | Specify the scheduling policy for the web view when it is inactive and detached from the view hierarchy. Web views are not considered idle when playing media or loading web pages. A suspended web view will pause JavaScript execution and page layout.
--
-- ObjC selector: @- setInactiveSchedulingPolicy:@
setInactiveSchedulingPolicy :: IsWKPreferences wkPreferences => wkPreferences -> WKInactiveSchedulingPolicy -> IO ()
setInactiveSchedulingPolicy wkPreferences value =
  sendMessage wkPreferences setInactiveSchedulingPolicySelector value

-- | @- javaEnabled@
javaEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
javaEnabled wkPreferences =
  sendMessage wkPreferences javaEnabledSelector

-- | @- setJavaEnabled:@
setJavaEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setJavaEnabled wkPreferences value =
  sendMessage wkPreferences setJavaEnabledSelector value

-- | @- plugInsEnabled@
plugInsEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
plugInsEnabled wkPreferences =
  sendMessage wkPreferences plugInsEnabledSelector

-- | @- setPlugInsEnabled:@
setPlugInsEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setPlugInsEnabled wkPreferences value =
  sendMessage wkPreferences setPlugInsEnabledSelector value

-- | @- javaScriptEnabled@
javaScriptEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
javaScriptEnabled wkPreferences =
  sendMessage wkPreferences javaScriptEnabledSelector

-- | @- setJavaScriptEnabled:@
setJavaScriptEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setJavaScriptEnabled wkPreferences value =
  sendMessage wkPreferences setJavaScriptEnabledSelector value

-- | A Boolean value indicating whether LookToScroll is enabled.
--
-- The default value is @NO@.
--
-- ObjC selector: @- isLookToScrollEnabled@
isLookToScrollEnabled :: IsWKPreferences wkPreferences => wkPreferences -> IO Bool
isLookToScrollEnabled wkPreferences =
  sendMessage wkPreferences isLookToScrollEnabledSelector

-- | A Boolean value indicating whether LookToScroll is enabled.
--
-- The default value is @NO@.
--
-- ObjC selector: @- setIsLookToScrollEnabled:@
setIsLookToScrollEnabled :: IsWKPreferences wkPreferences => wkPreferences -> Bool -> IO ()
setIsLookToScrollEnabled wkPreferences value =
  sendMessage wkPreferences setIsLookToScrollEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumFontSize@
minimumFontSizeSelector :: Selector '[] CDouble
minimumFontSizeSelector = mkSelector "minimumFontSize"

-- | @Selector@ for @setMinimumFontSize:@
setMinimumFontSizeSelector :: Selector '[CDouble] ()
setMinimumFontSizeSelector = mkSelector "setMinimumFontSize:"

-- | @Selector@ for @javaScriptCanOpenWindowsAutomatically@
javaScriptCanOpenWindowsAutomaticallySelector :: Selector '[] Bool
javaScriptCanOpenWindowsAutomaticallySelector = mkSelector "javaScriptCanOpenWindowsAutomatically"

-- | @Selector@ for @setJavaScriptCanOpenWindowsAutomatically:@
setJavaScriptCanOpenWindowsAutomaticallySelector :: Selector '[Bool] ()
setJavaScriptCanOpenWindowsAutomaticallySelector = mkSelector "setJavaScriptCanOpenWindowsAutomatically:"

-- | @Selector@ for @fraudulentWebsiteWarningEnabled@
fraudulentWebsiteWarningEnabledSelector :: Selector '[] Bool
fraudulentWebsiteWarningEnabledSelector = mkSelector "fraudulentWebsiteWarningEnabled"

-- | @Selector@ for @setFraudulentWebsiteWarningEnabled:@
setFraudulentWebsiteWarningEnabledSelector :: Selector '[Bool] ()
setFraudulentWebsiteWarningEnabledSelector = mkSelector "setFraudulentWebsiteWarningEnabled:"

-- | @Selector@ for @shouldPrintBackgrounds@
shouldPrintBackgroundsSelector :: Selector '[] Bool
shouldPrintBackgroundsSelector = mkSelector "shouldPrintBackgrounds"

-- | @Selector@ for @setShouldPrintBackgrounds:@
setShouldPrintBackgroundsSelector :: Selector '[Bool] ()
setShouldPrintBackgroundsSelector = mkSelector "setShouldPrintBackgrounds:"

-- | @Selector@ for @tabFocusesLinks@
tabFocusesLinksSelector :: Selector '[] Bool
tabFocusesLinksSelector = mkSelector "tabFocusesLinks"

-- | @Selector@ for @setTabFocusesLinks:@
setTabFocusesLinksSelector :: Selector '[Bool] ()
setTabFocusesLinksSelector = mkSelector "setTabFocusesLinks:"

-- | @Selector@ for @textInteractionEnabled@
textInteractionEnabledSelector :: Selector '[] Bool
textInteractionEnabledSelector = mkSelector "textInteractionEnabled"

-- | @Selector@ for @setTextInteractionEnabled:@
setTextInteractionEnabledSelector :: Selector '[Bool] ()
setTextInteractionEnabledSelector = mkSelector "setTextInteractionEnabled:"

-- | @Selector@ for @siteSpecificQuirksModeEnabled@
siteSpecificQuirksModeEnabledSelector :: Selector '[] Bool
siteSpecificQuirksModeEnabledSelector = mkSelector "siteSpecificQuirksModeEnabled"

-- | @Selector@ for @setSiteSpecificQuirksModeEnabled:@
setSiteSpecificQuirksModeEnabledSelector :: Selector '[Bool] ()
setSiteSpecificQuirksModeEnabledSelector = mkSelector "setSiteSpecificQuirksModeEnabled:"

-- | @Selector@ for @elementFullscreenEnabled@
elementFullscreenEnabledSelector :: Selector '[] Bool
elementFullscreenEnabledSelector = mkSelector "elementFullscreenEnabled"

-- | @Selector@ for @setElementFullscreenEnabled:@
setElementFullscreenEnabledSelector :: Selector '[Bool] ()
setElementFullscreenEnabledSelector = mkSelector "setElementFullscreenEnabled:"

-- | @Selector@ for @inactiveSchedulingPolicy@
inactiveSchedulingPolicySelector :: Selector '[] WKInactiveSchedulingPolicy
inactiveSchedulingPolicySelector = mkSelector "inactiveSchedulingPolicy"

-- | @Selector@ for @setInactiveSchedulingPolicy:@
setInactiveSchedulingPolicySelector :: Selector '[WKInactiveSchedulingPolicy] ()
setInactiveSchedulingPolicySelector = mkSelector "setInactiveSchedulingPolicy:"

-- | @Selector@ for @javaEnabled@
javaEnabledSelector :: Selector '[] Bool
javaEnabledSelector = mkSelector "javaEnabled"

-- | @Selector@ for @setJavaEnabled:@
setJavaEnabledSelector :: Selector '[Bool] ()
setJavaEnabledSelector = mkSelector "setJavaEnabled:"

-- | @Selector@ for @plugInsEnabled@
plugInsEnabledSelector :: Selector '[] Bool
plugInsEnabledSelector = mkSelector "plugInsEnabled"

-- | @Selector@ for @setPlugInsEnabled:@
setPlugInsEnabledSelector :: Selector '[Bool] ()
setPlugInsEnabledSelector = mkSelector "setPlugInsEnabled:"

-- | @Selector@ for @javaScriptEnabled@
javaScriptEnabledSelector :: Selector '[] Bool
javaScriptEnabledSelector = mkSelector "javaScriptEnabled"

-- | @Selector@ for @setJavaScriptEnabled:@
setJavaScriptEnabledSelector :: Selector '[Bool] ()
setJavaScriptEnabledSelector = mkSelector "setJavaScriptEnabled:"

-- | @Selector@ for @isLookToScrollEnabled@
isLookToScrollEnabledSelector :: Selector '[] Bool
isLookToScrollEnabledSelector = mkSelector "isLookToScrollEnabled"

-- | @Selector@ for @setIsLookToScrollEnabled:@
setIsLookToScrollEnabledSelector :: Selector '[Bool] ()
setIsLookToScrollEnabledSelector = mkSelector "setIsLookToScrollEnabled:"

