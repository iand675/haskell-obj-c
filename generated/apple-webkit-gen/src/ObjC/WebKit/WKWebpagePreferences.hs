{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKWebpagePreferences object is a collection of properties that determine the preferences to use when loading and rendering a page.
--
-- Contains properties used to determine webpage preferences.
--
-- Generated bindings for @WKWebpagePreferences@.
module ObjC.WebKit.WKWebpagePreferences
  ( WKWebpagePreferences
  , IsWKWebpagePreferences(..)
  , preferredContentMode
  , setPreferredContentMode
  , allowsContentJavaScript
  , setAllowsContentJavaScript
  , lockdownModeEnabled
  , setLockdownModeEnabled
  , preferredHTTPSNavigationPolicy
  , setPreferredHTTPSNavigationPolicy
  , allowsContentJavaScriptSelector
  , lockdownModeEnabledSelector
  , preferredContentModeSelector
  , preferredHTTPSNavigationPolicySelector
  , setAllowsContentJavaScriptSelector
  , setLockdownModeEnabledSelector
  , setPreferredContentModeSelector
  , setPreferredHTTPSNavigationPolicySelector

  -- * Enum types
  , WKContentMode(WKContentMode)
  , pattern WKContentModeRecommended
  , pattern WKContentModeMobile
  , pattern WKContentModeDesktop
  , WKWebpagePreferencesUpgradeToHTTPSPolicy(WKWebpagePreferencesUpgradeToHTTPSPolicy)
  , pattern WKWebpagePreferencesUpgradeToHTTPSPolicyKeepAsRequested
  , pattern WKWebpagePreferencesUpgradeToHTTPSPolicyAutomaticFallbackToHTTP
  , pattern WKWebpagePreferencesUpgradeToHTTPSPolicyUserMediatedFallbackToHTTP
  , pattern WKWebpagePreferencesUpgradeToHTTPSPolicyErrorOnFailure

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

-- | A WKContentMode indicating the content mode to prefer when loading and rendering a webpage.
--
-- The default value is WKContentModeRecommended. The stated preference is ignored on subframe navigation
--
-- ObjC selector: @- preferredContentMode@
preferredContentMode :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO WKContentMode
preferredContentMode wkWebpagePreferences =
  sendMessage wkWebpagePreferences preferredContentModeSelector

-- | A WKContentMode indicating the content mode to prefer when loading and rendering a webpage.
--
-- The default value is WKContentModeRecommended. The stated preference is ignored on subframe navigation
--
-- ObjC selector: @- setPreferredContentMode:@
setPreferredContentMode :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> WKContentMode -> IO ()
setPreferredContentMode wkWebpagePreferences value =
  sendMessage wkWebpagePreferences setPreferredContentModeSelector value

-- | @- allowsContentJavaScript@
allowsContentJavaScript :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO Bool
allowsContentJavaScript wkWebpagePreferences =
  sendMessage wkWebpagePreferences allowsContentJavaScriptSelector

-- | @- setAllowsContentJavaScript:@
setAllowsContentJavaScript :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> Bool -> IO ()
setAllowsContentJavaScript wkWebpagePreferences value =
  sendMessage wkWebpagePreferences setAllowsContentJavaScriptSelector value

-- | A boolean indicating whether lockdown mode is enabled.
--
-- This mode trades off performance and compatibility in favor of security. The default value depends on the system setting.
--
-- ObjC selector: @- lockdownModeEnabled@
lockdownModeEnabled :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO Bool
lockdownModeEnabled wkWebpagePreferences =
  sendMessage wkWebpagePreferences lockdownModeEnabledSelector

-- | A boolean indicating whether lockdown mode is enabled.
--
-- This mode trades off performance and compatibility in favor of security. The default value depends on the system setting.
--
-- ObjC selector: @- setLockdownModeEnabled:@
setLockdownModeEnabled :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> Bool -> IO ()
setLockdownModeEnabled wkWebpagePreferences value =
  sendMessage wkWebpagePreferences setLockdownModeEnabledSelector value

-- | A WKWebpagePreferencesUpgradeToHTTPSPolicy indicating the desired mode used when performing a top-level navigation to a webpage.
--
-- The default value is WKWebpagePreferencesUpgradeToHTTPSPolicyKeepAsRequested. The stated preference is ignored on subframe navigation, and it may be ignored based on system configuration. The upgradeKnownHostsToHTTPS property on WKWebViewConfiguration supercedes this policy for known hosts.
--
-- ObjC selector: @- preferredHTTPSNavigationPolicy@
preferredHTTPSNavigationPolicy :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO WKWebpagePreferencesUpgradeToHTTPSPolicy
preferredHTTPSNavigationPolicy wkWebpagePreferences =
  sendMessage wkWebpagePreferences preferredHTTPSNavigationPolicySelector

-- | A WKWebpagePreferencesUpgradeToHTTPSPolicy indicating the desired mode used when performing a top-level navigation to a webpage.
--
-- The default value is WKWebpagePreferencesUpgradeToHTTPSPolicyKeepAsRequested. The stated preference is ignored on subframe navigation, and it may be ignored based on system configuration. The upgradeKnownHostsToHTTPS property on WKWebViewConfiguration supercedes this policy for known hosts.
--
-- ObjC selector: @- setPreferredHTTPSNavigationPolicy:@
setPreferredHTTPSNavigationPolicy :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> WKWebpagePreferencesUpgradeToHTTPSPolicy -> IO ()
setPreferredHTTPSNavigationPolicy wkWebpagePreferences value =
  sendMessage wkWebpagePreferences setPreferredHTTPSNavigationPolicySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preferredContentMode@
preferredContentModeSelector :: Selector '[] WKContentMode
preferredContentModeSelector = mkSelector "preferredContentMode"

-- | @Selector@ for @setPreferredContentMode:@
setPreferredContentModeSelector :: Selector '[WKContentMode] ()
setPreferredContentModeSelector = mkSelector "setPreferredContentMode:"

-- | @Selector@ for @allowsContentJavaScript@
allowsContentJavaScriptSelector :: Selector '[] Bool
allowsContentJavaScriptSelector = mkSelector "allowsContentJavaScript"

-- | @Selector@ for @setAllowsContentJavaScript:@
setAllowsContentJavaScriptSelector :: Selector '[Bool] ()
setAllowsContentJavaScriptSelector = mkSelector "setAllowsContentJavaScript:"

-- | @Selector@ for @lockdownModeEnabled@
lockdownModeEnabledSelector :: Selector '[] Bool
lockdownModeEnabledSelector = mkSelector "lockdownModeEnabled"

-- | @Selector@ for @setLockdownModeEnabled:@
setLockdownModeEnabledSelector :: Selector '[Bool] ()
setLockdownModeEnabledSelector = mkSelector "setLockdownModeEnabled:"

-- | @Selector@ for @preferredHTTPSNavigationPolicy@
preferredHTTPSNavigationPolicySelector :: Selector '[] WKWebpagePreferencesUpgradeToHTTPSPolicy
preferredHTTPSNavigationPolicySelector = mkSelector "preferredHTTPSNavigationPolicy"

-- | @Selector@ for @setPreferredHTTPSNavigationPolicy:@
setPreferredHTTPSNavigationPolicySelector :: Selector '[WKWebpagePreferencesUpgradeToHTTPSPolicy] ()
setPreferredHTTPSNavigationPolicySelector = mkSelector "setPreferredHTTPSNavigationPolicy:"

