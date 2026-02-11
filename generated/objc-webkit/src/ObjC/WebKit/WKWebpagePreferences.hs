{-# LANGUAGE PatternSynonyms #-}
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
  , preferredContentModeSelector
  , setPreferredContentModeSelector
  , allowsContentJavaScriptSelector
  , setAllowsContentJavaScriptSelector
  , lockdownModeEnabledSelector
  , setLockdownModeEnabledSelector
  , preferredHTTPSNavigationPolicySelector
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

-- | A WKContentMode indicating the content mode to prefer when loading and rendering a webpage.
--
-- The default value is WKContentModeRecommended. The stated preference is ignored on subframe navigation
--
-- ObjC selector: @- preferredContentMode@
preferredContentMode :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO WKContentMode
preferredContentMode wkWebpagePreferences  =
  fmap (coerce :: CLong -> WKContentMode) $ sendMsg wkWebpagePreferences (mkSelector "preferredContentMode") retCLong []

-- | A WKContentMode indicating the content mode to prefer when loading and rendering a webpage.
--
-- The default value is WKContentModeRecommended. The stated preference is ignored on subframe navigation
--
-- ObjC selector: @- setPreferredContentMode:@
setPreferredContentMode :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> WKContentMode -> IO ()
setPreferredContentMode wkWebpagePreferences  value =
  sendMsg wkWebpagePreferences (mkSelector "setPreferredContentMode:") retVoid [argCLong (coerce value)]

-- | @- allowsContentJavaScript@
allowsContentJavaScript :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO Bool
allowsContentJavaScript wkWebpagePreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebpagePreferences (mkSelector "allowsContentJavaScript") retCULong []

-- | @- setAllowsContentJavaScript:@
setAllowsContentJavaScript :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> Bool -> IO ()
setAllowsContentJavaScript wkWebpagePreferences  value =
  sendMsg wkWebpagePreferences (mkSelector "setAllowsContentJavaScript:") retVoid [argCULong (if value then 1 else 0)]

-- | A boolean indicating whether lockdown mode is enabled.
--
-- This mode trades off performance and compatibility in favor of security. The default value depends on the system setting.
--
-- ObjC selector: @- lockdownModeEnabled@
lockdownModeEnabled :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO Bool
lockdownModeEnabled wkWebpagePreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebpagePreferences (mkSelector "lockdownModeEnabled") retCULong []

-- | A boolean indicating whether lockdown mode is enabled.
--
-- This mode trades off performance and compatibility in favor of security. The default value depends on the system setting.
--
-- ObjC selector: @- setLockdownModeEnabled:@
setLockdownModeEnabled :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> Bool -> IO ()
setLockdownModeEnabled wkWebpagePreferences  value =
  sendMsg wkWebpagePreferences (mkSelector "setLockdownModeEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A WKWebpagePreferencesUpgradeToHTTPSPolicy indicating the desired mode used when performing a top-level navigation to a webpage.
--
-- The default value is WKWebpagePreferencesUpgradeToHTTPSPolicyKeepAsRequested. The stated preference is ignored on subframe navigation, and it may be ignored based on system configuration. The upgradeKnownHostsToHTTPS property on WKWebViewConfiguration supercedes this policy for known hosts.
--
-- ObjC selector: @- preferredHTTPSNavigationPolicy@
preferredHTTPSNavigationPolicy :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> IO WKWebpagePreferencesUpgradeToHTTPSPolicy
preferredHTTPSNavigationPolicy wkWebpagePreferences  =
  fmap (coerce :: CLong -> WKWebpagePreferencesUpgradeToHTTPSPolicy) $ sendMsg wkWebpagePreferences (mkSelector "preferredHTTPSNavigationPolicy") retCLong []

-- | A WKWebpagePreferencesUpgradeToHTTPSPolicy indicating the desired mode used when performing a top-level navigation to a webpage.
--
-- The default value is WKWebpagePreferencesUpgradeToHTTPSPolicyKeepAsRequested. The stated preference is ignored on subframe navigation, and it may be ignored based on system configuration. The upgradeKnownHostsToHTTPS property on WKWebViewConfiguration supercedes this policy for known hosts.
--
-- ObjC selector: @- setPreferredHTTPSNavigationPolicy:@
setPreferredHTTPSNavigationPolicy :: IsWKWebpagePreferences wkWebpagePreferences => wkWebpagePreferences -> WKWebpagePreferencesUpgradeToHTTPSPolicy -> IO ()
setPreferredHTTPSNavigationPolicy wkWebpagePreferences  value =
  sendMsg wkWebpagePreferences (mkSelector "setPreferredHTTPSNavigationPolicy:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preferredContentMode@
preferredContentModeSelector :: Selector
preferredContentModeSelector = mkSelector "preferredContentMode"

-- | @Selector@ for @setPreferredContentMode:@
setPreferredContentModeSelector :: Selector
setPreferredContentModeSelector = mkSelector "setPreferredContentMode:"

-- | @Selector@ for @allowsContentJavaScript@
allowsContentJavaScriptSelector :: Selector
allowsContentJavaScriptSelector = mkSelector "allowsContentJavaScript"

-- | @Selector@ for @setAllowsContentJavaScript:@
setAllowsContentJavaScriptSelector :: Selector
setAllowsContentJavaScriptSelector = mkSelector "setAllowsContentJavaScript:"

-- | @Selector@ for @lockdownModeEnabled@
lockdownModeEnabledSelector :: Selector
lockdownModeEnabledSelector = mkSelector "lockdownModeEnabled"

-- | @Selector@ for @setLockdownModeEnabled:@
setLockdownModeEnabledSelector :: Selector
setLockdownModeEnabledSelector = mkSelector "setLockdownModeEnabled:"

-- | @Selector@ for @preferredHTTPSNavigationPolicy@
preferredHTTPSNavigationPolicySelector :: Selector
preferredHTTPSNavigationPolicySelector = mkSelector "preferredHTTPSNavigationPolicy"

-- | @Selector@ for @setPreferredHTTPSNavigationPolicy:@
setPreferredHTTPSNavigationPolicySelector :: Selector
setPreferredHTTPSNavigationPolicySelector = mkSelector "setPreferredHTTPSNavigationPolicy:"

