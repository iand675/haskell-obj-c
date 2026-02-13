{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The object you use to delete web-usage data.
--
-- This class provides an easy way for you to delete web history, including:
--
-- - All history - History associated to a specific URL - History during a specific time interval
--
-- Generated bindings for @STWebHistory@.
module ObjC.ScreenTime.STWebHistory
  ( STWebHistory
  , IsSTWebHistory(..)
  , initWithBundleIdentifier_profileIdentifier_error
  , initWithProfileIdentifier
  , initWithBundleIdentifier_error
  , deleteHistoryForURL
  , deleteHistoryDuringInterval
  , deleteAllHistory
  , deleteAllHistorySelector
  , deleteHistoryDuringIntervalSelector
  , deleteHistoryForURLSelector
  , initWithBundleIdentifier_errorSelector
  , initWithBundleIdentifier_profileIdentifier_errorSelector
  , initWithProfileIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenTime.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a web history instance to delete web-usage data associated to the bundle identifier and profile identifier you specify.
--
-- The default value for @bundleIdentifier@ is @Bundle.main.bundleIdentifier@. This is the recommended identifier to use, except for example, if a helper process is presenting web UI and you want to group that web-usage under the main app’s bundle identifier.
--
-- The default value for @profileIdentifier@ is @nil@. This identifier can be used to delete browsing history for a specific profile. Using @nil@ will only delete web history reported without a profile identifier.
--
-- - Parameters:   - bundleIdentifier: The bundle identifier.   - profileIdentifier: The identifier of the current browsing profile.   - error: Any error that occurred while changing the bundle identifier.
--
-- ObjC selector: @- initWithBundleIdentifier:profileIdentifier:error:@
initWithBundleIdentifier_profileIdentifier_error :: (IsSTWebHistory stWebHistory, IsNSString bundleIdentifier, IsNSString profileIdentifier, IsNSError error_) => stWebHistory -> bundleIdentifier -> profileIdentifier -> error_ -> IO (Id STWebHistory)
initWithBundleIdentifier_profileIdentifier_error stWebHistory bundleIdentifier profileIdentifier error_ =
  sendOwnedMessage stWebHistory initWithBundleIdentifier_profileIdentifier_errorSelector (toNSString bundleIdentifier) (toNSString profileIdentifier) (toNSError error_)

-- | Creates a web history instance to delete web-usage data associated to the profile identifier you specify.
--
-- The default value for @profileIdentifier@ is @nil@. This identifier can be used to delete browsing history for a specific profile. Using @nil@ will only delete web history reported without a profile identifier.
--
-- - Parameters:   - profileIdentifier: The identifier of the current browsing profile.
--
-- ObjC selector: @- initWithProfileIdentifier:@
initWithProfileIdentifier :: (IsSTWebHistory stWebHistory, IsNSString profileIdentifier) => stWebHistory -> profileIdentifier -> IO (Id STWebHistory)
initWithProfileIdentifier stWebHistory profileIdentifier =
  sendOwnedMessage stWebHistory initWithProfileIdentifierSelector (toNSString profileIdentifier)

-- | Creates a web history instance to delete web-usage data associated to the bundle identifier you specify.
--
-- The default value for @bundleIdentifier@ is @Bundle.main.bundleIdentifier@. This is the recommended identifier to use, except for example, if a helper process is presenting web UI and you want to group that web-usage under the main app’s bundle identifier.
--
-- - Parameters:   - bundleIdentifier: The bundle identifier.   - error: Any error that occurred while changing the bundle identifier.
--
-- ObjC selector: @- initWithBundleIdentifier:error:@
initWithBundleIdentifier_error :: (IsSTWebHistory stWebHistory, IsNSString bundleIdentifier, IsNSError error_) => stWebHistory -> bundleIdentifier -> error_ -> IO (Id STWebHistory)
initWithBundleIdentifier_error stWebHistory bundleIdentifier error_ =
  sendOwnedMessage stWebHistory initWithBundleIdentifier_errorSelector (toNSString bundleIdentifier) (toNSError error_)

-- | Deletes all the web history for the URL you specify.
--
-- The framework references the entire URL to determine which web-usage data to delete.
--
-- - Parameters:   - url: The URL associated with the web history to delete.
--
-- ObjC selector: @- deleteHistoryForURL:@
deleteHistoryForURL :: (IsSTWebHistory stWebHistory, IsNSURL url) => stWebHistory -> url -> IO ()
deleteHistoryForURL stWebHistory url =
  sendMessage stWebHistory deleteHistoryForURLSelector (toNSURL url)

-- | Deletes web history that occurred during the date interval you specify.
--
-- - Parameters:   - interval: The date interval of web history you want to delete.
--
-- ObjC selector: @- deleteHistoryDuringInterval:@
deleteHistoryDuringInterval :: (IsSTWebHistory stWebHistory, IsNSDateInterval interval) => stWebHistory -> interval -> IO ()
deleteHistoryDuringInterval stWebHistory interval =
  sendMessage stWebHistory deleteHistoryDuringIntervalSelector (toNSDateInterval interval)

-- | Deletes all web history associated with the bundle identifier you specified during initialization.
--
-- ObjC selector: @- deleteAllHistory@
deleteAllHistory :: IsSTWebHistory stWebHistory => stWebHistory -> IO ()
deleteAllHistory stWebHistory =
  sendMessage stWebHistory deleteAllHistorySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundleIdentifier:profileIdentifier:error:@
initWithBundleIdentifier_profileIdentifier_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] (Id STWebHistory)
initWithBundleIdentifier_profileIdentifier_errorSelector = mkSelector "initWithBundleIdentifier:profileIdentifier:error:"

-- | @Selector@ for @initWithProfileIdentifier:@
initWithProfileIdentifierSelector :: Selector '[Id NSString] (Id STWebHistory)
initWithProfileIdentifierSelector = mkSelector "initWithProfileIdentifier:"

-- | @Selector@ for @initWithBundleIdentifier:error:@
initWithBundleIdentifier_errorSelector :: Selector '[Id NSString, Id NSError] (Id STWebHistory)
initWithBundleIdentifier_errorSelector = mkSelector "initWithBundleIdentifier:error:"

-- | @Selector@ for @deleteHistoryForURL:@
deleteHistoryForURLSelector :: Selector '[Id NSURL] ()
deleteHistoryForURLSelector = mkSelector "deleteHistoryForURL:"

-- | @Selector@ for @deleteHistoryDuringInterval:@
deleteHistoryDuringIntervalSelector :: Selector '[Id NSDateInterval] ()
deleteHistoryDuringIntervalSelector = mkSelector "deleteHistoryDuringInterval:"

-- | @Selector@ for @deleteAllHistory@
deleteAllHistorySelector :: Selector '[] ()
deleteAllHistorySelector = mkSelector "deleteAllHistory"

