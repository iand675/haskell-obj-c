{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRApplicationUsage@.
module ObjC.SensorKit.SRApplicationUsage
  ( SRApplicationUsage
  , IsSRApplicationUsage(..)
  , bundleIdentifier
  , usageTime
  , reportApplicationIdentifier
  , textInputSessions
  , supplementalCategories
  , relativeStartTime
  , bundleIdentifierSelector
  , relativeStartTimeSelector
  , reportApplicationIdentifierSelector
  , supplementalCategoriesSelector
  , textInputSessionsSelector
  , usageTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The bundle identifier of the app in use. Only populated for Apple apps.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSString)
bundleIdentifier srApplicationUsage =
  sendMessage srApplicationUsage bundleIdentifierSelector

-- | The amount of time the app is used
--
-- ObjC selector: @- usageTime@
usageTime :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO CDouble
usageTime srApplicationUsage =
  sendMessage srApplicationUsage usageTimeSelector

-- | reportApplicationIdentifier
--
-- An application identifier that is valid for the duration of the report.
--
-- This is useful for identifying distinct application uses within the same report duration without revealing the actual application identifier.
--
-- ObjC selector: @- reportApplicationIdentifier@
reportApplicationIdentifier :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSString)
reportApplicationIdentifier srApplicationUsage =
  sendMessage srApplicationUsage reportApplicationIdentifierSelector

-- | textInputSessions
--
-- The text input session types that occurred during this application usage
--
-- The list of text input sessions describes the order and type of text input that may have occured during an application usage. Multiple sessions of the same text input type will appear as separate array entries. If no text input occurred, this array will be empty.
--
-- ObjC selector: @- textInputSessions@
textInputSessions :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSArray)
textInputSessions srApplicationUsage =
  sendMessage srApplicationUsage textInputSessionsSelector

-- | supplementalCategories
--
-- Additional categories that describe this app
--
-- ObjC selector: @- supplementalCategories@
supplementalCategories :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSArray)
supplementalCategories srApplicationUsage =
  sendMessage srApplicationUsage supplementalCategoriesSelector

-- | relativeStartTime
--
-- App start time relative to the first app start time in the report interval
--
-- relativeStartTime value for the very first app in the report interval is equal to 0, N seconds for the seccond app and so on. This will allow to order app uses and determine the time between app uses.
--
-- ObjC selector: @- relativeStartTime@
relativeStartTime :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO CDouble
relativeStartTime srApplicationUsage =
  sendMessage srApplicationUsage relativeStartTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @usageTime@
usageTimeSelector :: Selector '[] CDouble
usageTimeSelector = mkSelector "usageTime"

-- | @Selector@ for @reportApplicationIdentifier@
reportApplicationIdentifierSelector :: Selector '[] (Id NSString)
reportApplicationIdentifierSelector = mkSelector "reportApplicationIdentifier"

-- | @Selector@ for @textInputSessions@
textInputSessionsSelector :: Selector '[] (Id NSArray)
textInputSessionsSelector = mkSelector "textInputSessions"

-- | @Selector@ for @supplementalCategories@
supplementalCategoriesSelector :: Selector '[] (Id NSArray)
supplementalCategoriesSelector = mkSelector "supplementalCategories"

-- | @Selector@ for @relativeStartTime@
relativeStartTimeSelector :: Selector '[] CDouble
relativeStartTimeSelector = mkSelector "relativeStartTime"

