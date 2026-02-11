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
  , usageTimeSelector
  , reportApplicationIdentifierSelector
  , textInputSessionsSelector
  , supplementalCategoriesSelector
  , relativeStartTimeSelector


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

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The bundle identifier of the app in use. Only populated for Apple apps.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSString)
bundleIdentifier srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The amount of time the app is used
--
-- ObjC selector: @- usageTime@
usageTime :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO CDouble
usageTime srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "usageTime") retCDouble []

-- | reportApplicationIdentifier
--
-- An application identifier that is valid for the duration of the report.
--
-- This is useful for identifying distinct application uses within the same report duration without revealing the actual application identifier.
--
-- ObjC selector: @- reportApplicationIdentifier@
reportApplicationIdentifier :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSString)
reportApplicationIdentifier srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "reportApplicationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | textInputSessions
--
-- The text input session types that occurred during this application usage
--
-- The list of text input sessions describes the order and type of text input that may have occured during an application usage. Multiple sessions of the same text input type will appear as separate array entries. If no text input occurred, this array will be empty.
--
-- ObjC selector: @- textInputSessions@
textInputSessions :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSArray)
textInputSessions srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "textInputSessions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | supplementalCategories
--
-- Additional categories that describe this app
--
-- ObjC selector: @- supplementalCategories@
supplementalCategories :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO (Id NSArray)
supplementalCategories srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "supplementalCategories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | relativeStartTime
--
-- App start time relative to the first app start time in the report interval
--
-- relativeStartTime value for the very first app in the report interval is equal to 0, N seconds for the seccond app and so on. This will allow to order app uses and determine the time between app uses.
--
-- ObjC selector: @- relativeStartTime@
relativeStartTime :: IsSRApplicationUsage srApplicationUsage => srApplicationUsage -> IO CDouble
relativeStartTime srApplicationUsage  =
    sendMsg srApplicationUsage (mkSelector "relativeStartTime") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @usageTime@
usageTimeSelector :: Selector
usageTimeSelector = mkSelector "usageTime"

-- | @Selector@ for @reportApplicationIdentifier@
reportApplicationIdentifierSelector :: Selector
reportApplicationIdentifierSelector = mkSelector "reportApplicationIdentifier"

-- | @Selector@ for @textInputSessions@
textInputSessionsSelector :: Selector
textInputSessionsSelector = mkSelector "textInputSessions"

-- | @Selector@ for @supplementalCategories@
supplementalCategoriesSelector :: Selector
supplementalCategoriesSelector = mkSelector "supplementalCategories"

-- | @Selector@ for @relativeStartTime@
relativeStartTimeSelector :: Selector
relativeStartTimeSelector = mkSelector "relativeStartTime"

