{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMMovementDisorderManager
--
-- A CMMovementDisorderManager object with methods for persistence and query of movement disorder results.
--
-- Generated bindings for @CMMovementDisorderManager@.
module ObjC.CoreMotion.CMMovementDisorderManager
  ( CMMovementDisorderManager
  , IsCMMovementDisorderManager(..)
  , isAvailable
  , version
  , authorizationStatus
  , monitorKinesiasForDuration
  , queryDyskineticSymptomFromDate_toDate_withHandler
  , queryTremorFromDate_toDate_withHandler
  , lastProcessedDate
  , monitorKinesiasExpirationDate
  , authorizationStatusSelector
  , isAvailableSelector
  , lastProcessedDateSelector
  , monitorKinesiasExpirationDateSelector
  , monitorKinesiasForDurationSelector
  , queryDyskineticSymptomFromDate_toDate_withHandlerSelector
  , queryTremorFromDate_toDate_withHandlerSelector
  , versionSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | isAvailable
--
-- Whether movement disorder results are available on this platform.
--
-- Returns: Returns the availability of movement disorder results on this platform.
--
-- ObjC selector: @+ isAvailable@
isAvailable :: IO Bool
isAvailable  =
  do
    cls' <- getRequiredClass "CMMovementDisorderManager"
    sendClassMessage cls' isAvailableSelector

-- | version
--
-- What version of movement disorder software is available on this platform.
--
-- Returns: Returns the version number of the movement disorder software available on this platform, nil if not.         Format follows Major.Minor.Fix format (e.g. 1.0.0)
--
-- ObjC selector: @+ version@
version :: IO (Id NSString)
version  =
  do
    cls' <- getRequiredClass "CMMovementDisorderManager"
    sendClassMessage cls' versionSelector

-- | authorizationStatus
--
-- Authorization status of movement disorder results for this user.
--
-- Returns: Returns the authorization status of movement disorder results for this user.
--
-- ObjC selector: @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMMovementDisorderManager"
    sendClassMessage cls' authorizationStatusSelector

-- | monitorKinesiasForDuration:
--
-- Enables the calculation and persistence of result values for the specified duration in seconds.
--
-- @duration@ — The duration in seconds to enable the calculation and persistence of result values.
--
-- Warning: Please note that the maximum duration allowed is seven (7) days.
--
-- ObjC selector: @- monitorKinesiasForDuration:@
monitorKinesiasForDuration :: IsCMMovementDisorderManager cmMovementDisorderManager => cmMovementDisorderManager -> CDouble -> IO ()
monitorKinesiasForDuration cmMovementDisorderManager duration =
  sendMessage cmMovementDisorderManager monitorKinesiasForDurationSelector duration

-- | queryDyskineticSymptomFromDate:toDate:withHandler:
--
-- Queries the system for result values for the specified date range.
--
-- @fromDate@ — The begin date for the query range.
--
-- @toDate@ — The end date for the query range.
--
-- @handler@ — The completion handler for accessing and processing result values.
--
-- Warning: Please note that movement disorder results are available for a maximum of seven (7) days.
--
-- ObjC selector: @- queryDyskineticSymptomFromDate:toDate:withHandler:@
queryDyskineticSymptomFromDate_toDate_withHandler :: (IsCMMovementDisorderManager cmMovementDisorderManager, IsNSDate fromDate, IsNSDate toDate) => cmMovementDisorderManager -> fromDate -> toDate -> Ptr () -> IO ()
queryDyskineticSymptomFromDate_toDate_withHandler cmMovementDisorderManager fromDate toDate handler =
  sendMessage cmMovementDisorderManager queryDyskineticSymptomFromDate_toDate_withHandlerSelector (toNSDate fromDate) (toNSDate toDate) handler

-- | queryTremorFromDate:toDate:withHandler:
--
-- Queries the system for result values for the specified date range.
--
-- @fromDate@ — The begin date for the query range.
--
-- @toDate@ — The end date for the query range.
--
-- @handler@ — The completion handler for accessing and processing result values.
--
-- Warning: Please note that movement disorder results are available for a maximum of seven (7) days.
--
-- ObjC selector: @- queryTremorFromDate:toDate:withHandler:@
queryTremorFromDate_toDate_withHandler :: (IsCMMovementDisorderManager cmMovementDisorderManager, IsNSDate fromDate, IsNSDate toDate) => cmMovementDisorderManager -> fromDate -> toDate -> Ptr () -> IO ()
queryTremorFromDate_toDate_withHandler cmMovementDisorderManager fromDate toDate handler =
  sendMessage cmMovementDisorderManager queryTremorFromDate_toDate_withHandlerSelector (toNSDate fromDate) (toNSDate toDate) handler

-- | lastProcessedDate
--
-- The last time that data has been processed; queries for periods before this point will return their final results. Data after this point may become available later if monitoring is continuing.
--
-- Warning: Returns nil if no data has been processed or monitoring was not enabled.
--
-- ObjC selector: @- lastProcessedDate@
lastProcessedDate :: IsCMMovementDisorderManager cmMovementDisorderManager => cmMovementDisorderManager -> IO (Id NSDate)
lastProcessedDate cmMovementDisorderManager =
  sendMessage cmMovementDisorderManager lastProcessedDateSelector

-- | monitorKinesiasExpirationDate
--
-- The expiration date for the most recent monitoring period.
--
-- Warning: Returns nil if no previous monitoring period is available.
--
-- ObjC selector: @- monitorKinesiasExpirationDate@
monitorKinesiasExpirationDate :: IsCMMovementDisorderManager cmMovementDisorderManager => cmMovementDisorderManager -> IO (Id NSDate)
monitorKinesiasExpirationDate cmMovementDisorderManager =
  sendMessage cmMovementDisorderManager monitorKinesiasExpirationDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAvailable@
isAvailableSelector :: Selector '[] Bool
isAvailableSelector = mkSelector "isAvailable"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @monitorKinesiasForDuration:@
monitorKinesiasForDurationSelector :: Selector '[CDouble] ()
monitorKinesiasForDurationSelector = mkSelector "monitorKinesiasForDuration:"

-- | @Selector@ for @queryDyskineticSymptomFromDate:toDate:withHandler:@
queryDyskineticSymptomFromDate_toDate_withHandlerSelector :: Selector '[Id NSDate, Id NSDate, Ptr ()] ()
queryDyskineticSymptomFromDate_toDate_withHandlerSelector = mkSelector "queryDyskineticSymptomFromDate:toDate:withHandler:"

-- | @Selector@ for @queryTremorFromDate:toDate:withHandler:@
queryTremorFromDate_toDate_withHandlerSelector :: Selector '[Id NSDate, Id NSDate, Ptr ()] ()
queryTremorFromDate_toDate_withHandlerSelector = mkSelector "queryTremorFromDate:toDate:withHandler:"

-- | @Selector@ for @lastProcessedDate@
lastProcessedDateSelector :: Selector '[] (Id NSDate)
lastProcessedDateSelector = mkSelector "lastProcessedDate"

-- | @Selector@ for @monitorKinesiasExpirationDate@
monitorKinesiasExpirationDateSelector :: Selector '[] (Id NSDate)
monitorKinesiasExpirationDateSelector = mkSelector "monitorKinesiasExpirationDate"

