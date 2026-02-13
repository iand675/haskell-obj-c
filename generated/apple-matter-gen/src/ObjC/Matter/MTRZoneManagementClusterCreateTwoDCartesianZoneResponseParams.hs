{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams@.
module ObjC.Matter.MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams
  ( MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams
  , IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams(..)
  , initWithResponseValue_error
  , zoneID
  , setZoneID
  , initWithResponseValue_errorSelector
  , setZoneIDSelector
  , zoneIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams -> responseValue -> error_ -> IO (Id MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams)
initWithResponseValue_error mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams responseValue error_ =
  sendOwnedMessage mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams => mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams =
  sendMessage mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams zoneIDSelector

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams, IsNSNumber value) => mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams -> value -> IO ()
setZoneID mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams value =
  sendMessage mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams setZoneIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id NSNumber)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector '[Id NSNumber] ()
setZoneIDSelector = mkSelector "setZoneID:"

