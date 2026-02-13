{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneInformationStruct@.
module ObjC.Matter.MTRZoneManagementClusterZoneInformationStruct
  ( MTRZoneManagementClusterZoneInformationStruct
  , IsMTRZoneManagementClusterZoneInformationStruct(..)
  , zoneID
  , setZoneID
  , zoneType
  , setZoneType
  , zoneSource
  , setZoneSource
  , twoDCartesianZone
  , setTwoDCartesianZone
  , setTwoDCartesianZoneSelector
  , setZoneIDSelector
  , setZoneSourceSelector
  , setZoneTypeSelector
  , twoDCartesianZoneSelector
  , zoneIDSelector
  , zoneSourceSelector
  , zoneTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterZoneInformationStruct =
  sendMessage mtrZoneManagementClusterZoneInformationStruct zoneIDSelector

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsNSNumber value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setZoneID mtrZoneManagementClusterZoneInformationStruct value =
  sendMessage mtrZoneManagementClusterZoneInformationStruct setZoneIDSelector (toNSNumber value)

-- | @- zoneType@
zoneType :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id NSNumber)
zoneType mtrZoneManagementClusterZoneInformationStruct =
  sendMessage mtrZoneManagementClusterZoneInformationStruct zoneTypeSelector

-- | @- setZoneType:@
setZoneType :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsNSNumber value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setZoneType mtrZoneManagementClusterZoneInformationStruct value =
  sendMessage mtrZoneManagementClusterZoneInformationStruct setZoneTypeSelector (toNSNumber value)

-- | @- zoneSource@
zoneSource :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id NSNumber)
zoneSource mtrZoneManagementClusterZoneInformationStruct =
  sendMessage mtrZoneManagementClusterZoneInformationStruct zoneSourceSelector

-- | @- setZoneSource:@
setZoneSource :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsNSNumber value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setZoneSource mtrZoneManagementClusterZoneInformationStruct value =
  sendMessage mtrZoneManagementClusterZoneInformationStruct setZoneSourceSelector (toNSNumber value)

-- | @- twoDCartesianZone@
twoDCartesianZone :: IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct => mtrZoneManagementClusterZoneInformationStruct -> IO (Id MTRZoneManagementClusterTwoDCartesianZoneStruct)
twoDCartesianZone mtrZoneManagementClusterZoneInformationStruct =
  sendMessage mtrZoneManagementClusterZoneInformationStruct twoDCartesianZoneSelector

-- | @- setTwoDCartesianZone:@
setTwoDCartesianZone :: (IsMTRZoneManagementClusterZoneInformationStruct mtrZoneManagementClusterZoneInformationStruct, IsMTRZoneManagementClusterTwoDCartesianZoneStruct value) => mtrZoneManagementClusterZoneInformationStruct -> value -> IO ()
setTwoDCartesianZone mtrZoneManagementClusterZoneInformationStruct value =
  sendMessage mtrZoneManagementClusterZoneInformationStruct setTwoDCartesianZoneSelector (toMTRZoneManagementClusterTwoDCartesianZoneStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id NSNumber)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector '[Id NSNumber] ()
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @zoneType@
zoneTypeSelector :: Selector '[] (Id NSNumber)
zoneTypeSelector = mkSelector "zoneType"

-- | @Selector@ for @setZoneType:@
setZoneTypeSelector :: Selector '[Id NSNumber] ()
setZoneTypeSelector = mkSelector "setZoneType:"

-- | @Selector@ for @zoneSource@
zoneSourceSelector :: Selector '[] (Id NSNumber)
zoneSourceSelector = mkSelector "zoneSource"

-- | @Selector@ for @setZoneSource:@
setZoneSourceSelector :: Selector '[Id NSNumber] ()
setZoneSourceSelector = mkSelector "setZoneSource:"

-- | @Selector@ for @twoDCartesianZone@
twoDCartesianZoneSelector :: Selector '[] (Id MTRZoneManagementClusterTwoDCartesianZoneStruct)
twoDCartesianZoneSelector = mkSelector "twoDCartesianZone"

-- | @Selector@ for @setTwoDCartesianZone:@
setTwoDCartesianZoneSelector :: Selector '[Id MTRZoneManagementClusterTwoDCartesianZoneStruct] ()
setTwoDCartesianZoneSelector = mkSelector "setTwoDCartesianZone:"

