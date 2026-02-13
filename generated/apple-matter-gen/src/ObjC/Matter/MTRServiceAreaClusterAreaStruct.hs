{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterAreaStruct@.
module ObjC.Matter.MTRServiceAreaClusterAreaStruct
  ( MTRServiceAreaClusterAreaStruct
  , IsMTRServiceAreaClusterAreaStruct(..)
  , areaID
  , setAreaID
  , mapID
  , setMapID
  , areaInfo
  , setAreaInfo
  , areaIDSelector
  , areaInfoSelector
  , mapIDSelector
  , setAreaIDSelector
  , setAreaInfoSelector
  , setMapIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- areaID@
areaID :: IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct => mtrServiceAreaClusterAreaStruct -> IO (Id NSNumber)
areaID mtrServiceAreaClusterAreaStruct =
  sendMessage mtrServiceAreaClusterAreaStruct areaIDSelector

-- | @- setAreaID:@
setAreaID :: (IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct, IsNSNumber value) => mtrServiceAreaClusterAreaStruct -> value -> IO ()
setAreaID mtrServiceAreaClusterAreaStruct value =
  sendMessage mtrServiceAreaClusterAreaStruct setAreaIDSelector (toNSNumber value)

-- | @- mapID@
mapID :: IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct => mtrServiceAreaClusterAreaStruct -> IO (Id NSNumber)
mapID mtrServiceAreaClusterAreaStruct =
  sendMessage mtrServiceAreaClusterAreaStruct mapIDSelector

-- | @- setMapID:@
setMapID :: (IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct, IsNSNumber value) => mtrServiceAreaClusterAreaStruct -> value -> IO ()
setMapID mtrServiceAreaClusterAreaStruct value =
  sendMessage mtrServiceAreaClusterAreaStruct setMapIDSelector (toNSNumber value)

-- | @- areaInfo@
areaInfo :: IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct => mtrServiceAreaClusterAreaStruct -> IO (Id MTRServiceAreaClusterAreaInfoStruct)
areaInfo mtrServiceAreaClusterAreaStruct =
  sendMessage mtrServiceAreaClusterAreaStruct areaInfoSelector

-- | @- setAreaInfo:@
setAreaInfo :: (IsMTRServiceAreaClusterAreaStruct mtrServiceAreaClusterAreaStruct, IsMTRServiceAreaClusterAreaInfoStruct value) => mtrServiceAreaClusterAreaStruct -> value -> IO ()
setAreaInfo mtrServiceAreaClusterAreaStruct value =
  sendMessage mtrServiceAreaClusterAreaStruct setAreaInfoSelector (toMTRServiceAreaClusterAreaInfoStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areaID@
areaIDSelector :: Selector '[] (Id NSNumber)
areaIDSelector = mkSelector "areaID"

-- | @Selector@ for @setAreaID:@
setAreaIDSelector :: Selector '[Id NSNumber] ()
setAreaIDSelector = mkSelector "setAreaID:"

-- | @Selector@ for @mapID@
mapIDSelector :: Selector '[] (Id NSNumber)
mapIDSelector = mkSelector "mapID"

-- | @Selector@ for @setMapID:@
setMapIDSelector :: Selector '[Id NSNumber] ()
setMapIDSelector = mkSelector "setMapID:"

-- | @Selector@ for @areaInfo@
areaInfoSelector :: Selector '[] (Id MTRServiceAreaClusterAreaInfoStruct)
areaInfoSelector = mkSelector "areaInfo"

-- | @Selector@ for @setAreaInfo:@
setAreaInfoSelector :: Selector '[Id MTRServiceAreaClusterAreaInfoStruct] ()
setAreaInfoSelector = mkSelector "setAreaInfo:"

