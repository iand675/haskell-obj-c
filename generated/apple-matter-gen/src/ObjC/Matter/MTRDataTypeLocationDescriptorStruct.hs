{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeLocationDescriptorStruct@.
module ObjC.Matter.MTRDataTypeLocationDescriptorStruct
  ( MTRDataTypeLocationDescriptorStruct
  , IsMTRDataTypeLocationDescriptorStruct(..)
  , locationName
  , setLocationName
  , floorNumber
  , setFloorNumber
  , areaType
  , setAreaType
  , areaTypeSelector
  , floorNumberSelector
  , locationNameSelector
  , setAreaTypeSelector
  , setFloorNumberSelector
  , setLocationNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- locationName@
locationName :: IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct => mtrDataTypeLocationDescriptorStruct -> IO (Id NSString)
locationName mtrDataTypeLocationDescriptorStruct =
  sendMessage mtrDataTypeLocationDescriptorStruct locationNameSelector

-- | @- setLocationName:@
setLocationName :: (IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct, IsNSString value) => mtrDataTypeLocationDescriptorStruct -> value -> IO ()
setLocationName mtrDataTypeLocationDescriptorStruct value =
  sendMessage mtrDataTypeLocationDescriptorStruct setLocationNameSelector (toNSString value)

-- | @- floorNumber@
floorNumber :: IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct => mtrDataTypeLocationDescriptorStruct -> IO (Id NSNumber)
floorNumber mtrDataTypeLocationDescriptorStruct =
  sendMessage mtrDataTypeLocationDescriptorStruct floorNumberSelector

-- | @- setFloorNumber:@
setFloorNumber :: (IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct, IsNSNumber value) => mtrDataTypeLocationDescriptorStruct -> value -> IO ()
setFloorNumber mtrDataTypeLocationDescriptorStruct value =
  sendMessage mtrDataTypeLocationDescriptorStruct setFloorNumberSelector (toNSNumber value)

-- | @- areaType@
areaType :: IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct => mtrDataTypeLocationDescriptorStruct -> IO (Id NSNumber)
areaType mtrDataTypeLocationDescriptorStruct =
  sendMessage mtrDataTypeLocationDescriptorStruct areaTypeSelector

-- | @- setAreaType:@
setAreaType :: (IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct, IsNSNumber value) => mtrDataTypeLocationDescriptorStruct -> value -> IO ()
setAreaType mtrDataTypeLocationDescriptorStruct value =
  sendMessage mtrDataTypeLocationDescriptorStruct setAreaTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationName@
locationNameSelector :: Selector '[] (Id NSString)
locationNameSelector = mkSelector "locationName"

-- | @Selector@ for @setLocationName:@
setLocationNameSelector :: Selector '[Id NSString] ()
setLocationNameSelector = mkSelector "setLocationName:"

-- | @Selector@ for @floorNumber@
floorNumberSelector :: Selector '[] (Id NSNumber)
floorNumberSelector = mkSelector "floorNumber"

-- | @Selector@ for @setFloorNumber:@
setFloorNumberSelector :: Selector '[Id NSNumber] ()
setFloorNumberSelector = mkSelector "setFloorNumber:"

-- | @Selector@ for @areaType@
areaTypeSelector :: Selector '[] (Id NSNumber)
areaTypeSelector = mkSelector "areaType"

-- | @Selector@ for @setAreaType:@
setAreaTypeSelector :: Selector '[Id NSNumber] ()
setAreaTypeSelector = mkSelector "setAreaType:"

