{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREcosystemInformationClusterEcosystemLocationStruct@.
module ObjC.Matter.MTREcosystemInformationClusterEcosystemLocationStruct
  ( MTREcosystemInformationClusterEcosystemLocationStruct
  , IsMTREcosystemInformationClusterEcosystemLocationStruct(..)
  , uniqueLocationID
  , setUniqueLocationID
  , locationDescriptor
  , setLocationDescriptor
  , locationDescriptorLastEdit
  , setLocationDescriptorLastEdit
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
  , locationDescriptorLastEditSelector
  , locationDescriptorSelector
  , setFabricIndexSelector
  , setLocationDescriptorLastEditSelector
  , setLocationDescriptorSelector
  , setUniqueLocationIDSelector
  , uniqueLocationIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- uniqueLocationID@
uniqueLocationID :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id NSString)
uniqueLocationID mtrEcosystemInformationClusterEcosystemLocationStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct uniqueLocationIDSelector

-- | @- setUniqueLocationID:@
setUniqueLocationID :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsNSString value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setUniqueLocationID mtrEcosystemInformationClusterEcosystemLocationStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct setUniqueLocationIDSelector (toNSString value)

-- | @- locationDescriptor@
locationDescriptor :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id MTRDataTypeLocationDescriptorStruct)
locationDescriptor mtrEcosystemInformationClusterEcosystemLocationStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct locationDescriptorSelector

-- | @- setLocationDescriptor:@
setLocationDescriptor :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsMTRDataTypeLocationDescriptorStruct value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setLocationDescriptor mtrEcosystemInformationClusterEcosystemLocationStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct setLocationDescriptorSelector (toMTRDataTypeLocationDescriptorStruct value)

-- | @- locationDescriptorLastEdit@
locationDescriptorLastEdit :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id NSNumber)
locationDescriptorLastEdit mtrEcosystemInformationClusterEcosystemLocationStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct locationDescriptorLastEditSelector

-- | @- setLocationDescriptorLastEdit:@
setLocationDescriptorLastEdit :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setLocationDescriptorLastEdit mtrEcosystemInformationClusterEcosystemLocationStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct setLocationDescriptorLastEditSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id NSNumber)
fabricIndex mtrEcosystemInformationClusterEcosystemLocationStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setFabricIndex mtrEcosystemInformationClusterEcosystemLocationStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemLocationStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueLocationID@
uniqueLocationIDSelector :: Selector '[] (Id NSString)
uniqueLocationIDSelector = mkSelector "uniqueLocationID"

-- | @Selector@ for @setUniqueLocationID:@
setUniqueLocationIDSelector :: Selector '[Id NSString] ()
setUniqueLocationIDSelector = mkSelector "setUniqueLocationID:"

-- | @Selector@ for @locationDescriptor@
locationDescriptorSelector :: Selector '[] (Id MTRDataTypeLocationDescriptorStruct)
locationDescriptorSelector = mkSelector "locationDescriptor"

-- | @Selector@ for @setLocationDescriptor:@
setLocationDescriptorSelector :: Selector '[Id MTRDataTypeLocationDescriptorStruct] ()
setLocationDescriptorSelector = mkSelector "setLocationDescriptor:"

-- | @Selector@ for @locationDescriptorLastEdit@
locationDescriptorLastEditSelector :: Selector '[] (Id NSNumber)
locationDescriptorLastEditSelector = mkSelector "locationDescriptorLastEdit"

-- | @Selector@ for @setLocationDescriptorLastEdit:@
setLocationDescriptorLastEditSelector :: Selector '[Id NSNumber] ()
setLocationDescriptorLastEditSelector = mkSelector "setLocationDescriptorLastEdit:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

