{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterExtensionFieldSetStruct@.
module ObjC.Matter.MTRScenesManagementClusterExtensionFieldSetStruct
  ( MTRScenesManagementClusterExtensionFieldSetStruct
  , IsMTRScenesManagementClusterExtensionFieldSetStruct(..)
  , clusterID
  , setClusterID
  , attributeValueList
  , setAttributeValueList
  , attributeValueListSelector
  , clusterIDSelector
  , setAttributeValueListSelector
  , setClusterIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- clusterID@
clusterID :: IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct => mtrScenesManagementClusterExtensionFieldSetStruct -> IO (Id NSNumber)
clusterID mtrScenesManagementClusterExtensionFieldSetStruct =
  sendMessage mtrScenesManagementClusterExtensionFieldSetStruct clusterIDSelector

-- | @- setClusterID:@
setClusterID :: (IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct, IsNSNumber value) => mtrScenesManagementClusterExtensionFieldSetStruct -> value -> IO ()
setClusterID mtrScenesManagementClusterExtensionFieldSetStruct value =
  sendMessage mtrScenesManagementClusterExtensionFieldSetStruct setClusterIDSelector (toNSNumber value)

-- | @- attributeValueList@
attributeValueList :: IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct => mtrScenesManagementClusterExtensionFieldSetStruct -> IO (Id NSArray)
attributeValueList mtrScenesManagementClusterExtensionFieldSetStruct =
  sendMessage mtrScenesManagementClusterExtensionFieldSetStruct attributeValueListSelector

-- | @- setAttributeValueList:@
setAttributeValueList :: (IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct, IsNSArray value) => mtrScenesManagementClusterExtensionFieldSetStruct -> value -> IO ()
setAttributeValueList mtrScenesManagementClusterExtensionFieldSetStruct value =
  sendMessage mtrScenesManagementClusterExtensionFieldSetStruct setAttributeValueListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clusterID@
clusterIDSelector :: Selector '[] (Id NSNumber)
clusterIDSelector = mkSelector "clusterID"

-- | @Selector@ for @setClusterID:@
setClusterIDSelector :: Selector '[Id NSNumber] ()
setClusterIDSelector = mkSelector "setClusterID:"

-- | @Selector@ for @attributeValueList@
attributeValueListSelector :: Selector '[] (Id NSArray)
attributeValueListSelector = mkSelector "attributeValueList"

-- | @Selector@ for @setAttributeValueList:@
setAttributeValueListSelector :: Selector '[Id NSArray] ()
setAttributeValueListSelector = mkSelector "setAttributeValueList:"

