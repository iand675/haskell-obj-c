{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIncrementalStoreNode@.
module ObjC.CoreData.NSIncrementalStoreNode
  ( NSIncrementalStoreNode
  , IsNSIncrementalStoreNode(..)
  , initWithObjectID_withValues_version
  , updateWithValues_version
  , valueForPropertyDescription
  , objectID
  , version
  , initWithObjectID_withValues_versionSelector
  , objectIDSelector
  , updateWithValues_versionSelector
  , valueForPropertyDescriptionSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithObjectID:withValues:version:@
initWithObjectID_withValues_version :: (IsNSIncrementalStoreNode nsIncrementalStoreNode, IsNSManagedObjectID objectID, IsNSDictionary values) => nsIncrementalStoreNode -> objectID -> values -> CULong -> IO (Id NSIncrementalStoreNode)
initWithObjectID_withValues_version nsIncrementalStoreNode objectID values version =
  sendOwnedMessage nsIncrementalStoreNode initWithObjectID_withValues_versionSelector (toNSManagedObjectID objectID) (toNSDictionary values) version

-- | @- updateWithValues:version:@
updateWithValues_version :: (IsNSIncrementalStoreNode nsIncrementalStoreNode, IsNSDictionary values) => nsIncrementalStoreNode -> values -> CULong -> IO ()
updateWithValues_version nsIncrementalStoreNode values version =
  sendMessage nsIncrementalStoreNode updateWithValues_versionSelector (toNSDictionary values) version

-- | @- valueForPropertyDescription:@
valueForPropertyDescription :: (IsNSIncrementalStoreNode nsIncrementalStoreNode, IsNSPropertyDescription prop) => nsIncrementalStoreNode -> prop -> IO RawId
valueForPropertyDescription nsIncrementalStoreNode prop =
  sendMessage nsIncrementalStoreNode valueForPropertyDescriptionSelector (toNSPropertyDescription prop)

-- | @- objectID@
objectID :: IsNSIncrementalStoreNode nsIncrementalStoreNode => nsIncrementalStoreNode -> IO (Id NSManagedObjectID)
objectID nsIncrementalStoreNode =
  sendMessage nsIncrementalStoreNode objectIDSelector

-- | @- version@
version :: IsNSIncrementalStoreNode nsIncrementalStoreNode => nsIncrementalStoreNode -> IO CULong
version nsIncrementalStoreNode =
  sendMessage nsIncrementalStoreNode versionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObjectID:withValues:version:@
initWithObjectID_withValues_versionSelector :: Selector '[Id NSManagedObjectID, Id NSDictionary, CULong] (Id NSIncrementalStoreNode)
initWithObjectID_withValues_versionSelector = mkSelector "initWithObjectID:withValues:version:"

-- | @Selector@ for @updateWithValues:version:@
updateWithValues_versionSelector :: Selector '[Id NSDictionary, CULong] ()
updateWithValues_versionSelector = mkSelector "updateWithValues:version:"

-- | @Selector@ for @valueForPropertyDescription:@
valueForPropertyDescriptionSelector :: Selector '[Id NSPropertyDescription] RawId
valueForPropertyDescriptionSelector = mkSelector "valueForPropertyDescription:"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector '[] (Id NSManagedObjectID)
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @version@
versionSelector :: Selector '[] CULong
versionSelector = mkSelector "version"

