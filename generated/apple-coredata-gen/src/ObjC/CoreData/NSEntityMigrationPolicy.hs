{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEntityMigrationPolicy@.
module ObjC.CoreData.NSEntityMigrationPolicy
  ( NSEntityMigrationPolicy
  , IsNSEntityMigrationPolicy(..)
  , beginEntityMapping_manager_error
  , createDestinationInstancesForSourceInstance_entityMapping_manager_error
  , endInstanceCreationForEntityMapping_manager_error
  , createRelationshipsForDestinationInstance_entityMapping_manager_error
  , endRelationshipCreationForEntityMapping_manager_error
  , performCustomValidationForEntityMapping_manager_error
  , endEntityMapping_manager_error
  , beginEntityMapping_manager_errorSelector
  , createDestinationInstancesForSourceInstance_entityMapping_manager_errorSelector
  , createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector
  , endEntityMapping_manager_errorSelector
  , endInstanceCreationForEntityMapping_manager_errorSelector
  , endRelationshipCreationForEntityMapping_manager_errorSelector
  , performCustomValidationForEntityMapping_manager_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- beginEntityMapping:manager:error:@
beginEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
beginEntityMapping_manager_error nsEntityMigrationPolicy mapping manager error_ =
  sendMessage nsEntityMigrationPolicy beginEntityMapping_manager_errorSelector (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- | @- createDestinationInstancesForSourceInstance:entityMapping:manager:error:@
createDestinationInstancesForSourceInstance_entityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSManagedObject sInstance, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> sInstance -> mapping -> manager -> error_ -> IO Bool
createDestinationInstancesForSourceInstance_entityMapping_manager_error nsEntityMigrationPolicy sInstance mapping manager error_ =
  sendMessage nsEntityMigrationPolicy createDestinationInstancesForSourceInstance_entityMapping_manager_errorSelector (toNSManagedObject sInstance) (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- | @- endInstanceCreationForEntityMapping:manager:error:@
endInstanceCreationForEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
endInstanceCreationForEntityMapping_manager_error nsEntityMigrationPolicy mapping manager error_ =
  sendMessage nsEntityMigrationPolicy endInstanceCreationForEntityMapping_manager_errorSelector (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- | @- createRelationshipsForDestinationInstance:entityMapping:manager:error:@
createRelationshipsForDestinationInstance_entityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSManagedObject dInstance, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> dInstance -> mapping -> manager -> error_ -> IO Bool
createRelationshipsForDestinationInstance_entityMapping_manager_error nsEntityMigrationPolicy dInstance mapping manager error_ =
  sendMessage nsEntityMigrationPolicy createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector (toNSManagedObject dInstance) (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- | @- endRelationshipCreationForEntityMapping:manager:error:@
endRelationshipCreationForEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
endRelationshipCreationForEntityMapping_manager_error nsEntityMigrationPolicy mapping manager error_ =
  sendMessage nsEntityMigrationPolicy endRelationshipCreationForEntityMapping_manager_errorSelector (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- | @- performCustomValidationForEntityMapping:manager:error:@
performCustomValidationForEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
performCustomValidationForEntityMapping_manager_error nsEntityMigrationPolicy mapping manager error_ =
  sendMessage nsEntityMigrationPolicy performCustomValidationForEntityMapping_manager_errorSelector (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- | @- endEntityMapping:manager:error:@
endEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
endEntityMapping_manager_error nsEntityMigrationPolicy mapping manager error_ =
  sendMessage nsEntityMigrationPolicy endEntityMapping_manager_errorSelector (toNSEntityMapping mapping) (toNSMigrationManager manager) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginEntityMapping:manager:error:@
beginEntityMapping_manager_errorSelector :: Selector '[Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
beginEntityMapping_manager_errorSelector = mkSelector "beginEntityMapping:manager:error:"

-- | @Selector@ for @createDestinationInstancesForSourceInstance:entityMapping:manager:error:@
createDestinationInstancesForSourceInstance_entityMapping_manager_errorSelector :: Selector '[Id NSManagedObject, Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
createDestinationInstancesForSourceInstance_entityMapping_manager_errorSelector = mkSelector "createDestinationInstancesForSourceInstance:entityMapping:manager:error:"

-- | @Selector@ for @endInstanceCreationForEntityMapping:manager:error:@
endInstanceCreationForEntityMapping_manager_errorSelector :: Selector '[Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
endInstanceCreationForEntityMapping_manager_errorSelector = mkSelector "endInstanceCreationForEntityMapping:manager:error:"

-- | @Selector@ for @createRelationshipsForDestinationInstance:entityMapping:manager:error:@
createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector :: Selector '[Id NSManagedObject, Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector = mkSelector "createRelationshipsForDestinationInstance:entityMapping:manager:error:"

-- | @Selector@ for @endRelationshipCreationForEntityMapping:manager:error:@
endRelationshipCreationForEntityMapping_manager_errorSelector :: Selector '[Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
endRelationshipCreationForEntityMapping_manager_errorSelector = mkSelector "endRelationshipCreationForEntityMapping:manager:error:"

-- | @Selector@ for @performCustomValidationForEntityMapping:manager:error:@
performCustomValidationForEntityMapping_manager_errorSelector :: Selector '[Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
performCustomValidationForEntityMapping_manager_errorSelector = mkSelector "performCustomValidationForEntityMapping:manager:error:"

-- | @Selector@ for @endEntityMapping:manager:error:@
endEntityMapping_manager_errorSelector :: Selector '[Id NSEntityMapping, Id NSMigrationManager, Id NSError] Bool
endEntityMapping_manager_errorSelector = mkSelector "endEntityMapping:manager:error:"

