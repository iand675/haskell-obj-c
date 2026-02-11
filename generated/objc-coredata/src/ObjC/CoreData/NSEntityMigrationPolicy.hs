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
  , endInstanceCreationForEntityMapping_manager_errorSelector
  , createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector
  , endRelationshipCreationForEntityMapping_manager_errorSelector
  , performCustomValidationForEntityMapping_manager_errorSelector
  , endEntityMapping_manager_errorSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- beginEntityMapping:manager:error:@
beginEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
beginEntityMapping_manager_error nsEntityMigrationPolicy  mapping manager error_ =
withObjCPtr mapping $ \raw_mapping ->
  withObjCPtr manager $ \raw_manager ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "beginEntityMapping:manager:error:") retCULong [argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- createDestinationInstancesForSourceInstance:entityMapping:manager:error:@
createDestinationInstancesForSourceInstance_entityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSManagedObject sInstance, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> sInstance -> mapping -> manager -> error_ -> IO Bool
createDestinationInstancesForSourceInstance_entityMapping_manager_error nsEntityMigrationPolicy  sInstance mapping manager error_ =
withObjCPtr sInstance $ \raw_sInstance ->
  withObjCPtr mapping $ \raw_mapping ->
    withObjCPtr manager $ \raw_manager ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "createDestinationInstancesForSourceInstance:entityMapping:manager:error:") retCULong [argPtr (castPtr raw_sInstance :: Ptr ()), argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- endInstanceCreationForEntityMapping:manager:error:@
endInstanceCreationForEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
endInstanceCreationForEntityMapping_manager_error nsEntityMigrationPolicy  mapping manager error_ =
withObjCPtr mapping $ \raw_mapping ->
  withObjCPtr manager $ \raw_manager ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "endInstanceCreationForEntityMapping:manager:error:") retCULong [argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- createRelationshipsForDestinationInstance:entityMapping:manager:error:@
createRelationshipsForDestinationInstance_entityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSManagedObject dInstance, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> dInstance -> mapping -> manager -> error_ -> IO Bool
createRelationshipsForDestinationInstance_entityMapping_manager_error nsEntityMigrationPolicy  dInstance mapping manager error_ =
withObjCPtr dInstance $ \raw_dInstance ->
  withObjCPtr mapping $ \raw_mapping ->
    withObjCPtr manager $ \raw_manager ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "createRelationshipsForDestinationInstance:entityMapping:manager:error:") retCULong [argPtr (castPtr raw_dInstance :: Ptr ()), argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- endRelationshipCreationForEntityMapping:manager:error:@
endRelationshipCreationForEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
endRelationshipCreationForEntityMapping_manager_error nsEntityMigrationPolicy  mapping manager error_ =
withObjCPtr mapping $ \raw_mapping ->
  withObjCPtr manager $ \raw_manager ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "endRelationshipCreationForEntityMapping:manager:error:") retCULong [argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- performCustomValidationForEntityMapping:manager:error:@
performCustomValidationForEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
performCustomValidationForEntityMapping_manager_error nsEntityMigrationPolicy  mapping manager error_ =
withObjCPtr mapping $ \raw_mapping ->
  withObjCPtr manager $ \raw_manager ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "performCustomValidationForEntityMapping:manager:error:") retCULong [argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- endEntityMapping:manager:error:@
endEntityMapping_manager_error :: (IsNSEntityMigrationPolicy nsEntityMigrationPolicy, IsNSEntityMapping mapping, IsNSMigrationManager manager, IsNSError error_) => nsEntityMigrationPolicy -> mapping -> manager -> error_ -> IO Bool
endEntityMapping_manager_error nsEntityMigrationPolicy  mapping manager error_ =
withObjCPtr mapping $ \raw_mapping ->
  withObjCPtr manager $ \raw_manager ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityMigrationPolicy (mkSelector "endEntityMapping:manager:error:") retCULong [argPtr (castPtr raw_mapping :: Ptr ()), argPtr (castPtr raw_manager :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginEntityMapping:manager:error:@
beginEntityMapping_manager_errorSelector :: Selector
beginEntityMapping_manager_errorSelector = mkSelector "beginEntityMapping:manager:error:"

-- | @Selector@ for @createDestinationInstancesForSourceInstance:entityMapping:manager:error:@
createDestinationInstancesForSourceInstance_entityMapping_manager_errorSelector :: Selector
createDestinationInstancesForSourceInstance_entityMapping_manager_errorSelector = mkSelector "createDestinationInstancesForSourceInstance:entityMapping:manager:error:"

-- | @Selector@ for @endInstanceCreationForEntityMapping:manager:error:@
endInstanceCreationForEntityMapping_manager_errorSelector :: Selector
endInstanceCreationForEntityMapping_manager_errorSelector = mkSelector "endInstanceCreationForEntityMapping:manager:error:"

-- | @Selector@ for @createRelationshipsForDestinationInstance:entityMapping:manager:error:@
createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector :: Selector
createRelationshipsForDestinationInstance_entityMapping_manager_errorSelector = mkSelector "createRelationshipsForDestinationInstance:entityMapping:manager:error:"

-- | @Selector@ for @endRelationshipCreationForEntityMapping:manager:error:@
endRelationshipCreationForEntityMapping_manager_errorSelector :: Selector
endRelationshipCreationForEntityMapping_manager_errorSelector = mkSelector "endRelationshipCreationForEntityMapping:manager:error:"

-- | @Selector@ for @performCustomValidationForEntityMapping:manager:error:@
performCustomValidationForEntityMapping_manager_errorSelector :: Selector
performCustomValidationForEntityMapping_manager_errorSelector = mkSelector "performCustomValidationForEntityMapping:manager:error:"

-- | @Selector@ for @endEntityMapping:manager:error:@
endEntityMapping_manager_errorSelector :: Selector
endEntityMapping_manager_errorSelector = mkSelector "endEntityMapping:manager:error:"

