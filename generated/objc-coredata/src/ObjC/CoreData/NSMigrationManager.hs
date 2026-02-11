{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMigrationManager@.
module ObjC.CoreData.NSMigrationManager
  ( NSMigrationManager
  , IsNSMigrationManager(..)
  , initWithSourceModel_destinationModel
  , migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error
  , reset
  , sourceEntityForEntityMapping
  , destinationEntityForEntityMapping
  , associateSourceInstance_withDestinationInstance_forEntityMapping
  , destinationInstancesForEntityMappingNamed_sourceInstances
  , sourceInstancesForEntityMappingNamed_destinationInstances
  , cancelMigrationWithError
  , usesStoreSpecificMigrationManager
  , setUsesStoreSpecificMigrationManager
  , mappingModel
  , sourceModel
  , destinationModel
  , sourceContext
  , destinationContext
  , currentEntityMapping
  , migrationProgress
  , userInfo
  , setUserInfo
  , initWithSourceModel_destinationModelSelector
  , migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector
  , resetSelector
  , sourceEntityForEntityMappingSelector
  , destinationEntityForEntityMappingSelector
  , associateSourceInstance_withDestinationInstance_forEntityMappingSelector
  , destinationInstancesForEntityMappingNamed_sourceInstancesSelector
  , sourceInstancesForEntityMappingNamed_destinationInstancesSelector
  , cancelMigrationWithErrorSelector
  , usesStoreSpecificMigrationManagerSelector
  , setUsesStoreSpecificMigrationManagerSelector
  , mappingModelSelector
  , sourceModelSelector
  , destinationModelSelector
  , sourceContextSelector
  , destinationContextSelector
  , currentEntityMappingSelector
  , migrationProgressSelector
  , userInfoSelector
  , setUserInfoSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSourceModel:destinationModel:@
initWithSourceModel_destinationModel :: (IsNSMigrationManager nsMigrationManager, IsNSManagedObjectModel sourceModel, IsNSManagedObjectModel destinationModel) => nsMigrationManager -> sourceModel -> destinationModel -> IO (Id NSMigrationManager)
initWithSourceModel_destinationModel nsMigrationManager  sourceModel destinationModel =
withObjCPtr sourceModel $ \raw_sourceModel ->
  withObjCPtr destinationModel $ \raw_destinationModel ->
      sendMsg nsMigrationManager (mkSelector "initWithSourceModel:destinationModel:") (retPtr retVoid) [argPtr (castPtr raw_sourceModel :: Ptr ()), argPtr (castPtr raw_destinationModel :: Ptr ())] >>= ownedObject . castPtr

-- | @- migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:@
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error :: (IsNSMigrationManager nsMigrationManager, IsNSURL sourceURL, IsNSString sStoreType, IsNSDictionary sOptions, IsNSMappingModel mappings, IsNSURL dURL, IsNSString dStoreType, IsNSDictionary dOptions, IsNSError error_) => nsMigrationManager -> sourceURL -> sStoreType -> sOptions -> mappings -> dURL -> dStoreType -> dOptions -> error_ -> IO Bool
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error nsMigrationManager  sourceURL sStoreType sOptions mappings dURL dStoreType dOptions error_ =
withObjCPtr sourceURL $ \raw_sourceURL ->
  withObjCPtr sStoreType $ \raw_sStoreType ->
    withObjCPtr sOptions $ \raw_sOptions ->
      withObjCPtr mappings $ \raw_mappings ->
        withObjCPtr dURL $ \raw_dURL ->
          withObjCPtr dStoreType $ \raw_dStoreType ->
            withObjCPtr dOptions $ \raw_dOptions ->
              withObjCPtr error_ $ \raw_error_ ->
                  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMigrationManager (mkSelector "migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:") retCULong [argPtr (castPtr raw_sourceURL :: Ptr ()), argPtr (castPtr raw_sStoreType :: Ptr ()), argPtr (castPtr raw_sOptions :: Ptr ()), argPtr (castPtr raw_mappings :: Ptr ()), argPtr (castPtr raw_dURL :: Ptr ()), argPtr (castPtr raw_dStoreType :: Ptr ()), argPtr (castPtr raw_dOptions :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- reset@
reset :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO ()
reset nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "reset") retVoid []

-- | @- sourceEntityForEntityMapping:@
sourceEntityForEntityMapping :: (IsNSMigrationManager nsMigrationManager, IsNSEntityMapping mEntity) => nsMigrationManager -> mEntity -> IO (Id NSEntityDescription)
sourceEntityForEntityMapping nsMigrationManager  mEntity =
withObjCPtr mEntity $ \raw_mEntity ->
    sendMsg nsMigrationManager (mkSelector "sourceEntityForEntityMapping:") (retPtr retVoid) [argPtr (castPtr raw_mEntity :: Ptr ())] >>= retainedObject . castPtr

-- | @- destinationEntityForEntityMapping:@
destinationEntityForEntityMapping :: (IsNSMigrationManager nsMigrationManager, IsNSEntityMapping mEntity) => nsMigrationManager -> mEntity -> IO (Id NSEntityDescription)
destinationEntityForEntityMapping nsMigrationManager  mEntity =
withObjCPtr mEntity $ \raw_mEntity ->
    sendMsg nsMigrationManager (mkSelector "destinationEntityForEntityMapping:") (retPtr retVoid) [argPtr (castPtr raw_mEntity :: Ptr ())] >>= retainedObject . castPtr

-- | @- associateSourceInstance:withDestinationInstance:forEntityMapping:@
associateSourceInstance_withDestinationInstance_forEntityMapping :: (IsNSMigrationManager nsMigrationManager, IsNSManagedObject sourceInstance, IsNSManagedObject destinationInstance, IsNSEntityMapping entityMapping) => nsMigrationManager -> sourceInstance -> destinationInstance -> entityMapping -> IO ()
associateSourceInstance_withDestinationInstance_forEntityMapping nsMigrationManager  sourceInstance destinationInstance entityMapping =
withObjCPtr sourceInstance $ \raw_sourceInstance ->
  withObjCPtr destinationInstance $ \raw_destinationInstance ->
    withObjCPtr entityMapping $ \raw_entityMapping ->
        sendMsg nsMigrationManager (mkSelector "associateSourceInstance:withDestinationInstance:forEntityMapping:") retVoid [argPtr (castPtr raw_sourceInstance :: Ptr ()), argPtr (castPtr raw_destinationInstance :: Ptr ()), argPtr (castPtr raw_entityMapping :: Ptr ())]

-- | @- destinationInstancesForEntityMappingNamed:sourceInstances:@
destinationInstancesForEntityMappingNamed_sourceInstances :: (IsNSMigrationManager nsMigrationManager, IsNSString mappingName, IsNSArray sourceInstances) => nsMigrationManager -> mappingName -> sourceInstances -> IO (Id NSArray)
destinationInstancesForEntityMappingNamed_sourceInstances nsMigrationManager  mappingName sourceInstances =
withObjCPtr mappingName $ \raw_mappingName ->
  withObjCPtr sourceInstances $ \raw_sourceInstances ->
      sendMsg nsMigrationManager (mkSelector "destinationInstancesForEntityMappingNamed:sourceInstances:") (retPtr retVoid) [argPtr (castPtr raw_mappingName :: Ptr ()), argPtr (castPtr raw_sourceInstances :: Ptr ())] >>= retainedObject . castPtr

-- | @- sourceInstancesForEntityMappingNamed:destinationInstances:@
sourceInstancesForEntityMappingNamed_destinationInstances :: (IsNSMigrationManager nsMigrationManager, IsNSString mappingName, IsNSArray destinationInstances) => nsMigrationManager -> mappingName -> destinationInstances -> IO (Id NSArray)
sourceInstancesForEntityMappingNamed_destinationInstances nsMigrationManager  mappingName destinationInstances =
withObjCPtr mappingName $ \raw_mappingName ->
  withObjCPtr destinationInstances $ \raw_destinationInstances ->
      sendMsg nsMigrationManager (mkSelector "sourceInstancesForEntityMappingNamed:destinationInstances:") (retPtr retVoid) [argPtr (castPtr raw_mappingName :: Ptr ()), argPtr (castPtr raw_destinationInstances :: Ptr ())] >>= retainedObject . castPtr

-- | @- cancelMigrationWithError:@
cancelMigrationWithError :: (IsNSMigrationManager nsMigrationManager, IsNSError error_) => nsMigrationManager -> error_ -> IO ()
cancelMigrationWithError nsMigrationManager  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsMigrationManager (mkSelector "cancelMigrationWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- usesStoreSpecificMigrationManager@
usesStoreSpecificMigrationManager :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO Bool
usesStoreSpecificMigrationManager nsMigrationManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMigrationManager (mkSelector "usesStoreSpecificMigrationManager") retCULong []

-- | @- setUsesStoreSpecificMigrationManager:@
setUsesStoreSpecificMigrationManager :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> Bool -> IO ()
setUsesStoreSpecificMigrationManager nsMigrationManager  value =
  sendMsg nsMigrationManager (mkSelector "setUsesStoreSpecificMigrationManager:") retVoid [argCULong (if value then 1 else 0)]

-- | @- mappingModel@
mappingModel :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSMappingModel)
mappingModel nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "mappingModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceModel@
sourceModel :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectModel)
sourceModel nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "sourceModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destinationModel@
destinationModel :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectModel)
destinationModel nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "destinationModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceContext@
sourceContext :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectContext)
sourceContext nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "sourceContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destinationContext@
destinationContext :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectContext)
destinationContext nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "destinationContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentEntityMapping@
currentEntityMapping :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSEntityMapping)
currentEntityMapping nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "currentEntityMapping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- migrationProgress@
migrationProgress :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO CFloat
migrationProgress nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "migrationProgress") retCFloat []

-- | @- userInfo@
userInfo :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSDictionary)
userInfo nsMigrationManager  =
  sendMsg nsMigrationManager (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSMigrationManager nsMigrationManager, IsNSDictionary value) => nsMigrationManager -> value -> IO ()
setUserInfo nsMigrationManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMigrationManager (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceModel:destinationModel:@
initWithSourceModel_destinationModelSelector :: Selector
initWithSourceModel_destinationModelSelector = mkSelector "initWithSourceModel:destinationModel:"

-- | @Selector@ for @migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:@
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector :: Selector
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector = mkSelector "migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @sourceEntityForEntityMapping:@
sourceEntityForEntityMappingSelector :: Selector
sourceEntityForEntityMappingSelector = mkSelector "sourceEntityForEntityMapping:"

-- | @Selector@ for @destinationEntityForEntityMapping:@
destinationEntityForEntityMappingSelector :: Selector
destinationEntityForEntityMappingSelector = mkSelector "destinationEntityForEntityMapping:"

-- | @Selector@ for @associateSourceInstance:withDestinationInstance:forEntityMapping:@
associateSourceInstance_withDestinationInstance_forEntityMappingSelector :: Selector
associateSourceInstance_withDestinationInstance_forEntityMappingSelector = mkSelector "associateSourceInstance:withDestinationInstance:forEntityMapping:"

-- | @Selector@ for @destinationInstancesForEntityMappingNamed:sourceInstances:@
destinationInstancesForEntityMappingNamed_sourceInstancesSelector :: Selector
destinationInstancesForEntityMappingNamed_sourceInstancesSelector = mkSelector "destinationInstancesForEntityMappingNamed:sourceInstances:"

-- | @Selector@ for @sourceInstancesForEntityMappingNamed:destinationInstances:@
sourceInstancesForEntityMappingNamed_destinationInstancesSelector :: Selector
sourceInstancesForEntityMappingNamed_destinationInstancesSelector = mkSelector "sourceInstancesForEntityMappingNamed:destinationInstances:"

-- | @Selector@ for @cancelMigrationWithError:@
cancelMigrationWithErrorSelector :: Selector
cancelMigrationWithErrorSelector = mkSelector "cancelMigrationWithError:"

-- | @Selector@ for @usesStoreSpecificMigrationManager@
usesStoreSpecificMigrationManagerSelector :: Selector
usesStoreSpecificMigrationManagerSelector = mkSelector "usesStoreSpecificMigrationManager"

-- | @Selector@ for @setUsesStoreSpecificMigrationManager:@
setUsesStoreSpecificMigrationManagerSelector :: Selector
setUsesStoreSpecificMigrationManagerSelector = mkSelector "setUsesStoreSpecificMigrationManager:"

-- | @Selector@ for @mappingModel@
mappingModelSelector :: Selector
mappingModelSelector = mkSelector "mappingModel"

-- | @Selector@ for @sourceModel@
sourceModelSelector :: Selector
sourceModelSelector = mkSelector "sourceModel"

-- | @Selector@ for @destinationModel@
destinationModelSelector :: Selector
destinationModelSelector = mkSelector "destinationModel"

-- | @Selector@ for @sourceContext@
sourceContextSelector :: Selector
sourceContextSelector = mkSelector "sourceContext"

-- | @Selector@ for @destinationContext@
destinationContextSelector :: Selector
destinationContextSelector = mkSelector "destinationContext"

-- | @Selector@ for @currentEntityMapping@
currentEntityMappingSelector :: Selector
currentEntityMappingSelector = mkSelector "currentEntityMapping"

-- | @Selector@ for @migrationProgress@
migrationProgressSelector :: Selector
migrationProgressSelector = mkSelector "migrationProgress"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

