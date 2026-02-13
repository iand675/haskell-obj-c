{-# LANGUAGE DataKinds #-}
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
  , associateSourceInstance_withDestinationInstance_forEntityMappingSelector
  , cancelMigrationWithErrorSelector
  , currentEntityMappingSelector
  , destinationContextSelector
  , destinationEntityForEntityMappingSelector
  , destinationInstancesForEntityMappingNamed_sourceInstancesSelector
  , destinationModelSelector
  , initWithSourceModel_destinationModelSelector
  , mappingModelSelector
  , migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector
  , migrationProgressSelector
  , resetSelector
  , setUserInfoSelector
  , setUsesStoreSpecificMigrationManagerSelector
  , sourceContextSelector
  , sourceEntityForEntityMappingSelector
  , sourceInstancesForEntityMappingNamed_destinationInstancesSelector
  , sourceModelSelector
  , userInfoSelector
  , usesStoreSpecificMigrationManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSourceModel:destinationModel:@
initWithSourceModel_destinationModel :: (IsNSMigrationManager nsMigrationManager, IsNSManagedObjectModel sourceModel, IsNSManagedObjectModel destinationModel) => nsMigrationManager -> sourceModel -> destinationModel -> IO (Id NSMigrationManager)
initWithSourceModel_destinationModel nsMigrationManager sourceModel destinationModel =
  sendOwnedMessage nsMigrationManager initWithSourceModel_destinationModelSelector (toNSManagedObjectModel sourceModel) (toNSManagedObjectModel destinationModel)

-- | @- migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:@
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error :: (IsNSMigrationManager nsMigrationManager, IsNSURL sourceURL, IsNSString sStoreType, IsNSDictionary sOptions, IsNSMappingModel mappings, IsNSURL dURL, IsNSString dStoreType, IsNSDictionary dOptions, IsNSError error_) => nsMigrationManager -> sourceURL -> sStoreType -> sOptions -> mappings -> dURL -> dStoreType -> dOptions -> error_ -> IO Bool
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_error nsMigrationManager sourceURL sStoreType sOptions mappings dURL dStoreType dOptions error_ =
  sendMessage nsMigrationManager migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector (toNSURL sourceURL) (toNSString sStoreType) (toNSDictionary sOptions) (toNSMappingModel mappings) (toNSURL dURL) (toNSString dStoreType) (toNSDictionary dOptions) (toNSError error_)

-- | @- reset@
reset :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO ()
reset nsMigrationManager =
  sendMessage nsMigrationManager resetSelector

-- | @- sourceEntityForEntityMapping:@
sourceEntityForEntityMapping :: (IsNSMigrationManager nsMigrationManager, IsNSEntityMapping mEntity) => nsMigrationManager -> mEntity -> IO (Id NSEntityDescription)
sourceEntityForEntityMapping nsMigrationManager mEntity =
  sendMessage nsMigrationManager sourceEntityForEntityMappingSelector (toNSEntityMapping mEntity)

-- | @- destinationEntityForEntityMapping:@
destinationEntityForEntityMapping :: (IsNSMigrationManager nsMigrationManager, IsNSEntityMapping mEntity) => nsMigrationManager -> mEntity -> IO (Id NSEntityDescription)
destinationEntityForEntityMapping nsMigrationManager mEntity =
  sendMessage nsMigrationManager destinationEntityForEntityMappingSelector (toNSEntityMapping mEntity)

-- | @- associateSourceInstance:withDestinationInstance:forEntityMapping:@
associateSourceInstance_withDestinationInstance_forEntityMapping :: (IsNSMigrationManager nsMigrationManager, IsNSManagedObject sourceInstance, IsNSManagedObject destinationInstance, IsNSEntityMapping entityMapping) => nsMigrationManager -> sourceInstance -> destinationInstance -> entityMapping -> IO ()
associateSourceInstance_withDestinationInstance_forEntityMapping nsMigrationManager sourceInstance destinationInstance entityMapping =
  sendMessage nsMigrationManager associateSourceInstance_withDestinationInstance_forEntityMappingSelector (toNSManagedObject sourceInstance) (toNSManagedObject destinationInstance) (toNSEntityMapping entityMapping)

-- | @- destinationInstancesForEntityMappingNamed:sourceInstances:@
destinationInstancesForEntityMappingNamed_sourceInstances :: (IsNSMigrationManager nsMigrationManager, IsNSString mappingName, IsNSArray sourceInstances) => nsMigrationManager -> mappingName -> sourceInstances -> IO (Id NSArray)
destinationInstancesForEntityMappingNamed_sourceInstances nsMigrationManager mappingName sourceInstances =
  sendMessage nsMigrationManager destinationInstancesForEntityMappingNamed_sourceInstancesSelector (toNSString mappingName) (toNSArray sourceInstances)

-- | @- sourceInstancesForEntityMappingNamed:destinationInstances:@
sourceInstancesForEntityMappingNamed_destinationInstances :: (IsNSMigrationManager nsMigrationManager, IsNSString mappingName, IsNSArray destinationInstances) => nsMigrationManager -> mappingName -> destinationInstances -> IO (Id NSArray)
sourceInstancesForEntityMappingNamed_destinationInstances nsMigrationManager mappingName destinationInstances =
  sendMessage nsMigrationManager sourceInstancesForEntityMappingNamed_destinationInstancesSelector (toNSString mappingName) (toNSArray destinationInstances)

-- | @- cancelMigrationWithError:@
cancelMigrationWithError :: (IsNSMigrationManager nsMigrationManager, IsNSError error_) => nsMigrationManager -> error_ -> IO ()
cancelMigrationWithError nsMigrationManager error_ =
  sendMessage nsMigrationManager cancelMigrationWithErrorSelector (toNSError error_)

-- | @- usesStoreSpecificMigrationManager@
usesStoreSpecificMigrationManager :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO Bool
usesStoreSpecificMigrationManager nsMigrationManager =
  sendMessage nsMigrationManager usesStoreSpecificMigrationManagerSelector

-- | @- setUsesStoreSpecificMigrationManager:@
setUsesStoreSpecificMigrationManager :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> Bool -> IO ()
setUsesStoreSpecificMigrationManager nsMigrationManager value =
  sendMessage nsMigrationManager setUsesStoreSpecificMigrationManagerSelector value

-- | @- mappingModel@
mappingModel :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSMappingModel)
mappingModel nsMigrationManager =
  sendMessage nsMigrationManager mappingModelSelector

-- | @- sourceModel@
sourceModel :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectModel)
sourceModel nsMigrationManager =
  sendMessage nsMigrationManager sourceModelSelector

-- | @- destinationModel@
destinationModel :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectModel)
destinationModel nsMigrationManager =
  sendMessage nsMigrationManager destinationModelSelector

-- | @- sourceContext@
sourceContext :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectContext)
sourceContext nsMigrationManager =
  sendMessage nsMigrationManager sourceContextSelector

-- | @- destinationContext@
destinationContext :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSManagedObjectContext)
destinationContext nsMigrationManager =
  sendMessage nsMigrationManager destinationContextSelector

-- | @- currentEntityMapping@
currentEntityMapping :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSEntityMapping)
currentEntityMapping nsMigrationManager =
  sendMessage nsMigrationManager currentEntityMappingSelector

-- | @- migrationProgress@
migrationProgress :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO CFloat
migrationProgress nsMigrationManager =
  sendMessage nsMigrationManager migrationProgressSelector

-- | @- userInfo@
userInfo :: IsNSMigrationManager nsMigrationManager => nsMigrationManager -> IO (Id NSDictionary)
userInfo nsMigrationManager =
  sendMessage nsMigrationManager userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSMigrationManager nsMigrationManager, IsNSDictionary value) => nsMigrationManager -> value -> IO ()
setUserInfo nsMigrationManager value =
  sendMessage nsMigrationManager setUserInfoSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceModel:destinationModel:@
initWithSourceModel_destinationModelSelector :: Selector '[Id NSManagedObjectModel, Id NSManagedObjectModel] (Id NSMigrationManager)
initWithSourceModel_destinationModelSelector = mkSelector "initWithSourceModel:destinationModel:"

-- | @Selector@ for @migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:@
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSDictionary, Id NSMappingModel, Id NSURL, Id NSString, Id NSDictionary, Id NSError] Bool
migrateStoreFromURL_type_options_withMappingModel_toDestinationURL_destinationType_destinationOptions_errorSelector = mkSelector "migrateStoreFromURL:type:options:withMappingModel:toDestinationURL:destinationType:destinationOptions:error:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @sourceEntityForEntityMapping:@
sourceEntityForEntityMappingSelector :: Selector '[Id NSEntityMapping] (Id NSEntityDescription)
sourceEntityForEntityMappingSelector = mkSelector "sourceEntityForEntityMapping:"

-- | @Selector@ for @destinationEntityForEntityMapping:@
destinationEntityForEntityMappingSelector :: Selector '[Id NSEntityMapping] (Id NSEntityDescription)
destinationEntityForEntityMappingSelector = mkSelector "destinationEntityForEntityMapping:"

-- | @Selector@ for @associateSourceInstance:withDestinationInstance:forEntityMapping:@
associateSourceInstance_withDestinationInstance_forEntityMappingSelector :: Selector '[Id NSManagedObject, Id NSManagedObject, Id NSEntityMapping] ()
associateSourceInstance_withDestinationInstance_forEntityMappingSelector = mkSelector "associateSourceInstance:withDestinationInstance:forEntityMapping:"

-- | @Selector@ for @destinationInstancesForEntityMappingNamed:sourceInstances:@
destinationInstancesForEntityMappingNamed_sourceInstancesSelector :: Selector '[Id NSString, Id NSArray] (Id NSArray)
destinationInstancesForEntityMappingNamed_sourceInstancesSelector = mkSelector "destinationInstancesForEntityMappingNamed:sourceInstances:"

-- | @Selector@ for @sourceInstancesForEntityMappingNamed:destinationInstances:@
sourceInstancesForEntityMappingNamed_destinationInstancesSelector :: Selector '[Id NSString, Id NSArray] (Id NSArray)
sourceInstancesForEntityMappingNamed_destinationInstancesSelector = mkSelector "sourceInstancesForEntityMappingNamed:destinationInstances:"

-- | @Selector@ for @cancelMigrationWithError:@
cancelMigrationWithErrorSelector :: Selector '[Id NSError] ()
cancelMigrationWithErrorSelector = mkSelector "cancelMigrationWithError:"

-- | @Selector@ for @usesStoreSpecificMigrationManager@
usesStoreSpecificMigrationManagerSelector :: Selector '[] Bool
usesStoreSpecificMigrationManagerSelector = mkSelector "usesStoreSpecificMigrationManager"

-- | @Selector@ for @setUsesStoreSpecificMigrationManager:@
setUsesStoreSpecificMigrationManagerSelector :: Selector '[Bool] ()
setUsesStoreSpecificMigrationManagerSelector = mkSelector "setUsesStoreSpecificMigrationManager:"

-- | @Selector@ for @mappingModel@
mappingModelSelector :: Selector '[] (Id NSMappingModel)
mappingModelSelector = mkSelector "mappingModel"

-- | @Selector@ for @sourceModel@
sourceModelSelector :: Selector '[] (Id NSManagedObjectModel)
sourceModelSelector = mkSelector "sourceModel"

-- | @Selector@ for @destinationModel@
destinationModelSelector :: Selector '[] (Id NSManagedObjectModel)
destinationModelSelector = mkSelector "destinationModel"

-- | @Selector@ for @sourceContext@
sourceContextSelector :: Selector '[] (Id NSManagedObjectContext)
sourceContextSelector = mkSelector "sourceContext"

-- | @Selector@ for @destinationContext@
destinationContextSelector :: Selector '[] (Id NSManagedObjectContext)
destinationContextSelector = mkSelector "destinationContext"

-- | @Selector@ for @currentEntityMapping@
currentEntityMappingSelector :: Selector '[] (Id NSEntityMapping)
currentEntityMappingSelector = mkSelector "currentEntityMapping"

-- | @Selector@ for @migrationProgress@
migrationProgressSelector :: Selector '[] CFloat
migrationProgressSelector = mkSelector "migrationProgress"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

