{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSManagedObjectModel@.
module ObjC.CoreData.NSManagedObjectModel
  ( NSManagedObjectModel
  , IsNSManagedObjectModel(..)
  , mergedModelFromBundles
  , modelByMergingModels
  , init_
  , initWithContentsOfURL
  , entitiesForConfiguration
  , setEntities_forConfiguration
  , setFetchRequestTemplate_forName
  , fetchRequestTemplateForName
  , fetchRequestFromTemplateWithName_substitutionVariables
  , mergedModelFromBundles_forStoreMetadata
  , modelByMergingModels_forStoreMetadata
  , isConfiguration_compatibleWithStoreMetadata
  , checksumsForVersionedModelAtURL_error
  , entitiesByName
  , entities
  , setEntities
  , configurations
  , localizationDictionary
  , setLocalizationDictionary
  , fetchRequestTemplatesByName
  , versionIdentifiers
  , setVersionIdentifiers
  , entityVersionHashesByName
  , versionChecksum
  , checksumsForVersionedModelAtURL_errorSelector
  , configurationsSelector
  , entitiesByNameSelector
  , entitiesForConfigurationSelector
  , entitiesSelector
  , entityVersionHashesByNameSelector
  , fetchRequestFromTemplateWithName_substitutionVariablesSelector
  , fetchRequestTemplateForNameSelector
  , fetchRequestTemplatesByNameSelector
  , initSelector
  , initWithContentsOfURLSelector
  , isConfiguration_compatibleWithStoreMetadataSelector
  , localizationDictionarySelector
  , mergedModelFromBundlesSelector
  , mergedModelFromBundles_forStoreMetadataSelector
  , modelByMergingModelsSelector
  , modelByMergingModels_forStoreMetadataSelector
  , setEntitiesSelector
  , setEntities_forConfigurationSelector
  , setFetchRequestTemplate_forNameSelector
  , setLocalizationDictionarySelector
  , setVersionIdentifiersSelector
  , versionChecksumSelector
  , versionIdentifiersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ mergedModelFromBundles:@
mergedModelFromBundles :: IsNSArray bundles => bundles -> IO (Id NSManagedObjectModel)
mergedModelFromBundles bundles =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    sendClassMessage cls' mergedModelFromBundlesSelector (toNSArray bundles)

-- | @+ modelByMergingModels:@
modelByMergingModels :: IsNSArray models => models -> IO (Id NSManagedObjectModel)
modelByMergingModels models =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    sendClassMessage cls' modelByMergingModelsSelector (toNSArray models)

-- | @- init@
init_ :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSManagedObjectModel)
init_ nsManagedObjectModel =
  sendOwnedMessage nsManagedObjectModel initSelector

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSURL url) => nsManagedObjectModel -> url -> IO (Id NSManagedObjectModel)
initWithContentsOfURL nsManagedObjectModel url =
  sendOwnedMessage nsManagedObjectModel initWithContentsOfURLSelector (toNSURL url)

-- | @- entitiesForConfiguration:@
entitiesForConfiguration :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString configuration) => nsManagedObjectModel -> configuration -> IO (Id NSArray)
entitiesForConfiguration nsManagedObjectModel configuration =
  sendMessage nsManagedObjectModel entitiesForConfigurationSelector (toNSString configuration)

-- | @- setEntities:forConfiguration:@
setEntities_forConfiguration :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSArray entities, IsNSString configuration) => nsManagedObjectModel -> entities -> configuration -> IO ()
setEntities_forConfiguration nsManagedObjectModel entities configuration =
  sendMessage nsManagedObjectModel setEntities_forConfigurationSelector (toNSArray entities) (toNSString configuration)

-- | @- setFetchRequestTemplate:forName:@
setFetchRequestTemplate_forName :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSFetchRequest fetchRequestTemplate, IsNSString name) => nsManagedObjectModel -> fetchRequestTemplate -> name -> IO ()
setFetchRequestTemplate_forName nsManagedObjectModel fetchRequestTemplate name =
  sendMessage nsManagedObjectModel setFetchRequestTemplate_forNameSelector (toNSFetchRequest fetchRequestTemplate) (toNSString name)

-- | @- fetchRequestTemplateForName:@
fetchRequestTemplateForName :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString name) => nsManagedObjectModel -> name -> IO (Id NSFetchRequest)
fetchRequestTemplateForName nsManagedObjectModel name =
  sendMessage nsManagedObjectModel fetchRequestTemplateForNameSelector (toNSString name)

-- | @- fetchRequestFromTemplateWithName:substitutionVariables:@
fetchRequestFromTemplateWithName_substitutionVariables :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString name, IsNSDictionary variables) => nsManagedObjectModel -> name -> variables -> IO (Id NSFetchRequest)
fetchRequestFromTemplateWithName_substitutionVariables nsManagedObjectModel name variables =
  sendMessage nsManagedObjectModel fetchRequestFromTemplateWithName_substitutionVariablesSelector (toNSString name) (toNSDictionary variables)

-- | @+ mergedModelFromBundles:forStoreMetadata:@
mergedModelFromBundles_forStoreMetadata :: (IsNSArray bundles, IsNSDictionary metadata) => bundles -> metadata -> IO (Id NSManagedObjectModel)
mergedModelFromBundles_forStoreMetadata bundles metadata =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    sendClassMessage cls' mergedModelFromBundles_forStoreMetadataSelector (toNSArray bundles) (toNSDictionary metadata)

-- | @+ modelByMergingModels:forStoreMetadata:@
modelByMergingModels_forStoreMetadata :: (IsNSArray models, IsNSDictionary metadata) => models -> metadata -> IO (Id NSManagedObjectModel)
modelByMergingModels_forStoreMetadata models metadata =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    sendClassMessage cls' modelByMergingModels_forStoreMetadataSelector (toNSArray models) (toNSDictionary metadata)

-- | @- isConfiguration:compatibleWithStoreMetadata:@
isConfiguration_compatibleWithStoreMetadata :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString configuration, IsNSDictionary metadata) => nsManagedObjectModel -> configuration -> metadata -> IO Bool
isConfiguration_compatibleWithStoreMetadata nsManagedObjectModel configuration metadata =
  sendMessage nsManagedObjectModel isConfiguration_compatibleWithStoreMetadataSelector (toNSString configuration) (toNSDictionary metadata)

-- | @+ checksumsForVersionedModelAtURL:error:@
checksumsForVersionedModelAtURL_error :: (IsNSURL modelURL, IsNSError error_) => modelURL -> error_ -> IO (Id NSDictionary)
checksumsForVersionedModelAtURL_error modelURL error_ =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    sendClassMessage cls' checksumsForVersionedModelAtURL_errorSelector (toNSURL modelURL) (toNSError error_)

-- | @- entitiesByName@
entitiesByName :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
entitiesByName nsManagedObjectModel =
  sendMessage nsManagedObjectModel entitiesByNameSelector

-- | @- entities@
entities :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSArray)
entities nsManagedObjectModel =
  sendMessage nsManagedObjectModel entitiesSelector

-- | @- setEntities:@
setEntities :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSArray value) => nsManagedObjectModel -> value -> IO ()
setEntities nsManagedObjectModel value =
  sendMessage nsManagedObjectModel setEntitiesSelector (toNSArray value)

-- | @- configurations@
configurations :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSArray)
configurations nsManagedObjectModel =
  sendMessage nsManagedObjectModel configurationsSelector

-- | @- localizationDictionary@
localizationDictionary :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
localizationDictionary nsManagedObjectModel =
  sendMessage nsManagedObjectModel localizationDictionarySelector

-- | @- setLocalizationDictionary:@
setLocalizationDictionary :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSDictionary value) => nsManagedObjectModel -> value -> IO ()
setLocalizationDictionary nsManagedObjectModel value =
  sendMessage nsManagedObjectModel setLocalizationDictionarySelector (toNSDictionary value)

-- | @- fetchRequestTemplatesByName@
fetchRequestTemplatesByName :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
fetchRequestTemplatesByName nsManagedObjectModel =
  sendMessage nsManagedObjectModel fetchRequestTemplatesByNameSelector

-- | @- versionIdentifiers@
versionIdentifiers :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSSet)
versionIdentifiers nsManagedObjectModel =
  sendMessage nsManagedObjectModel versionIdentifiersSelector

-- | @- setVersionIdentifiers:@
setVersionIdentifiers :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSSet value) => nsManagedObjectModel -> value -> IO ()
setVersionIdentifiers nsManagedObjectModel value =
  sendMessage nsManagedObjectModel setVersionIdentifiersSelector (toNSSet value)

-- | @- entityVersionHashesByName@
entityVersionHashesByName :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
entityVersionHashesByName nsManagedObjectModel =
  sendMessage nsManagedObjectModel entityVersionHashesByNameSelector

-- | @- versionChecksum@
versionChecksum :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSString)
versionChecksum nsManagedObjectModel =
  sendMessage nsManagedObjectModel versionChecksumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mergedModelFromBundles:@
mergedModelFromBundlesSelector :: Selector '[Id NSArray] (Id NSManagedObjectModel)
mergedModelFromBundlesSelector = mkSelector "mergedModelFromBundles:"

-- | @Selector@ for @modelByMergingModels:@
modelByMergingModelsSelector :: Selector '[Id NSArray] (Id NSManagedObjectModel)
modelByMergingModelsSelector = mkSelector "modelByMergingModels:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSManagedObjectModel)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSManagedObjectModel)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @entitiesForConfiguration:@
entitiesForConfigurationSelector :: Selector '[Id NSString] (Id NSArray)
entitiesForConfigurationSelector = mkSelector "entitiesForConfiguration:"

-- | @Selector@ for @setEntities:forConfiguration:@
setEntities_forConfigurationSelector :: Selector '[Id NSArray, Id NSString] ()
setEntities_forConfigurationSelector = mkSelector "setEntities:forConfiguration:"

-- | @Selector@ for @setFetchRequestTemplate:forName:@
setFetchRequestTemplate_forNameSelector :: Selector '[Id NSFetchRequest, Id NSString] ()
setFetchRequestTemplate_forNameSelector = mkSelector "setFetchRequestTemplate:forName:"

-- | @Selector@ for @fetchRequestTemplateForName:@
fetchRequestTemplateForNameSelector :: Selector '[Id NSString] (Id NSFetchRequest)
fetchRequestTemplateForNameSelector = mkSelector "fetchRequestTemplateForName:"

-- | @Selector@ for @fetchRequestFromTemplateWithName:substitutionVariables:@
fetchRequestFromTemplateWithName_substitutionVariablesSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSFetchRequest)
fetchRequestFromTemplateWithName_substitutionVariablesSelector = mkSelector "fetchRequestFromTemplateWithName:substitutionVariables:"

-- | @Selector@ for @mergedModelFromBundles:forStoreMetadata:@
mergedModelFromBundles_forStoreMetadataSelector :: Selector '[Id NSArray, Id NSDictionary] (Id NSManagedObjectModel)
mergedModelFromBundles_forStoreMetadataSelector = mkSelector "mergedModelFromBundles:forStoreMetadata:"

-- | @Selector@ for @modelByMergingModels:forStoreMetadata:@
modelByMergingModels_forStoreMetadataSelector :: Selector '[Id NSArray, Id NSDictionary] (Id NSManagedObjectModel)
modelByMergingModels_forStoreMetadataSelector = mkSelector "modelByMergingModels:forStoreMetadata:"

-- | @Selector@ for @isConfiguration:compatibleWithStoreMetadata:@
isConfiguration_compatibleWithStoreMetadataSelector :: Selector '[Id NSString, Id NSDictionary] Bool
isConfiguration_compatibleWithStoreMetadataSelector = mkSelector "isConfiguration:compatibleWithStoreMetadata:"

-- | @Selector@ for @checksumsForVersionedModelAtURL:error:@
checksumsForVersionedModelAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSDictionary)
checksumsForVersionedModelAtURL_errorSelector = mkSelector "checksumsForVersionedModelAtURL:error:"

-- | @Selector@ for @entitiesByName@
entitiesByNameSelector :: Selector '[] (Id NSDictionary)
entitiesByNameSelector = mkSelector "entitiesByName"

-- | @Selector@ for @entities@
entitiesSelector :: Selector '[] (Id NSArray)
entitiesSelector = mkSelector "entities"

-- | @Selector@ for @setEntities:@
setEntitiesSelector :: Selector '[Id NSArray] ()
setEntitiesSelector = mkSelector "setEntities:"

-- | @Selector@ for @configurations@
configurationsSelector :: Selector '[] (Id NSArray)
configurationsSelector = mkSelector "configurations"

-- | @Selector@ for @localizationDictionary@
localizationDictionarySelector :: Selector '[] (Id NSDictionary)
localizationDictionarySelector = mkSelector "localizationDictionary"

-- | @Selector@ for @setLocalizationDictionary:@
setLocalizationDictionarySelector :: Selector '[Id NSDictionary] ()
setLocalizationDictionarySelector = mkSelector "setLocalizationDictionary:"

-- | @Selector@ for @fetchRequestTemplatesByName@
fetchRequestTemplatesByNameSelector :: Selector '[] (Id NSDictionary)
fetchRequestTemplatesByNameSelector = mkSelector "fetchRequestTemplatesByName"

-- | @Selector@ for @versionIdentifiers@
versionIdentifiersSelector :: Selector '[] (Id NSSet)
versionIdentifiersSelector = mkSelector "versionIdentifiers"

-- | @Selector@ for @setVersionIdentifiers:@
setVersionIdentifiersSelector :: Selector '[Id NSSet] ()
setVersionIdentifiersSelector = mkSelector "setVersionIdentifiers:"

-- | @Selector@ for @entityVersionHashesByName@
entityVersionHashesByNameSelector :: Selector '[] (Id NSDictionary)
entityVersionHashesByNameSelector = mkSelector "entityVersionHashesByName"

-- | @Selector@ for @versionChecksum@
versionChecksumSelector :: Selector '[] (Id NSString)
versionChecksumSelector = mkSelector "versionChecksum"

