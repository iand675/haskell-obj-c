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
  , mergedModelFromBundlesSelector
  , modelByMergingModelsSelector
  , initSelector
  , initWithContentsOfURLSelector
  , entitiesForConfigurationSelector
  , setEntities_forConfigurationSelector
  , setFetchRequestTemplate_forNameSelector
  , fetchRequestTemplateForNameSelector
  , fetchRequestFromTemplateWithName_substitutionVariablesSelector
  , mergedModelFromBundles_forStoreMetadataSelector
  , modelByMergingModels_forStoreMetadataSelector
  , isConfiguration_compatibleWithStoreMetadataSelector
  , checksumsForVersionedModelAtURL_errorSelector
  , entitiesByNameSelector
  , entitiesSelector
  , setEntitiesSelector
  , configurationsSelector
  , localizationDictionarySelector
  , setLocalizationDictionarySelector
  , fetchRequestTemplatesByNameSelector
  , versionIdentifiersSelector
  , setVersionIdentifiersSelector
  , entityVersionHashesByNameSelector
  , versionChecksumSelector


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

-- | @+ mergedModelFromBundles:@
mergedModelFromBundles :: IsNSArray bundles => bundles -> IO (Id NSManagedObjectModel)
mergedModelFromBundles bundles =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    withObjCPtr bundles $ \raw_bundles ->
      sendClassMsg cls' (mkSelector "mergedModelFromBundles:") (retPtr retVoid) [argPtr (castPtr raw_bundles :: Ptr ())] >>= retainedObject . castPtr

-- | @+ modelByMergingModels:@
modelByMergingModels :: IsNSArray models => models -> IO (Id NSManagedObjectModel)
modelByMergingModels models =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    withObjCPtr models $ \raw_models ->
      sendClassMsg cls' (mkSelector "modelByMergingModels:") (retPtr retVoid) [argPtr (castPtr raw_models :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSManagedObjectModel)
init_ nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSURL url) => nsManagedObjectModel -> url -> IO (Id NSManagedObjectModel)
initWithContentsOfURL nsManagedObjectModel  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsManagedObjectModel (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- entitiesForConfiguration:@
entitiesForConfiguration :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString configuration) => nsManagedObjectModel -> configuration -> IO (Id NSArray)
entitiesForConfiguration nsManagedObjectModel  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg nsManagedObjectModel (mkSelector "entitiesForConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= retainedObject . castPtr

-- | @- setEntities:forConfiguration:@
setEntities_forConfiguration :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSArray entities, IsNSString configuration) => nsManagedObjectModel -> entities -> configuration -> IO ()
setEntities_forConfiguration nsManagedObjectModel  entities configuration =
withObjCPtr entities $ \raw_entities ->
  withObjCPtr configuration $ \raw_configuration ->
      sendMsg nsManagedObjectModel (mkSelector "setEntities:forConfiguration:") retVoid [argPtr (castPtr raw_entities :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ())]

-- | @- setFetchRequestTemplate:forName:@
setFetchRequestTemplate_forName :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSFetchRequest fetchRequestTemplate, IsNSString name) => nsManagedObjectModel -> fetchRequestTemplate -> name -> IO ()
setFetchRequestTemplate_forName nsManagedObjectModel  fetchRequestTemplate name =
withObjCPtr fetchRequestTemplate $ \raw_fetchRequestTemplate ->
  withObjCPtr name $ \raw_name ->
      sendMsg nsManagedObjectModel (mkSelector "setFetchRequestTemplate:forName:") retVoid [argPtr (castPtr raw_fetchRequestTemplate :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- fetchRequestTemplateForName:@
fetchRequestTemplateForName :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString name) => nsManagedObjectModel -> name -> IO (Id NSFetchRequest)
fetchRequestTemplateForName nsManagedObjectModel  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsManagedObjectModel (mkSelector "fetchRequestTemplateForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- fetchRequestFromTemplateWithName:substitutionVariables:@
fetchRequestFromTemplateWithName_substitutionVariables :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString name, IsNSDictionary variables) => nsManagedObjectModel -> name -> variables -> IO (Id NSFetchRequest)
fetchRequestFromTemplateWithName_substitutionVariables nsManagedObjectModel  name variables =
withObjCPtr name $ \raw_name ->
  withObjCPtr variables $ \raw_variables ->
      sendMsg nsManagedObjectModel (mkSelector "fetchRequestFromTemplateWithName:substitutionVariables:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_variables :: Ptr ())] >>= retainedObject . castPtr

-- | @+ mergedModelFromBundles:forStoreMetadata:@
mergedModelFromBundles_forStoreMetadata :: (IsNSArray bundles, IsNSDictionary metadata) => bundles -> metadata -> IO (Id NSManagedObjectModel)
mergedModelFromBundles_forStoreMetadata bundles metadata =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    withObjCPtr bundles $ \raw_bundles ->
      withObjCPtr metadata $ \raw_metadata ->
        sendClassMsg cls' (mkSelector "mergedModelFromBundles:forStoreMetadata:") (retPtr retVoid) [argPtr (castPtr raw_bundles :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @+ modelByMergingModels:forStoreMetadata:@
modelByMergingModels_forStoreMetadata :: (IsNSArray models, IsNSDictionary metadata) => models -> metadata -> IO (Id NSManagedObjectModel)
modelByMergingModels_forStoreMetadata models metadata =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    withObjCPtr models $ \raw_models ->
      withObjCPtr metadata $ \raw_metadata ->
        sendClassMsg cls' (mkSelector "modelByMergingModels:forStoreMetadata:") (retPtr retVoid) [argPtr (castPtr raw_models :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- isConfiguration:compatibleWithStoreMetadata:@
isConfiguration_compatibleWithStoreMetadata :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSString configuration, IsNSDictionary metadata) => nsManagedObjectModel -> configuration -> metadata -> IO Bool
isConfiguration_compatibleWithStoreMetadata nsManagedObjectModel  configuration metadata =
withObjCPtr configuration $ \raw_configuration ->
  withObjCPtr metadata $ \raw_metadata ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectModel (mkSelector "isConfiguration:compatibleWithStoreMetadata:") retCULong [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())]

-- | @+ checksumsForVersionedModelAtURL:error:@
checksumsForVersionedModelAtURL_error :: (IsNSURL modelURL, IsNSError error_) => modelURL -> error_ -> IO (Id NSDictionary)
checksumsForVersionedModelAtURL_error modelURL error_ =
  do
    cls' <- getRequiredClass "NSManagedObjectModel"
    withObjCPtr modelURL $ \raw_modelURL ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "checksumsForVersionedModelAtURL:error:") (retPtr retVoid) [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- entitiesByName@
entitiesByName :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
entitiesByName nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "entitiesByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- entities@
entities :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSArray)
entities nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "entities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEntities:@
setEntities :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSArray value) => nsManagedObjectModel -> value -> IO ()
setEntities nsManagedObjectModel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectModel (mkSelector "setEntities:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- configurations@
configurations :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSArray)
configurations nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "configurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizationDictionary@
localizationDictionary :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
localizationDictionary nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "localizationDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizationDictionary:@
setLocalizationDictionary :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSDictionary value) => nsManagedObjectModel -> value -> IO ()
setLocalizationDictionary nsManagedObjectModel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectModel (mkSelector "setLocalizationDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fetchRequestTemplatesByName@
fetchRequestTemplatesByName :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
fetchRequestTemplatesByName nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "fetchRequestTemplatesByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- versionIdentifiers@
versionIdentifiers :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSSet)
versionIdentifiers nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "versionIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersionIdentifiers:@
setVersionIdentifiers :: (IsNSManagedObjectModel nsManagedObjectModel, IsNSSet value) => nsManagedObjectModel -> value -> IO ()
setVersionIdentifiers nsManagedObjectModel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectModel (mkSelector "setVersionIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- entityVersionHashesByName@
entityVersionHashesByName :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSDictionary)
entityVersionHashesByName nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "entityVersionHashesByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- versionChecksum@
versionChecksum :: IsNSManagedObjectModel nsManagedObjectModel => nsManagedObjectModel -> IO (Id NSString)
versionChecksum nsManagedObjectModel  =
  sendMsg nsManagedObjectModel (mkSelector "versionChecksum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mergedModelFromBundles:@
mergedModelFromBundlesSelector :: Selector
mergedModelFromBundlesSelector = mkSelector "mergedModelFromBundles:"

-- | @Selector@ for @modelByMergingModels:@
modelByMergingModelsSelector :: Selector
modelByMergingModelsSelector = mkSelector "modelByMergingModels:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @entitiesForConfiguration:@
entitiesForConfigurationSelector :: Selector
entitiesForConfigurationSelector = mkSelector "entitiesForConfiguration:"

-- | @Selector@ for @setEntities:forConfiguration:@
setEntities_forConfigurationSelector :: Selector
setEntities_forConfigurationSelector = mkSelector "setEntities:forConfiguration:"

-- | @Selector@ for @setFetchRequestTemplate:forName:@
setFetchRequestTemplate_forNameSelector :: Selector
setFetchRequestTemplate_forNameSelector = mkSelector "setFetchRequestTemplate:forName:"

-- | @Selector@ for @fetchRequestTemplateForName:@
fetchRequestTemplateForNameSelector :: Selector
fetchRequestTemplateForNameSelector = mkSelector "fetchRequestTemplateForName:"

-- | @Selector@ for @fetchRequestFromTemplateWithName:substitutionVariables:@
fetchRequestFromTemplateWithName_substitutionVariablesSelector :: Selector
fetchRequestFromTemplateWithName_substitutionVariablesSelector = mkSelector "fetchRequestFromTemplateWithName:substitutionVariables:"

-- | @Selector@ for @mergedModelFromBundles:forStoreMetadata:@
mergedModelFromBundles_forStoreMetadataSelector :: Selector
mergedModelFromBundles_forStoreMetadataSelector = mkSelector "mergedModelFromBundles:forStoreMetadata:"

-- | @Selector@ for @modelByMergingModels:forStoreMetadata:@
modelByMergingModels_forStoreMetadataSelector :: Selector
modelByMergingModels_forStoreMetadataSelector = mkSelector "modelByMergingModels:forStoreMetadata:"

-- | @Selector@ for @isConfiguration:compatibleWithStoreMetadata:@
isConfiguration_compatibleWithStoreMetadataSelector :: Selector
isConfiguration_compatibleWithStoreMetadataSelector = mkSelector "isConfiguration:compatibleWithStoreMetadata:"

-- | @Selector@ for @checksumsForVersionedModelAtURL:error:@
checksumsForVersionedModelAtURL_errorSelector :: Selector
checksumsForVersionedModelAtURL_errorSelector = mkSelector "checksumsForVersionedModelAtURL:error:"

-- | @Selector@ for @entitiesByName@
entitiesByNameSelector :: Selector
entitiesByNameSelector = mkSelector "entitiesByName"

-- | @Selector@ for @entities@
entitiesSelector :: Selector
entitiesSelector = mkSelector "entities"

-- | @Selector@ for @setEntities:@
setEntitiesSelector :: Selector
setEntitiesSelector = mkSelector "setEntities:"

-- | @Selector@ for @configurations@
configurationsSelector :: Selector
configurationsSelector = mkSelector "configurations"

-- | @Selector@ for @localizationDictionary@
localizationDictionarySelector :: Selector
localizationDictionarySelector = mkSelector "localizationDictionary"

-- | @Selector@ for @setLocalizationDictionary:@
setLocalizationDictionarySelector :: Selector
setLocalizationDictionarySelector = mkSelector "setLocalizationDictionary:"

-- | @Selector@ for @fetchRequestTemplatesByName@
fetchRequestTemplatesByNameSelector :: Selector
fetchRequestTemplatesByNameSelector = mkSelector "fetchRequestTemplatesByName"

-- | @Selector@ for @versionIdentifiers@
versionIdentifiersSelector :: Selector
versionIdentifiersSelector = mkSelector "versionIdentifiers"

-- | @Selector@ for @setVersionIdentifiers:@
setVersionIdentifiersSelector :: Selector
setVersionIdentifiersSelector = mkSelector "setVersionIdentifiers:"

-- | @Selector@ for @entityVersionHashesByName@
entityVersionHashesByNameSelector :: Selector
entityVersionHashesByNameSelector = mkSelector "entityVersionHashesByName"

-- | @Selector@ for @versionChecksum@
versionChecksumSelector :: Selector
versionChecksumSelector = mkSelector "versionChecksum"

