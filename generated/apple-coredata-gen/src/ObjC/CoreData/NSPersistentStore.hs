{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStore@.
module ObjC.CoreData.NSPersistentStore
  ( NSPersistentStore
  , IsNSPersistentStore(..)
  , metadataForPersistentStoreWithURL_error
  , setMetadata_forPersistentStoreWithURL_error
  , migrationManagerClass
  , initWithPersistentStoreCoordinator_configurationName_URL_options
  , init_
  , loadMetadata
  , didAddToPersistentStoreCoordinator
  , willRemoveFromPersistentStoreCoordinator
  , persistentStoreCoordinator
  , configurationName
  , options
  , url
  , setURL
  , identifier
  , setIdentifier
  , type_
  , readOnly
  , setReadOnly
  , metadata
  , setMetadata
  , coreSpotlightExporter
  , configurationNameSelector
  , coreSpotlightExporterSelector
  , didAddToPersistentStoreCoordinatorSelector
  , identifierSelector
  , initSelector
  , initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector
  , loadMetadataSelector
  , metadataForPersistentStoreWithURL_errorSelector
  , metadataSelector
  , migrationManagerClassSelector
  , optionsSelector
  , persistentStoreCoordinatorSelector
  , readOnlySelector
  , setIdentifierSelector
  , setMetadataSelector
  , setMetadata_forPersistentStoreWithURL_errorSelector
  , setReadOnlySelector
  , setURLSelector
  , typeSelector
  , urlSelector
  , willRemoveFromPersistentStoreCoordinatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreWithURL_error url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStore"
    sendClassMessage cls' metadataForPersistentStoreWithURL_errorSelector (toNSURL url) (toNSError error_)

-- | @+ setMetadata:forPersistentStoreWithURL:error:@
setMetadata_forPersistentStoreWithURL_error :: (IsNSDictionary metadata, IsNSURL url, IsNSError error_) => metadata -> url -> error_ -> IO Bool
setMetadata_forPersistentStoreWithURL_error metadata url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStore"
    sendClassMessage cls' setMetadata_forPersistentStoreWithURL_errorSelector (toNSDictionary metadata) (toNSURL url) (toNSError error_)

-- | @+ migrationManagerClass@
migrationManagerClass :: IO Class
migrationManagerClass  =
  do
    cls' <- getRequiredClass "NSPersistentStore"
    sendClassMessage cls' migrationManagerClassSelector

-- | @- initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_options :: (IsNSPersistentStore nsPersistentStore, IsNSPersistentStoreCoordinator root, IsNSString name, IsNSURL url, IsNSDictionary options) => nsPersistentStore -> root -> name -> url -> options -> IO (Id NSPersistentStore)
initWithPersistentStoreCoordinator_configurationName_URL_options nsPersistentStore root name url options =
  sendOwnedMessage nsPersistentStore initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector (toNSPersistentStoreCoordinator root) (toNSString name) (toNSURL url) (toNSDictionary options)

-- | @- init@
init_ :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSPersistentStore)
init_ nsPersistentStore =
  sendOwnedMessage nsPersistentStore initSelector

-- | @- loadMetadata:@
loadMetadata :: (IsNSPersistentStore nsPersistentStore, IsNSError error_) => nsPersistentStore -> error_ -> IO Bool
loadMetadata nsPersistentStore error_ =
  sendMessage nsPersistentStore loadMetadataSelector (toNSError error_)

-- | @- didAddToPersistentStoreCoordinator:@
didAddToPersistentStoreCoordinator :: (IsNSPersistentStore nsPersistentStore, IsNSPersistentStoreCoordinator coordinator) => nsPersistentStore -> coordinator -> IO ()
didAddToPersistentStoreCoordinator nsPersistentStore coordinator =
  sendMessage nsPersistentStore didAddToPersistentStoreCoordinatorSelector (toNSPersistentStoreCoordinator coordinator)

-- | @- willRemoveFromPersistentStoreCoordinator:@
willRemoveFromPersistentStoreCoordinator :: (IsNSPersistentStore nsPersistentStore, IsNSPersistentStoreCoordinator coordinator) => nsPersistentStore -> coordinator -> IO ()
willRemoveFromPersistentStoreCoordinator nsPersistentStore coordinator =
  sendMessage nsPersistentStore willRemoveFromPersistentStoreCoordinatorSelector (toNSPersistentStoreCoordinator coordinator)

-- | @- persistentStoreCoordinator@
persistentStoreCoordinator :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSPersistentStoreCoordinator)
persistentStoreCoordinator nsPersistentStore =
  sendMessage nsPersistentStore persistentStoreCoordinatorSelector

-- | @- configurationName@
configurationName :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSString)
configurationName nsPersistentStore =
  sendMessage nsPersistentStore configurationNameSelector

-- | @- options@
options :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSDictionary)
options nsPersistentStore =
  sendMessage nsPersistentStore optionsSelector

-- | @- URL@
url :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSURL)
url nsPersistentStore =
  sendMessage nsPersistentStore urlSelector

-- | @- setURL:@
setURL :: (IsNSPersistentStore nsPersistentStore, IsNSURL value) => nsPersistentStore -> value -> IO ()
setURL nsPersistentStore value =
  sendMessage nsPersistentStore setURLSelector (toNSURL value)

-- | @- identifier@
identifier :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSString)
identifier nsPersistentStore =
  sendMessage nsPersistentStore identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsNSPersistentStore nsPersistentStore, IsNSString value) => nsPersistentStore -> value -> IO ()
setIdentifier nsPersistentStore value =
  sendMessage nsPersistentStore setIdentifierSelector (toNSString value)

-- | @- type@
type_ :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSString)
type_ nsPersistentStore =
  sendMessage nsPersistentStore typeSelector

-- | @- readOnly@
readOnly :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO Bool
readOnly nsPersistentStore =
  sendMessage nsPersistentStore readOnlySelector

-- | @- setReadOnly:@
setReadOnly :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> Bool -> IO ()
setReadOnly nsPersistentStore value =
  sendMessage nsPersistentStore setReadOnlySelector value

-- | @- metadata@
metadata :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSDictionary)
metadata nsPersistentStore =
  sendMessage nsPersistentStore metadataSelector

-- | @- setMetadata:@
setMetadata :: (IsNSPersistentStore nsPersistentStore, IsNSDictionary value) => nsPersistentStore -> value -> IO ()
setMetadata nsPersistentStore value =
  sendMessage nsPersistentStore setMetadataSelector (toNSDictionary value)

-- | @- coreSpotlightExporter@
coreSpotlightExporter :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSCoreDataCoreSpotlightDelegate)
coreSpotlightExporter nsPersistentStore =
  sendMessage nsPersistentStore coreSpotlightExporterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSDictionary)
metadataForPersistentStoreWithURL_errorSelector = mkSelector "metadataForPersistentStoreWithURL:error:"

-- | @Selector@ for @setMetadata:forPersistentStoreWithURL:error:@
setMetadata_forPersistentStoreWithURL_errorSelector :: Selector '[Id NSDictionary, Id NSURL, Id NSError] Bool
setMetadata_forPersistentStoreWithURL_errorSelector = mkSelector "setMetadata:forPersistentStoreWithURL:error:"

-- | @Selector@ for @migrationManagerClass@
migrationManagerClassSelector :: Selector '[] Class
migrationManagerClassSelector = mkSelector "migrationManagerClass"

-- | @Selector@ for @initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector :: Selector '[Id NSPersistentStoreCoordinator, Id NSString, Id NSURL, Id NSDictionary] (Id NSPersistentStore)
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector = mkSelector "initWithPersistentStoreCoordinator:configurationName:URL:options:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPersistentStore)
initSelector = mkSelector "init"

-- | @Selector@ for @loadMetadata:@
loadMetadataSelector :: Selector '[Id NSError] Bool
loadMetadataSelector = mkSelector "loadMetadata:"

-- | @Selector@ for @didAddToPersistentStoreCoordinator:@
didAddToPersistentStoreCoordinatorSelector :: Selector '[Id NSPersistentStoreCoordinator] ()
didAddToPersistentStoreCoordinatorSelector = mkSelector "didAddToPersistentStoreCoordinator:"

-- | @Selector@ for @willRemoveFromPersistentStoreCoordinator:@
willRemoveFromPersistentStoreCoordinatorSelector :: Selector '[Id NSPersistentStoreCoordinator] ()
willRemoveFromPersistentStoreCoordinatorSelector = mkSelector "willRemoveFromPersistentStoreCoordinator:"

-- | @Selector@ for @persistentStoreCoordinator@
persistentStoreCoordinatorSelector :: Selector '[] (Id NSPersistentStoreCoordinator)
persistentStoreCoordinatorSelector = mkSelector "persistentStoreCoordinator"

-- | @Selector@ for @configurationName@
configurationNameSelector :: Selector '[] (Id NSString)
configurationNameSelector = mkSelector "configurationName"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSDictionary)
optionsSelector = mkSelector "options"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector '[Bool] ()
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id NSDictionary] ()
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @coreSpotlightExporter@
coreSpotlightExporterSelector :: Selector '[] (Id NSCoreDataCoreSpotlightDelegate)
coreSpotlightExporterSelector = mkSelector "coreSpotlightExporter"

