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
  , metadataForPersistentStoreWithURL_errorSelector
  , setMetadata_forPersistentStoreWithURL_errorSelector
  , migrationManagerClassSelector
  , initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector
  , initSelector
  , loadMetadataSelector
  , didAddToPersistentStoreCoordinatorSelector
  , willRemoveFromPersistentStoreCoordinatorSelector
  , persistentStoreCoordinatorSelector
  , configurationNameSelector
  , optionsSelector
  , urlSelector
  , setURLSelector
  , identifierSelector
  , setIdentifierSelector
  , typeSelector
  , readOnlySelector
  , setReadOnlySelector
  , metadataSelector
  , setMetadataSelector
  , coreSpotlightExporterSelector


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

-- | @+ metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreWithURL_error url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStore"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "metadataForPersistentStoreWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setMetadata:forPersistentStoreWithURL:error:@
setMetadata_forPersistentStoreWithURL_error :: (IsNSDictionary metadata, IsNSURL url, IsNSError error_) => metadata -> url -> error_ -> IO Bool
setMetadata_forPersistentStoreWithURL_error metadata url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStore"
    withObjCPtr metadata $ \raw_metadata ->
      withObjCPtr url $ \raw_url ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "setMetadata:forPersistentStoreWithURL:error:") retCULong [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ migrationManagerClass@
migrationManagerClass :: IO Class
migrationManagerClass  =
  do
    cls' <- getRequiredClass "NSPersistentStore"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "migrationManagerClass") (retPtr retVoid) []

-- | @- initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_options :: (IsNSPersistentStore nsPersistentStore, IsNSPersistentStoreCoordinator root, IsNSString name, IsNSURL url, IsNSDictionary options) => nsPersistentStore -> root -> name -> url -> options -> IO (Id NSPersistentStore)
initWithPersistentStoreCoordinator_configurationName_URL_options nsPersistentStore  root name url options =
withObjCPtr root $ \raw_root ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
          sendMsg nsPersistentStore (mkSelector "initWithPersistentStoreCoordinator:configurationName:URL:options:") (retPtr retVoid) [argPtr (castPtr raw_root :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSPersistentStore)
init_ nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- loadMetadata:@
loadMetadata :: (IsNSPersistentStore nsPersistentStore, IsNSError error_) => nsPersistentStore -> error_ -> IO Bool
loadMetadata nsPersistentStore  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStore (mkSelector "loadMetadata:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- didAddToPersistentStoreCoordinator:@
didAddToPersistentStoreCoordinator :: (IsNSPersistentStore nsPersistentStore, IsNSPersistentStoreCoordinator coordinator) => nsPersistentStore -> coordinator -> IO ()
didAddToPersistentStoreCoordinator nsPersistentStore  coordinator =
withObjCPtr coordinator $ \raw_coordinator ->
    sendMsg nsPersistentStore (mkSelector "didAddToPersistentStoreCoordinator:") retVoid [argPtr (castPtr raw_coordinator :: Ptr ())]

-- | @- willRemoveFromPersistentStoreCoordinator:@
willRemoveFromPersistentStoreCoordinator :: (IsNSPersistentStore nsPersistentStore, IsNSPersistentStoreCoordinator coordinator) => nsPersistentStore -> coordinator -> IO ()
willRemoveFromPersistentStoreCoordinator nsPersistentStore  coordinator =
withObjCPtr coordinator $ \raw_coordinator ->
    sendMsg nsPersistentStore (mkSelector "willRemoveFromPersistentStoreCoordinator:") retVoid [argPtr (castPtr raw_coordinator :: Ptr ())]

-- | @- persistentStoreCoordinator@
persistentStoreCoordinator :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSPersistentStoreCoordinator)
persistentStoreCoordinator nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "persistentStoreCoordinator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- configurationName@
configurationName :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSString)
configurationName nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "configurationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSDictionary)
options nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSURL)
url nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsNSPersistentStore nsPersistentStore, IsNSURL value) => nsPersistentStore -> value -> IO ()
setURL nsPersistentStore  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStore (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSString)
identifier nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsNSPersistentStore nsPersistentStore, IsNSString value) => nsPersistentStore -> value -> IO ()
setIdentifier nsPersistentStore  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStore (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSString)
type_ nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- readOnly@
readOnly :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO Bool
readOnly nsPersistentStore  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStore (mkSelector "readOnly") retCULong []

-- | @- setReadOnly:@
setReadOnly :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> Bool -> IO ()
setReadOnly nsPersistentStore  value =
  sendMsg nsPersistentStore (mkSelector "setReadOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- metadata@
metadata :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSDictionary)
metadata nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadata:@
setMetadata :: (IsNSPersistentStore nsPersistentStore, IsNSDictionary value) => nsPersistentStore -> value -> IO ()
setMetadata nsPersistentStore  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStore (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coreSpotlightExporter@
coreSpotlightExporter :: IsNSPersistentStore nsPersistentStore => nsPersistentStore -> IO (Id NSCoreDataCoreSpotlightDelegate)
coreSpotlightExporter nsPersistentStore  =
  sendMsg nsPersistentStore (mkSelector "coreSpotlightExporter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_errorSelector :: Selector
metadataForPersistentStoreWithURL_errorSelector = mkSelector "metadataForPersistentStoreWithURL:error:"

-- | @Selector@ for @setMetadata:forPersistentStoreWithURL:error:@
setMetadata_forPersistentStoreWithURL_errorSelector :: Selector
setMetadata_forPersistentStoreWithURL_errorSelector = mkSelector "setMetadata:forPersistentStoreWithURL:error:"

-- | @Selector@ for @migrationManagerClass@
migrationManagerClassSelector :: Selector
migrationManagerClassSelector = mkSelector "migrationManagerClass"

-- | @Selector@ for @initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector :: Selector
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector = mkSelector "initWithPersistentStoreCoordinator:configurationName:URL:options:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @loadMetadata:@
loadMetadataSelector :: Selector
loadMetadataSelector = mkSelector "loadMetadata:"

-- | @Selector@ for @didAddToPersistentStoreCoordinator:@
didAddToPersistentStoreCoordinatorSelector :: Selector
didAddToPersistentStoreCoordinatorSelector = mkSelector "didAddToPersistentStoreCoordinator:"

-- | @Selector@ for @willRemoveFromPersistentStoreCoordinator:@
willRemoveFromPersistentStoreCoordinatorSelector :: Selector
willRemoveFromPersistentStoreCoordinatorSelector = mkSelector "willRemoveFromPersistentStoreCoordinator:"

-- | @Selector@ for @persistentStoreCoordinator@
persistentStoreCoordinatorSelector :: Selector
persistentStoreCoordinatorSelector = mkSelector "persistentStoreCoordinator"

-- | @Selector@ for @configurationName@
configurationNameSelector :: Selector
configurationNameSelector = mkSelector "configurationName"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @coreSpotlightExporter@
coreSpotlightExporterSelector :: Selector
coreSpotlightExporterSelector = mkSelector "coreSpotlightExporter"

