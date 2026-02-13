{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMappingModel@.
module ObjC.CoreData.NSMappingModel
  ( NSMappingModel
  , IsNSMappingModel(..)
  , mappingModelFromBundles_forSourceModel_destinationModel
  , inferredMappingModelForSourceModel_destinationModel_error
  , initWithContentsOfURL
  , entityMappings
  , setEntityMappings
  , entityMappingsByName
  , entityMappingsByNameSelector
  , entityMappingsSelector
  , inferredMappingModelForSourceModel_destinationModel_errorSelector
  , initWithContentsOfURLSelector
  , mappingModelFromBundles_forSourceModel_destinationModelSelector
  , setEntityMappingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ mappingModelFromBundles:forSourceModel:destinationModel:@
mappingModelFromBundles_forSourceModel_destinationModel :: (IsNSArray bundles, IsNSManagedObjectModel sourceModel, IsNSManagedObjectModel destinationModel) => bundles -> sourceModel -> destinationModel -> IO (Id NSMappingModel)
mappingModelFromBundles_forSourceModel_destinationModel bundles sourceModel destinationModel =
  do
    cls' <- getRequiredClass "NSMappingModel"
    sendClassMessage cls' mappingModelFromBundles_forSourceModel_destinationModelSelector (toNSArray bundles) (toNSManagedObjectModel sourceModel) (toNSManagedObjectModel destinationModel)

-- | @+ inferredMappingModelForSourceModel:destinationModel:error:@
inferredMappingModelForSourceModel_destinationModel_error :: (IsNSManagedObjectModel sourceModel, IsNSManagedObjectModel destinationModel, IsNSError error_) => sourceModel -> destinationModel -> error_ -> IO (Id NSMappingModel)
inferredMappingModelForSourceModel_destinationModel_error sourceModel destinationModel error_ =
  do
    cls' <- getRequiredClass "NSMappingModel"
    sendClassMessage cls' inferredMappingModelForSourceModel_destinationModel_errorSelector (toNSManagedObjectModel sourceModel) (toNSManagedObjectModel destinationModel) (toNSError error_)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSMappingModel nsMappingModel, IsNSURL url) => nsMappingModel -> url -> IO (Id NSMappingModel)
initWithContentsOfURL nsMappingModel url =
  sendOwnedMessage nsMappingModel initWithContentsOfURLSelector (toNSURL url)

-- | @- entityMappings@
entityMappings :: IsNSMappingModel nsMappingModel => nsMappingModel -> IO (Id NSArray)
entityMappings nsMappingModel =
  sendMessage nsMappingModel entityMappingsSelector

-- | @- setEntityMappings:@
setEntityMappings :: (IsNSMappingModel nsMappingModel, IsNSArray value) => nsMappingModel -> value -> IO ()
setEntityMappings nsMappingModel value =
  sendMessage nsMappingModel setEntityMappingsSelector (toNSArray value)

-- | @- entityMappingsByName@
entityMappingsByName :: IsNSMappingModel nsMappingModel => nsMappingModel -> IO (Id NSDictionary)
entityMappingsByName nsMappingModel =
  sendMessage nsMappingModel entityMappingsByNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mappingModelFromBundles:forSourceModel:destinationModel:@
mappingModelFromBundles_forSourceModel_destinationModelSelector :: Selector '[Id NSArray, Id NSManagedObjectModel, Id NSManagedObjectModel] (Id NSMappingModel)
mappingModelFromBundles_forSourceModel_destinationModelSelector = mkSelector "mappingModelFromBundles:forSourceModel:destinationModel:"

-- | @Selector@ for @inferredMappingModelForSourceModel:destinationModel:error:@
inferredMappingModelForSourceModel_destinationModel_errorSelector :: Selector '[Id NSManagedObjectModel, Id NSManagedObjectModel, Id NSError] (Id NSMappingModel)
inferredMappingModelForSourceModel_destinationModel_errorSelector = mkSelector "inferredMappingModelForSourceModel:destinationModel:error:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSMappingModel)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @entityMappings@
entityMappingsSelector :: Selector '[] (Id NSArray)
entityMappingsSelector = mkSelector "entityMappings"

-- | @Selector@ for @setEntityMappings:@
setEntityMappingsSelector :: Selector '[Id NSArray] ()
setEntityMappingsSelector = mkSelector "setEntityMappings:"

-- | @Selector@ for @entityMappingsByName@
entityMappingsByNameSelector :: Selector '[] (Id NSDictionary)
entityMappingsByNameSelector = mkSelector "entityMappingsByName"

