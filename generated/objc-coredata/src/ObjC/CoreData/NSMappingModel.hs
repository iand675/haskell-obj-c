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
  , mappingModelFromBundles_forSourceModel_destinationModelSelector
  , inferredMappingModelForSourceModel_destinationModel_errorSelector
  , initWithContentsOfURLSelector
  , entityMappingsSelector
  , setEntityMappingsSelector
  , entityMappingsByNameSelector


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

-- | @+ mappingModelFromBundles:forSourceModel:destinationModel:@
mappingModelFromBundles_forSourceModel_destinationModel :: (IsNSArray bundles, IsNSManagedObjectModel sourceModel, IsNSManagedObjectModel destinationModel) => bundles -> sourceModel -> destinationModel -> IO (Id NSMappingModel)
mappingModelFromBundles_forSourceModel_destinationModel bundles sourceModel destinationModel =
  do
    cls' <- getRequiredClass "NSMappingModel"
    withObjCPtr bundles $ \raw_bundles ->
      withObjCPtr sourceModel $ \raw_sourceModel ->
        withObjCPtr destinationModel $ \raw_destinationModel ->
          sendClassMsg cls' (mkSelector "mappingModelFromBundles:forSourceModel:destinationModel:") (retPtr retVoid) [argPtr (castPtr raw_bundles :: Ptr ()), argPtr (castPtr raw_sourceModel :: Ptr ()), argPtr (castPtr raw_destinationModel :: Ptr ())] >>= retainedObject . castPtr

-- | @+ inferredMappingModelForSourceModel:destinationModel:error:@
inferredMappingModelForSourceModel_destinationModel_error :: (IsNSManagedObjectModel sourceModel, IsNSManagedObjectModel destinationModel, IsNSError error_) => sourceModel -> destinationModel -> error_ -> IO (Id NSMappingModel)
inferredMappingModelForSourceModel_destinationModel_error sourceModel destinationModel error_ =
  do
    cls' <- getRequiredClass "NSMappingModel"
    withObjCPtr sourceModel $ \raw_sourceModel ->
      withObjCPtr destinationModel $ \raw_destinationModel ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "inferredMappingModelForSourceModel:destinationModel:error:") (retPtr retVoid) [argPtr (castPtr raw_sourceModel :: Ptr ()), argPtr (castPtr raw_destinationModel :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSMappingModel nsMappingModel, IsNSURL url) => nsMappingModel -> url -> IO (Id NSMappingModel)
initWithContentsOfURL nsMappingModel  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsMappingModel (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- entityMappings@
entityMappings :: IsNSMappingModel nsMappingModel => nsMappingModel -> IO (Id NSArray)
entityMappings nsMappingModel  =
  sendMsg nsMappingModel (mkSelector "entityMappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEntityMappings:@
setEntityMappings :: (IsNSMappingModel nsMappingModel, IsNSArray value) => nsMappingModel -> value -> IO ()
setEntityMappings nsMappingModel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMappingModel (mkSelector "setEntityMappings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- entityMappingsByName@
entityMappingsByName :: IsNSMappingModel nsMappingModel => nsMappingModel -> IO (Id NSDictionary)
entityMappingsByName nsMappingModel  =
  sendMsg nsMappingModel (mkSelector "entityMappingsByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mappingModelFromBundles:forSourceModel:destinationModel:@
mappingModelFromBundles_forSourceModel_destinationModelSelector :: Selector
mappingModelFromBundles_forSourceModel_destinationModelSelector = mkSelector "mappingModelFromBundles:forSourceModel:destinationModel:"

-- | @Selector@ for @inferredMappingModelForSourceModel:destinationModel:error:@
inferredMappingModelForSourceModel_destinationModel_errorSelector :: Selector
inferredMappingModelForSourceModel_destinationModel_errorSelector = mkSelector "inferredMappingModelForSourceModel:destinationModel:error:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @entityMappings@
entityMappingsSelector :: Selector
entityMappingsSelector = mkSelector "entityMappings"

-- | @Selector@ for @setEntityMappings:@
setEntityMappingsSelector :: Selector
setEntityMappingsSelector = mkSelector "setEntityMappings:"

-- | @Selector@ for @entityMappingsByName@
entityMappingsByNameSelector :: Selector
entityMappingsByNameSelector = mkSelector "entityMappingsByName"

