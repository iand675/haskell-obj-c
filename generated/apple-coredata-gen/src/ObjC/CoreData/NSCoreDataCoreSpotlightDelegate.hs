{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCoreDataCoreSpotlightDelegate@.
module ObjC.CoreData.NSCoreDataCoreSpotlightDelegate
  ( NSCoreDataCoreSpotlightDelegate
  , IsNSCoreDataCoreSpotlightDelegate(..)
  , domainIdentifier
  , indexName
  , init_
  , initForStoreWithDescription_coordinator
  , initForStoreWithDescription_model
  , startSpotlightIndexing
  , stopSpotlightIndexing
  , deleteSpotlightIndexWithCompletionHandler
  , attributeSetForObject
  , searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandler
  , searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandler
  , indexingEnabled
  , attributeSetForObjectSelector
  , deleteSpotlightIndexWithCompletionHandlerSelector
  , domainIdentifierSelector
  , indexNameSelector
  , indexingEnabledSelector
  , initForStoreWithDescription_coordinatorSelector
  , initForStoreWithDescription_modelSelector
  , initSelector
  , searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector
  , searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector
  , startSpotlightIndexingSelector
  , stopSpotlightIndexingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- domainIdentifier@
domainIdentifier :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO (Id NSString)
domainIdentifier nsCoreDataCoreSpotlightDelegate =
  sendMessage nsCoreDataCoreSpotlightDelegate domainIdentifierSelector

-- | @- indexName@
indexName :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO (Id NSString)
indexName nsCoreDataCoreSpotlightDelegate =
  sendMessage nsCoreDataCoreSpotlightDelegate indexNameSelector

-- | @- init@
init_ :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO (Id NSCoreDataCoreSpotlightDelegate)
init_ nsCoreDataCoreSpotlightDelegate =
  sendOwnedMessage nsCoreDataCoreSpotlightDelegate initSelector

-- | @- initForStoreWithDescription:coordinator:@
initForStoreWithDescription_coordinator :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsNSPersistentStoreDescription description, IsNSPersistentStoreCoordinator psc) => nsCoreDataCoreSpotlightDelegate -> description -> psc -> IO (Id NSCoreDataCoreSpotlightDelegate)
initForStoreWithDescription_coordinator nsCoreDataCoreSpotlightDelegate description psc =
  sendOwnedMessage nsCoreDataCoreSpotlightDelegate initForStoreWithDescription_coordinatorSelector (toNSPersistentStoreDescription description) (toNSPersistentStoreCoordinator psc)

-- | @- initForStoreWithDescription:model:@
initForStoreWithDescription_model :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsNSPersistentStoreDescription description, IsNSManagedObjectModel model) => nsCoreDataCoreSpotlightDelegate -> description -> model -> IO (Id NSCoreDataCoreSpotlightDelegate)
initForStoreWithDescription_model nsCoreDataCoreSpotlightDelegate description model =
  sendOwnedMessage nsCoreDataCoreSpotlightDelegate initForStoreWithDescription_modelSelector (toNSPersistentStoreDescription description) (toNSManagedObjectModel model)

-- | @- startSpotlightIndexing@
startSpotlightIndexing :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO ()
startSpotlightIndexing nsCoreDataCoreSpotlightDelegate =
  sendMessage nsCoreDataCoreSpotlightDelegate startSpotlightIndexingSelector

-- | @- stopSpotlightIndexing@
stopSpotlightIndexing :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO ()
stopSpotlightIndexing nsCoreDataCoreSpotlightDelegate =
  sendMessage nsCoreDataCoreSpotlightDelegate stopSpotlightIndexingSelector

-- | @- deleteSpotlightIndexWithCompletionHandler:@
deleteSpotlightIndexWithCompletionHandler :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> Ptr () -> IO ()
deleteSpotlightIndexWithCompletionHandler nsCoreDataCoreSpotlightDelegate completionHandler =
  sendMessage nsCoreDataCoreSpotlightDelegate deleteSpotlightIndexWithCompletionHandlerSelector completionHandler

-- | @- attributeSetForObject:@
attributeSetForObject :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsNSManagedObject object) => nsCoreDataCoreSpotlightDelegate -> object -> IO (Id CSSearchableItemAttributeSet)
attributeSetForObject nsCoreDataCoreSpotlightDelegate object =
  sendMessage nsCoreDataCoreSpotlightDelegate attributeSetForObjectSelector (toNSManagedObject object)

-- | @- searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:@
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandler :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsCSSearchableIndex searchableIndex) => nsCoreDataCoreSpotlightDelegate -> searchableIndex -> Ptr () -> IO ()
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandler nsCoreDataCoreSpotlightDelegate searchableIndex acknowledgementHandler =
  sendMessage nsCoreDataCoreSpotlightDelegate searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector (toCSSearchableIndex searchableIndex) acknowledgementHandler

-- | @- searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:@
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandler :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsCSSearchableIndex searchableIndex, IsNSArray identifiers) => nsCoreDataCoreSpotlightDelegate -> searchableIndex -> identifiers -> Ptr () -> IO ()
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandler nsCoreDataCoreSpotlightDelegate searchableIndex identifiers acknowledgementHandler =
  sendMessage nsCoreDataCoreSpotlightDelegate searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector (toCSSearchableIndex searchableIndex) (toNSArray identifiers) acknowledgementHandler

-- | @- indexingEnabled@
indexingEnabled :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO Bool
indexingEnabled nsCoreDataCoreSpotlightDelegate =
  sendMessage nsCoreDataCoreSpotlightDelegate indexingEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector '[] (Id NSString)
domainIdentifierSelector = mkSelector "domainIdentifier"

-- | @Selector@ for @indexName@
indexNameSelector :: Selector '[] (Id NSString)
indexNameSelector = mkSelector "indexName"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCoreDataCoreSpotlightDelegate)
initSelector = mkSelector "init"

-- | @Selector@ for @initForStoreWithDescription:coordinator:@
initForStoreWithDescription_coordinatorSelector :: Selector '[Id NSPersistentStoreDescription, Id NSPersistentStoreCoordinator] (Id NSCoreDataCoreSpotlightDelegate)
initForStoreWithDescription_coordinatorSelector = mkSelector "initForStoreWithDescription:coordinator:"

-- | @Selector@ for @initForStoreWithDescription:model:@
initForStoreWithDescription_modelSelector :: Selector '[Id NSPersistentStoreDescription, Id NSManagedObjectModel] (Id NSCoreDataCoreSpotlightDelegate)
initForStoreWithDescription_modelSelector = mkSelector "initForStoreWithDescription:model:"

-- | @Selector@ for @startSpotlightIndexing@
startSpotlightIndexingSelector :: Selector '[] ()
startSpotlightIndexingSelector = mkSelector "startSpotlightIndexing"

-- | @Selector@ for @stopSpotlightIndexing@
stopSpotlightIndexingSelector :: Selector '[] ()
stopSpotlightIndexingSelector = mkSelector "stopSpotlightIndexing"

-- | @Selector@ for @deleteSpotlightIndexWithCompletionHandler:@
deleteSpotlightIndexWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
deleteSpotlightIndexWithCompletionHandlerSelector = mkSelector "deleteSpotlightIndexWithCompletionHandler:"

-- | @Selector@ for @attributeSetForObject:@
attributeSetForObjectSelector :: Selector '[Id NSManagedObject] (Id CSSearchableItemAttributeSet)
attributeSetForObjectSelector = mkSelector "attributeSetForObject:"

-- | @Selector@ for @searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:@
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector :: Selector '[Id CSSearchableIndex, Ptr ()] ()
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector = mkSelector "searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:"

-- | @Selector@ for @searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:@
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector :: Selector '[Id CSSearchableIndex, Id NSArray, Ptr ()] ()
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector = mkSelector "searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:"

-- | @Selector@ for @indexingEnabled@
indexingEnabledSelector :: Selector '[] Bool
indexingEnabledSelector = mkSelector "indexingEnabled"

