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
  , domainIdentifierSelector
  , indexNameSelector
  , initSelector
  , initForStoreWithDescription_coordinatorSelector
  , initForStoreWithDescription_modelSelector
  , startSpotlightIndexingSelector
  , stopSpotlightIndexingSelector
  , deleteSpotlightIndexWithCompletionHandlerSelector
  , attributeSetForObjectSelector
  , searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector
  , searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector
  , indexingEnabledSelector


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
import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- domainIdentifier@
domainIdentifier :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO (Id NSString)
domainIdentifier nsCoreDataCoreSpotlightDelegate  =
  sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "domainIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexName@
indexName :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO (Id NSString)
indexName nsCoreDataCoreSpotlightDelegate  =
  sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "indexName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO (Id NSCoreDataCoreSpotlightDelegate)
init_ nsCoreDataCoreSpotlightDelegate  =
  sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initForStoreWithDescription:coordinator:@
initForStoreWithDescription_coordinator :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsNSPersistentStoreDescription description, IsNSPersistentStoreCoordinator psc) => nsCoreDataCoreSpotlightDelegate -> description -> psc -> IO (Id NSCoreDataCoreSpotlightDelegate)
initForStoreWithDescription_coordinator nsCoreDataCoreSpotlightDelegate  description psc =
withObjCPtr description $ \raw_description ->
  withObjCPtr psc $ \raw_psc ->
      sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "initForStoreWithDescription:coordinator:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_psc :: Ptr ())] >>= ownedObject . castPtr

-- | @- initForStoreWithDescription:model:@
initForStoreWithDescription_model :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsNSPersistentStoreDescription description, IsNSManagedObjectModel model) => nsCoreDataCoreSpotlightDelegate -> description -> model -> IO (Id NSCoreDataCoreSpotlightDelegate)
initForStoreWithDescription_model nsCoreDataCoreSpotlightDelegate  description model =
withObjCPtr description $ \raw_description ->
  withObjCPtr model $ \raw_model ->
      sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "initForStoreWithDescription:model:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_model :: Ptr ())] >>= ownedObject . castPtr

-- | @- startSpotlightIndexing@
startSpotlightIndexing :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO ()
startSpotlightIndexing nsCoreDataCoreSpotlightDelegate  =
  sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "startSpotlightIndexing") retVoid []

-- | @- stopSpotlightIndexing@
stopSpotlightIndexing :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO ()
stopSpotlightIndexing nsCoreDataCoreSpotlightDelegate  =
  sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "stopSpotlightIndexing") retVoid []

-- | @- deleteSpotlightIndexWithCompletionHandler:@
deleteSpotlightIndexWithCompletionHandler :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> Ptr () -> IO ()
deleteSpotlightIndexWithCompletionHandler nsCoreDataCoreSpotlightDelegate  completionHandler =
  sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "deleteSpotlightIndexWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- attributeSetForObject:@
attributeSetForObject :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsNSManagedObject object) => nsCoreDataCoreSpotlightDelegate -> object -> IO (Id CSSearchableItemAttributeSet)
attributeSetForObject nsCoreDataCoreSpotlightDelegate  object =
withObjCPtr object $ \raw_object ->
    sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "attributeSetForObject:") (retPtr retVoid) [argPtr (castPtr raw_object :: Ptr ())] >>= retainedObject . castPtr

-- | @- searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:@
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandler :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsCSSearchableIndex searchableIndex) => nsCoreDataCoreSpotlightDelegate -> searchableIndex -> Ptr () -> IO ()
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandler nsCoreDataCoreSpotlightDelegate  searchableIndex acknowledgementHandler =
withObjCPtr searchableIndex $ \raw_searchableIndex ->
    sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:") retVoid [argPtr (castPtr raw_searchableIndex :: Ptr ()), argPtr (castPtr acknowledgementHandler :: Ptr ())]

-- | @- searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:@
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandler :: (IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate, IsCSSearchableIndex searchableIndex, IsNSArray identifiers) => nsCoreDataCoreSpotlightDelegate -> searchableIndex -> identifiers -> Ptr () -> IO ()
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandler nsCoreDataCoreSpotlightDelegate  searchableIndex identifiers acknowledgementHandler =
withObjCPtr searchableIndex $ \raw_searchableIndex ->
  withObjCPtr identifiers $ \raw_identifiers ->
      sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:") retVoid [argPtr (castPtr raw_searchableIndex :: Ptr ()), argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr acknowledgementHandler :: Ptr ())]

-- | @- indexingEnabled@
indexingEnabled :: IsNSCoreDataCoreSpotlightDelegate nsCoreDataCoreSpotlightDelegate => nsCoreDataCoreSpotlightDelegate -> IO Bool
indexingEnabled nsCoreDataCoreSpotlightDelegate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCoreDataCoreSpotlightDelegate (mkSelector "indexingEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector
domainIdentifierSelector = mkSelector "domainIdentifier"

-- | @Selector@ for @indexName@
indexNameSelector :: Selector
indexNameSelector = mkSelector "indexName"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initForStoreWithDescription:coordinator:@
initForStoreWithDescription_coordinatorSelector :: Selector
initForStoreWithDescription_coordinatorSelector = mkSelector "initForStoreWithDescription:coordinator:"

-- | @Selector@ for @initForStoreWithDescription:model:@
initForStoreWithDescription_modelSelector :: Selector
initForStoreWithDescription_modelSelector = mkSelector "initForStoreWithDescription:model:"

-- | @Selector@ for @startSpotlightIndexing@
startSpotlightIndexingSelector :: Selector
startSpotlightIndexingSelector = mkSelector "startSpotlightIndexing"

-- | @Selector@ for @stopSpotlightIndexing@
stopSpotlightIndexingSelector :: Selector
stopSpotlightIndexingSelector = mkSelector "stopSpotlightIndexing"

-- | @Selector@ for @deleteSpotlightIndexWithCompletionHandler:@
deleteSpotlightIndexWithCompletionHandlerSelector :: Selector
deleteSpotlightIndexWithCompletionHandlerSelector = mkSelector "deleteSpotlightIndexWithCompletionHandler:"

-- | @Selector@ for @attributeSetForObject:@
attributeSetForObjectSelector :: Selector
attributeSetForObjectSelector = mkSelector "attributeSetForObject:"

-- | @Selector@ for @searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:@
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector :: Selector
searchableIndex_reindexAllSearchableItemsWithAcknowledgementHandlerSelector = mkSelector "searchableIndex:reindexAllSearchableItemsWithAcknowledgementHandler:"

-- | @Selector@ for @searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:@
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector :: Selector
searchableIndex_reindexSearchableItemsWithIdentifiers_acknowledgementHandlerSelector = mkSelector "searchableIndex:reindexSearchableItemsWithIdentifiers:acknowledgementHandler:"

-- | @Selector@ for @indexingEnabled@
indexingEnabledSelector :: Selector
indexingEnabledSelector = mkSelector "indexingEnabled"

