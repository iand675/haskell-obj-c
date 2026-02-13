{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSearchableIndex@.
module ObjC.CoreSpotlight.CSSearchableIndex
  ( CSSearchableIndex
  , IsCSSearchableIndex(..)
  , isIndexingAvailable
  , defaultSearchableIndex
  , initWithName
  , initWithName_protectionClass
  , indexSearchableItems_completionHandler
  , deleteSearchableItemsWithIdentifiers_completionHandler
  , deleteSearchableItemsWithDomainIdentifiers_completionHandler
  , deleteAllSearchableItemsWithCompletionHandler
  , fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandler
  , beginIndexBatch
  , endIndexBatchWithExpectedClientState_newClientState_completionHandler
  , endIndexBatchWithClientState_completionHandler
  , fetchLastClientStateWithCompletionHandler
  , indexDelegate
  , setIndexDelegate
  , beginIndexBatchSelector
  , defaultSearchableIndexSelector
  , deleteAllSearchableItemsWithCompletionHandlerSelector
  , deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector
  , deleteSearchableItemsWithIdentifiers_completionHandlerSelector
  , endIndexBatchWithClientState_completionHandlerSelector
  , endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector
  , fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector
  , fetchLastClientStateWithCompletionHandlerSelector
  , indexDelegateSelector
  , indexSearchableItems_completionHandlerSelector
  , initWithNameSelector
  , initWithName_protectionClassSelector
  , isIndexingAvailableSelector
  , setIndexDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @+ isIndexingAvailable@
isIndexingAvailable :: IO Bool
isIndexingAvailable  =
  do
    cls' <- getRequiredClass "CSSearchableIndex"
    sendClassMessage cls' isIndexingAvailableSelector

-- | @+ defaultSearchableIndex@
defaultSearchableIndex :: IO (Id CSSearchableIndex)
defaultSearchableIndex  =
  do
    cls' <- getRequiredClass "CSSearchableIndex"
    sendClassMessage cls' defaultSearchableIndexSelector

-- | @- initWithName:@
initWithName :: (IsCSSearchableIndex csSearchableIndex, IsNSString name) => csSearchableIndex -> name -> IO (Id CSSearchableIndex)
initWithName csSearchableIndex name =
  sendOwnedMessage csSearchableIndex initWithNameSelector (toNSString name)

-- | @- initWithName:protectionClass:@
initWithName_protectionClass :: (IsCSSearchableIndex csSearchableIndex, IsNSString name, IsNSString protectionClass) => csSearchableIndex -> name -> protectionClass -> IO (Id CSSearchableIndex)
initWithName_protectionClass csSearchableIndex name protectionClass =
  sendOwnedMessage csSearchableIndex initWithName_protectionClassSelector (toNSString name) (toNSString protectionClass)

-- | @- indexSearchableItems:completionHandler:@
indexSearchableItems_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSArray items) => csSearchableIndex -> items -> Ptr () -> IO ()
indexSearchableItems_completionHandler csSearchableIndex items completionHandler =
  sendMessage csSearchableIndex indexSearchableItems_completionHandlerSelector (toNSArray items) completionHandler

-- | @- deleteSearchableItemsWithIdentifiers:completionHandler:@
deleteSearchableItemsWithIdentifiers_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSArray identifiers) => csSearchableIndex -> identifiers -> Ptr () -> IO ()
deleteSearchableItemsWithIdentifiers_completionHandler csSearchableIndex identifiers completionHandler =
  sendMessage csSearchableIndex deleteSearchableItemsWithIdentifiers_completionHandlerSelector (toNSArray identifiers) completionHandler

-- | @- deleteSearchableItemsWithDomainIdentifiers:completionHandler:@
deleteSearchableItemsWithDomainIdentifiers_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSArray domainIdentifiers) => csSearchableIndex -> domainIdentifiers -> Ptr () -> IO ()
deleteSearchableItemsWithDomainIdentifiers_completionHandler csSearchableIndex domainIdentifiers completionHandler =
  sendMessage csSearchableIndex deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector (toNSArray domainIdentifiers) completionHandler

-- | @- deleteAllSearchableItemsWithCompletionHandler:@
deleteAllSearchableItemsWithCompletionHandler :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> Ptr () -> IO ()
deleteAllSearchableItemsWithCompletionHandler csSearchableIndex completionHandler =
  sendMessage csSearchableIndex deleteAllSearchableItemsWithCompletionHandlerSelector completionHandler

-- | @- fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:@
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSString bundleIdentifier, IsNSString itemIdentifier, IsUTType contentType) => csSearchableIndex -> bundleIdentifier -> itemIdentifier -> contentType -> Ptr () -> IO ()
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandler csSearchableIndex bundleIdentifier itemIdentifier contentType completionHandler =
  sendMessage csSearchableIndex fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector (toNSString bundleIdentifier) (toNSString itemIdentifier) (toUTType contentType) completionHandler

-- | @- beginIndexBatch@
beginIndexBatch :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> IO ()
beginIndexBatch csSearchableIndex =
  sendMessage csSearchableIndex beginIndexBatchSelector

-- | @- endIndexBatchWithExpectedClientState:newClientState:completionHandler:@
endIndexBatchWithExpectedClientState_newClientState_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSData expectedClientState, IsNSData newClientState) => csSearchableIndex -> expectedClientState -> newClientState -> Ptr () -> IO ()
endIndexBatchWithExpectedClientState_newClientState_completionHandler csSearchableIndex expectedClientState newClientState completionHandler =
  sendMessage csSearchableIndex endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector (toNSData expectedClientState) (toNSData newClientState) completionHandler

-- | @- endIndexBatchWithClientState:completionHandler:@
endIndexBatchWithClientState_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSData clientState) => csSearchableIndex -> clientState -> Ptr () -> IO ()
endIndexBatchWithClientState_completionHandler csSearchableIndex clientState completionHandler =
  sendMessage csSearchableIndex endIndexBatchWithClientState_completionHandlerSelector (toNSData clientState) completionHandler

-- | @- fetchLastClientStateWithCompletionHandler:@
fetchLastClientStateWithCompletionHandler :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> Ptr () -> IO ()
fetchLastClientStateWithCompletionHandler csSearchableIndex completionHandler =
  sendMessage csSearchableIndex fetchLastClientStateWithCompletionHandlerSelector completionHandler

-- | @- indexDelegate@
indexDelegate :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> IO RawId
indexDelegate csSearchableIndex =
  sendMessage csSearchableIndex indexDelegateSelector

-- | @- setIndexDelegate:@
setIndexDelegate :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> RawId -> IO ()
setIndexDelegate csSearchableIndex value =
  sendMessage csSearchableIndex setIndexDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isIndexingAvailable@
isIndexingAvailableSelector :: Selector '[] Bool
isIndexingAvailableSelector = mkSelector "isIndexingAvailable"

-- | @Selector@ for @defaultSearchableIndex@
defaultSearchableIndexSelector :: Selector '[] (Id CSSearchableIndex)
defaultSearchableIndexSelector = mkSelector "defaultSearchableIndex"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id CSSearchableIndex)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:protectionClass:@
initWithName_protectionClassSelector :: Selector '[Id NSString, Id NSString] (Id CSSearchableIndex)
initWithName_protectionClassSelector = mkSelector "initWithName:protectionClass:"

-- | @Selector@ for @indexSearchableItems:completionHandler:@
indexSearchableItems_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
indexSearchableItems_completionHandlerSelector = mkSelector "indexSearchableItems:completionHandler:"

-- | @Selector@ for @deleteSearchableItemsWithIdentifiers:completionHandler:@
deleteSearchableItemsWithIdentifiers_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
deleteSearchableItemsWithIdentifiers_completionHandlerSelector = mkSelector "deleteSearchableItemsWithIdentifiers:completionHandler:"

-- | @Selector@ for @deleteSearchableItemsWithDomainIdentifiers:completionHandler:@
deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector = mkSelector "deleteSearchableItemsWithDomainIdentifiers:completionHandler:"

-- | @Selector@ for @deleteAllSearchableItemsWithCompletionHandler:@
deleteAllSearchableItemsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
deleteAllSearchableItemsWithCompletionHandlerSelector = mkSelector "deleteAllSearchableItemsWithCompletionHandler:"

-- | @Selector@ for @fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:@
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Id UTType, Ptr ()] ()
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector = mkSelector "fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:"

-- | @Selector@ for @beginIndexBatch@
beginIndexBatchSelector :: Selector '[] ()
beginIndexBatchSelector = mkSelector "beginIndexBatch"

-- | @Selector@ for @endIndexBatchWithExpectedClientState:newClientState:completionHandler:@
endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector :: Selector '[Id NSData, Id NSData, Ptr ()] ()
endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector = mkSelector "endIndexBatchWithExpectedClientState:newClientState:completionHandler:"

-- | @Selector@ for @endIndexBatchWithClientState:completionHandler:@
endIndexBatchWithClientState_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
endIndexBatchWithClientState_completionHandlerSelector = mkSelector "endIndexBatchWithClientState:completionHandler:"

-- | @Selector@ for @fetchLastClientStateWithCompletionHandler:@
fetchLastClientStateWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
fetchLastClientStateWithCompletionHandlerSelector = mkSelector "fetchLastClientStateWithCompletionHandler:"

-- | @Selector@ for @indexDelegate@
indexDelegateSelector :: Selector '[] RawId
indexDelegateSelector = mkSelector "indexDelegate"

-- | @Selector@ for @setIndexDelegate:@
setIndexDelegateSelector :: Selector '[RawId] ()
setIndexDelegateSelector = mkSelector "setIndexDelegate:"

