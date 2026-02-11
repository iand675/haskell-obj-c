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
  , isIndexingAvailableSelector
  , defaultSearchableIndexSelector
  , initWithNameSelector
  , initWithName_protectionClassSelector
  , indexSearchableItems_completionHandlerSelector
  , deleteSearchableItemsWithIdentifiers_completionHandlerSelector
  , deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector
  , deleteAllSearchableItemsWithCompletionHandlerSelector
  , fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector
  , beginIndexBatchSelector
  , endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector
  , endIndexBatchWithClientState_completionHandlerSelector
  , fetchLastClientStateWithCompletionHandlerSelector
  , indexDelegateSelector
  , setIndexDelegateSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @+ isIndexingAvailable@
isIndexingAvailable :: IO Bool
isIndexingAvailable  =
  do
    cls' <- getRequiredClass "CSSearchableIndex"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isIndexingAvailable") retCULong []

-- | @+ defaultSearchableIndex@
defaultSearchableIndex :: IO (Id CSSearchableIndex)
defaultSearchableIndex  =
  do
    cls' <- getRequiredClass "CSSearchableIndex"
    sendClassMsg cls' (mkSelector "defaultSearchableIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsCSSearchableIndex csSearchableIndex, IsNSString name) => csSearchableIndex -> name -> IO (Id CSSearchableIndex)
initWithName csSearchableIndex  name =
  withObjCPtr name $ \raw_name ->
      sendMsg csSearchableIndex (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:protectionClass:@
initWithName_protectionClass :: (IsCSSearchableIndex csSearchableIndex, IsNSString name, IsNSString protectionClass) => csSearchableIndex -> name -> protectionClass -> IO (Id CSSearchableIndex)
initWithName_protectionClass csSearchableIndex  name protectionClass =
  withObjCPtr name $ \raw_name ->
    withObjCPtr protectionClass $ \raw_protectionClass ->
        sendMsg csSearchableIndex (mkSelector "initWithName:protectionClass:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_protectionClass :: Ptr ())] >>= ownedObject . castPtr

-- | @- indexSearchableItems:completionHandler:@
indexSearchableItems_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSArray items) => csSearchableIndex -> items -> Ptr () -> IO ()
indexSearchableItems_completionHandler csSearchableIndex  items completionHandler =
  withObjCPtr items $ \raw_items ->
      sendMsg csSearchableIndex (mkSelector "indexSearchableItems:completionHandler:") retVoid [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- deleteSearchableItemsWithIdentifiers:completionHandler:@
deleteSearchableItemsWithIdentifiers_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSArray identifiers) => csSearchableIndex -> identifiers -> Ptr () -> IO ()
deleteSearchableItemsWithIdentifiers_completionHandler csSearchableIndex  identifiers completionHandler =
  withObjCPtr identifiers $ \raw_identifiers ->
      sendMsg csSearchableIndex (mkSelector "deleteSearchableItemsWithIdentifiers:completionHandler:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- deleteSearchableItemsWithDomainIdentifiers:completionHandler:@
deleteSearchableItemsWithDomainIdentifiers_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSArray domainIdentifiers) => csSearchableIndex -> domainIdentifiers -> Ptr () -> IO ()
deleteSearchableItemsWithDomainIdentifiers_completionHandler csSearchableIndex  domainIdentifiers completionHandler =
  withObjCPtr domainIdentifiers $ \raw_domainIdentifiers ->
      sendMsg csSearchableIndex (mkSelector "deleteSearchableItemsWithDomainIdentifiers:completionHandler:") retVoid [argPtr (castPtr raw_domainIdentifiers :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- deleteAllSearchableItemsWithCompletionHandler:@
deleteAllSearchableItemsWithCompletionHandler :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> Ptr () -> IO ()
deleteAllSearchableItemsWithCompletionHandler csSearchableIndex  completionHandler =
    sendMsg csSearchableIndex (mkSelector "deleteAllSearchableItemsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:@
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSString bundleIdentifier, IsNSString itemIdentifier, IsUTType contentType) => csSearchableIndex -> bundleIdentifier -> itemIdentifier -> contentType -> Ptr () -> IO ()
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandler csSearchableIndex  bundleIdentifier itemIdentifier contentType completionHandler =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
    withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      withObjCPtr contentType $ \raw_contentType ->
          sendMsg csSearchableIndex (mkSelector "fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:") retVoid [argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argPtr (castPtr raw_itemIdentifier :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- beginIndexBatch@
beginIndexBatch :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> IO ()
beginIndexBatch csSearchableIndex  =
    sendMsg csSearchableIndex (mkSelector "beginIndexBatch") retVoid []

-- | @- endIndexBatchWithExpectedClientState:newClientState:completionHandler:@
endIndexBatchWithExpectedClientState_newClientState_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSData expectedClientState, IsNSData newClientState) => csSearchableIndex -> expectedClientState -> newClientState -> Ptr () -> IO ()
endIndexBatchWithExpectedClientState_newClientState_completionHandler csSearchableIndex  expectedClientState newClientState completionHandler =
  withObjCPtr expectedClientState $ \raw_expectedClientState ->
    withObjCPtr newClientState $ \raw_newClientState ->
        sendMsg csSearchableIndex (mkSelector "endIndexBatchWithExpectedClientState:newClientState:completionHandler:") retVoid [argPtr (castPtr raw_expectedClientState :: Ptr ()), argPtr (castPtr raw_newClientState :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endIndexBatchWithClientState:completionHandler:@
endIndexBatchWithClientState_completionHandler :: (IsCSSearchableIndex csSearchableIndex, IsNSData clientState) => csSearchableIndex -> clientState -> Ptr () -> IO ()
endIndexBatchWithClientState_completionHandler csSearchableIndex  clientState completionHandler =
  withObjCPtr clientState $ \raw_clientState ->
      sendMsg csSearchableIndex (mkSelector "endIndexBatchWithClientState:completionHandler:") retVoid [argPtr (castPtr raw_clientState :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchLastClientStateWithCompletionHandler:@
fetchLastClientStateWithCompletionHandler :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> Ptr () -> IO ()
fetchLastClientStateWithCompletionHandler csSearchableIndex  completionHandler =
    sendMsg csSearchableIndex (mkSelector "fetchLastClientStateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- indexDelegate@
indexDelegate :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> IO RawId
indexDelegate csSearchableIndex  =
    fmap (RawId . castPtr) $ sendMsg csSearchableIndex (mkSelector "indexDelegate") (retPtr retVoid) []

-- | @- setIndexDelegate:@
setIndexDelegate :: IsCSSearchableIndex csSearchableIndex => csSearchableIndex -> RawId -> IO ()
setIndexDelegate csSearchableIndex  value =
    sendMsg csSearchableIndex (mkSelector "setIndexDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isIndexingAvailable@
isIndexingAvailableSelector :: Selector
isIndexingAvailableSelector = mkSelector "isIndexingAvailable"

-- | @Selector@ for @defaultSearchableIndex@
defaultSearchableIndexSelector :: Selector
defaultSearchableIndexSelector = mkSelector "defaultSearchableIndex"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:protectionClass:@
initWithName_protectionClassSelector :: Selector
initWithName_protectionClassSelector = mkSelector "initWithName:protectionClass:"

-- | @Selector@ for @indexSearchableItems:completionHandler:@
indexSearchableItems_completionHandlerSelector :: Selector
indexSearchableItems_completionHandlerSelector = mkSelector "indexSearchableItems:completionHandler:"

-- | @Selector@ for @deleteSearchableItemsWithIdentifiers:completionHandler:@
deleteSearchableItemsWithIdentifiers_completionHandlerSelector :: Selector
deleteSearchableItemsWithIdentifiers_completionHandlerSelector = mkSelector "deleteSearchableItemsWithIdentifiers:completionHandler:"

-- | @Selector@ for @deleteSearchableItemsWithDomainIdentifiers:completionHandler:@
deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector :: Selector
deleteSearchableItemsWithDomainIdentifiers_completionHandlerSelector = mkSelector "deleteSearchableItemsWithDomainIdentifiers:completionHandler:"

-- | @Selector@ for @deleteAllSearchableItemsWithCompletionHandler:@
deleteAllSearchableItemsWithCompletionHandlerSelector :: Selector
deleteAllSearchableItemsWithCompletionHandlerSelector = mkSelector "deleteAllSearchableItemsWithCompletionHandler:"

-- | @Selector@ for @fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:@
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector :: Selector
fetchDataForBundleIdentifier_itemIdentifier_contentType_completionHandlerSelector = mkSelector "fetchDataForBundleIdentifier:itemIdentifier:contentType:completionHandler:"

-- | @Selector@ for @beginIndexBatch@
beginIndexBatchSelector :: Selector
beginIndexBatchSelector = mkSelector "beginIndexBatch"

-- | @Selector@ for @endIndexBatchWithExpectedClientState:newClientState:completionHandler:@
endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector :: Selector
endIndexBatchWithExpectedClientState_newClientState_completionHandlerSelector = mkSelector "endIndexBatchWithExpectedClientState:newClientState:completionHandler:"

-- | @Selector@ for @endIndexBatchWithClientState:completionHandler:@
endIndexBatchWithClientState_completionHandlerSelector :: Selector
endIndexBatchWithClientState_completionHandlerSelector = mkSelector "endIndexBatchWithClientState:completionHandler:"

-- | @Selector@ for @fetchLastClientStateWithCompletionHandler:@
fetchLastClientStateWithCompletionHandlerSelector :: Selector
fetchLastClientStateWithCompletionHandlerSelector = mkSelector "fetchLastClientStateWithCompletionHandler:"

-- | @Selector@ for @indexDelegate@
indexDelegateSelector :: Selector
indexDelegateSelector = mkSelector "indexDelegate"

-- | @Selector@ for @setIndexDelegate:@
setIndexDelegateSelector :: Selector
setIndexDelegateSelector = mkSelector "setIndexDelegate:"

