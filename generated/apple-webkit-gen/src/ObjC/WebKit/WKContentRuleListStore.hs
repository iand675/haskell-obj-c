{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKContentRuleListStore@.
module ObjC.WebKit.WKContentRuleListStore
  ( WKContentRuleListStore
  , IsWKContentRuleListStore(..)
  , defaultStore
  , storeWithURL
  , compileContentRuleListForIdentifier_encodedContentRuleList_completionHandler
  , lookUpContentRuleListForIdentifier_completionHandler
  , removeContentRuleListForIdentifier_completionHandler
  , compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector
  , defaultStoreSelector
  , lookUpContentRuleListForIdentifier_completionHandlerSelector
  , removeContentRuleListForIdentifier_completionHandlerSelector
  , storeWithURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultStore@
defaultStore :: IO (Id WKContentRuleListStore)
defaultStore  =
  do
    cls' <- getRequiredClass "WKContentRuleListStore"
    sendClassMessage cls' defaultStoreSelector

-- | @+ storeWithURL:@
storeWithURL :: IsNSURL url => url -> IO (Id WKContentRuleListStore)
storeWithURL url =
  do
    cls' <- getRequiredClass "WKContentRuleListStore"
    sendClassMessage cls' storeWithURLSelector (toNSURL url)

-- | @- compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:@
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandler :: (IsWKContentRuleListStore wkContentRuleListStore, IsNSString identifier, IsNSString encodedContentRuleList) => wkContentRuleListStore -> identifier -> encodedContentRuleList -> Ptr () -> IO ()
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandler wkContentRuleListStore identifier encodedContentRuleList completionHandler =
  sendMessage wkContentRuleListStore compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector (toNSString identifier) (toNSString encodedContentRuleList) completionHandler

-- | @- lookUpContentRuleListForIdentifier:completionHandler:@
lookUpContentRuleListForIdentifier_completionHandler :: (IsWKContentRuleListStore wkContentRuleListStore, IsNSString identifier) => wkContentRuleListStore -> identifier -> Ptr () -> IO ()
lookUpContentRuleListForIdentifier_completionHandler wkContentRuleListStore identifier completionHandler =
  sendMessage wkContentRuleListStore lookUpContentRuleListForIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | @- removeContentRuleListForIdentifier:completionHandler:@
removeContentRuleListForIdentifier_completionHandler :: (IsWKContentRuleListStore wkContentRuleListStore, IsNSString identifier) => wkContentRuleListStore -> identifier -> Ptr () -> IO ()
removeContentRuleListForIdentifier_completionHandler wkContentRuleListStore identifier completionHandler =
  sendMessage wkContentRuleListStore removeContentRuleListForIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultStore@
defaultStoreSelector :: Selector '[] (Id WKContentRuleListStore)
defaultStoreSelector = mkSelector "defaultStore"

-- | @Selector@ for @storeWithURL:@
storeWithURLSelector :: Selector '[Id NSURL] (Id WKContentRuleListStore)
storeWithURLSelector = mkSelector "storeWithURL:"

-- | @Selector@ for @compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:@
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Ptr ()] ()
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector = mkSelector "compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:"

-- | @Selector@ for @lookUpContentRuleListForIdentifier:completionHandler:@
lookUpContentRuleListForIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
lookUpContentRuleListForIdentifier_completionHandlerSelector = mkSelector "lookUpContentRuleListForIdentifier:completionHandler:"

-- | @Selector@ for @removeContentRuleListForIdentifier:completionHandler:@
removeContentRuleListForIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
removeContentRuleListForIdentifier_completionHandlerSelector = mkSelector "removeContentRuleListForIdentifier:completionHandler:"

