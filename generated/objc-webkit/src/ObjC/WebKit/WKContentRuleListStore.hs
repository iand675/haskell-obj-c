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
  , defaultStoreSelector
  , storeWithURLSelector
  , compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector
  , lookUpContentRuleListForIdentifier_completionHandlerSelector
  , removeContentRuleListForIdentifier_completionHandlerSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultStore@
defaultStore :: IO (Id WKContentRuleListStore)
defaultStore  =
  do
    cls' <- getRequiredClass "WKContentRuleListStore"
    sendClassMsg cls' (mkSelector "defaultStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ storeWithURL:@
storeWithURL :: IsNSURL url => url -> IO (Id WKContentRuleListStore)
storeWithURL url =
  do
    cls' <- getRequiredClass "WKContentRuleListStore"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "storeWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:@
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandler :: (IsWKContentRuleListStore wkContentRuleListStore, IsNSString identifier, IsNSString encodedContentRuleList) => wkContentRuleListStore -> identifier -> encodedContentRuleList -> Ptr () -> IO ()
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandler wkContentRuleListStore  identifier encodedContentRuleList completionHandler =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr encodedContentRuleList $ \raw_encodedContentRuleList ->
      sendMsg wkContentRuleListStore (mkSelector "compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_encodedContentRuleList :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- lookUpContentRuleListForIdentifier:completionHandler:@
lookUpContentRuleListForIdentifier_completionHandler :: (IsWKContentRuleListStore wkContentRuleListStore, IsNSString identifier) => wkContentRuleListStore -> identifier -> Ptr () -> IO ()
lookUpContentRuleListForIdentifier_completionHandler wkContentRuleListStore  identifier completionHandler =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg wkContentRuleListStore (mkSelector "lookUpContentRuleListForIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeContentRuleListForIdentifier:completionHandler:@
removeContentRuleListForIdentifier_completionHandler :: (IsWKContentRuleListStore wkContentRuleListStore, IsNSString identifier) => wkContentRuleListStore -> identifier -> Ptr () -> IO ()
removeContentRuleListForIdentifier_completionHandler wkContentRuleListStore  identifier completionHandler =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg wkContentRuleListStore (mkSelector "removeContentRuleListForIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultStore@
defaultStoreSelector :: Selector
defaultStoreSelector = mkSelector "defaultStore"

-- | @Selector@ for @storeWithURL:@
storeWithURLSelector :: Selector
storeWithURLSelector = mkSelector "storeWithURL:"

-- | @Selector@ for @compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:@
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector :: Selector
compileContentRuleListForIdentifier_encodedContentRuleList_completionHandlerSelector = mkSelector "compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:"

-- | @Selector@ for @lookUpContentRuleListForIdentifier:completionHandler:@
lookUpContentRuleListForIdentifier_completionHandlerSelector :: Selector
lookUpContentRuleListForIdentifier_completionHandlerSelector = mkSelector "lookUpContentRuleListForIdentifier:completionHandler:"

-- | @Selector@ for @removeContentRuleListForIdentifier:completionHandler:@
removeContentRuleListForIdentifier_completionHandlerSelector :: Selector
removeContentRuleListForIdentifier_completionHandlerSelector = mkSelector "removeContentRuleListForIdentifier:completionHandler:"

