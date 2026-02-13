{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExtensionContext@.
module ObjC.Foundation.NSExtensionContext
  ( NSExtensionContext
  , IsNSExtensionContext(..)
  , completeRequestReturningItems_completionHandler
  , cancelRequestWithError
  , openURL_completionHandler
  , inputItems
  , cancelRequestWithErrorSelector
  , completeRequestReturningItems_completionHandlerSelector
  , inputItemsSelector
  , openURL_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsNSExtensionContext nsExtensionContext, IsNSArray items) => nsExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler nsExtensionContext items completionHandler =
  sendMessage nsExtensionContext completeRequestReturningItems_completionHandlerSelector (toNSArray items) completionHandler

-- | @- cancelRequestWithError:@
cancelRequestWithError :: (IsNSExtensionContext nsExtensionContext, IsNSError error_) => nsExtensionContext -> error_ -> IO ()
cancelRequestWithError nsExtensionContext error_ =
  sendMessage nsExtensionContext cancelRequestWithErrorSelector (toNSError error_)

-- | @- openURL:completionHandler:@
openURL_completionHandler :: (IsNSExtensionContext nsExtensionContext, IsNSURL url) => nsExtensionContext -> url -> Ptr () -> IO ()
openURL_completionHandler nsExtensionContext url completionHandler =
  sendMessage nsExtensionContext openURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- inputItems@
inputItems :: IsNSExtensionContext nsExtensionContext => nsExtensionContext -> IO (Id NSArray)
inputItems nsExtensionContext =
  sendMessage nsExtensionContext inputItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector '[Id NSError] ()
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

-- | @Selector@ for @openURL:completionHandler:@
openURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
openURL_completionHandlerSelector = mkSelector "openURL:completionHandler:"

-- | @Selector@ for @inputItems@
inputItemsSelector :: Selector '[] (Id NSArray)
inputItemsSelector = mkSelector "inputItems"

