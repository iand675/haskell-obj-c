{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An extension context provided to File Provider UI extensions.
--
-- Generated bindings for @FPUIActionExtensionContext@.
module ObjC.FileProviderUI.FPUIActionExtensionContext
  ( FPUIActionExtensionContext
  , IsFPUIActionExtensionContext(..)
  , completeRequest
  , completeRequestReturningItems_completionHandler
  , cancelRequestWithError
  , domainIdentifier
  , cancelRequestWithErrorSelector
  , completeRequestReturningItems_completionHandlerSelector
  , completeRequestSelector
  , domainIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProviderUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Marks the action as complete.
--
-- Call this method when the action completes successfully.
--
-- ObjC selector: @- completeRequest@
completeRequest :: IsFPUIActionExtensionContext fpuiActionExtensionContext => fpuiActionExtensionContext -> IO ()
completeRequest fpuiActionExtensionContext =
  sendMessage fpuiActionExtensionContext completeRequestSelector

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsFPUIActionExtensionContext fpuiActionExtensionContext, IsNSArray items) => fpuiActionExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler fpuiActionExtensionContext items completionHandler =
  sendMessage fpuiActionExtensionContext completeRequestReturningItems_completionHandlerSelector (toNSArray items) completionHandler

-- | Cancels the action and returns the provided error.
--
-- Call this method if the action fails. Set the error's domain to ``FPUIErrorDomain``. Set the error code to a ``FPUIExtensionErrorCode`` value.
--
-- ObjC selector: @- cancelRequestWithError:@
cancelRequestWithError :: (IsFPUIActionExtensionContext fpuiActionExtensionContext, IsNSError error_) => fpuiActionExtensionContext -> error_ -> IO ()
cancelRequestWithError fpuiActionExtensionContext error_ =
  sendMessage fpuiActionExtensionContext cancelRequestWithErrorSelector (toNSError error_)

-- | The identifier for the domain managed by the current file provider.
--
-- ObjC selector: @- domainIdentifier@
domainIdentifier :: IsFPUIActionExtensionContext fpuiActionExtensionContext => fpuiActionExtensionContext -> IO (Id NSString)
domainIdentifier fpuiActionExtensionContext =
  sendMessage fpuiActionExtensionContext domainIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completeRequest@
completeRequestSelector :: Selector '[] ()
completeRequestSelector = mkSelector "completeRequest"

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector '[Id NSError] ()
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector '[] (Id NSString)
domainIdentifierSelector = mkSelector "domainIdentifier"

