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
  , completeRequestSelector
  , completeRequestReturningItems_completionHandlerSelector
  , cancelRequestWithErrorSelector
  , domainIdentifierSelector


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

import ObjC.FileProviderUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Marks the action as complete.
--
-- Call this method when the action completes successfully.
--
-- ObjC selector: @- completeRequest@
completeRequest :: IsFPUIActionExtensionContext fpuiActionExtensionContext => fpuiActionExtensionContext -> IO ()
completeRequest fpuiActionExtensionContext  =
  sendMsg fpuiActionExtensionContext (mkSelector "completeRequest") retVoid []

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsFPUIActionExtensionContext fpuiActionExtensionContext, IsNSArray items) => fpuiActionExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler fpuiActionExtensionContext  items completionHandler =
withObjCPtr items $ \raw_items ->
    sendMsg fpuiActionExtensionContext (mkSelector "completeRequestReturningItems:completionHandler:") retVoid [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Cancels the action and returns the provided error.
--
-- Call this method if the action fails. Set the error's domain to ``FPUIErrorDomain``. Set the error code to a ``FPUIExtensionErrorCode`` value.
--
-- ObjC selector: @- cancelRequestWithError:@
cancelRequestWithError :: (IsFPUIActionExtensionContext fpuiActionExtensionContext, IsNSError error_) => fpuiActionExtensionContext -> error_ -> IO ()
cancelRequestWithError fpuiActionExtensionContext  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg fpuiActionExtensionContext (mkSelector "cancelRequestWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | The identifier for the domain managed by the current file provider.
--
-- ObjC selector: @- domainIdentifier@
domainIdentifier :: IsFPUIActionExtensionContext fpuiActionExtensionContext => fpuiActionExtensionContext -> IO (Id NSString)
domainIdentifier fpuiActionExtensionContext  =
  sendMsg fpuiActionExtensionContext (mkSelector "domainIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completeRequest@
completeRequestSelector :: Selector
completeRequestSelector = mkSelector "completeRequest"

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector
domainIdentifierSelector = mkSelector "domainIdentifier"

