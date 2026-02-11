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
  , completeRequestReturningItems_completionHandlerSelector
  , cancelRequestWithErrorSelector
  , openURL_completionHandlerSelector
  , inputItemsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsNSExtensionContext nsExtensionContext, IsNSArray items) => nsExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler nsExtensionContext  items completionHandler =
withObjCPtr items $ \raw_items ->
    sendMsg nsExtensionContext (mkSelector "completeRequestReturningItems:completionHandler:") retVoid [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancelRequestWithError:@
cancelRequestWithError :: (IsNSExtensionContext nsExtensionContext, IsNSError error_) => nsExtensionContext -> error_ -> IO ()
cancelRequestWithError nsExtensionContext  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsExtensionContext (mkSelector "cancelRequestWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- openURL:completionHandler:@
openURL_completionHandler :: (IsNSExtensionContext nsExtensionContext, IsNSURL url) => nsExtensionContext -> url -> Ptr () -> IO ()
openURL_completionHandler nsExtensionContext  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsExtensionContext (mkSelector "openURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- inputItems@
inputItems :: IsNSExtensionContext nsExtensionContext => nsExtensionContext -> IO (Id NSArray)
inputItems nsExtensionContext  =
  sendMsg nsExtensionContext (mkSelector "inputItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

-- | @Selector@ for @openURL:completionHandler:@
openURL_completionHandlerSelector :: Selector
openURL_completionHandlerSelector = mkSelector "openURL:completionHandler:"

-- | @Selector@ for @inputItems@
inputItemsSelector :: Selector
inputItemsSelector = mkSelector "inputItems"

