{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileProviderService@.
module ObjC.Foundation.NSFileProviderService
  ( NSFileProviderService
  , IsNSFileProviderService(..)
  , getFileProviderConnectionWithCompletionHandler
  , name
  , getFileProviderConnectionWithCompletionHandlerSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- getFileProviderConnectionWithCompletionHandler:@
getFileProviderConnectionWithCompletionHandler :: IsNSFileProviderService nsFileProviderService => nsFileProviderService -> Ptr () -> IO ()
getFileProviderConnectionWithCompletionHandler nsFileProviderService completionHandler =
  sendMessage nsFileProviderService getFileProviderConnectionWithCompletionHandlerSelector completionHandler

-- | @- name@
name :: IsNSFileProviderService nsFileProviderService => nsFileProviderService -> IO (Id NSString)
name nsFileProviderService =
  sendMessage nsFileProviderService nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getFileProviderConnectionWithCompletionHandler:@
getFileProviderConnectionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getFileProviderConnectionWithCompletionHandlerSelector = mkSelector "getFileProviderConnectionWithCompletionHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

