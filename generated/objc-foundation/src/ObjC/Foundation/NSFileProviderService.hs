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

-- | @- getFileProviderConnectionWithCompletionHandler:@
getFileProviderConnectionWithCompletionHandler :: IsNSFileProviderService nsFileProviderService => nsFileProviderService -> Ptr () -> IO ()
getFileProviderConnectionWithCompletionHandler nsFileProviderService  completionHandler =
  sendMsg nsFileProviderService (mkSelector "getFileProviderConnectionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- name@
name :: IsNSFileProviderService nsFileProviderService => nsFileProviderService -> IO (Id NSString)
name nsFileProviderService  =
  sendMsg nsFileProviderService (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getFileProviderConnectionWithCompletionHandler:@
getFileProviderConnectionWithCompletionHandlerSelector :: Selector
getFileProviderConnectionWithCompletionHandlerSelector = mkSelector "getFileProviderConnectionWithCompletionHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

