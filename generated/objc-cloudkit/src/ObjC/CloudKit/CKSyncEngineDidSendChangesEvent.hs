{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished sending changes to the server.
--
-- You should receive one @CKSyncEngineDidSendChangesEvent@ for every @CKSyncEngineWillSendChangesEvent@.
--
-- Generated bindings for @CKSyncEngineDidSendChangesEvent@.
module ObjC.CloudKit.CKSyncEngineDidSendChangesEvent
  ( CKSyncEngineDidSendChangesEvent
  , IsCKSyncEngineDidSendChangesEvent(..)
  , context
  , contextSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- context@
context :: IsCKSyncEngineDidSendChangesEvent ckSyncEngineDidSendChangesEvent => ckSyncEngineDidSendChangesEvent -> IO (Id CKSyncEngineSendChangesContext)
context ckSyncEngineDidSendChangesEvent  =
  sendMsg ckSyncEngineDidSendChangesEvent (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

