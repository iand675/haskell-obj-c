{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine state was updated, and you should persist it locally.
--
-- In order to function properly and efficiently, @CKSyncEngine@ tracks some state internally. When the sync engine state changes, it will give you the latest serialized version in a ``CKSyncEngine/Event/StateUpdate``. This event will happen occasionally when the sync engine modifies the state internally during normal sync operation. This event will also happen when you change the state yourself.
--
-- The sync engine does not persist this state to disk, so you need to persist it in alongside your own local data. The next time your process launches, use this latest state serialization in ``CKSyncEngineConfiguration/stateSerialization`` to initialize your sync engine.
--
-- This state is directly tied to the changes you fetch and send with the sync engine. You should ensure that any changes fetched prior to receiving this state are also persisted alongside this state.
--
-- Generated bindings for @CKSyncEngineStateUpdateEvent@.
module ObjC.CloudKit.CKSyncEngineStateUpdateEvent
  ( CKSyncEngineStateUpdateEvent
  , IsCKSyncEngineStateUpdateEvent(..)
  , stateSerialization
  , stateSerializationSelector


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

-- | @- stateSerialization@
stateSerialization :: IsCKSyncEngineStateUpdateEvent ckSyncEngineStateUpdateEvent => ckSyncEngineStateUpdateEvent -> IO (Id CKSyncEngineStateSerialization)
stateSerialization ckSyncEngineStateUpdateEvent  =
  sendMsg ckSyncEngineStateUpdateEvent (mkSelector "stateSerialization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateSerialization@
stateSerializationSelector :: Selector
stateSerializationSelector = mkSelector "stateSerialization"

