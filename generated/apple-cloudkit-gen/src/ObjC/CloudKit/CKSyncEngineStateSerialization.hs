{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A serialized representation of a ``CKSyncEngineState``.
--
-- This will be passed to your delegate via ``CKSyncEngine/Event/StateUpdate``. You should use @NSSecureCoding@ to persist this locally alongside your other data and use it the next time you initialize your sync engine.
--
-- Generated bindings for @CKSyncEngineStateSerialization@.
module ObjC.CloudKit.CKSyncEngineStateSerialization
  ( CKSyncEngineStateSerialization
  , IsCKSyncEngineStateSerialization(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineStateSerialization ckSyncEngineStateSerialization => ckSyncEngineStateSerialization -> IO (Id CKSyncEngineStateSerialization)
init_ ckSyncEngineStateSerialization =
  sendOwnedMessage ckSyncEngineStateSerialization initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineStateSerialization)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineStateSerialization"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineStateSerialization)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineStateSerialization)
newSelector = mkSelector "new"

