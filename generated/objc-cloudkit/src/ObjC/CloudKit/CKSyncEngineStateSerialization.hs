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

-- | @- init@
init_ :: IsCKSyncEngineStateSerialization ckSyncEngineStateSerialization => ckSyncEngineStateSerialization -> IO (Id CKSyncEngineStateSerialization)
init_ ckSyncEngineStateSerialization  =
  sendMsg ckSyncEngineStateSerialization (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineStateSerialization)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineStateSerialization"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

