{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished fetching changes from the server.
--
-- This might be a good signal to perform any post-processing tasks required after persisting fetched changes to disk.
--
-- You should receive one @CKSyncEngineDidFetchChangesEvent@ for each @CKSyncEngineWillFetchChangesEvent@.
--
-- Generated bindings for @CKSyncEngineDidFetchChangesEvent@.
module ObjC.CloudKit.CKSyncEngineDidFetchChangesEvent
  ( CKSyncEngineDidFetchChangesEvent
  , IsCKSyncEngineDidFetchChangesEvent(..)
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
context :: IsCKSyncEngineDidFetchChangesEvent ckSyncEngineDidFetchChangesEvent => ckSyncEngineDidFetchChangesEvent -> IO (Id CKSyncEngineFetchChangesContext)
context ckSyncEngineDidFetchChangesEvent  =
  sendMsg ckSyncEngineDidFetchChangesEvent (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

