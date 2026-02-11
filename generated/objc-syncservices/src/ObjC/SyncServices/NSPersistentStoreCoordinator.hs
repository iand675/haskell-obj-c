{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreCoordinator@.
module ObjC.SyncServices.NSPersistentStoreCoordinator
  ( NSPersistentStoreCoordinator
  , IsNSPersistentStoreCoordinator(..)
  , syncWithClient_inBackground_handler_error
  , syncWithClient_inBackground_handler_errorSelector


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

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- syncWithClient:inBackground:handler:error:@
syncWithClient_inBackground_handler_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsISyncClient client, IsNSError rError) => nsPersistentStoreCoordinator -> client -> Bool -> RawId -> rError -> IO Bool
syncWithClient_inBackground_handler_error nsPersistentStoreCoordinator  client flag syncHandler rError =
withObjCPtr client $ \raw_client ->
  withObjCPtr rError $ \raw_rError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "syncWithClient:inBackground:handler:error:") retCULong [argPtr (castPtr raw_client :: Ptr ()), argCULong (if flag then 1 else 0), argPtr (castPtr (unRawId syncHandler) :: Ptr ()), argPtr (castPtr raw_rError :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @syncWithClient:inBackground:handler:error:@
syncWithClient_inBackground_handler_errorSelector :: Selector
syncWithClient_inBackground_handler_errorSelector = mkSelector "syncWithClient:inBackground:handler:error:"

