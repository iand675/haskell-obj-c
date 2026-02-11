{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreRequest@.
module ObjC.CoreData.NSPersistentStoreRequest
  ( NSPersistentStoreRequest
  , IsNSPersistentStoreRequest(..)
  , affectedStores
  , setAffectedStores
  , requestType
  , affectedStoresSelector
  , setAffectedStoresSelector
  , requestTypeSelector

  -- * Enum types
  , NSPersistentStoreRequestType(NSPersistentStoreRequestType)
  , pattern NSFetchRequestType
  , pattern NSSaveRequestType
  , pattern NSBatchInsertRequestType
  , pattern NSBatchUpdateRequestType
  , pattern NSBatchDeleteRequestType

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- affectedStores@
affectedStores :: IsNSPersistentStoreRequest nsPersistentStoreRequest => nsPersistentStoreRequest -> IO (Id NSArray)
affectedStores nsPersistentStoreRequest  =
  sendMsg nsPersistentStoreRequest (mkSelector "affectedStores") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAffectedStores:@
setAffectedStores :: (IsNSPersistentStoreRequest nsPersistentStoreRequest, IsNSArray value) => nsPersistentStoreRequest -> value -> IO ()
setAffectedStores nsPersistentStoreRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStoreRequest (mkSelector "setAffectedStores:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requestType@
requestType :: IsNSPersistentStoreRequest nsPersistentStoreRequest => nsPersistentStoreRequest -> IO NSPersistentStoreRequestType
requestType nsPersistentStoreRequest  =
  fmap (coerce :: CULong -> NSPersistentStoreRequestType) $ sendMsg nsPersistentStoreRequest (mkSelector "requestType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @affectedStores@
affectedStoresSelector :: Selector
affectedStoresSelector = mkSelector "affectedStores"

-- | @Selector@ for @setAffectedStores:@
setAffectedStoresSelector :: Selector
setAffectedStoresSelector = mkSelector "setAffectedStores:"

-- | @Selector@ for @requestType@
requestTypeSelector :: Selector
requestTypeSelector = mkSelector "requestType"

