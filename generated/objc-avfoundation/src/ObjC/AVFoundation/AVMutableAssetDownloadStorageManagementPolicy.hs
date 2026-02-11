{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A mutable subclass of AVAssetDownloadStorageManagementPolicy.
--
-- System will put in best-effort to evict all the assets based on expirationDate before evicting based on priority.
--
-- Generated bindings for @AVMutableAssetDownloadStorageManagementPolicy@.
module ObjC.AVFoundation.AVMutableAssetDownloadStorageManagementPolicy
  ( AVMutableAssetDownloadStorageManagementPolicy
  , IsAVMutableAssetDownloadStorageManagementPolicy(..)
  , priority
  , setPriority
  , expirationDate
  , setExpirationDate
  , prioritySelector
  , setPrioritySelector
  , expirationDateSelector
  , setExpirationDateSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Indicates the eviction priority of downloaded asset.
--
-- Assets with default priority will be purged first before assets with higher priorities. In case this is not set, default priority is used.
--
-- ObjC selector: @- priority@
priority :: IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy => avMutableAssetDownloadStorageManagementPolicy -> IO (Id NSString)
priority avMutableAssetDownloadStorageManagementPolicy  =
  sendMsg avMutableAssetDownloadStorageManagementPolicy (mkSelector "priority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the eviction priority of downloaded asset.
--
-- Assets with default priority will be purged first before assets with higher priorities. In case this is not set, default priority is used.
--
-- ObjC selector: @- setPriority:@
setPriority :: (IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy, IsNSString value) => avMutableAssetDownloadStorageManagementPolicy -> value -> IO ()
setPriority avMutableAssetDownloadStorageManagementPolicy  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableAssetDownloadStorageManagementPolicy (mkSelector "setPriority:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Returns the expiration date of asset.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy => avMutableAssetDownloadStorageManagementPolicy -> IO (Id NSDate)
expirationDate avMutableAssetDownloadStorageManagementPolicy  =
  sendMsg avMutableAssetDownloadStorageManagementPolicy (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the expiration date of asset.
--
-- ObjC selector: @- setExpirationDate:@
setExpirationDate :: (IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy, IsNSDate value) => avMutableAssetDownloadStorageManagementPolicy -> value -> IO ()
setExpirationDate avMutableAssetDownloadStorageManagementPolicy  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableAssetDownloadStorageManagementPolicy (mkSelector "setExpirationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector
setExpirationDateSelector = mkSelector "setExpirationDate:"

