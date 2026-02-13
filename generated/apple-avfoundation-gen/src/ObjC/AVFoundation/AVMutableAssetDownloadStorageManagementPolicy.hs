{-# LANGUAGE DataKinds #-}
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
  , expirationDateSelector
  , prioritySelector
  , setExpirationDateSelector
  , setPrioritySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
priority avMutableAssetDownloadStorageManagementPolicy =
  sendMessage avMutableAssetDownloadStorageManagementPolicy prioritySelector

-- | Indicates the eviction priority of downloaded asset.
--
-- Assets with default priority will be purged first before assets with higher priorities. In case this is not set, default priority is used.
--
-- ObjC selector: @- setPriority:@
setPriority :: (IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy, IsNSString value) => avMutableAssetDownloadStorageManagementPolicy -> value -> IO ()
setPriority avMutableAssetDownloadStorageManagementPolicy value =
  sendMessage avMutableAssetDownloadStorageManagementPolicy setPrioritySelector (toNSString value)

-- | Returns the expiration date of asset.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy => avMutableAssetDownloadStorageManagementPolicy -> IO (Id NSDate)
expirationDate avMutableAssetDownloadStorageManagementPolicy =
  sendMessage avMutableAssetDownloadStorageManagementPolicy expirationDateSelector

-- | Returns the expiration date of asset.
--
-- ObjC selector: @- setExpirationDate:@
setExpirationDate :: (IsAVMutableAssetDownloadStorageManagementPolicy avMutableAssetDownloadStorageManagementPolicy, IsNSDate value) => avMutableAssetDownloadStorageManagementPolicy -> value -> IO ()
setExpirationDate avMutableAssetDownloadStorageManagementPolicy value =
  sendMessage avMutableAssetDownloadStorageManagementPolicy setExpirationDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] (Id NSString)
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[Id NSString] ()
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector '[Id NSDate] ()
setExpirationDateSelector = mkSelector "setExpirationDate:"

