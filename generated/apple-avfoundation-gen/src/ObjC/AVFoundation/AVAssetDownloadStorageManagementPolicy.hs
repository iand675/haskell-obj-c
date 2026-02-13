{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class to inform the system of a policy for automatic purging of downloaded AVAssets.
--
-- System will put in best-effort to evict all the assets based on expirationDate before evicting based on priority.
--
-- Generated bindings for @AVAssetDownloadStorageManagementPolicy@.
module ObjC.AVFoundation.AVAssetDownloadStorageManagementPolicy
  ( AVAssetDownloadStorageManagementPolicy
  , IsAVAssetDownloadStorageManagementPolicy(..)
  , priority
  , expirationDate
  , expirationDateSelector
  , prioritySelector


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
priority :: IsAVAssetDownloadStorageManagementPolicy avAssetDownloadStorageManagementPolicy => avAssetDownloadStorageManagementPolicy -> IO (Id NSString)
priority avAssetDownloadStorageManagementPolicy =
  sendMessage avAssetDownloadStorageManagementPolicy prioritySelector

-- | Returns the expiration date of asset.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsAVAssetDownloadStorageManagementPolicy avAssetDownloadStorageManagementPolicy => avAssetDownloadStorageManagementPolicy -> IO (Id NSDate)
expirationDate avAssetDownloadStorageManagementPolicy =
  sendMessage avAssetDownloadStorageManagementPolicy expirationDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] (Id NSString)
prioritySelector = mkSelector "priority"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

