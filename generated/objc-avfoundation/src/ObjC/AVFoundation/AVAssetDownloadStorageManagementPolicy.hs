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
  , prioritySelector
  , expirationDateSelector


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
priority :: IsAVAssetDownloadStorageManagementPolicy avAssetDownloadStorageManagementPolicy => avAssetDownloadStorageManagementPolicy -> IO (Id NSString)
priority avAssetDownloadStorageManagementPolicy  =
  sendMsg avAssetDownloadStorageManagementPolicy (mkSelector "priority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the expiration date of asset.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsAVAssetDownloadStorageManagementPolicy avAssetDownloadStorageManagementPolicy => avAssetDownloadStorageManagementPolicy -> IO (Id NSDate)
expirationDate avAssetDownloadStorageManagementPolicy  =
  sendMsg avAssetDownloadStorageManagementPolicy (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

