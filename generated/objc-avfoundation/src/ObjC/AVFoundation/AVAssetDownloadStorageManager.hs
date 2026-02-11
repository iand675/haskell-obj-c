{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVAssetDownloadStorageManager manages the policy for automatic purging of downloaded AVAssets. The policy is vended as AVAssetDownloadStorageManagementPolicy object.
--
-- When a storage management policy needs to be set on an asset, sharedDownloadStorageManager singleton needs to be fetched.  The new policy can then be set by using setStorageManagementPolicy and the location of the downloaded asset.
--
-- Generated bindings for @AVAssetDownloadStorageManager@.
module ObjC.AVFoundation.AVAssetDownloadStorageManager
  ( AVAssetDownloadStorageManager
  , IsAVAssetDownloadStorageManager(..)
  , sharedDownloadStorageManager
  , setStorageManagementPolicy_forURL
  , storageManagementPolicyForURL
  , sharedDownloadStorageManagerSelector
  , setStorageManagementPolicy_forURLSelector
  , storageManagementPolicyForURLSelector


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

-- | returns singleton instance.
--
-- ObjC selector: @+ sharedDownloadStorageManager@
sharedDownloadStorageManager :: IO (Id AVAssetDownloadStorageManager)
sharedDownloadStorageManager  =
  do
    cls' <- getRequiredClass "AVAssetDownloadStorageManager"
    sendClassMsg cls' (mkSelector "sharedDownloadStorageManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the policy for asset with disk backing at downloadStorageURL.
--
-- - Parameter downloadStorageURL: The location of downloaded asset.
--
-- ObjC selector: @- setStorageManagementPolicy:forURL:@
setStorageManagementPolicy_forURL :: (IsAVAssetDownloadStorageManager avAssetDownloadStorageManager, IsAVAssetDownloadStorageManagementPolicy storageManagementPolicy, IsNSURL downloadStorageURL) => avAssetDownloadStorageManager -> storageManagementPolicy -> downloadStorageURL -> IO ()
setStorageManagementPolicy_forURL avAssetDownloadStorageManager  storageManagementPolicy downloadStorageURL =
withObjCPtr storageManagementPolicy $ \raw_storageManagementPolicy ->
  withObjCPtr downloadStorageURL $ \raw_downloadStorageURL ->
      sendMsg avAssetDownloadStorageManager (mkSelector "setStorageManagementPolicy:forURL:") retVoid [argPtr (castPtr raw_storageManagementPolicy :: Ptr ()), argPtr (castPtr raw_downloadStorageURL :: Ptr ())]

-- | Returns the storage management policy for asset downloaded at downloadStorageURL. This may be nil if a storageManagementPolicy was never set on the downloaded asset.
--
-- - Parameter downloadStorageURL: The location of downloaded asset.
--
-- ObjC selector: @- storageManagementPolicyForURL:@
storageManagementPolicyForURL :: (IsAVAssetDownloadStorageManager avAssetDownloadStorageManager, IsNSURL downloadStorageURL) => avAssetDownloadStorageManager -> downloadStorageURL -> IO (Id AVAssetDownloadStorageManagementPolicy)
storageManagementPolicyForURL avAssetDownloadStorageManager  downloadStorageURL =
withObjCPtr downloadStorageURL $ \raw_downloadStorageURL ->
    sendMsg avAssetDownloadStorageManager (mkSelector "storageManagementPolicyForURL:") (retPtr retVoid) [argPtr (castPtr raw_downloadStorageURL :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedDownloadStorageManager@
sharedDownloadStorageManagerSelector :: Selector
sharedDownloadStorageManagerSelector = mkSelector "sharedDownloadStorageManager"

-- | @Selector@ for @setStorageManagementPolicy:forURL:@
setStorageManagementPolicy_forURLSelector :: Selector
setStorageManagementPolicy_forURLSelector = mkSelector "setStorageManagementPolicy:forURL:"

-- | @Selector@ for @storageManagementPolicyForURL:@
storageManagementPolicyForURLSelector :: Selector
storageManagementPolicyForURLSelector = mkSelector "storageManagementPolicyForURL:"

