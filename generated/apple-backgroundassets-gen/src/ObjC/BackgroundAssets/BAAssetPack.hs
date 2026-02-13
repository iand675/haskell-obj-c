{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An archive of assets that the system downloads together.
--
-- An instance of this class can be invalidated when the asset pack that it represents is updated on the server.
--
-- Generated bindings for @BAAssetPack@.
module ObjC.BackgroundAssets.BAAssetPack
  ( BAAssetPack
  , IsBAAssetPack(..)
  , init_
  , new
  , download
  , downloadForContentRequest
  , identifier
  , downloadSize
  , version
  , userInfo
  , downloadForContentRequestSelector
  , downloadSelector
  , downloadSizeSelector
  , identifierSelector
  , initSelector
  , newSelector
  , userInfoSelector
  , versionSelector

  -- * Enum types
  , BAContentRequest(BAContentRequest)
  , pattern BAContentRequestInstall
  , pattern BAContentRequestUpdate
  , pattern BAContentRequestPeriodic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.BackgroundAssets.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id BAAssetPack)
init_ baAssetPack =
  sendOwnedMessage baAssetPack initSelector

-- | @+ new@
new :: IO (Id BAAssetPack)
new  =
  do
    cls' <- getRequiredClass "BAAssetPack"
    sendOwnedClassMessage cls' newSelector

-- | Creates a download object for the asset pack that you schedule using a download manager. - Remark: Use this method in your main app; use ``BAAssetPack/downloadForContentRequest:`` instead in your downloader extension.
--
-- ObjC selector: @- download@
download :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id BADownload)
download baAssetPack =
  sendMessage baAssetPack downloadSelector

-- | Creates a download object for the asset pack that you schedule using a download manager. - Parameter contentRequest: The content request for the current extension invocation. - Returns: A download object. - Remark: Use this method in your downloader extension; use ``BAAssetPack/download`` instead in your main app.
--
-- ObjC selector: @- downloadForContentRequest:@
downloadForContentRequest :: IsBAAssetPack baAssetPack => baAssetPack -> BAContentRequest -> IO (Id BADownload)
downloadForContentRequest baAssetPack contentRequest =
  sendMessage baAssetPack downloadForContentRequestSelector contentRequest

-- | A unique identifier for the asset pack.
--
-- ObjC selector: @- identifier@
identifier :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id NSString)
identifier baAssetPack =
  sendMessage baAssetPack identifierSelector

-- | The size of the download file containing the asset pack in bytes.
--
-- This is different than the installation size, which could be larger.
--
-- ObjC selector: @- downloadSize@
downloadSize :: IsBAAssetPack baAssetPack => baAssetPack -> IO CLong
downloadSize baAssetPack =
  sendMessage baAssetPack downloadSizeSelector

-- | The asset pack’s version number
--
-- ObjC selector: @- version@
version :: IsBAAssetPack baAssetPack => baAssetPack -> IO CLong
version baAssetPack =
  sendMessage baAssetPack versionSelector

-- | JSON-encoded custom information that’s associated with the asset pack.
--
-- This property is @nil@ for Apple-hosted asset packs.
--
-- ObjC selector: @- userInfo@
userInfo :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id NSData)
userInfo baAssetPack =
  sendMessage baAssetPack userInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BAAssetPack)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BAAssetPack)
newSelector = mkSelector "new"

-- | @Selector@ for @download@
downloadSelector :: Selector '[] (Id BADownload)
downloadSelector = mkSelector "download"

-- | @Selector@ for @downloadForContentRequest:@
downloadForContentRequestSelector :: Selector '[BAContentRequest] (Id BADownload)
downloadForContentRequestSelector = mkSelector "downloadForContentRequest:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @downloadSize@
downloadSizeSelector :: Selector '[] CLong
downloadSizeSelector = mkSelector "downloadSize"

-- | @Selector@ for @version@
versionSelector :: Selector '[] CLong
versionSelector = mkSelector "version"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSData)
userInfoSelector = mkSelector "userInfo"

