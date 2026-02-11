{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , downloadSelector
  , downloadForContentRequestSelector
  , identifierSelector
  , downloadSizeSelector
  , versionSelector
  , userInfoSelector

  -- * Enum types
  , BAContentRequest(BAContentRequest)
  , pattern BAContentRequestInstall
  , pattern BAContentRequestUpdate
  , pattern BAContentRequestPeriodic

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

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.BackgroundAssets.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id BAAssetPack)
init_ baAssetPack  =
    sendMsg baAssetPack (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BAAssetPack)
new  =
  do
    cls' <- getRequiredClass "BAAssetPack"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a download object for the asset pack that you schedule using a download manager. - Remark: Use this method in your main app; use ``BAAssetPack/downloadForContentRequest:`` instead in your downloader extension.
--
-- ObjC selector: @- download@
download :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id BADownload)
download baAssetPack  =
    sendMsg baAssetPack (mkSelector "download") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates a download object for the asset pack that you schedule using a download manager. - Parameter contentRequest: The content request for the current extension invocation. - Returns: A download object. - Remark: Use this method in your downloader extension; use ``BAAssetPack/download`` instead in your main app.
--
-- ObjC selector: @- downloadForContentRequest:@
downloadForContentRequest :: IsBAAssetPack baAssetPack => baAssetPack -> BAContentRequest -> IO (Id BADownload)
downloadForContentRequest baAssetPack  contentRequest =
    sendMsg baAssetPack (mkSelector "downloadForContentRequest:") (retPtr retVoid) [argCLong (coerce contentRequest)] >>= retainedObject . castPtr

-- | A unique identifier for the asset pack.
--
-- ObjC selector: @- identifier@
identifier :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id NSString)
identifier baAssetPack  =
    sendMsg baAssetPack (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The size of the download file containing the asset pack in bytes.
--
-- This is different than the installation size, which could be larger.
--
-- ObjC selector: @- downloadSize@
downloadSize :: IsBAAssetPack baAssetPack => baAssetPack -> IO CLong
downloadSize baAssetPack  =
    sendMsg baAssetPack (mkSelector "downloadSize") retCLong []

-- | The asset pack’s version number
--
-- ObjC selector: @- version@
version :: IsBAAssetPack baAssetPack => baAssetPack -> IO CLong
version baAssetPack  =
    sendMsg baAssetPack (mkSelector "version") retCLong []

-- | JSON-encoded custom information that’s associated with the asset pack.
--
-- This property is @nil@ for Apple-hosted asset packs.
--
-- ObjC selector: @- userInfo@
userInfo :: IsBAAssetPack baAssetPack => baAssetPack -> IO (Id NSData)
userInfo baAssetPack  =
    sendMsg baAssetPack (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @download@
downloadSelector :: Selector
downloadSelector = mkSelector "download"

-- | @Selector@ for @downloadForContentRequest:@
downloadForContentRequestSelector :: Selector
downloadForContentRequestSelector = mkSelector "downloadForContentRequest:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @downloadSize@
downloadSizeSelector :: Selector
downloadSizeSelector = mkSelector "downloadSize"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

