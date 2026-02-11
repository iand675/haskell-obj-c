{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A directory on the host that can be exposed to a guest.
--
-- Generated bindings for @VZSharedDirectory@.
module ObjC.Virtualization.VZSharedDirectory
  ( VZSharedDirectory
  , IsVZSharedDirectory(..)
  , new
  , init_
  , initWithURL_readOnly
  , url
  , readOnly
  , newSelector
  , initSelector
  , initWithURL_readOnlySelector
  , urlSelector
  , readOnlySelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZSharedDirectory)
new  =
  do
    cls' <- getRequiredClass "VZSharedDirectory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZSharedDirectory vzSharedDirectory => vzSharedDirectory -> IO (Id VZSharedDirectory)
init_ vzSharedDirectory  =
  sendMsg vzSharedDirectory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize with a host directory.
--
-- @url@ — Local file URL to expose to the guest.
--
-- @readOnly@ — Whether or not the directory will be exposed as read-only to the guest.
--
-- ObjC selector: @- initWithURL:readOnly:@
initWithURL_readOnly :: (IsVZSharedDirectory vzSharedDirectory, IsNSURL url) => vzSharedDirectory -> url -> Bool -> IO (Id VZSharedDirectory)
initWithURL_readOnly vzSharedDirectory  url readOnly =
withObjCPtr url $ \raw_url ->
    sendMsg vzSharedDirectory (mkSelector "initWithURL:readOnly:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if readOnly then 1 else 0)] >>= ownedObject . castPtr

-- | File URL to a directory on the host to expose to the guest.
--
-- The URL must point to an existing directory path in the host file system.
--
-- ObjC selector: @- URL@
url :: IsVZSharedDirectory vzSharedDirectory => vzSharedDirectory -> IO (Id NSURL)
url vzSharedDirectory  =
  sendMsg vzSharedDirectory (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether or not the directory will be exposed as read-only to the guest.
--
-- ObjC selector: @- readOnly@
readOnly :: IsVZSharedDirectory vzSharedDirectory => vzSharedDirectory -> IO Bool
readOnly vzSharedDirectory  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzSharedDirectory (mkSelector "readOnly") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:readOnly:@
initWithURL_readOnlySelector :: Selector
initWithURL_readOnlySelector = mkSelector "initWithURL:readOnly:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

