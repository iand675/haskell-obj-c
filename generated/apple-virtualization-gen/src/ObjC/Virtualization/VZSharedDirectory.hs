{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithURL_readOnlySelector
  , newSelector
  , readOnlySelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZSharedDirectory)
new  =
  do
    cls' <- getRequiredClass "VZSharedDirectory"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZSharedDirectory vzSharedDirectory => vzSharedDirectory -> IO (Id VZSharedDirectory)
init_ vzSharedDirectory =
  sendOwnedMessage vzSharedDirectory initSelector

-- | Initialize with a host directory.
--
-- @url@ — Local file URL to expose to the guest.
--
-- @readOnly@ — Whether or not the directory will be exposed as read-only to the guest.
--
-- ObjC selector: @- initWithURL:readOnly:@
initWithURL_readOnly :: (IsVZSharedDirectory vzSharedDirectory, IsNSURL url) => vzSharedDirectory -> url -> Bool -> IO (Id VZSharedDirectory)
initWithURL_readOnly vzSharedDirectory url readOnly =
  sendOwnedMessage vzSharedDirectory initWithURL_readOnlySelector (toNSURL url) readOnly

-- | File URL to a directory on the host to expose to the guest.
--
-- The URL must point to an existing directory path in the host file system.
--
-- ObjC selector: @- URL@
url :: IsVZSharedDirectory vzSharedDirectory => vzSharedDirectory -> IO (Id NSURL)
url vzSharedDirectory =
  sendMessage vzSharedDirectory urlSelector

-- | Whether or not the directory will be exposed as read-only to the guest.
--
-- ObjC selector: @- readOnly@
readOnly :: IsVZSharedDirectory vzSharedDirectory => vzSharedDirectory -> IO Bool
readOnly vzSharedDirectory =
  sendMessage vzSharedDirectory readOnlySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZSharedDirectory)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZSharedDirectory)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:readOnly:@
initWithURL_readOnlySelector :: Selector '[Id NSURL, Bool] (Id VZSharedDirectory)
initWithURL_readOnlySelector = mkSelector "initWithURL:readOnly:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

