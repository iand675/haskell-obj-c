{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a directory share.
--
-- A directory share defines how host directories get exposed to a virtual machine guest.
--
-- VZDirectoryShare should not be instantiated directly.    One of its subclasses like VZSingleDirectoryShare or VZMultipleDirectoryShare should be used instead.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- Generated bindings for @VZDirectoryShare@.
module ObjC.Virtualization.VZDirectoryShare
  ( VZDirectoryShare
  , IsVZDirectoryShare(..)
  , new
  , init_
  , newSelector
  , initSelector


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
new :: IO (Id VZDirectoryShare)
new  =
  do
    cls' <- getRequiredClass "VZDirectoryShare"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZDirectoryShare vzDirectoryShare => vzDirectoryShare -> IO (Id VZDirectoryShare)
init_ vzDirectoryShare  =
  sendMsg vzDirectoryShare (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

