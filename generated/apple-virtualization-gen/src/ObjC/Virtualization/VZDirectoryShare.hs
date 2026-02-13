{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector


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
new :: IO (Id VZDirectoryShare)
new  =
  do
    cls' <- getRequiredClass "VZDirectoryShare"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZDirectoryShare vzDirectoryShare => vzDirectoryShare -> IO (Id VZDirectoryShare)
init_ vzDirectoryShare =
  sendOwnedMessage vzDirectoryShare initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZDirectoryShare)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZDirectoryShare)
initSelector = mkSelector "init"

