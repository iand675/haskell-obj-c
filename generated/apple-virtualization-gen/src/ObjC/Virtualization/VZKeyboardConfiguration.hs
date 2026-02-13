{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a keyboard configuration.
--
-- VZKeyboardConfiguration should not be instantiated directly.    One of its subclasses like VZUSBKeyboardConfiguration or VZMacKeyboardConfiguration should be used instead.
--
-- See: VZUSBKeyboardConfiguration
--
-- See: VZMacKeyboardConfiguration
--
-- Generated bindings for @VZKeyboardConfiguration@.
module ObjC.Virtualization.VZKeyboardConfiguration
  ( VZKeyboardConfiguration
  , IsVZKeyboardConfiguration(..)
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
new :: IO (Id VZKeyboardConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZKeyboardConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZKeyboardConfiguration vzKeyboardConfiguration => vzKeyboardConfiguration -> IO (Id VZKeyboardConfiguration)
init_ vzKeyboardConfiguration =
  sendOwnedMessage vzKeyboardConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZKeyboardConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZKeyboardConfiguration)
initSelector = mkSelector "init"

