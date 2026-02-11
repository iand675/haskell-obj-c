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
new :: IO (Id VZKeyboardConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZKeyboardConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZKeyboardConfiguration vzKeyboardConfiguration => vzKeyboardConfiguration -> IO (Id VZKeyboardConfiguration)
init_ vzKeyboardConfiguration  =
  sendMsg vzKeyboardConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

