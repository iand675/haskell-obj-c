{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a graphics display configuration.
--
-- VZGraphicsDisplayConfiguration should not be instantiated directly.    One of its subclasses should be used instead.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZVirtioGraphicsScanoutConfiguration
--
-- Generated bindings for @VZGraphicsDisplayConfiguration@.
module ObjC.Virtualization.VZGraphicsDisplayConfiguration
  ( VZGraphicsDisplayConfiguration
  , IsVZGraphicsDisplayConfiguration(..)
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
new :: IO (Id VZGraphicsDisplayConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDisplayConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZGraphicsDisplayConfiguration vzGraphicsDisplayConfiguration => vzGraphicsDisplayConfiguration -> IO (Id VZGraphicsDisplayConfiguration)
init_ vzGraphicsDisplayConfiguration  =
  sendMsg vzGraphicsDisplayConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

