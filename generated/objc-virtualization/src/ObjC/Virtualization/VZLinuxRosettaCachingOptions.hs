{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a VZLinuxRosettaCachingOptions.
--
-- VZLinuxRosettaCachingOptions define the communication mechanism between the Rosetta daemon and the Rosetta runtime.
--
-- VZLinuxRosettaCachingOptions should not be instantiated directly.    One of its subclasses like VZLinuxRosettaUnixSocketCachingOptions or VZLinuxRosettaAbstractCachingOptions should be used instead.
--
-- See: VZLinuxRosettaUnixSocketCachingOptions
--
-- See: VZLinuxRosettaAbstractCachingOptions
--
-- Generated bindings for @VZLinuxRosettaCachingOptions@.
module ObjC.Virtualization.VZLinuxRosettaCachingOptions
  ( VZLinuxRosettaCachingOptions
  , IsVZLinuxRosettaCachingOptions(..)
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
new :: IO (Id VZLinuxRosettaCachingOptions)
new  =
  do
    cls' <- getRequiredClass "VZLinuxRosettaCachingOptions"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZLinuxRosettaCachingOptions vzLinuxRosettaCachingOptions => vzLinuxRosettaCachingOptions -> IO (Id VZLinuxRosettaCachingOptions)
init_ vzLinuxRosettaCachingOptions  =
  sendMsg vzLinuxRosettaCachingOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

