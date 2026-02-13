{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VZGraphicsDeviceConfiguration@.
module ObjC.Virtualization.VZGraphicsDeviceConfiguration
  ( VZGraphicsDeviceConfiguration
  , IsVZGraphicsDeviceConfiguration(..)
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
new :: IO (Id VZGraphicsDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZGraphicsDeviceConfiguration vzGraphicsDeviceConfiguration => vzGraphicsDeviceConfiguration -> IO (Id VZGraphicsDeviceConfiguration)
init_ vzGraphicsDeviceConfiguration =
  sendOwnedMessage vzGraphicsDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZGraphicsDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZGraphicsDeviceConfiguration)
initSelector = mkSelector "init"

