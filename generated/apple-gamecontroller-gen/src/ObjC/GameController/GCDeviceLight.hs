{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A controller light is an abstract representation of the light-emitting capabilities of a GCController instance.
--
-- Generated bindings for @GCDeviceLight@.
module ObjC.GameController.GCDeviceLight
  ( GCDeviceLight
  , IsGCDeviceLight(..)
  , init_
  , color
  , setColor
  , colorSelector
  , initSelector
  , setColorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCDeviceLight gcDeviceLight => gcDeviceLight -> IO (Id GCDeviceLight)
init_ gcDeviceLight =
  sendOwnedMessage gcDeviceLight initSelector

-- | @- color@
color :: IsGCDeviceLight gcDeviceLight => gcDeviceLight -> IO (Id GCColor)
color gcDeviceLight =
  sendMessage gcDeviceLight colorSelector

-- | @- setColor:@
setColor :: (IsGCDeviceLight gcDeviceLight, IsGCColor value) => gcDeviceLight -> value -> IO ()
setColor gcDeviceLight value =
  sendMessage gcDeviceLight setColorSelector (toGCColor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCDeviceLight)
initSelector = mkSelector "init"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id GCColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id GCColor] ()
setColorSelector = mkSelector "setColor:"

