{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a color used by a GCDeviceLight.
--
-- See: GCDeviceLight
--
-- Generated bindings for @GCColor@.
module ObjC.GameController.GCColor
  ( GCColor
  , IsGCColor(..)
  , init_
  , initWithRed_green_blue
  , red
  , green
  , blue
  , blueSelector
  , greenSelector
  , initSelector
  , initWithRed_green_blueSelector
  , redSelector


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
init_ :: IsGCColor gcColor => gcColor -> IO (Id GCColor)
init_ gcColor =
  sendOwnedMessage gcColor initSelector

-- | @- initWithRed:green:blue:@
initWithRed_green_blue :: IsGCColor gcColor => gcColor -> CFloat -> CFloat -> CFloat -> IO (Id GCColor)
initWithRed_green_blue gcColor red green blue =
  sendOwnedMessage gcColor initWithRed_green_blueSelector red green blue

-- | @- red@
red :: IsGCColor gcColor => gcColor -> IO CFloat
red gcColor =
  sendMessage gcColor redSelector

-- | @- green@
green :: IsGCColor gcColor => gcColor -> IO CFloat
green gcColor =
  sendMessage gcColor greenSelector

-- | @- blue@
blue :: IsGCColor gcColor => gcColor -> IO CFloat
blue gcColor =
  sendMessage gcColor blueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCColor)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRed:green:blue:@
initWithRed_green_blueSelector :: Selector '[CFloat, CFloat, CFloat] (Id GCColor)
initWithRed_green_blueSelector = mkSelector "initWithRed:green:blue:"

-- | @Selector@ for @red@
redSelector :: Selector '[] CFloat
redSelector = mkSelector "red"

-- | @Selector@ for @green@
greenSelector :: Selector '[] CFloat
greenSelector = mkSelector "green"

-- | @Selector@ for @blue@
blueSelector :: Selector '[] CFloat
blueSelector = mkSelector "blue"

