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
  , initSelector
  , initWithRed_green_blueSelector
  , redSelector
  , greenSelector
  , blueSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCColor gcColor => gcColor -> IO (Id GCColor)
init_ gcColor  =
  sendMsg gcColor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRed:green:blue:@
initWithRed_green_blue :: IsGCColor gcColor => gcColor -> CFloat -> CFloat -> CFloat -> IO (Id GCColor)
initWithRed_green_blue gcColor  red green blue =
  sendMsg gcColor (mkSelector "initWithRed:green:blue:") (retPtr retVoid) [argCFloat (fromIntegral red), argCFloat (fromIntegral green), argCFloat (fromIntegral blue)] >>= ownedObject . castPtr

-- | @- red@
red :: IsGCColor gcColor => gcColor -> IO CFloat
red gcColor  =
  sendMsg gcColor (mkSelector "red") retCFloat []

-- | @- green@
green :: IsGCColor gcColor => gcColor -> IO CFloat
green gcColor  =
  sendMsg gcColor (mkSelector "green") retCFloat []

-- | @- blue@
blue :: IsGCColor gcColor => gcColor -> IO CFloat
blue gcColor  =
  sendMsg gcColor (mkSelector "blue") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRed:green:blue:@
initWithRed_green_blueSelector :: Selector
initWithRed_green_blueSelector = mkSelector "initWithRed:green:blue:"

-- | @Selector@ for @red@
redSelector :: Selector
redSelector = mkSelector "red"

-- | @Selector@ for @green@
greenSelector :: Selector
greenSelector = mkSelector "green"

-- | @Selector@ for @blue@
blueSelector :: Selector
blueSelector = mkSelector "blue"

