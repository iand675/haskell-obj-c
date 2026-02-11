{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Produces 3D spherical noise with an infinite number of spheres-within-spheres of constantly-increasing radius.
--
-- Generated bindings for @GKSpheresNoiseSource@.
module ObjC.GameplayKit.GKSpheresNoiseSource
  ( GKSpheresNoiseSource
  , IsGKSpheresNoiseSource(..)
  , spheresNoiseWithFrequency
  , initWithFrequency
  , frequency
  , setFrequency
  , spheresNoiseWithFrequencySelector
  , initWithFrequencySelector
  , frequencySelector
  , setFrequencySelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ spheresNoiseWithFrequency:@
spheresNoiseWithFrequency :: CDouble -> IO (Id GKSpheresNoiseSource)
spheresNoiseWithFrequency frequency =
  do
    cls' <- getRequiredClass "GKSpheresNoiseSource"
    sendClassMsg cls' (mkSelector "spheresNoiseWithFrequency:") (retPtr retVoid) [argCDouble (fromIntegral frequency)] >>= retainedObject . castPtr

-- | @- initWithFrequency:@
initWithFrequency :: IsGKSpheresNoiseSource gkSpheresNoiseSource => gkSpheresNoiseSource -> CDouble -> IO (Id GKSpheresNoiseSource)
initWithFrequency gkSpheresNoiseSource  frequency =
  sendMsg gkSpheresNoiseSource (mkSelector "initWithFrequency:") (retPtr retVoid) [argCDouble (fromIntegral frequency)] >>= ownedObject . castPtr

-- | @- frequency@
frequency :: IsGKSpheresNoiseSource gkSpheresNoiseSource => gkSpheresNoiseSource -> IO CDouble
frequency gkSpheresNoiseSource  =
  sendMsg gkSpheresNoiseSource (mkSelector "frequency") retCDouble []

-- | @- setFrequency:@
setFrequency :: IsGKSpheresNoiseSource gkSpheresNoiseSource => gkSpheresNoiseSource -> CDouble -> IO ()
setFrequency gkSpheresNoiseSource  value =
  sendMsg gkSpheresNoiseSource (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @spheresNoiseWithFrequency:@
spheresNoiseWithFrequencySelector :: Selector
spheresNoiseWithFrequencySelector = mkSelector "spheresNoiseWithFrequency:"

-- | @Selector@ for @initWithFrequency:@
initWithFrequencySelector :: Selector
initWithFrequencySelector = mkSelector "initWithFrequency:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

