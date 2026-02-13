{-# LANGUAGE DataKinds #-}
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
  , frequencySelector
  , initWithFrequencySelector
  , setFrequencySelector
  , spheresNoiseWithFrequencySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ spheresNoiseWithFrequency:@
spheresNoiseWithFrequency :: CDouble -> IO (Id GKSpheresNoiseSource)
spheresNoiseWithFrequency frequency =
  do
    cls' <- getRequiredClass "GKSpheresNoiseSource"
    sendClassMessage cls' spheresNoiseWithFrequencySelector frequency

-- | @- initWithFrequency:@
initWithFrequency :: IsGKSpheresNoiseSource gkSpheresNoiseSource => gkSpheresNoiseSource -> CDouble -> IO (Id GKSpheresNoiseSource)
initWithFrequency gkSpheresNoiseSource frequency =
  sendOwnedMessage gkSpheresNoiseSource initWithFrequencySelector frequency

-- | @- frequency@
frequency :: IsGKSpheresNoiseSource gkSpheresNoiseSource => gkSpheresNoiseSource -> IO CDouble
frequency gkSpheresNoiseSource =
  sendMessage gkSpheresNoiseSource frequencySelector

-- | @- setFrequency:@
setFrequency :: IsGKSpheresNoiseSource gkSpheresNoiseSource => gkSpheresNoiseSource -> CDouble -> IO ()
setFrequency gkSpheresNoiseSource value =
  sendMessage gkSpheresNoiseSource setFrequencySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @spheresNoiseWithFrequency:@
spheresNoiseWithFrequencySelector :: Selector '[CDouble] (Id GKSpheresNoiseSource)
spheresNoiseWithFrequencySelector = mkSelector "spheresNoiseWithFrequency:"

-- | @Selector@ for @initWithFrequency:@
initWithFrequencySelector :: Selector '[CDouble] (Id GKSpheresNoiseSource)
initWithFrequencySelector = mkSelector "initWithFrequency:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

