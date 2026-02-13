{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Coherent noise is smoothly-changing, semi-random noise.  A given input always produces the same output. A small change in input produces a small change in output.  A large change in input produces a random change in output. This class is not intended to be instantiated.
--
-- Generated bindings for @GKCoherentNoiseSource@.
module ObjC.GameplayKit.GKCoherentNoiseSource
  ( GKCoherentNoiseSource
  , IsGKCoherentNoiseSource(..)
  , frequency
  , setFrequency
  , octaveCount
  , setOctaveCount
  , lacunarity
  , setLacunarity
  , seed
  , setSeed
  , frequencySelector
  , lacunaritySelector
  , octaveCountSelector
  , seedSelector
  , setFrequencySelector
  , setLacunaritySelector
  , setOctaveCountSelector
  , setSeedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- frequency@
frequency :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CDouble
frequency gkCoherentNoiseSource =
  sendMessage gkCoherentNoiseSource frequencySelector

-- | @- setFrequency:@
setFrequency :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CDouble -> IO ()
setFrequency gkCoherentNoiseSource value =
  sendMessage gkCoherentNoiseSource setFrequencySelector value

-- | @- octaveCount@
octaveCount :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CLong
octaveCount gkCoherentNoiseSource =
  sendMessage gkCoherentNoiseSource octaveCountSelector

-- | @- setOctaveCount:@
setOctaveCount :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CLong -> IO ()
setOctaveCount gkCoherentNoiseSource value =
  sendMessage gkCoherentNoiseSource setOctaveCountSelector value

-- | @- lacunarity@
lacunarity :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CDouble
lacunarity gkCoherentNoiseSource =
  sendMessage gkCoherentNoiseSource lacunaritySelector

-- | @- setLacunarity:@
setLacunarity :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CDouble -> IO ()
setLacunarity gkCoherentNoiseSource value =
  sendMessage gkCoherentNoiseSource setLacunaritySelector value

-- | @- seed@
seed :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CInt
seed gkCoherentNoiseSource =
  sendMessage gkCoherentNoiseSource seedSelector

-- | @- setSeed:@
setSeed :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CInt -> IO ()
setSeed gkCoherentNoiseSource value =
  sendMessage gkCoherentNoiseSource setSeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @octaveCount@
octaveCountSelector :: Selector '[] CLong
octaveCountSelector = mkSelector "octaveCount"

-- | @Selector@ for @setOctaveCount:@
setOctaveCountSelector :: Selector '[CLong] ()
setOctaveCountSelector = mkSelector "setOctaveCount:"

-- | @Selector@ for @lacunarity@
lacunaritySelector :: Selector '[] CDouble
lacunaritySelector = mkSelector "lacunarity"

-- | @Selector@ for @setLacunarity:@
setLacunaritySelector :: Selector '[CDouble] ()
setLacunaritySelector = mkSelector "setLacunarity:"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CInt
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector '[CInt] ()
setSeedSelector = mkSelector "setSeed:"

