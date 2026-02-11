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
  , setFrequencySelector
  , octaveCountSelector
  , setOctaveCountSelector
  , lacunaritySelector
  , setLacunaritySelector
  , seedSelector
  , setSeedSelector


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

-- | @- frequency@
frequency :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CDouble
frequency gkCoherentNoiseSource  =
  sendMsg gkCoherentNoiseSource (mkSelector "frequency") retCDouble []

-- | @- setFrequency:@
setFrequency :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CDouble -> IO ()
setFrequency gkCoherentNoiseSource  value =
  sendMsg gkCoherentNoiseSource (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- | @- octaveCount@
octaveCount :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CLong
octaveCount gkCoherentNoiseSource  =
  sendMsg gkCoherentNoiseSource (mkSelector "octaveCount") retCLong []

-- | @- setOctaveCount:@
setOctaveCount :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CLong -> IO ()
setOctaveCount gkCoherentNoiseSource  value =
  sendMsg gkCoherentNoiseSource (mkSelector "setOctaveCount:") retVoid [argCLong (fromIntegral value)]

-- | @- lacunarity@
lacunarity :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CDouble
lacunarity gkCoherentNoiseSource  =
  sendMsg gkCoherentNoiseSource (mkSelector "lacunarity") retCDouble []

-- | @- setLacunarity:@
setLacunarity :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CDouble -> IO ()
setLacunarity gkCoherentNoiseSource  value =
  sendMsg gkCoherentNoiseSource (mkSelector "setLacunarity:") retVoid [argCDouble (fromIntegral value)]

-- | @- seed@
seed :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> IO CInt
seed gkCoherentNoiseSource  =
  sendMsg gkCoherentNoiseSource (mkSelector "seed") retCInt []

-- | @- setSeed:@
setSeed :: IsGKCoherentNoiseSource gkCoherentNoiseSource => gkCoherentNoiseSource -> CInt -> IO ()
setSeed gkCoherentNoiseSource  value =
  sendMsg gkCoherentNoiseSource (mkSelector "setSeed:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @octaveCount@
octaveCountSelector :: Selector
octaveCountSelector = mkSelector "octaveCount"

-- | @Selector@ for @setOctaveCount:@
setOctaveCountSelector :: Selector
setOctaveCountSelector = mkSelector "setOctaveCount:"

-- | @Selector@ for @lacunarity@
lacunaritySelector :: Selector
lacunaritySelector = mkSelector "lacunarity"

-- | @Selector@ for @setLacunarity:@
setLacunaritySelector :: Selector
setLacunaritySelector = mkSelector "setLacunarity:"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector
setSeedSelector = mkSelector "setSeed:"

