{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A deterministic pseudo-random source that generates random numbers based on a mersenne twister algorithm. This is a deterministic random source suitable for creating reliable gameplay mechanics. It is slightly slower than an Arc4 source, but more random, in that it has a longer period until repeating sequences.
--
-- While deterministic, this is not a cryptographic random source. It is however suitable for obfuscation of gameplay data.
--
-- Generated bindings for @GKMersenneTwisterRandomSource@.
module ObjC.GameplayKit.GKMersenneTwisterRandomSource
  ( GKMersenneTwisterRandomSource
  , IsGKMersenneTwisterRandomSource(..)
  , init_
  , initWithSeed
  , seed
  , setSeed
  , initSelector
  , initWithSeedSelector
  , seedSelector
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

-- | Initializes a linear congruential random source with bits from a high entropy system resource like SecRandomCopyBytes.
--
-- ObjC selector: @- init@
init_ :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> IO (Id GKMersenneTwisterRandomSource)
init_ gkMersenneTwisterRandomSource =
  sendOwnedMessage gkMersenneTwisterRandomSource initSelector

-- | Initializes a linear congruential random source with bits the given 64 bit seed.
--
-- ObjC selector: @- initWithSeed:@
initWithSeed :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> CULong -> IO (Id GKMersenneTwisterRandomSource)
initWithSeed gkMersenneTwisterRandomSource seed =
  sendOwnedMessage gkMersenneTwisterRandomSource initWithSeedSelector seed

-- | The seed used to stir the mersenne twister random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- seed@
seed :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> IO CULong
seed gkMersenneTwisterRandomSource =
  sendMessage gkMersenneTwisterRandomSource seedSelector

-- | The seed used to stir the mersenne twister random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- setSeed:@
setSeed :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> CULong -> IO ()
setSeed gkMersenneTwisterRandomSource value =
  sendMessage gkMersenneTwisterRandomSource setSeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKMersenneTwisterRandomSource)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSeed:@
initWithSeedSelector :: Selector '[CULong] (Id GKMersenneTwisterRandomSource)
initWithSeedSelector = mkSelector "initWithSeed:"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CULong
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector '[CULong] ()
setSeedSelector = mkSelector "setSeed:"

