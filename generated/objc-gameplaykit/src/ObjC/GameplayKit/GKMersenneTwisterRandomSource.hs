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

-- | Initializes a linear congruential random source with bits from a high entropy system resource like SecRandomCopyBytes.
--
-- ObjC selector: @- init@
init_ :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> IO (Id GKMersenneTwisterRandomSource)
init_ gkMersenneTwisterRandomSource  =
  sendMsg gkMersenneTwisterRandomSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a linear congruential random source with bits the given 64 bit seed.
--
-- ObjC selector: @- initWithSeed:@
initWithSeed :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> CULong -> IO (Id GKMersenneTwisterRandomSource)
initWithSeed gkMersenneTwisterRandomSource  seed =
  sendMsg gkMersenneTwisterRandomSource (mkSelector "initWithSeed:") (retPtr retVoid) [argCULong (fromIntegral seed)] >>= ownedObject . castPtr

-- | The seed used to stir the mersenne twister random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- seed@
seed :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> IO CULong
seed gkMersenneTwisterRandomSource  =
  sendMsg gkMersenneTwisterRandomSource (mkSelector "seed") retCULong []

-- | The seed used to stir the mersenne twister random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- setSeed:@
setSeed :: IsGKMersenneTwisterRandomSource gkMersenneTwisterRandomSource => gkMersenneTwisterRandomSource -> CULong -> IO ()
setSeed gkMersenneTwisterRandomSource  value =
  sendMsg gkMersenneTwisterRandomSource (mkSelector "setSeed:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSeed:@
initWithSeedSelector :: Selector
initWithSeedSelector = mkSelector "initWithSeed:"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector
setSeedSelector = mkSelector "setSeed:"

