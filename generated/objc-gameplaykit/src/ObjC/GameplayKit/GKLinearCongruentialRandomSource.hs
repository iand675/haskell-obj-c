{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A deterministic pseudo-random source that generates random numbers based on a linear congruential algorithm. This is a deterministic random source suitable for creating reliable gameplay mechanics. It is slightly faster than an Arc4 source, but less random. In particular the lower bits of the generated values are less random than the higher bits.
--
-- While deterministic, this is not a cryptographic random source. It is also not suitable for obfuscation of gameplay data.
--
-- Generated bindings for @GKLinearCongruentialRandomSource@.
module ObjC.GameplayKit.GKLinearCongruentialRandomSource
  ( GKLinearCongruentialRandomSource
  , IsGKLinearCongruentialRandomSource(..)
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

-- | Initializes a linear congruential random source with bits from high entropy system resource like SecRandomCopyBytes.
--
-- ObjC selector: @- init@
init_ :: IsGKLinearCongruentialRandomSource gkLinearCongruentialRandomSource => gkLinearCongruentialRandomSource -> IO (Id GKLinearCongruentialRandomSource)
init_ gkLinearCongruentialRandomSource  =
  sendMsg gkLinearCongruentialRandomSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a linear congruential random source with bits the given 64 bit seed.
--
-- ObjC selector: @- initWithSeed:@
initWithSeed :: IsGKLinearCongruentialRandomSource gkLinearCongruentialRandomSource => gkLinearCongruentialRandomSource -> CULong -> IO (Id GKLinearCongruentialRandomSource)
initWithSeed gkLinearCongruentialRandomSource  seed =
  sendMsg gkLinearCongruentialRandomSource (mkSelector "initWithSeed:") (retPtr retVoid) [argCULong (fromIntegral seed)] >>= ownedObject . castPtr

-- | The seed used to stir the linear congruential random source. The seed changes each time a random value is generated from this source, as the seed is the state buffer. The seed is encoded through archiving.
--
-- ObjC selector: @- seed@
seed :: IsGKLinearCongruentialRandomSource gkLinearCongruentialRandomSource => gkLinearCongruentialRandomSource -> IO CULong
seed gkLinearCongruentialRandomSource  =
  sendMsg gkLinearCongruentialRandomSource (mkSelector "seed") retCULong []

-- | The seed used to stir the linear congruential random source. The seed changes each time a random value is generated from this source, as the seed is the state buffer. The seed is encoded through archiving.
--
-- ObjC selector: @- setSeed:@
setSeed :: IsGKLinearCongruentialRandomSource gkLinearCongruentialRandomSource => gkLinearCongruentialRandomSource -> CULong -> IO ()
setSeed gkLinearCongruentialRandomSource  value =
  sendMsg gkLinearCongruentialRandomSource (mkSelector "setSeed:") retVoid [argCULong (fromIntegral value)]

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

