{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A deterministic pseudo-random source that generates random numbers based on an arc4 algorithm. This is a deterministic random source suitable for creating reliable gameplay mechanics.
--
-- While deterministic, this is not a cryptographic random source, however it may be useful for obfuscation of gameplay data in manner similar to a stream cipher.
--
-- Generated bindings for @GKARC4RandomSource@.
module ObjC.GameplayKit.GKARC4RandomSource
  ( GKARC4RandomSource
  , IsGKARC4RandomSource(..)
  , init_
  , initWithSeed
  , dropValuesWithCount
  , seed
  , setSeed
  , dropValuesWithCountSelector
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

-- | Initializes an arc4 random source with bits from high entropy system resource like SecRandomCopyBytes.
--
-- ObjC selector: @- init@
init_ :: IsGKARC4RandomSource gkarC4RandomSource => gkarC4RandomSource -> IO (Id GKARC4RandomSource)
init_ gkarC4RandomSource =
  sendOwnedMessage gkarC4RandomSource initSelector

-- | Initializes an arc4 random source with bits from the seed.
--
-- ObjC selector: @- initWithSeed:@
initWithSeed :: (IsGKARC4RandomSource gkarC4RandomSource, IsNSData seed) => gkarC4RandomSource -> seed -> IO (Id GKARC4RandomSource)
initWithSeed gkarC4RandomSource seed =
  sendOwnedMessage gkarC4RandomSource initWithSeedSelector (toNSData seed)

-- | Arc4 based random sources have repeatable initial sequences. If used for obfuscation you should drop N values from the start, where N should be any number larger than 768 to ensure the initial sequence is flushed.
--
-- ObjC selector: @- dropValuesWithCount:@
dropValuesWithCount :: IsGKARC4RandomSource gkarC4RandomSource => gkarC4RandomSource -> CULong -> IO ()
dropValuesWithCount gkarC4RandomSource count =
  sendMessage gkarC4RandomSource dropValuesWithCountSelector count

-- | The seed used to stir the arc4 random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- seed@
seed :: IsGKARC4RandomSource gkarC4RandomSource => gkarC4RandomSource -> IO (Id NSData)
seed gkarC4RandomSource =
  sendMessage gkarC4RandomSource seedSelector

-- | The seed used to stir the arc4 random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- setSeed:@
setSeed :: (IsGKARC4RandomSource gkarC4RandomSource, IsNSData value) => gkarC4RandomSource -> value -> IO ()
setSeed gkarC4RandomSource value =
  sendMessage gkarC4RandomSource setSeedSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKARC4RandomSource)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSeed:@
initWithSeedSelector :: Selector '[Id NSData] (Id GKARC4RandomSource)
initWithSeedSelector = mkSelector "initWithSeed:"

-- | @Selector@ for @dropValuesWithCount:@
dropValuesWithCountSelector :: Selector '[CULong] ()
dropValuesWithCountSelector = mkSelector "dropValuesWithCount:"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] (Id NSData)
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector '[Id NSData] ()
setSeedSelector = mkSelector "setSeed:"

