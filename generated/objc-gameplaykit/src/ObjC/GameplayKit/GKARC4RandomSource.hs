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
  , initSelector
  , initWithSeedSelector
  , dropValuesWithCountSelector
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

-- | Initializes an arc4 random source with bits from high entropy system resource like SecRandomCopyBytes.
--
-- ObjC selector: @- init@
init_ :: IsGKARC4RandomSource gkarC4RandomSource => gkarC4RandomSource -> IO (Id GKARC4RandomSource)
init_ gkarC4RandomSource  =
  sendMsg gkarC4RandomSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an arc4 random source with bits from the seed.
--
-- ObjC selector: @- initWithSeed:@
initWithSeed :: (IsGKARC4RandomSource gkarC4RandomSource, IsNSData seed) => gkarC4RandomSource -> seed -> IO (Id GKARC4RandomSource)
initWithSeed gkarC4RandomSource  seed =
withObjCPtr seed $ \raw_seed ->
    sendMsg gkarC4RandomSource (mkSelector "initWithSeed:") (retPtr retVoid) [argPtr (castPtr raw_seed :: Ptr ())] >>= ownedObject . castPtr

-- | Arc4 based random sources have repeatable initial sequences. If used for obfuscation you should drop N values from the start, where N should be any number larger than 768 to ensure the initial sequence is flushed.
--
-- ObjC selector: @- dropValuesWithCount:@
dropValuesWithCount :: IsGKARC4RandomSource gkarC4RandomSource => gkarC4RandomSource -> CULong -> IO ()
dropValuesWithCount gkarC4RandomSource  count =
  sendMsg gkarC4RandomSource (mkSelector "dropValuesWithCount:") retVoid [argCULong (fromIntegral count)]

-- | The seed used to stir the arc4 random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- seed@
seed :: IsGKARC4RandomSource gkarC4RandomSource => gkarC4RandomSource -> IO (Id NSData)
seed gkarC4RandomSource  =
  sendMsg gkarC4RandomSource (mkSelector "seed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The seed used to stir the arc4 random source. The seed is not encoded through archiving, but the equivalent state buffers are encoded.
--
-- ObjC selector: @- setSeed:@
setSeed :: (IsGKARC4RandomSource gkarC4RandomSource, IsNSData value) => gkarC4RandomSource -> value -> IO ()
setSeed gkarC4RandomSource  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkarC4RandomSource (mkSelector "setSeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSeed:@
initWithSeedSelector :: Selector
initWithSeedSelector = mkSelector "initWithSeed:"

-- | @Selector@ for @dropValuesWithCount:@
dropValuesWithCountSelector :: Selector
dropValuesWithCountSelector = mkSelector "dropValuesWithCount:"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector
setSeedSelector = mkSelector "setSeed:"

