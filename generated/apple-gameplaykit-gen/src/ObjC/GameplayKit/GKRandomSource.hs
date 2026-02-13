{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete random source that can generate random numbers. The implementation details are up to the system and if a particular algorithm is needed then use one of the provided subclasses.
--
-- For certain specialized applications a shared system source may be needed and for those instances there is a wrapped interface over arc4random_*, accessible via +[GKRandomSource sharedRandom].
--
-- See: GKARC4RandomSource
--
-- See: GKLinearCongruentialRandomSource
--
-- See: GKMersenneTwisterRandomSource
--
-- See: GKRandomSource.systemRandom
--
-- Generated bindings for @GKRandomSource@.
module ObjC.GameplayKit.GKRandomSource
  ( GKRandomSource
  , IsGKRandomSource(..)
  , init_
  , initWithCoder
  , sharedRandom
  , arrayByShufflingObjectsInArray
  , arrayByShufflingObjectsInArraySelector
  , initSelector
  , initWithCoderSelector
  , sharedRandomSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new random source initialized using bits from an entropy source like SecRandomCopyBytes. When used directly from the base class; this source is deterministic and performant but the underlying implementation details are not specified. Use a subclass with a specific algorithm implementation guaranteed if your application requires very stringent random source charateristics.
--
-- See: GKARC4RandomSource
--
-- See: GKLinearCongruentialRandomSource
--
-- See: GKMersenneTwisterRandomSource
--
-- ObjC selector: @- init@
init_ :: IsGKRandomSource gkRandomSource => gkRandomSource -> IO (Id GKRandomSource)
init_ gkRandomSource =
  sendOwnedMessage gkRandomSource initSelector

-- | Deserializes a random source from an NSCoder. All random sources support coding for serializing and deserializing the state of the random source. Each subclass has its own contract for what parts of the state is preserved when serialized but the general contract is that a serialized source must generate the same sequence of values as the original source would from the instant it was serialized.
--
-- Note that the sharedRandom instance is an exception as it is explicitly seedless and a shared singleton instance. When serialized and deserialized it will return the current sharedRandom instance instead.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsGKRandomSource gkRandomSource, IsNSCoder aDecoder) => gkRandomSource -> aDecoder -> IO (Id GKRandomSource)
initWithCoder gkRandomSource aDecoder =
  sendOwnedMessage gkRandomSource initWithCoderSelector (toNSCoder aDecoder)

-- | Returns a shared instance of a random source that uses the system's underlying random source. Using this instance modifies the outcome of future calls to the arc4random family of C calls. It is also affected by calls to the C apis and should not be used for sources that are intended to be deterministic.
--
-- Note that while it may seem semantically similar to a GKARC4RandomSource, this is not a drop in replacement.
--
-- ObjC selector: @+ sharedRandom@
sharedRandom :: IO (Id GKRandomSource)
sharedRandom  =
  do
    cls' <- getRequiredClass "GKRandomSource"
    sendClassMessage cls' sharedRandomSelector

-- | Returns a shuffled instance of the given array. The objects in the array are shuffled based on a Fisher-Yates shuffle.
--
-- Any random, be it custom, source or a distribution, that can provide a number with an upper bound of at least the array.count is suitable for this shuffle.
--
-- ObjC selector: @- arrayByShufflingObjectsInArray:@
arrayByShufflingObjectsInArray :: (IsGKRandomSource gkRandomSource, IsNSArray array) => gkRandomSource -> array -> IO (Id NSArray)
arrayByShufflingObjectsInArray gkRandomSource array =
  sendMessage gkRandomSource arrayByShufflingObjectsInArraySelector (toNSArray array)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKRandomSource)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id GKRandomSource)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sharedRandom@
sharedRandomSelector :: Selector '[] (Id GKRandomSource)
sharedRandomSelector = mkSelector "sharedRandom"

-- | @Selector@ for @arrayByShufflingObjectsInArray:@
arrayByShufflingObjectsInArraySelector :: Selector '[Id NSArray] (Id NSArray)
arrayByShufflingObjectsInArraySelector = mkSelector "arrayByShufflingObjectsInArray:"

