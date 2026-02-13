{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A random distribution is a random source itself with a specific mapping from the input source to the output values. The distribution is uniform, meaning there is no bias towards any of the possible outcomes.
--
-- Generated bindings for @GKRandomDistribution@.
module ObjC.GameplayKit.GKRandomDistribution
  ( GKRandomDistribution
  , IsGKRandomDistribution(..)
  , initWithRandomSource_lowestValue_highestValue
  , nextInt
  , nextIntWithUpperBound
  , nextUniform
  , nextBool
  , distributionWithLowestValue_highestValue
  , distributionForDieWithSideCount
  , d6
  , d20
  , lowestValue
  , highestValue
  , numberOfPossibleOutcomes
  , d20Selector
  , d6Selector
  , distributionForDieWithSideCountSelector
  , distributionWithLowestValue_highestValueSelector
  , highestValueSelector
  , initWithRandomSource_lowestValue_highestValueSelector
  , lowestValueSelector
  , nextBoolSelector
  , nextIntSelector
  , nextIntWithUpperBoundSelector
  , nextUniformSelector
  , numberOfPossibleOutcomesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a random distribution within the range [lowest, highest] using a source to grab input values from.
--
-- ObjC selector: @- initWithRandomSource:lowestValue:highestValue:@
initWithRandomSource_lowestValue_highestValue :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> RawId -> CLong -> CLong -> IO (Id GKRandomDistribution)
initWithRandomSource_lowestValue_highestValue gkRandomDistribution source lowestInclusive highestInclusive =
  sendOwnedMessage gkRandomDistribution initWithRandomSource_lowestValue_highestValueSelector source lowestInclusive highestInclusive

-- | Returns the next integer in the distribution sequence and moves ahead to the next one. The value is in the range of [lowest, highest].
--
-- ObjC selector: @- nextInt@
nextInt :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CLong
nextInt gkRandomDistribution =
  sendMessage gkRandomDistribution nextIntSelector

-- | Returns the next unsigned value in the distribution sequence that is less than upperBound. The value never equals or exceeeds upperBounds, and in this case it will also never exceed the highest value of the distribution.
--
-- ObjC selector: @- nextIntWithUpperBound:@
nextIntWithUpperBound :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> CULong -> IO CULong
nextIntWithUpperBound gkRandomDistribution upperBound =
  sendMessage gkRandomDistribution nextIntWithUpperBoundSelector upperBound

-- | Returns the next uniform float in the random sequence and moves ahead to the next one. The value is in the range of [lowest / higest, 1.0].
--
-- The value is quantized to the distribution's lowest and highest bounds. Thus on a d20 distribution the value is quantized to 5% increments. The output value 0 is not possible to get unless the lowest value bound is also 0 or below.
--
-- See: nextInt
--
-- ObjC selector: @- nextUniform@
nextUniform :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CFloat
nextUniform gkRandomDistribution =
  sendMessage gkRandomDistribution nextUniformSelector

-- | Returns the next true or false value in the distribution sequence and moves ahead to the next one. The value is either nonzero (true) or zero (false). Use this for simple boolean switches in logic that don't require fuzzy evaluation. For fuzzy evaluation use nextUniform.
--
-- By default this is based on the referenced source's definition of nextBool.
--
-- See: GKRandomSource.nextBool
--
-- ObjC selector: @- nextBool@
nextBool :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO Bool
nextBool gkRandomDistribution =
  sendMessage gkRandomDistribution nextBoolSelector

-- | Convenience creation of random distribution within the range [lowest, highest] using an isolated source to grab input values from. This is equivalent to calling alloc followed by initWithSource:lowest:highest:, where source is [[GKRandomSource alloc] init].
--
-- See: initWithRandomSource:lowestValue:highestValue:
--
-- ObjC selector: @+ distributionWithLowestValue:highestValue:@
distributionWithLowestValue_highestValue :: CLong -> CLong -> IO (Id GKRandomDistribution)
distributionWithLowestValue_highestValue lowestInclusive highestInclusive =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMessage cls' distributionWithLowestValue_highestValueSelector lowestInclusive highestInclusive

-- | Convenience creation of random distribution with the die like range [1, sideCount] using an isolated source to grab input values from. This is equivalent to calling alloc followed by initWithSource:lowest:highest:, where source is [[GKRandomSource alloc] init].
--
-- See: initWithRandomSource:lowestValue:highestValue:
--
-- ObjC selector: @+ distributionForDieWithSideCount:@
distributionForDieWithSideCount :: CLong -> IO (Id GKRandomDistribution)
distributionForDieWithSideCount sideCount =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMessage cls' distributionForDieWithSideCountSelector sideCount

-- | Convenience creation for the very common d6 range [1, 6] with an isolated random source shielded from outside sources.
--
-- ObjC selector: @+ d6@
d6 :: IO (Id GKRandomDistribution)
d6  =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMessage cls' d6Selector

-- | Convenience creation for the very common d20 range [1, 20] with an isolated random source shielded from outside sources.
--
-- ObjC selector: @+ d20@
d20 :: IO (Id GKRandomDistribution)
d20  =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMessage cls' d20Selector

-- | The lowest value the distribution will output.
--
-- ObjC selector: @- lowestValue@
lowestValue :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CLong
lowestValue gkRandomDistribution =
  sendMessage gkRandomDistribution lowestValueSelector

-- | The highest value the distribution will output.
--
-- ObjC selector: @- highestValue@
highestValue :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CLong
highestValue gkRandomDistribution =
  sendMessage gkRandomDistribution highestValueSelector

-- | The number of unique possible outcomes, depending on the distribution type this is not always highest - lowest + 1.
--
-- ObjC selector: @- numberOfPossibleOutcomes@
numberOfPossibleOutcomes :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CULong
numberOfPossibleOutcomes gkRandomDistribution =
  sendMessage gkRandomDistribution numberOfPossibleOutcomesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRandomSource:lowestValue:highestValue:@
initWithRandomSource_lowestValue_highestValueSelector :: Selector '[RawId, CLong, CLong] (Id GKRandomDistribution)
initWithRandomSource_lowestValue_highestValueSelector = mkSelector "initWithRandomSource:lowestValue:highestValue:"

-- | @Selector@ for @nextInt@
nextIntSelector :: Selector '[] CLong
nextIntSelector = mkSelector "nextInt"

-- | @Selector@ for @nextIntWithUpperBound:@
nextIntWithUpperBoundSelector :: Selector '[CULong] CULong
nextIntWithUpperBoundSelector = mkSelector "nextIntWithUpperBound:"

-- | @Selector@ for @nextUniform@
nextUniformSelector :: Selector '[] CFloat
nextUniformSelector = mkSelector "nextUniform"

-- | @Selector@ for @nextBool@
nextBoolSelector :: Selector '[] Bool
nextBoolSelector = mkSelector "nextBool"

-- | @Selector@ for @distributionWithLowestValue:highestValue:@
distributionWithLowestValue_highestValueSelector :: Selector '[CLong, CLong] (Id GKRandomDistribution)
distributionWithLowestValue_highestValueSelector = mkSelector "distributionWithLowestValue:highestValue:"

-- | @Selector@ for @distributionForDieWithSideCount:@
distributionForDieWithSideCountSelector :: Selector '[CLong] (Id GKRandomDistribution)
distributionForDieWithSideCountSelector = mkSelector "distributionForDieWithSideCount:"

-- | @Selector@ for @d6@
d6Selector :: Selector '[] (Id GKRandomDistribution)
d6Selector = mkSelector "d6"

-- | @Selector@ for @d20@
d20Selector :: Selector '[] (Id GKRandomDistribution)
d20Selector = mkSelector "d20"

-- | @Selector@ for @lowestValue@
lowestValueSelector :: Selector '[] CLong
lowestValueSelector = mkSelector "lowestValue"

-- | @Selector@ for @highestValue@
highestValueSelector :: Selector '[] CLong
highestValueSelector = mkSelector "highestValue"

-- | @Selector@ for @numberOfPossibleOutcomes@
numberOfPossibleOutcomesSelector :: Selector '[] CULong
numberOfPossibleOutcomesSelector = mkSelector "numberOfPossibleOutcomes"

