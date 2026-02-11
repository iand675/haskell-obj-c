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
  , initWithRandomSource_lowestValue_highestValueSelector
  , nextIntSelector
  , nextIntWithUpperBoundSelector
  , nextUniformSelector
  , nextBoolSelector
  , distributionWithLowestValue_highestValueSelector
  , distributionForDieWithSideCountSelector
  , d6Selector
  , d20Selector
  , lowestValueSelector
  , highestValueSelector
  , numberOfPossibleOutcomesSelector


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

-- | Initializes a random distribution within the range [lowest, highest] using a source to grab input values from.
--
-- ObjC selector: @- initWithRandomSource:lowestValue:highestValue:@
initWithRandomSource_lowestValue_highestValue :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> RawId -> CLong -> CLong -> IO (Id GKRandomDistribution)
initWithRandomSource_lowestValue_highestValue gkRandomDistribution  source lowestInclusive highestInclusive =
  sendMsg gkRandomDistribution (mkSelector "initWithRandomSource:lowestValue:highestValue:") (retPtr retVoid) [argPtr (castPtr (unRawId source) :: Ptr ()), argCLong (fromIntegral lowestInclusive), argCLong (fromIntegral highestInclusive)] >>= ownedObject . castPtr

-- | Returns the next integer in the distribution sequence and moves ahead to the next one. The value is in the range of [lowest, highest].
--
-- ObjC selector: @- nextInt@
nextInt :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CLong
nextInt gkRandomDistribution  =
  sendMsg gkRandomDistribution (mkSelector "nextInt") retCLong []

-- | Returns the next unsigned value in the distribution sequence that is less than upperBound. The value never equals or exceeeds upperBounds, and in this case it will also never exceed the highest value of the distribution.
--
-- ObjC selector: @- nextIntWithUpperBound:@
nextIntWithUpperBound :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> CULong -> IO CULong
nextIntWithUpperBound gkRandomDistribution  upperBound =
  sendMsg gkRandomDistribution (mkSelector "nextIntWithUpperBound:") retCULong [argCULong (fromIntegral upperBound)]

-- | Returns the next uniform float in the random sequence and moves ahead to the next one. The value is in the range of [lowest / higest, 1.0].
--
-- The value is quantized to the distribution's lowest and highest bounds. Thus on a d20 distribution the value is quantized to 5% increments. The output value 0 is not possible to get unless the lowest value bound is also 0 or below.
--
-- See: nextInt
--
-- ObjC selector: @- nextUniform@
nextUniform :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CFloat
nextUniform gkRandomDistribution  =
  sendMsg gkRandomDistribution (mkSelector "nextUniform") retCFloat []

-- | Returns the next true or false value in the distribution sequence and moves ahead to the next one. The value is either nonzero (true) or zero (false). Use this for simple boolean switches in logic that don't require fuzzy evaluation. For fuzzy evaluation use nextUniform.
--
-- By default this is based on the referenced source's definition of nextBool.
--
-- See: GKRandomSource.nextBool
--
-- ObjC selector: @- nextBool@
nextBool :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO Bool
nextBool gkRandomDistribution  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkRandomDistribution (mkSelector "nextBool") retCULong []

-- | Convenience creation of random distribution within the range [lowest, highest] using an isolated source to grab input values from. This is equivalent to calling alloc followed by initWithSource:lowest:highest:, where source is [[GKRandomSource alloc] init].
--
-- See: initWithRandomSource:lowestValue:highestValue:
--
-- ObjC selector: @+ distributionWithLowestValue:highestValue:@
distributionWithLowestValue_highestValue :: CLong -> CLong -> IO (Id GKRandomDistribution)
distributionWithLowestValue_highestValue lowestInclusive highestInclusive =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMsg cls' (mkSelector "distributionWithLowestValue:highestValue:") (retPtr retVoid) [argCLong (fromIntegral lowestInclusive), argCLong (fromIntegral highestInclusive)] >>= retainedObject . castPtr

-- | Convenience creation of random distribution with the die like range [1, sideCount] using an isolated source to grab input values from. This is equivalent to calling alloc followed by initWithSource:lowest:highest:, where source is [[GKRandomSource alloc] init].
--
-- See: initWithRandomSource:lowestValue:highestValue:
--
-- ObjC selector: @+ distributionForDieWithSideCount:@
distributionForDieWithSideCount :: CLong -> IO (Id GKRandomDistribution)
distributionForDieWithSideCount sideCount =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMsg cls' (mkSelector "distributionForDieWithSideCount:") (retPtr retVoid) [argCLong (fromIntegral sideCount)] >>= retainedObject . castPtr

-- | Convenience creation for the very common d6 range [1, 6] with an isolated random source shielded from outside sources.
--
-- ObjC selector: @+ d6@
d6 :: IO (Id GKRandomDistribution)
d6  =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMsg cls' (mkSelector "d6") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience creation for the very common d20 range [1, 20] with an isolated random source shielded from outside sources.
--
-- ObjC selector: @+ d20@
d20 :: IO (Id GKRandomDistribution)
d20  =
  do
    cls' <- getRequiredClass "GKRandomDistribution"
    sendClassMsg cls' (mkSelector "d20") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The lowest value the distribution will output.
--
-- ObjC selector: @- lowestValue@
lowestValue :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CLong
lowestValue gkRandomDistribution  =
  sendMsg gkRandomDistribution (mkSelector "lowestValue") retCLong []

-- | The highest value the distribution will output.
--
-- ObjC selector: @- highestValue@
highestValue :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CLong
highestValue gkRandomDistribution  =
  sendMsg gkRandomDistribution (mkSelector "highestValue") retCLong []

-- | The number of unique possible outcomes, depending on the distribution type this is not always highest - lowest + 1.
--
-- ObjC selector: @- numberOfPossibleOutcomes@
numberOfPossibleOutcomes :: IsGKRandomDistribution gkRandomDistribution => gkRandomDistribution -> IO CULong
numberOfPossibleOutcomes gkRandomDistribution  =
  sendMsg gkRandomDistribution (mkSelector "numberOfPossibleOutcomes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRandomSource:lowestValue:highestValue:@
initWithRandomSource_lowestValue_highestValueSelector :: Selector
initWithRandomSource_lowestValue_highestValueSelector = mkSelector "initWithRandomSource:lowestValue:highestValue:"

-- | @Selector@ for @nextInt@
nextIntSelector :: Selector
nextIntSelector = mkSelector "nextInt"

-- | @Selector@ for @nextIntWithUpperBound:@
nextIntWithUpperBoundSelector :: Selector
nextIntWithUpperBoundSelector = mkSelector "nextIntWithUpperBound:"

-- | @Selector@ for @nextUniform@
nextUniformSelector :: Selector
nextUniformSelector = mkSelector "nextUniform"

-- | @Selector@ for @nextBool@
nextBoolSelector :: Selector
nextBoolSelector = mkSelector "nextBool"

-- | @Selector@ for @distributionWithLowestValue:highestValue:@
distributionWithLowestValue_highestValueSelector :: Selector
distributionWithLowestValue_highestValueSelector = mkSelector "distributionWithLowestValue:highestValue:"

-- | @Selector@ for @distributionForDieWithSideCount:@
distributionForDieWithSideCountSelector :: Selector
distributionForDieWithSideCountSelector = mkSelector "distributionForDieWithSideCount:"

-- | @Selector@ for @d6@
d6Selector :: Selector
d6Selector = mkSelector "d6"

-- | @Selector@ for @d20@
d20Selector :: Selector
d20Selector = mkSelector "d20"

-- | @Selector@ for @lowestValue@
lowestValueSelector :: Selector
lowestValueSelector = mkSelector "lowestValue"

-- | @Selector@ for @highestValue@
highestValueSelector :: Selector
highestValueSelector = mkSelector "highestValue"

-- | @Selector@ for @numberOfPossibleOutcomes@
numberOfPossibleOutcomesSelector :: Selector
numberOfPossibleOutcomesSelector = mkSelector "numberOfPossibleOutcomes"

