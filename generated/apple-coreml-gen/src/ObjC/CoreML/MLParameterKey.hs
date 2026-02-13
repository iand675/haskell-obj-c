{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class to specify list of supported model update parameters.
--
-- Generated bindings for @MLParameterKey@.
module ObjC.CoreML.MLParameterKey
  ( MLParameterKey
  , IsMLParameterKey(..)
  , init_
  , new
  , scopedTo
  , learningRate
  , momentum
  , miniBatchSize
  , beta1
  , beta2
  , eps
  , epochs
  , shuffle
  , seed
  , numberOfNeighbors
  , weights
  , biases
  , linkedModelFileName
  , linkedModelSearchPath
  , beta1Selector
  , beta2Selector
  , biasesSelector
  , epochsSelector
  , epsSelector
  , initSelector
  , learningRateSelector
  , linkedModelFileNameSelector
  , linkedModelSearchPathSelector
  , miniBatchSizeSelector
  , momentumSelector
  , newSelector
  , numberOfNeighborsSelector
  , scopedToSelector
  , seedSelector
  , shuffleSelector
  , weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLParameterKey mlParameterKey => mlParameterKey -> IO (Id MLParameterKey)
init_ mlParameterKey =
  sendOwnedMessage mlParameterKey initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendOwnedClassMessage cls' newSelector

-- | @- scopedTo:@
scopedTo :: (IsMLParameterKey mlParameterKey, IsNSString scope) => mlParameterKey -> scope -> IO (Id MLParameterKey)
scopedTo mlParameterKey scope =
  sendMessage mlParameterKey scopedToSelector (toNSString scope)

-- | @+ learningRate@
learningRate :: IO (Id MLParameterKey)
learningRate  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' learningRateSelector

-- | @+ momentum@
momentum :: IO (Id MLParameterKey)
momentum  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' momentumSelector

-- | @+ miniBatchSize@
miniBatchSize :: IO (Id MLParameterKey)
miniBatchSize  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' miniBatchSizeSelector

-- | @+ beta1@
beta1 :: IO (Id MLParameterKey)
beta1  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' beta1Selector

-- | @+ beta2@
beta2 :: IO (Id MLParameterKey)
beta2  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' beta2Selector

-- | @+ eps@
eps :: IO (Id MLParameterKey)
eps  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' epsSelector

-- | @+ epochs@
epochs :: IO (Id MLParameterKey)
epochs  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' epochsSelector

-- | @+ shuffle@
shuffle :: IO (Id MLParameterKey)
shuffle  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' shuffleSelector

-- | @+ seed@
seed :: IO (Id MLParameterKey)
seed  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' seedSelector

-- | @+ numberOfNeighbors@
numberOfNeighbors :: IO (Id MLParameterKey)
numberOfNeighbors  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' numberOfNeighborsSelector

-- | @+ weights@
weights :: IO (Id MLParameterKey)
weights  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' weightsSelector

-- | @+ biases@
biases :: IO (Id MLParameterKey)
biases  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' biasesSelector

-- | @+ linkedModelFileName@
linkedModelFileName :: IO (Id MLParameterKey)
linkedModelFileName  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' linkedModelFileNameSelector

-- | @+ linkedModelSearchPath@
linkedModelSearchPath :: IO (Id MLParameterKey)
linkedModelSearchPath  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMessage cls' linkedModelSearchPathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLParameterKey)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @scopedTo:@
scopedToSelector :: Selector '[Id NSString] (Id MLParameterKey)
scopedToSelector = mkSelector "scopedTo:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector '[] (Id MLParameterKey)
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @momentum@
momentumSelector :: Selector '[] (Id MLParameterKey)
momentumSelector = mkSelector "momentum"

-- | @Selector@ for @miniBatchSize@
miniBatchSizeSelector :: Selector '[] (Id MLParameterKey)
miniBatchSizeSelector = mkSelector "miniBatchSize"

-- | @Selector@ for @beta1@
beta1Selector :: Selector '[] (Id MLParameterKey)
beta1Selector = mkSelector "beta1"

-- | @Selector@ for @beta2@
beta2Selector :: Selector '[] (Id MLParameterKey)
beta2Selector = mkSelector "beta2"

-- | @Selector@ for @eps@
epsSelector :: Selector '[] (Id MLParameterKey)
epsSelector = mkSelector "eps"

-- | @Selector@ for @epochs@
epochsSelector :: Selector '[] (Id MLParameterKey)
epochsSelector = mkSelector "epochs"

-- | @Selector@ for @shuffle@
shuffleSelector :: Selector '[] (Id MLParameterKey)
shuffleSelector = mkSelector "shuffle"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] (Id MLParameterKey)
seedSelector = mkSelector "seed"

-- | @Selector@ for @numberOfNeighbors@
numberOfNeighborsSelector :: Selector '[] (Id MLParameterKey)
numberOfNeighborsSelector = mkSelector "numberOfNeighbors"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] (Id MLParameterKey)
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector '[] (Id MLParameterKey)
biasesSelector = mkSelector "biases"

-- | @Selector@ for @linkedModelFileName@
linkedModelFileNameSelector :: Selector '[] (Id MLParameterKey)
linkedModelFileNameSelector = mkSelector "linkedModelFileName"

-- | @Selector@ for @linkedModelSearchPath@
linkedModelSearchPathSelector :: Selector '[] (Id MLParameterKey)
linkedModelSearchPathSelector = mkSelector "linkedModelSearchPath"

