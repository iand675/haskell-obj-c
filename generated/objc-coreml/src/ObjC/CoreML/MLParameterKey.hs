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
  , initSelector
  , newSelector
  , scopedToSelector
  , learningRateSelector
  , momentumSelector
  , miniBatchSizeSelector
  , beta1Selector
  , beta2Selector
  , epsSelector
  , epochsSelector
  , shuffleSelector
  , seedSelector
  , numberOfNeighborsSelector
  , weightsSelector
  , biasesSelector
  , linkedModelFileNameSelector
  , linkedModelSearchPathSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLParameterKey mlParameterKey => mlParameterKey -> IO (Id MLParameterKey)
init_ mlParameterKey  =
  sendMsg mlParameterKey (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- | @- scopedTo:@
scopedTo :: (IsMLParameterKey mlParameterKey, IsNSString scope) => mlParameterKey -> scope -> IO (Id MLParameterKey)
scopedTo mlParameterKey  scope =
withObjCPtr scope $ \raw_scope ->
    sendMsg mlParameterKey (mkSelector "scopedTo:") (retPtr retVoid) [argPtr (castPtr raw_scope :: Ptr ())] >>= retainedObject . castPtr

-- | @+ learningRate@
learningRate :: IO (Id MLParameterKey)
learningRate  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "learningRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ momentum@
momentum :: IO (Id MLParameterKey)
momentum  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "momentum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ miniBatchSize@
miniBatchSize :: IO (Id MLParameterKey)
miniBatchSize  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "miniBatchSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ beta1@
beta1 :: IO (Id MLParameterKey)
beta1  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "beta1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ beta2@
beta2 :: IO (Id MLParameterKey)
beta2  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "beta2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ eps@
eps :: IO (Id MLParameterKey)
eps  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "eps") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ epochs@
epochs :: IO (Id MLParameterKey)
epochs  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "epochs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ shuffle@
shuffle :: IO (Id MLParameterKey)
shuffle  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "shuffle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ seed@
seed :: IO (Id MLParameterKey)
seed  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "seed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ numberOfNeighbors@
numberOfNeighbors :: IO (Id MLParameterKey)
numberOfNeighbors  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "numberOfNeighbors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ weights@
weights :: IO (Id MLParameterKey)
weights  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ biases@
biases :: IO (Id MLParameterKey)
biases  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "biases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ linkedModelFileName@
linkedModelFileName :: IO (Id MLParameterKey)
linkedModelFileName  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "linkedModelFileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ linkedModelSearchPath@
linkedModelSearchPath :: IO (Id MLParameterKey)
linkedModelSearchPath  =
  do
    cls' <- getRequiredClass "MLParameterKey"
    sendClassMsg cls' (mkSelector "linkedModelSearchPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @scopedTo:@
scopedToSelector :: Selector
scopedToSelector = mkSelector "scopedTo:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @momentum@
momentumSelector :: Selector
momentumSelector = mkSelector "momentum"

-- | @Selector@ for @miniBatchSize@
miniBatchSizeSelector :: Selector
miniBatchSizeSelector = mkSelector "miniBatchSize"

-- | @Selector@ for @beta1@
beta1Selector :: Selector
beta1Selector = mkSelector "beta1"

-- | @Selector@ for @beta2@
beta2Selector :: Selector
beta2Selector = mkSelector "beta2"

-- | @Selector@ for @eps@
epsSelector :: Selector
epsSelector = mkSelector "eps"

-- | @Selector@ for @epochs@
epochsSelector :: Selector
epochsSelector = mkSelector "epochs"

-- | @Selector@ for @shuffle@
shuffleSelector :: Selector
shuffleSelector = mkSelector "shuffle"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @numberOfNeighbors@
numberOfNeighborsSelector :: Selector
numberOfNeighborsSelector = mkSelector "numberOfNeighbors"

-- | @Selector@ for @weights@
weightsSelector :: Selector
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector
biasesSelector = mkSelector "biases"

-- | @Selector@ for @linkedModelFileName@
linkedModelFileNameSelector :: Selector
linkedModelFileNameSelector = mkSelector "linkedModelFileName"

-- | @Selector@ for @linkedModelSearchPath@
linkedModelSearchPathSelector :: Selector
linkedModelSearchPathSelector = mkSelector "linkedModelSearchPath"

