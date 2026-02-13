{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCDropoutLayer
--
-- A dropout layer
--
-- Generated bindings for @MLCDropoutLayer@.
module ObjC.MLCompute.MLCDropoutLayer
  ( MLCDropoutLayer
  , IsMLCDropoutLayer(..)
  , layerWithRate_seed
  , rate
  , seed
  , layerWithRate_seedSelector
  , rateSelector
  , seedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a dropout layer
--
-- @rate@ — A scalar float value. The probability that each element is dropped.
--
-- @seed@ — The seed used to generate random numbers.
--
-- Returns: A new dropout layer
--
-- ObjC selector: @+ layerWithRate:seed:@
layerWithRate_seed :: CFloat -> CULong -> IO (Id MLCDropoutLayer)
layerWithRate_seed rate seed =
  do
    cls' <- getRequiredClass "MLCDropoutLayer"
    sendClassMessage cls' layerWithRate_seedSelector rate seed

-- | rate
--
-- The probability that each element is dropped
--
-- ObjC selector: @- rate@
rate :: IsMLCDropoutLayer mlcDropoutLayer => mlcDropoutLayer -> IO CFloat
rate mlcDropoutLayer =
  sendMessage mlcDropoutLayer rateSelector

-- | seed
--
-- The initial seed used to generate random numbers
--
-- ObjC selector: @- seed@
seed :: IsMLCDropoutLayer mlcDropoutLayer => mlcDropoutLayer -> IO CULong
seed mlcDropoutLayer =
  sendMessage mlcDropoutLayer seedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithRate:seed:@
layerWithRate_seedSelector :: Selector '[CFloat, CULong] (Id MLCDropoutLayer)
layerWithRate_seedSelector = mkSelector "layerWithRate:seed:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CULong
seedSelector = mkSelector "seed"

