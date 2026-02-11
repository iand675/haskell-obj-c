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
    sendClassMsg cls' (mkSelector "layerWithRate:seed:") (retPtr retVoid) [argCFloat (fromIntegral rate), argCULong (fromIntegral seed)] >>= retainedObject . castPtr

-- | rate
--
-- The probability that each element is dropped
--
-- ObjC selector: @- rate@
rate :: IsMLCDropoutLayer mlcDropoutLayer => mlcDropoutLayer -> IO CFloat
rate mlcDropoutLayer  =
  sendMsg mlcDropoutLayer (mkSelector "rate") retCFloat []

-- | seed
--
-- The initial seed used to generate random numbers
--
-- ObjC selector: @- seed@
seed :: IsMLCDropoutLayer mlcDropoutLayer => mlcDropoutLayer -> IO CULong
seed mlcDropoutLayer  =
  sendMsg mlcDropoutLayer (mkSelector "seed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithRate:seed:@
layerWithRate_seedSelector :: Selector
layerWithRate_seedSelector = mkSelector "layerWithRate:seed:"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

