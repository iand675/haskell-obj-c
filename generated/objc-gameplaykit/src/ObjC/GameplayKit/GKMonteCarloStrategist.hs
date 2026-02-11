{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The Monte Carlo Strategist is a generic AI that selects a game model update for a given player that results in the highest likelihood for that player to eventually win the game. It does this by sampling the updates available to the player in question. In doing this it will select the update it knows to produce the best result so far, expanding on this selection, simulating the rest of the game from that expansion, and then propogating the results (win or loss) upwards. It will do this until the budget has been reached, then returning the choice it has deemed best suited for the player in question.
--
-- Generated bindings for @GKMonteCarloStrategist@.
module ObjC.GameplayKit.GKMonteCarloStrategist
  ( GKMonteCarloStrategist
  , IsGKMonteCarloStrategist(..)
  , budget
  , setBudget
  , explorationParameter
  , setExplorationParameter
  , budgetSelector
  , setBudgetSelector
  , explorationParameterSelector
  , setExplorationParameterSelector


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

-- | The maximum number of samples that will be processed when searching for a move.
--
-- ObjC selector: @- budget@
budget :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> IO CULong
budget gkMonteCarloStrategist  =
  sendMsg gkMonteCarloStrategist (mkSelector "budget") retCULong []

-- | The maximum number of samples that will be processed when searching for a move.
--
-- ObjC selector: @- setBudget:@
setBudget :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> CULong -> IO ()
setBudget gkMonteCarloStrategist  value =
  sendMsg gkMonteCarloStrategist (mkSelector "setBudget:") retVoid [argCULong (fromIntegral value)]

-- | A weight that encourages exploration of less visited updates versus the continued exploitation of previously visited updates.
--
-- ObjC selector: @- explorationParameter@
explorationParameter :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> IO CULong
explorationParameter gkMonteCarloStrategist  =
  sendMsg gkMonteCarloStrategist (mkSelector "explorationParameter") retCULong []

-- | A weight that encourages exploration of less visited updates versus the continued exploitation of previously visited updates.
--
-- ObjC selector: @- setExplorationParameter:@
setExplorationParameter :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> CULong -> IO ()
setExplorationParameter gkMonteCarloStrategist  value =
  sendMsg gkMonteCarloStrategist (mkSelector "setExplorationParameter:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @budget@
budgetSelector :: Selector
budgetSelector = mkSelector "budget"

-- | @Selector@ for @setBudget:@
setBudgetSelector :: Selector
setBudgetSelector = mkSelector "setBudget:"

-- | @Selector@ for @explorationParameter@
explorationParameterSelector :: Selector
explorationParameterSelector = mkSelector "explorationParameter"

-- | @Selector@ for @setExplorationParameter:@
setExplorationParameterSelector :: Selector
setExplorationParameterSelector = mkSelector "setExplorationParameter:"

