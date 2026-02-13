{-# LANGUAGE DataKinds #-}
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
  , explorationParameterSelector
  , setBudgetSelector
  , setExplorationParameterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The maximum number of samples that will be processed when searching for a move.
--
-- ObjC selector: @- budget@
budget :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> IO CULong
budget gkMonteCarloStrategist =
  sendMessage gkMonteCarloStrategist budgetSelector

-- | The maximum number of samples that will be processed when searching for a move.
--
-- ObjC selector: @- setBudget:@
setBudget :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> CULong -> IO ()
setBudget gkMonteCarloStrategist value =
  sendMessage gkMonteCarloStrategist setBudgetSelector value

-- | A weight that encourages exploration of less visited updates versus the continued exploitation of previously visited updates.
--
-- ObjC selector: @- explorationParameter@
explorationParameter :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> IO CULong
explorationParameter gkMonteCarloStrategist =
  sendMessage gkMonteCarloStrategist explorationParameterSelector

-- | A weight that encourages exploration of less visited updates versus the continued exploitation of previously visited updates.
--
-- ObjC selector: @- setExplorationParameter:@
setExplorationParameter :: IsGKMonteCarloStrategist gkMonteCarloStrategist => gkMonteCarloStrategist -> CULong -> IO ()
setExplorationParameter gkMonteCarloStrategist value =
  sendMessage gkMonteCarloStrategist setExplorationParameterSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @budget@
budgetSelector :: Selector '[] CULong
budgetSelector = mkSelector "budget"

-- | @Selector@ for @setBudget:@
setBudgetSelector :: Selector '[CULong] ()
setBudgetSelector = mkSelector "setBudget:"

-- | @Selector@ for @explorationParameter@
explorationParameterSelector :: Selector '[] CULong
explorationParameterSelector = mkSelector "explorationParameter"

-- | @Selector@ for @setExplorationParameter:@
setExplorationParameterSelector :: Selector '[CULong] ()
setExplorationParameterSelector = mkSelector "setExplorationParameter:"

