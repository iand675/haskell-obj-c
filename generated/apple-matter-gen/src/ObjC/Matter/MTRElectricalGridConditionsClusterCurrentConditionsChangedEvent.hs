{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalGridConditionsClusterCurrentConditionsChangedEvent@.
module ObjC.Matter.MTRElectricalGridConditionsClusterCurrentConditionsChangedEvent
  ( MTRElectricalGridConditionsClusterCurrentConditionsChangedEvent
  , IsMTRElectricalGridConditionsClusterCurrentConditionsChangedEvent(..)
  , currentConditions
  , setCurrentConditions
  , currentConditionsSelector
  , setCurrentConditionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- currentConditions@
currentConditions :: IsMTRElectricalGridConditionsClusterCurrentConditionsChangedEvent mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent => mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent -> IO (Id MTRElectricalGridConditionsClusterElectricalGridConditionsStruct)
currentConditions mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent =
  sendMessage mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent currentConditionsSelector

-- | @- setCurrentConditions:@
setCurrentConditions :: (IsMTRElectricalGridConditionsClusterCurrentConditionsChangedEvent mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent, IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct value) => mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent -> value -> IO ()
setCurrentConditions mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent value =
  sendMessage mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent setCurrentConditionsSelector (toMTRElectricalGridConditionsClusterElectricalGridConditionsStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentConditions@
currentConditionsSelector :: Selector '[] (Id MTRElectricalGridConditionsClusterElectricalGridConditionsStruct)
currentConditionsSelector = mkSelector "currentConditions"

-- | @Selector@ for @setCurrentConditions:@
setCurrentConditionsSelector :: Selector '[Id MTRElectricalGridConditionsClusterElectricalGridConditionsStruct] ()
setCurrentConditionsSelector = mkSelector "setCurrentConditions:"

