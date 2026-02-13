{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLMonitoringRecord@.
module ObjC.CoreLocation.CLMonitoringRecord
  ( CLMonitoringRecord
  , IsCLMonitoringRecord(..)
  , init_
  , new
  , condition
  , lastEvent
  , conditionSelector
  , initSelector
  , lastEventSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLMonitoringRecord clMonitoringRecord => clMonitoringRecord -> IO (Id CLMonitoringRecord)
init_ clMonitoringRecord =
  sendOwnedMessage clMonitoringRecord initSelector

-- | @+ new@
new :: IO (Id CLMonitoringRecord)
new  =
  do
    cls' <- getRequiredClass "CLMonitoringRecord"
    sendOwnedClassMessage cls' newSelector

-- | @- condition@
condition :: IsCLMonitoringRecord clMonitoringRecord => clMonitoringRecord -> IO (Id CLCondition)
condition clMonitoringRecord =
  sendMessage clMonitoringRecord conditionSelector

-- | @- lastEvent@
lastEvent :: IsCLMonitoringRecord clMonitoringRecord => clMonitoringRecord -> IO (Id CLMonitoringEvent)
lastEvent clMonitoringRecord =
  sendMessage clMonitoringRecord lastEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLMonitoringRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLMonitoringRecord)
newSelector = mkSelector "new"

-- | @Selector@ for @condition@
conditionSelector :: Selector '[] (Id CLCondition)
conditionSelector = mkSelector "condition"

-- | @Selector@ for @lastEvent@
lastEventSelector :: Selector '[] (Id CLMonitoringEvent)
lastEventSelector = mkSelector "lastEvent"

