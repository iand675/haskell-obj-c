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
  , initSelector
  , newSelector
  , conditionSelector
  , lastEventSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLMonitoringRecord clMonitoringRecord => clMonitoringRecord -> IO (Id CLMonitoringRecord)
init_ clMonitoringRecord  =
  sendMsg clMonitoringRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLMonitoringRecord)
new  =
  do
    cls' <- getRequiredClass "CLMonitoringRecord"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- condition@
condition :: IsCLMonitoringRecord clMonitoringRecord => clMonitoringRecord -> IO (Id CLCondition)
condition clMonitoringRecord  =
  sendMsg clMonitoringRecord (mkSelector "condition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastEvent@
lastEvent :: IsCLMonitoringRecord clMonitoringRecord => clMonitoringRecord -> IO (Id CLMonitoringEvent)
lastEvent clMonitoringRecord  =
  sendMsg clMonitoringRecord (mkSelector "lastEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @condition@
conditionSelector :: Selector
conditionSelector = mkSelector "condition"

-- | @Selector@ for @lastEvent@
lastEventSelector :: Selector
lastEventSelector = mkSelector "lastEvent"

