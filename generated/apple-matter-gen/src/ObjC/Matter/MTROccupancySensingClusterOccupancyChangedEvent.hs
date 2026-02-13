{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROccupancySensingClusterOccupancyChangedEvent@.
module ObjC.Matter.MTROccupancySensingClusterOccupancyChangedEvent
  ( MTROccupancySensingClusterOccupancyChangedEvent
  , IsMTROccupancySensingClusterOccupancyChangedEvent(..)
  , occupancy
  , setOccupancy
  , occupancySelector
  , setOccupancySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- occupancy@
occupancy :: IsMTROccupancySensingClusterOccupancyChangedEvent mtrOccupancySensingClusterOccupancyChangedEvent => mtrOccupancySensingClusterOccupancyChangedEvent -> IO (Id NSNumber)
occupancy mtrOccupancySensingClusterOccupancyChangedEvent =
  sendMessage mtrOccupancySensingClusterOccupancyChangedEvent occupancySelector

-- | @- setOccupancy:@
setOccupancy :: (IsMTROccupancySensingClusterOccupancyChangedEvent mtrOccupancySensingClusterOccupancyChangedEvent, IsNSNumber value) => mtrOccupancySensingClusterOccupancyChangedEvent -> value -> IO ()
setOccupancy mtrOccupancySensingClusterOccupancyChangedEvent value =
  sendMessage mtrOccupancySensingClusterOccupancyChangedEvent setOccupancySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @occupancy@
occupancySelector :: Selector '[] (Id NSNumber)
occupancySelector = mkSelector "occupancy"

-- | @Selector@ for @setOccupancy:@
setOccupancySelector :: Selector '[Id NSNumber] ()
setOccupancySelector = mkSelector "setOccupancy:"

