{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterUnitRangeStruct@.
module ObjC.Matter.MTRClosureDimensionClusterUnitRangeStruct
  ( MTRClosureDimensionClusterUnitRangeStruct
  , IsMTRClosureDimensionClusterUnitRangeStruct(..)
  , min_
  , setMin
  , max_
  , setMax
  , maxSelector
  , minSelector
  , setMaxSelector
  , setMinSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- min@
min_ :: IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct => mtrClosureDimensionClusterUnitRangeStruct -> IO (Id NSNumber)
min_ mtrClosureDimensionClusterUnitRangeStruct =
  sendMessage mtrClosureDimensionClusterUnitRangeStruct minSelector

-- | @- setMin:@
setMin :: (IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct, IsNSNumber value) => mtrClosureDimensionClusterUnitRangeStruct -> value -> IO ()
setMin mtrClosureDimensionClusterUnitRangeStruct value =
  sendMessage mtrClosureDimensionClusterUnitRangeStruct setMinSelector (toNSNumber value)

-- | @- max@
max_ :: IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct => mtrClosureDimensionClusterUnitRangeStruct -> IO (Id NSNumber)
max_ mtrClosureDimensionClusterUnitRangeStruct =
  sendMessage mtrClosureDimensionClusterUnitRangeStruct maxSelector

-- | @- setMax:@
setMax :: (IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct, IsNSNumber value) => mtrClosureDimensionClusterUnitRangeStruct -> value -> IO ()
setMax mtrClosureDimensionClusterUnitRangeStruct value =
  sendMessage mtrClosureDimensionClusterUnitRangeStruct setMaxSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @min@
minSelector :: Selector '[] (Id NSNumber)
minSelector = mkSelector "min"

-- | @Selector@ for @setMin:@
setMinSelector :: Selector '[Id NSNumber] ()
setMinSelector = mkSelector "setMin:"

-- | @Selector@ for @max@
maxSelector :: Selector '[] (Id NSNumber)
maxSelector = mkSelector "max"

-- | @Selector@ for @setMax:@
setMaxSelector :: Selector '[Id NSNumber] ()
setMaxSelector = mkSelector "setMax:"

