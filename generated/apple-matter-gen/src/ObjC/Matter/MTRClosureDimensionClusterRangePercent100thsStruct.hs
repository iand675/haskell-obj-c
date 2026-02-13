{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterRangePercent100thsStruct@.
module ObjC.Matter.MTRClosureDimensionClusterRangePercent100thsStruct
  ( MTRClosureDimensionClusterRangePercent100thsStruct
  , IsMTRClosureDimensionClusterRangePercent100thsStruct(..)
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
min_ :: IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct => mtrClosureDimensionClusterRangePercent100thsStruct -> IO (Id NSNumber)
min_ mtrClosureDimensionClusterRangePercent100thsStruct =
  sendMessage mtrClosureDimensionClusterRangePercent100thsStruct minSelector

-- | @- setMin:@
setMin :: (IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct, IsNSNumber value) => mtrClosureDimensionClusterRangePercent100thsStruct -> value -> IO ()
setMin mtrClosureDimensionClusterRangePercent100thsStruct value =
  sendMessage mtrClosureDimensionClusterRangePercent100thsStruct setMinSelector (toNSNumber value)

-- | @- max@
max_ :: IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct => mtrClosureDimensionClusterRangePercent100thsStruct -> IO (Id NSNumber)
max_ mtrClosureDimensionClusterRangePercent100thsStruct =
  sendMessage mtrClosureDimensionClusterRangePercent100thsStruct maxSelector

-- | @- setMax:@
setMax :: (IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct, IsNSNumber value) => mtrClosureDimensionClusterRangePercent100thsStruct -> value -> IO ()
setMax mtrClosureDimensionClusterRangePercent100thsStruct value =
  sendMessage mtrClosureDimensionClusterRangePercent100thsStruct setMaxSelector (toNSNumber value)

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

