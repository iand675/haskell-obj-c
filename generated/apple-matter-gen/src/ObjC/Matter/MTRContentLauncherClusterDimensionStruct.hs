{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterDimensionStruct@.
module ObjC.Matter.MTRContentLauncherClusterDimensionStruct
  ( MTRContentLauncherClusterDimensionStruct
  , IsMTRContentLauncherClusterDimensionStruct(..)
  , width
  , setWidth
  , height
  , setHeight
  , metric
  , setMetric
  , heightSelector
  , metricSelector
  , setHeightSelector
  , setMetricSelector
  , setWidthSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- width@
width :: IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct => mtrContentLauncherClusterDimensionStruct -> IO (Id NSNumber)
width mtrContentLauncherClusterDimensionStruct =
  sendMessage mtrContentLauncherClusterDimensionStruct widthSelector

-- | @- setWidth:@
setWidth :: (IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct, IsNSNumber value) => mtrContentLauncherClusterDimensionStruct -> value -> IO ()
setWidth mtrContentLauncherClusterDimensionStruct value =
  sendMessage mtrContentLauncherClusterDimensionStruct setWidthSelector (toNSNumber value)

-- | @- height@
height :: IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct => mtrContentLauncherClusterDimensionStruct -> IO (Id NSNumber)
height mtrContentLauncherClusterDimensionStruct =
  sendMessage mtrContentLauncherClusterDimensionStruct heightSelector

-- | @- setHeight:@
setHeight :: (IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct, IsNSNumber value) => mtrContentLauncherClusterDimensionStruct -> value -> IO ()
setHeight mtrContentLauncherClusterDimensionStruct value =
  sendMessage mtrContentLauncherClusterDimensionStruct setHeightSelector (toNSNumber value)

-- | @- metric@
metric :: IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct => mtrContentLauncherClusterDimensionStruct -> IO (Id NSNumber)
metric mtrContentLauncherClusterDimensionStruct =
  sendMessage mtrContentLauncherClusterDimensionStruct metricSelector

-- | @- setMetric:@
setMetric :: (IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct, IsNSNumber value) => mtrContentLauncherClusterDimensionStruct -> value -> IO ()
setMetric mtrContentLauncherClusterDimensionStruct value =
  sendMessage mtrContentLauncherClusterDimensionStruct setMetricSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSNumber)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSNumber] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSNumber)
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[Id NSNumber] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @metric@
metricSelector :: Selector '[] (Id NSNumber)
metricSelector = mkSelector "metric"

-- | @Selector@ for @setMetric:@
setMetricSelector :: Selector '[Id NSNumber] ()
setMetricSelector = mkSelector "setMetric:"

