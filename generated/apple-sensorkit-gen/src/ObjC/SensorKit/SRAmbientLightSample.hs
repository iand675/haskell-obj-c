{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAmbientLightSample@.
module ObjC.SensorKit.SRAmbientLightSample
  ( SRAmbientLightSample
  , IsSRAmbientLightSample(..)
  , placement
  , lux
  , luxSelector
  , placementSelector

  -- * Enum types
  , SRAmbientLightSensorPlacement(SRAmbientLightSensorPlacement)
  , pattern SRAmbientLightSensorPlacementUnknown
  , pattern SRAmbientLightSensorPlacementFrontTop
  , pattern SRAmbientLightSensorPlacementFrontBottom
  , pattern SRAmbientLightSensorPlacementFrontRight
  , pattern SRAmbientLightSensorPlacementFrontLeft
  , pattern SRAmbientLightSensorPlacementFrontTopRight
  , pattern SRAmbientLightSensorPlacementFrontTopLeft
  , pattern SRAmbientLightSensorPlacementFrontBottomRight
  , pattern SRAmbientLightSensorPlacementFrontBottomLeft

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- placement@
placement :: IsSRAmbientLightSample srAmbientLightSample => srAmbientLightSample -> IO SRAmbientLightSensorPlacement
placement srAmbientLightSample =
  sendMessage srAmbientLightSample placementSelector

-- | @- lux@
lux :: IsSRAmbientLightSample srAmbientLightSample => srAmbientLightSample -> IO (Id NSMeasurement)
lux srAmbientLightSample =
  sendMessage srAmbientLightSample luxSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @placement@
placementSelector :: Selector '[] SRAmbientLightSensorPlacement
placementSelector = mkSelector "placement"

-- | @Selector@ for @lux@
luxSelector :: Selector '[] (Id NSMeasurement)
luxSelector = mkSelector "lux"

