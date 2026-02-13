{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCSteeringWheelElement@.
module ObjC.GameController.GCSteeringWheelElement
  ( GCSteeringWheelElement
  , IsGCSteeringWheelElement(..)
  , init_
  , maximumDegreesOfRotation
  , initSelector
  , maximumDegreesOfRotationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCSteeringWheelElement gcSteeringWheelElement => gcSteeringWheelElement -> IO (Id GCSteeringWheelElement)
init_ gcSteeringWheelElement =
  sendOwnedMessage gcSteeringWheelElement initSelector

-- | @- maximumDegreesOfRotation@
maximumDegreesOfRotation :: IsGCSteeringWheelElement gcSteeringWheelElement => gcSteeringWheelElement -> IO CFloat
maximumDegreesOfRotation gcSteeringWheelElement =
  sendMessage gcSteeringWheelElement maximumDegreesOfRotationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCSteeringWheelElement)
initSelector = mkSelector "init"

-- | @Selector@ for @maximumDegreesOfRotation@
maximumDegreesOfRotationSelector :: Selector '[] CFloat
maximumDegreesOfRotationSelector = mkSelector "maximumDegreesOfRotation"

