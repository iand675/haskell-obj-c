{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTouch@.
module ObjC.AppKit.NSTouch
  ( NSTouch
  , IsNSTouch(..)
  , locationInView
  , previousLocationInView
  , identity
  , phase
  , normalizedPosition
  , resting
  , device
  , deviceSize
  , type_
  , deviceSelector
  , deviceSizeSelector
  , identitySelector
  , locationInViewSelector
  , normalizedPositionSelector
  , phaseSelector
  , previousLocationInViewSelector
  , restingSelector
  , typeSelector

  -- * Enum types
  , NSTouchPhase(NSTouchPhase)
  , pattern NSTouchPhaseBegan
  , pattern NSTouchPhaseMoved
  , pattern NSTouchPhaseStationary
  , pattern NSTouchPhaseEnded
  , pattern NSTouchPhaseCancelled
  , pattern NSTouchPhaseTouching
  , pattern NSTouchPhaseAny
  , NSTouchType(NSTouchType)
  , pattern NSTouchTypeDirect
  , pattern NSTouchTypeIndirect

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- locationInView:@
locationInView :: (IsNSTouch nsTouch, IsNSView view) => nsTouch -> view -> IO NSPoint
locationInView nsTouch view =
  sendMessage nsTouch locationInViewSelector (toNSView view)

-- | @- previousLocationInView:@
previousLocationInView :: (IsNSTouch nsTouch, IsNSView view) => nsTouch -> view -> IO NSPoint
previousLocationInView nsTouch view =
  sendMessage nsTouch previousLocationInViewSelector (toNSView view)

-- | @- identity@
identity :: IsNSTouch nsTouch => nsTouch -> IO RawId
identity nsTouch =
  sendMessage nsTouch identitySelector

-- | @- phase@
phase :: IsNSTouch nsTouch => nsTouch -> IO NSTouchPhase
phase nsTouch =
  sendMessage nsTouch phaseSelector

-- | @- normalizedPosition@
normalizedPosition :: IsNSTouch nsTouch => nsTouch -> IO NSPoint
normalizedPosition nsTouch =
  sendMessage nsTouch normalizedPositionSelector

-- | @- resting@
resting :: IsNSTouch nsTouch => nsTouch -> IO Bool
resting nsTouch =
  sendMessage nsTouch restingSelector

-- | @- device@
device :: IsNSTouch nsTouch => nsTouch -> IO RawId
device nsTouch =
  sendMessage nsTouch deviceSelector

-- | @- deviceSize@
deviceSize :: IsNSTouch nsTouch => nsTouch -> IO NSSize
deviceSize nsTouch =
  sendMessage nsTouch deviceSizeSelector

-- | @- type@
type_ :: IsNSTouch nsTouch => nsTouch -> IO NSTouchType
type_ nsTouch =
  sendMessage nsTouch typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationInView:@
locationInViewSelector :: Selector '[Id NSView] NSPoint
locationInViewSelector = mkSelector "locationInView:"

-- | @Selector@ for @previousLocationInView:@
previousLocationInViewSelector :: Selector '[Id NSView] NSPoint
previousLocationInViewSelector = mkSelector "previousLocationInView:"

-- | @Selector@ for @identity@
identitySelector :: Selector '[] RawId
identitySelector = mkSelector "identity"

-- | @Selector@ for @phase@
phaseSelector :: Selector '[] NSTouchPhase
phaseSelector = mkSelector "phase"

-- | @Selector@ for @normalizedPosition@
normalizedPositionSelector :: Selector '[] NSPoint
normalizedPositionSelector = mkSelector "normalizedPosition"

-- | @Selector@ for @resting@
restingSelector :: Selector '[] Bool
restingSelector = mkSelector "resting"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @deviceSize@
deviceSizeSelector :: Selector '[] NSSize
deviceSizeSelector = mkSelector "deviceSize"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSTouchType
typeSelector = mkSelector "type"

