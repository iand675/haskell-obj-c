{-# LANGUAGE PatternSynonyms #-}
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
  , locationInViewSelector
  , previousLocationInViewSelector
  , identitySelector
  , phaseSelector
  , normalizedPositionSelector
  , restingSelector
  , deviceSelector
  , deviceSizeSelector
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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- locationInView:@
locationInView :: (IsNSTouch nsTouch, IsNSView view) => nsTouch -> view -> IO NSPoint
locationInView nsTouch  view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsTouch (mkSelector "locationInView:") retNSPoint [argPtr (castPtr raw_view :: Ptr ())]

-- | @- previousLocationInView:@
previousLocationInView :: (IsNSTouch nsTouch, IsNSView view) => nsTouch -> view -> IO NSPoint
previousLocationInView nsTouch  view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsTouch (mkSelector "previousLocationInView:") retNSPoint [argPtr (castPtr raw_view :: Ptr ())]

-- | @- identity@
identity :: IsNSTouch nsTouch => nsTouch -> IO RawId
identity nsTouch  =
    fmap (RawId . castPtr) $ sendMsg nsTouch (mkSelector "identity") (retPtr retVoid) []

-- | @- phase@
phase :: IsNSTouch nsTouch => nsTouch -> IO NSTouchPhase
phase nsTouch  =
    fmap (coerce :: CULong -> NSTouchPhase) $ sendMsg nsTouch (mkSelector "phase") retCULong []

-- | @- normalizedPosition@
normalizedPosition :: IsNSTouch nsTouch => nsTouch -> IO NSPoint
normalizedPosition nsTouch  =
    sendMsgStret nsTouch (mkSelector "normalizedPosition") retNSPoint []

-- | @- resting@
resting :: IsNSTouch nsTouch => nsTouch -> IO Bool
resting nsTouch  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTouch (mkSelector "resting") retCULong []

-- | @- device@
device :: IsNSTouch nsTouch => nsTouch -> IO RawId
device nsTouch  =
    fmap (RawId . castPtr) $ sendMsg nsTouch (mkSelector "device") (retPtr retVoid) []

-- | @- deviceSize@
deviceSize :: IsNSTouch nsTouch => nsTouch -> IO NSSize
deviceSize nsTouch  =
    sendMsgStret nsTouch (mkSelector "deviceSize") retNSSize []

-- | @- type@
type_ :: IsNSTouch nsTouch => nsTouch -> IO NSTouchType
type_ nsTouch  =
    fmap (coerce :: CLong -> NSTouchType) $ sendMsg nsTouch (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationInView:@
locationInViewSelector :: Selector
locationInViewSelector = mkSelector "locationInView:"

-- | @Selector@ for @previousLocationInView:@
previousLocationInViewSelector :: Selector
previousLocationInViewSelector = mkSelector "previousLocationInView:"

-- | @Selector@ for @identity@
identitySelector :: Selector
identitySelector = mkSelector "identity"

-- | @Selector@ for @phase@
phaseSelector :: Selector
phaseSelector = mkSelector "phase"

-- | @Selector@ for @normalizedPosition@
normalizedPositionSelector :: Selector
normalizedPositionSelector = mkSelector "normalizedPosition"

-- | @Selector@ for @resting@
restingSelector :: Selector
restingSelector = mkSelector "resting"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @deviceSize@
deviceSizeSelector :: Selector
deviceSizeSelector = mkSelector "deviceSize"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

