{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCDeviceHaptics@.
module ObjC.GameController.GCDeviceHaptics
  ( GCDeviceHaptics
  , IsGCDeviceHaptics(..)
  , init_
  , createEngineWithLocality
  , supportedLocalities
  , createEngineWithLocalitySelector
  , initSelector
  , supportedLocalitiesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCDeviceHaptics gcDeviceHaptics => gcDeviceHaptics -> IO (Id GCDeviceHaptics)
init_ gcDeviceHaptics =
  sendOwnedMessage gcDeviceHaptics initSelector

-- | Creates and returns a new instance of CHHapticEngine with a given GCHapticsLocality. Any patterns you send to this engine will play on all specified actuators.
--
-- Note: Often times, it is best to use GCHapticsLocalityDefault. Engines created with the default locality will give users an expected haptic experience. On most game controllers, this will cause your haptic patterns to play on the handles. If you want to play different experiences on different actuators (for example, using the left handle actuator as a woofer and the right actuator as a tweeter), you can create multiple engines (for example, one with a GCHapticsLocalityLeftHandle locality and another with a GCHapticsLocalityRightHandle locality).
--
-- See: CHHapticEngine
--
-- See: GCHapticsLocality
--
-- ObjC selector: @- createEngineWithLocality:@
createEngineWithLocality :: (IsGCDeviceHaptics gcDeviceHaptics, IsNSString locality) => gcDeviceHaptics -> locality -> IO (Id CHHapticEngine)
createEngineWithLocality gcDeviceHaptics locality =
  sendMessage gcDeviceHaptics createEngineWithLocalitySelector (toNSString locality)

-- | The set of supported haptic localities for this device - representing the locations of its haptic actuators.
--
-- Note: GCHapticsLocalityDefault and GCHapticsLocalityAll are guaranteed to be supported - and they may be equivalent.
--
-- See: GCHapticsLocality
--
-- ObjC selector: @- supportedLocalities@
supportedLocalities :: IsGCDeviceHaptics gcDeviceHaptics => gcDeviceHaptics -> IO (Id NSSet)
supportedLocalities gcDeviceHaptics =
  sendMessage gcDeviceHaptics supportedLocalitiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCDeviceHaptics)
initSelector = mkSelector "init"

-- | @Selector@ for @createEngineWithLocality:@
createEngineWithLocalitySelector :: Selector '[Id NSString] (Id CHHapticEngine)
createEngineWithLocalitySelector = mkSelector "createEngineWithLocality:"

-- | @Selector@ for @supportedLocalities@
supportedLocalitiesSelector :: Selector '[] (Id NSSet)
supportedLocalitiesSelector = mkSelector "supportedLocalities"

