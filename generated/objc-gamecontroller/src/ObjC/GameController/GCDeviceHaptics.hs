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
  , initSelector
  , createEngineWithLocalitySelector
  , supportedLocalitiesSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCDeviceHaptics gcDeviceHaptics => gcDeviceHaptics -> IO (Id GCDeviceHaptics)
init_ gcDeviceHaptics  =
  sendMsg gcDeviceHaptics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
createEngineWithLocality gcDeviceHaptics  locality =
withObjCPtr locality $ \raw_locality ->
    sendMsg gcDeviceHaptics (mkSelector "createEngineWithLocality:") (retPtr retVoid) [argPtr (castPtr raw_locality :: Ptr ())] >>= retainedObject . castPtr

-- | The set of supported haptic localities for this device - representing the locations of its haptic actuators.
--
-- Note: GCHapticsLocalityDefault and GCHapticsLocalityAll are guaranteed to be supported - and they may be equivalent.
--
-- See: GCHapticsLocality
--
-- ObjC selector: @- supportedLocalities@
supportedLocalities :: IsGCDeviceHaptics gcDeviceHaptics => gcDeviceHaptics -> IO (Id NSSet)
supportedLocalities gcDeviceHaptics  =
  sendMsg gcDeviceHaptics (mkSelector "supportedLocalities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @createEngineWithLocality:@
createEngineWithLocalitySelector :: Selector
createEngineWithLocalitySelector = mkSelector "createEngineWithLocality:"

-- | @Selector@ for @supportedLocalities@
supportedLocalitiesSelector :: Selector
supportedLocalitiesSelector = mkSelector "supportedLocalities"

