{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWristDetection@.
module ObjC.SensorKit.SRWristDetection
  ( SRWristDetection
  , IsSRWristDetection(..)
  , onWrist
  , wristLocation
  , crownOrientation
  , onWristDate
  , offWristDate
  , crownOrientationSelector
  , offWristDateSelector
  , onWristDateSelector
  , onWristSelector
  , wristLocationSelector

  -- * Enum types
  , SRCrownOrientation(SRCrownOrientation)
  , pattern SRCrownOrientationLeft
  , pattern SRCrownOrientationRight
  , SRWristLocation(SRWristLocation)
  , pattern SRWristLocationLeft
  , pattern SRWristLocationRight

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

-- | @- onWrist@
onWrist :: IsSRWristDetection srWristDetection => srWristDetection -> IO Bool
onWrist srWristDetection =
  sendMessage srWristDetection onWristSelector

-- | @- wristLocation@
wristLocation :: IsSRWristDetection srWristDetection => srWristDetection -> IO SRWristLocation
wristLocation srWristDetection =
  sendMessage srWristDetection wristLocationSelector

-- | @- crownOrientation@
crownOrientation :: IsSRWristDetection srWristDetection => srWristDetection -> IO SRCrownOrientation
crownOrientation srWristDetection =
  sendMessage srWristDetection crownOrientationSelector

-- | onWristDate
--
-- Start date of the recent on-wrist state.
--
-- - When the state changes from off-wrist to on-wrist, onWristDate would be updated to the current date, and offWristDate would remain the same. - When the state changes from on-wrist to off-wrist, offWristDate would be updated to the current date, and onWristDate would remain the same.
--
-- ObjC selector: @- onWristDate@
onWristDate :: IsSRWristDetection srWristDetection => srWristDetection -> IO (Id NSDate)
onWristDate srWristDetection =
  sendMessage srWristDetection onWristDateSelector

-- | offWristDate
--
-- Start date of the recent off-wrist state.
--
-- - When the state changes from off-wrist to on-wrist, onWristDate would be updated to the current date, and offWristDate would remain the same. - When the state changes from on-wrist to off-wrist, offWristDate would be updated to the current date, and onWristDate would remain the same.
--
-- ObjC selector: @- offWristDate@
offWristDate :: IsSRWristDetection srWristDetection => srWristDetection -> IO (Id NSDate)
offWristDate srWristDetection =
  sendMessage srWristDetection offWristDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @onWrist@
onWristSelector :: Selector '[] Bool
onWristSelector = mkSelector "onWrist"

-- | @Selector@ for @wristLocation@
wristLocationSelector :: Selector '[] SRWristLocation
wristLocationSelector = mkSelector "wristLocation"

-- | @Selector@ for @crownOrientation@
crownOrientationSelector :: Selector '[] SRCrownOrientation
crownOrientationSelector = mkSelector "crownOrientation"

-- | @Selector@ for @onWristDate@
onWristDateSelector :: Selector '[] (Id NSDate)
onWristDateSelector = mkSelector "onWristDate"

-- | @Selector@ for @offWristDate@
offWristDateSelector :: Selector '[] (Id NSDate)
offWristDateSelector = mkSelector "offWristDate"

