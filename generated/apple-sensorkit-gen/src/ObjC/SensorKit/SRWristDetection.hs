{-# LANGUAGE PatternSynonyms #-}
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
  , onWristSelector
  , wristLocationSelector
  , crownOrientationSelector
  , onWristDateSelector
  , offWristDateSelector

  -- * Enum types
  , SRCrownOrientation(SRCrownOrientation)
  , pattern SRCrownOrientationLeft
  , pattern SRCrownOrientationRight
  , SRWristLocation(SRWristLocation)
  , pattern SRWristLocationLeft
  , pattern SRWristLocationRight

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- onWrist@
onWrist :: IsSRWristDetection srWristDetection => srWristDetection -> IO Bool
onWrist srWristDetection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srWristDetection (mkSelector "onWrist") retCULong []

-- | @- wristLocation@
wristLocation :: IsSRWristDetection srWristDetection => srWristDetection -> IO SRWristLocation
wristLocation srWristDetection  =
    fmap (coerce :: CLong -> SRWristLocation) $ sendMsg srWristDetection (mkSelector "wristLocation") retCLong []

-- | @- crownOrientation@
crownOrientation :: IsSRWristDetection srWristDetection => srWristDetection -> IO SRCrownOrientation
crownOrientation srWristDetection  =
    fmap (coerce :: CLong -> SRCrownOrientation) $ sendMsg srWristDetection (mkSelector "crownOrientation") retCLong []

-- | onWristDate
--
-- Start date of the recent on-wrist state.
--
-- - When the state changes from off-wrist to on-wrist, onWristDate would be updated to the current date, and offWristDate would remain the same. - When the state changes from on-wrist to off-wrist, offWristDate would be updated to the current date, and onWristDate would remain the same.
--
-- ObjC selector: @- onWristDate@
onWristDate :: IsSRWristDetection srWristDetection => srWristDetection -> IO (Id NSDate)
onWristDate srWristDetection  =
    sendMsg srWristDetection (mkSelector "onWristDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | offWristDate
--
-- Start date of the recent off-wrist state.
--
-- - When the state changes from off-wrist to on-wrist, onWristDate would be updated to the current date, and offWristDate would remain the same. - When the state changes from on-wrist to off-wrist, offWristDate would be updated to the current date, and onWristDate would remain the same.
--
-- ObjC selector: @- offWristDate@
offWristDate :: IsSRWristDetection srWristDetection => srWristDetection -> IO (Id NSDate)
offWristDate srWristDetection  =
    sendMsg srWristDetection (mkSelector "offWristDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @onWrist@
onWristSelector :: Selector
onWristSelector = mkSelector "onWrist"

-- | @Selector@ for @wristLocation@
wristLocationSelector :: Selector
wristLocationSelector = mkSelector "wristLocation"

-- | @Selector@ for @crownOrientation@
crownOrientationSelector :: Selector
crownOrientationSelector = mkSelector "crownOrientation"

-- | @Selector@ for @onWristDate@
onWristDateSelector :: Selector
onWristDateSelector = mkSelector "onWristDate"

-- | @Selector@ for @offWristDate@
offWristDateSelector :: Selector
offWristDateSelector = mkSelector "offWristDate"

