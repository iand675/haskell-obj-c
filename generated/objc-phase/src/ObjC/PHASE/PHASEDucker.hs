{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDucker
--
-- A PHASEDucker is used to describe ducking behavior across different groups.
--
-- Generated bindings for @PHASEDucker@.
module ObjC.PHASE.PHASEDucker
  ( PHASEDucker
  , IsPHASEDucker(..)
  , init_
  , new
  , initWithEngine_sourceGroups_targetGroups_gain_attackTime_releaseTime_attackCurve_releaseCurve
  , activate
  , deactivate
  , sourceGroups
  , targetGroups
  , active
  , gain
  , attackTime
  , releaseTime
  , attackCurve
  , releaseCurve
  , identifier
  , initSelector
  , newSelector
  , initWithEngine_sourceGroups_targetGroups_gain_attackTime_releaseTime_attackCurve_releaseCurveSelector
  , activateSelector
  , deactivateSelector
  , sourceGroupsSelector
  , targetGroupsSelector
  , activeSelector
  , gainSelector
  , attackTimeSelector
  , releaseTimeSelector
  , attackCurveSelector
  , releaseCurveSelector
  , identifierSelector

  -- * Enum types
  , PHASECurveType(PHASECurveType)
  , pattern PHASECurveTypeLinear
  , pattern PHASECurveTypeSquared
  , pattern PHASECurveTypeInverseSquared
  , pattern PHASECurveTypeCubed
  , pattern PHASECurveTypeInverseCubed
  , pattern PHASECurveTypeSine
  , pattern PHASECurveTypeInverseSine
  , pattern PHASECurveTypeSigmoid
  , pattern PHASECurveTypeInverseSigmoid
  , pattern PHASECurveTypeHoldStartValue
  , pattern PHASECurveTypeJumpToEndValue

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

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEDucker phaseDucker => phaseDucker -> IO (Id PHASEDucker)
init_ phaseDucker  =
  sendMsg phaseDucker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEDucker)
new  =
  do
    cls' <- getRequiredClass "PHASEDucker"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithSourceGroups:targetGroups:attenuation:attackTime:releaseTime:
--
-- Whenever a generator node from any source group plays, all the generator nodes in the target groups will be        ducked by the given gain using the given attack and release times.
--
-- Note: The ducker is initialially inactive. The client must call activate() to make it active.        Once a ducker is active, it will listen for generator nodes to start playback in source groups. Once triggered, it will duck its target groups.        Deactivating a ducker will make it stop listening. Furthermore, it will enter the release phase if it has been previously triggered.        Dealloc'ing a ducker will force the ducker into its release phase if it is actively ducking and remove it from the system when it finishes.
--
-- @engine@ — The engine to register this ducker with.
--
-- @sourceGroups@ — The source groups that will trigger the ducker when a sound in one of the source groups starts playback.
--
-- @targetGroups@ — The target groups that will be ducked when a sound in one of the source groups triggers the ducker.
--
-- @gain@ — The linear gain scalar to apply when the ducker is engaged. 0 means full attenuation. 1 is no attenuation. Values are clamped to the range [0, 1].
--
-- @attackTime@ — The time for the attenuation gain to ramp into effect.        This value is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- @releaseTime@ — The time for the ducked sounds to ramp back to their original level.        This value is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- @attackCurve@ — The type of curve function to use during the attack phase of gain reduction.
--
-- @releaseCurve@ — The type of curve function to use during the release phase of gain reduction.
--
-- ObjC selector: @- initWithEngine:sourceGroups:targetGroups:gain:attackTime:releaseTime:attackCurve:releaseCurve:@
initWithEngine_sourceGroups_targetGroups_gain_attackTime_releaseTime_attackCurve_releaseCurve :: (IsPHASEDucker phaseDucker, IsPHASEEngine engine, IsNSSet sourceGroups, IsNSSet targetGroups) => phaseDucker -> engine -> sourceGroups -> targetGroups -> CDouble -> CDouble -> CDouble -> PHASECurveType -> PHASECurveType -> IO (Id PHASEDucker)
initWithEngine_sourceGroups_targetGroups_gain_attackTime_releaseTime_attackCurve_releaseCurve phaseDucker  engine sourceGroups targetGroups gain attackTime releaseTime attackCurve releaseCurve =
withObjCPtr engine $ \raw_engine ->
  withObjCPtr sourceGroups $ \raw_sourceGroups ->
    withObjCPtr targetGroups $ \raw_targetGroups ->
        sendMsg phaseDucker (mkSelector "initWithEngine:sourceGroups:targetGroups:gain:attackTime:releaseTime:attackCurve:releaseCurve:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argPtr (castPtr raw_sourceGroups :: Ptr ()), argPtr (castPtr raw_targetGroups :: Ptr ()), argCDouble (fromIntegral gain), argCDouble (fromIntegral attackTime), argCDouble (fromIntegral releaseTime), argCLong (coerce attackCurve), argCLong (coerce releaseCurve)] >>= ownedObject . castPtr

-- | activate
--
-- Activates the ducker
--
-- ObjC selector: @- activate@
activate :: IsPHASEDucker phaseDucker => phaseDucker -> IO ()
activate phaseDucker  =
  sendMsg phaseDucker (mkSelector "activate") retVoid []

-- | deactivate
--
-- Deactivates the ducker
--
-- ObjC selector: @- deactivate@
deactivate :: IsPHASEDucker phaseDucker => phaseDucker -> IO ()
deactivate phaseDucker  =
  sendMsg phaseDucker (mkSelector "deactivate") retVoid []

-- | sourceGroups
--
-- The source groups that will trigger the ducker when a sound in one of the source groups starts playback.
--
-- ObjC selector: @- sourceGroups@
sourceGroups :: IsPHASEDucker phaseDucker => phaseDucker -> IO (Id NSSet)
sourceGroups phaseDucker  =
  sendMsg phaseDucker (mkSelector "sourceGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | targetGroups
--
-- The target groups that will be ducked when a sound in one of the source groups triggers the ducker.
--
-- ObjC selector: @- targetGroups@
targetGroups :: IsPHASEDucker phaseDucker => phaseDucker -> IO (Id NSSet)
targetGroups phaseDucker  =
  sendMsg phaseDucker (mkSelector "targetGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | active
--
-- YES if the ducker is active; otherwise, NO.
--
-- ObjC selector: @- active@
active :: IsPHASEDucker phaseDucker => phaseDucker -> IO Bool
active phaseDucker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phaseDucker (mkSelector "active") retCULong []

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEDucker phaseDucker => phaseDucker -> IO CDouble
gain phaseDucker  =
  sendMsg phaseDucker (mkSelector "gain") retCDouble []

-- | attackTime
--
-- The time for the attenuation gain to ramp into effect.
--
-- Note: The attack time is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- attackTime@
attackTime :: IsPHASEDucker phaseDucker => phaseDucker -> IO CDouble
attackTime phaseDucker  =
  sendMsg phaseDucker (mkSelector "attackTime") retCDouble []

-- | releaseTime
--
-- The time for the ducked sounds to ramp back to their original level.
--
-- Note: The release time is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- ObjC selector: @- releaseTime@
releaseTime :: IsPHASEDucker phaseDucker => phaseDucker -> IO CDouble
releaseTime phaseDucker  =
  sendMsg phaseDucker (mkSelector "releaseTime") retCDouble []

-- | attackCurve
--
-- The type of curve function to use during the attack phase of gain reduction.
--
-- ObjC selector: @- attackCurve@
attackCurve :: IsPHASEDucker phaseDucker => phaseDucker -> IO PHASECurveType
attackCurve phaseDucker  =
  fmap (coerce :: CLong -> PHASECurveType) $ sendMsg phaseDucker (mkSelector "attackCurve") retCLong []

-- | releaseCurve
--
-- The type of curve function to use during the release phase of gain reduction.
--
-- ObjC selector: @- releaseCurve@
releaseCurve :: IsPHASEDucker phaseDucker => phaseDucker -> IO PHASECurveType
releaseCurve phaseDucker  =
  fmap (coerce :: CLong -> PHASECurveType) $ sendMsg phaseDucker (mkSelector "releaseCurve") retCLong []

-- | identifier
--
-- The identifier that uniquely represents this ducker.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEDucker phaseDucker => phaseDucker -> IO (Id NSString)
identifier phaseDucker  =
  sendMsg phaseDucker (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:sourceGroups:targetGroups:gain:attackTime:releaseTime:attackCurve:releaseCurve:@
initWithEngine_sourceGroups_targetGroups_gain_attackTime_releaseTime_attackCurve_releaseCurveSelector :: Selector
initWithEngine_sourceGroups_targetGroups_gain_attackTime_releaseTime_attackCurve_releaseCurveSelector = mkSelector "initWithEngine:sourceGroups:targetGroups:gain:attackTime:releaseTime:attackCurve:releaseCurve:"

-- | @Selector@ for @activate@
activateSelector :: Selector
activateSelector = mkSelector "activate"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @sourceGroups@
sourceGroupsSelector :: Selector
sourceGroupsSelector = mkSelector "sourceGroups"

-- | @Selector@ for @targetGroups@
targetGroupsSelector :: Selector
targetGroupsSelector = mkSelector "targetGroups"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @attackTime@
attackTimeSelector :: Selector
attackTimeSelector = mkSelector "attackTime"

-- | @Selector@ for @releaseTime@
releaseTimeSelector :: Selector
releaseTimeSelector = mkSelector "releaseTime"

-- | @Selector@ for @attackCurve@
attackCurveSelector :: Selector
attackCurveSelector = mkSelector "attackCurve"

-- | @Selector@ for @releaseCurve@
releaseCurveSelector :: Selector
releaseCurveSelector = mkSelector "releaseCurve"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

