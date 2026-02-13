{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEListener
--
-- A PHASEListener represents the listener's point of view within the simulated acoustic scene.
--
-- Generated bindings for @PHASEListener@.
module ObjC.PHASE.PHASEListener
  ( PHASEListener
  , IsPHASEListener(..)
  , init_
  , new
  , initWithEngine
  , gain
  , setGain
  , automaticHeadTrackingFlags
  , setAutomaticHeadTrackingFlags
  , automaticHeadTrackingFlagsSelector
  , gainSelector
  , initSelector
  , initWithEngineSelector
  , newSelector
  , setAutomaticHeadTrackingFlagsSelector
  , setGainSelector

  -- * Enum types
  , PHASEAutomaticHeadTrackingFlags(PHASEAutomaticHeadTrackingFlags)
  , pattern PHASEAutomaticHeadTrackingFlagOrientation
  , pattern PHASEAutomaticHeadTrackingFlagPosition

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEListener phaseListener => phaseListener -> IO (Id PHASEListener)
init_ phaseListener =
  sendOwnedMessage phaseListener initSelector

-- | @+ new@
new :: IO (Id PHASEListener)
new  =
  do
    cls' <- getRequiredClass "PHASEListener"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:
--
-- Initialize a new listener.
--
-- ObjC selector: @- initWithEngine:@
initWithEngine :: (IsPHASEListener phaseListener, IsPHASEEngine engine) => phaseListener -> engine -> IO (Id PHASEListener)
initWithEngine phaseListener engine =
  sendOwnedMessage phaseListener initWithEngineSelector (toPHASEEngine engine)

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEListener phaseListener => phaseListener -> IO CDouble
gain phaseListener =
  sendMessage phaseListener gainSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASEListener phaseListener => phaseListener -> CDouble -> IO ()
setGain phaseListener value =
  sendMessage phaseListener setGainSelector value

-- | automaticHeadTrackingFlags
--
-- A combination of flags to express automatic headtracking behaviors for this listener.
--
-- ObjC selector: @- automaticHeadTrackingFlags@
automaticHeadTrackingFlags :: IsPHASEListener phaseListener => phaseListener -> IO PHASEAutomaticHeadTrackingFlags
automaticHeadTrackingFlags phaseListener =
  sendMessage phaseListener automaticHeadTrackingFlagsSelector

-- | automaticHeadTrackingFlags
--
-- A combination of flags to express automatic headtracking behaviors for this listener.
--
-- ObjC selector: @- setAutomaticHeadTrackingFlags:@
setAutomaticHeadTrackingFlags :: IsPHASEListener phaseListener => phaseListener -> PHASEAutomaticHeadTrackingFlags -> IO ()
setAutomaticHeadTrackingFlags phaseListener value =
  sendMessage phaseListener setAutomaticHeadTrackingFlagsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEListener)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEListener)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:@
initWithEngineSelector :: Selector '[Id PHASEEngine] (Id PHASEListener)
initWithEngineSelector = mkSelector "initWithEngine:"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CDouble
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector '[CDouble] ()
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @automaticHeadTrackingFlags@
automaticHeadTrackingFlagsSelector :: Selector '[] PHASEAutomaticHeadTrackingFlags
automaticHeadTrackingFlagsSelector = mkSelector "automaticHeadTrackingFlags"

-- | @Selector@ for @setAutomaticHeadTrackingFlags:@
setAutomaticHeadTrackingFlagsSelector :: Selector '[PHASEAutomaticHeadTrackingFlags] ()
setAutomaticHeadTrackingFlagsSelector = mkSelector "setAutomaticHeadTrackingFlags:"

