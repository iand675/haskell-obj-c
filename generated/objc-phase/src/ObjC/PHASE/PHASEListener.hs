{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithEngineSelector
  , gainSelector
  , setGainSelector
  , automaticHeadTrackingFlagsSelector
  , setAutomaticHeadTrackingFlagsSelector

  -- * Enum types
  , PHASEAutomaticHeadTrackingFlags(PHASEAutomaticHeadTrackingFlags)
  , pattern PHASEAutomaticHeadTrackingFlagOrientation
  , pattern PHASEAutomaticHeadTrackingFlagPosition

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
init_ :: IsPHASEListener phaseListener => phaseListener -> IO (Id PHASEListener)
init_ phaseListener  =
  sendMsg phaseListener (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEListener)
new  =
  do
    cls' <- getRequiredClass "PHASEListener"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithEngine:
--
-- Initialize a new listener.
--
-- ObjC selector: @- initWithEngine:@
initWithEngine :: (IsPHASEListener phaseListener, IsPHASEEngine engine) => phaseListener -> engine -> IO (Id PHASEListener)
initWithEngine phaseListener  engine =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseListener (mkSelector "initWithEngine:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ())] >>= ownedObject . castPtr

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEListener phaseListener => phaseListener -> IO CDouble
gain phaseListener  =
  sendMsg phaseListener (mkSelector "gain") retCDouble []

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASEListener phaseListener => phaseListener -> CDouble -> IO ()
setGain phaseListener  value =
  sendMsg phaseListener (mkSelector "setGain:") retVoid [argCDouble (fromIntegral value)]

-- | automaticHeadTrackingFlags
--
-- A combination of flags to express automatic headtracking behaviors for this listener.
--
-- ObjC selector: @- automaticHeadTrackingFlags@
automaticHeadTrackingFlags :: IsPHASEListener phaseListener => phaseListener -> IO PHASEAutomaticHeadTrackingFlags
automaticHeadTrackingFlags phaseListener  =
  fmap (coerce :: CULong -> PHASEAutomaticHeadTrackingFlags) $ sendMsg phaseListener (mkSelector "automaticHeadTrackingFlags") retCULong []

-- | automaticHeadTrackingFlags
--
-- A combination of flags to express automatic headtracking behaviors for this listener.
--
-- ObjC selector: @- setAutomaticHeadTrackingFlags:@
setAutomaticHeadTrackingFlags :: IsPHASEListener phaseListener => phaseListener -> PHASEAutomaticHeadTrackingFlags -> IO ()
setAutomaticHeadTrackingFlags phaseListener  value =
  sendMsg phaseListener (mkSelector "setAutomaticHeadTrackingFlags:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:@
initWithEngineSelector :: Selector
initWithEngineSelector = mkSelector "initWithEngine:"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @automaticHeadTrackingFlags@
automaticHeadTrackingFlagsSelector :: Selector
automaticHeadTrackingFlagsSelector = mkSelector "automaticHeadTrackingFlags"

-- | @Selector@ for @setAutomaticHeadTrackingFlags:@
setAutomaticHeadTrackingFlagsSelector :: Selector
setAutomaticHeadTrackingFlagsSelector = mkSelector "setAutomaticHeadTrackingFlags:"

