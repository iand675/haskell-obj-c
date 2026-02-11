{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESource
--
-- A PHASESource represents where sound originates within the simulated acoustic scene.
--
-- PHASE supports both point sources and volumetric sources.        A point source simulates the sound from a point in space.        A volumetric source simulates the sound from a shape.
--
-- Generated bindings for @PHASESource@.
module ObjC.PHASE.PHASESource
  ( PHASESource
  , IsPHASESource(..)
  , init_
  , new
  , initWithEngine
  , initWithEngine_shapes
  , gain
  , setGain
  , shapes
  , initSelector
  , newSelector
  , initWithEngineSelector
  , initWithEngine_shapesSelector
  , gainSelector
  , setGainSelector
  , shapesSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESource phaseSource => phaseSource -> IO (Id PHASESource)
init_ phaseSource  =
  sendMsg phaseSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESource)
new  =
  do
    cls' <- getRequiredClass "PHASESource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithEngine:
--
-- Initialize a new point source.
--
-- ObjC selector: @- initWithEngine:@
initWithEngine :: (IsPHASESource phaseSource, IsPHASEEngine engine) => phaseSource -> engine -> IO (Id PHASESource)
initWithEngine phaseSource  engine =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseSource (mkSelector "initWithEngine:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ())] >>= ownedObject . castPtr

-- | initWithEngine:shapes:
--
-- Initialize a new volumetric source with shapes.
--
-- @shapes@ — The shape(s) of the source within the world
--
-- The shapes array cannot be empty, otherwise an exception is thrown.
--
-- Note: This function is thread-safe.        Clients can safely run this function to create multiple sources from multiple threads, if required.
--
-- ObjC selector: @- initWithEngine:shapes:@
initWithEngine_shapes :: (IsPHASESource phaseSource, IsPHASEEngine engine, IsNSArray shapes) => phaseSource -> engine -> shapes -> IO (Id PHASESource)
initWithEngine_shapes phaseSource  engine shapes =
withObjCPtr engine $ \raw_engine ->
  withObjCPtr shapes $ \raw_shapes ->
      sendMsg phaseSource (mkSelector "initWithEngine:shapes:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argPtr (castPtr raw_shapes :: Ptr ())] >>= ownedObject . castPtr

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASESource phaseSource => phaseSource -> IO CDouble
gain phaseSource  =
  sendMsg phaseSource (mkSelector "gain") retCDouble []

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASESource phaseSource => phaseSource -> CDouble -> IO ()
setGain phaseSource  value =
  sendMsg phaseSource (mkSelector "setGain:") retVoid [argCDouble (fromIntegral value)]

-- | shapes
--
-- Array of shapes associated with this source.
--
-- ObjC selector: @- shapes@
shapes :: IsPHASESource phaseSource => phaseSource -> IO (Id NSArray)
shapes phaseSource  =
  sendMsg phaseSource (mkSelector "shapes") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @initWithEngine:shapes:@
initWithEngine_shapesSelector :: Selector
initWithEngine_shapesSelector = mkSelector "initWithEngine:shapes:"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @shapes@
shapesSelector :: Selector
shapesSelector = mkSelector "shapes"

