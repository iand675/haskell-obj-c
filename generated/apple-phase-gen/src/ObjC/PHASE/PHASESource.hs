{-# LANGUAGE DataKinds #-}
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
  , gainSelector
  , initSelector
  , initWithEngineSelector
  , initWithEngine_shapesSelector
  , newSelector
  , setGainSelector
  , shapesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESource phaseSource => phaseSource -> IO (Id PHASESource)
init_ phaseSource =
  sendOwnedMessage phaseSource initSelector

-- | @+ new@
new :: IO (Id PHASESource)
new  =
  do
    cls' <- getRequiredClass "PHASESource"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:
--
-- Initialize a new point source.
--
-- ObjC selector: @- initWithEngine:@
initWithEngine :: (IsPHASESource phaseSource, IsPHASEEngine engine) => phaseSource -> engine -> IO (Id PHASESource)
initWithEngine phaseSource engine =
  sendOwnedMessage phaseSource initWithEngineSelector (toPHASEEngine engine)

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
initWithEngine_shapes phaseSource engine shapes =
  sendOwnedMessage phaseSource initWithEngine_shapesSelector (toPHASEEngine engine) (toNSArray shapes)

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASESource phaseSource => phaseSource -> IO CDouble
gain phaseSource =
  sendMessage phaseSource gainSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASESource phaseSource => phaseSource -> CDouble -> IO ()
setGain phaseSource value =
  sendMessage phaseSource setGainSelector value

-- | shapes
--
-- Array of shapes associated with this source.
--
-- ObjC selector: @- shapes@
shapes :: IsPHASESource phaseSource => phaseSource -> IO (Id NSArray)
shapes phaseSource =
  sendMessage phaseSource shapesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESource)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESource)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:@
initWithEngineSelector :: Selector '[Id PHASEEngine] (Id PHASESource)
initWithEngineSelector = mkSelector "initWithEngine:"

-- | @Selector@ for @initWithEngine:shapes:@
initWithEngine_shapesSelector :: Selector '[Id PHASEEngine, Id NSArray] (Id PHASESource)
initWithEngine_shapesSelector = mkSelector "initWithEngine:shapes:"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CDouble
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector '[CDouble] ()
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @shapes@
shapesSelector :: Selector '[] (Id NSArray)
shapesSelector = mkSelector "shapes"

