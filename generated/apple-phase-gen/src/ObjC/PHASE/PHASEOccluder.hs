{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEOccluder
--
-- A PHASEOccluder represents a shape (with associated materials) that can affect sound transmission within the simulated acoustic scene.
--
-- Generated bindings for @PHASEOccluder@.
module ObjC.PHASE.PHASEOccluder
  ( PHASEOccluder
  , IsPHASEOccluder(..)
  , init_
  , new
  , initWithEngine
  , initWithEngine_shapes
  , shapes
  , initSelector
  , initWithEngineSelector
  , initWithEngine_shapesSelector
  , newSelector
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
init_ :: IsPHASEOccluder phaseOccluder => phaseOccluder -> IO (Id PHASEOccluder)
init_ phaseOccluder =
  sendOwnedMessage phaseOccluder initSelector

-- | @+ new@
new :: IO (Id PHASEOccluder)
new  =
  do
    cls' <- getRequiredClass "PHASEOccluder"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithEngine:@
initWithEngine :: (IsPHASEOccluder phaseOccluder, IsPHASEEngine engine) => phaseOccluder -> engine -> IO (Id PHASEOccluder)
initWithEngine phaseOccluder engine =
  sendOwnedMessage phaseOccluder initWithEngineSelector (toPHASEEngine engine)

-- | initWithEngine:shapes:
--
-- Initialize a new occluder with shapes.
--
-- The shapes array cannot be empty, otherwise an exception is thrown.
--
-- Note: This function is thread-safe.        Clients can safely run this function to create multiple occluders from multiple threads, if required.
--
-- ObjC selector: @- initWithEngine:shapes:@
initWithEngine_shapes :: (IsPHASEOccluder phaseOccluder, IsPHASEEngine engine, IsNSArray shapes) => phaseOccluder -> engine -> shapes -> IO (Id PHASEOccluder)
initWithEngine_shapes phaseOccluder engine shapes =
  sendOwnedMessage phaseOccluder initWithEngine_shapesSelector (toPHASEEngine engine) (toNSArray shapes)

-- | shapes
--
-- Array of shapes associated with this occluder.
--
-- ObjC selector: @- shapes@
shapes :: IsPHASEOccluder phaseOccluder => phaseOccluder -> IO (Id NSArray)
shapes phaseOccluder =
  sendMessage phaseOccluder shapesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEOccluder)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEOccluder)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:@
initWithEngineSelector :: Selector '[Id PHASEEngine] (Id PHASEOccluder)
initWithEngineSelector = mkSelector "initWithEngine:"

-- | @Selector@ for @initWithEngine:shapes:@
initWithEngine_shapesSelector :: Selector '[Id PHASEEngine, Id NSArray] (Id PHASEOccluder)
initWithEngine_shapesSelector = mkSelector "initWithEngine:shapes:"

-- | @Selector@ for @shapes@
shapesSelector :: Selector '[] (Id NSArray)
shapesSelector = mkSelector "shapes"

