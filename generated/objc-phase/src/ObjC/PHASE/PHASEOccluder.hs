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
  , newSelector
  , initWithEngineSelector
  , initWithEngine_shapesSelector
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
init_ :: IsPHASEOccluder phaseOccluder => phaseOccluder -> IO (Id PHASEOccluder)
init_ phaseOccluder  =
  sendMsg phaseOccluder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEOccluder)
new  =
  do
    cls' <- getRequiredClass "PHASEOccluder"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithEngine:@
initWithEngine :: (IsPHASEOccluder phaseOccluder, IsPHASEEngine engine) => phaseOccluder -> engine -> IO (Id PHASEOccluder)
initWithEngine phaseOccluder  engine =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseOccluder (mkSelector "initWithEngine:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ())] >>= ownedObject . castPtr

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
initWithEngine_shapes phaseOccluder  engine shapes =
withObjCPtr engine $ \raw_engine ->
  withObjCPtr shapes $ \raw_shapes ->
      sendMsg phaseOccluder (mkSelector "initWithEngine:shapes:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ()), argPtr (castPtr raw_shapes :: Ptr ())] >>= ownedObject . castPtr

-- | shapes
--
-- Array of shapes associated with this occluder.
--
-- ObjC selector: @- shapes@
shapes :: IsPHASEOccluder phaseOccluder => phaseOccluder -> IO (Id NSArray)
shapes phaseOccluder  =
  sendMsg phaseOccluder (mkSelector "shapes") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @shapes@
shapesSelector :: Selector
shapesSelector = mkSelector "shapes"

