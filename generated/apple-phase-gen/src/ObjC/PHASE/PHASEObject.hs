{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEObject
--
-- A PHASEObject represents a 3D object in the engine, which can be organized into a hierarchy with relative transforms.
--
-- Generated bindings for @PHASEObject@.
module ObjC.PHASE.PHASEObject
  ( PHASEObject
  , IsPHASEObject(..)
  , init_
  , new
  , initWithEngine
  , addChild_error
  , removeChild
  , removeChildren
  , parent
  , children
  , addChild_errorSelector
  , childrenSelector
  , initSelector
  , initWithEngineSelector
  , newSelector
  , parentSelector
  , removeChildSelector
  , removeChildrenSelector


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
init_ :: IsPHASEObject phaseObject => phaseObject -> IO (Id PHASEObject)
init_ phaseObject =
  sendOwnedMessage phaseObject initSelector

-- | @+ new@
new :: IO (Id PHASEObject)
new  =
  do
    cls' <- getRequiredClass "PHASEObject"
    sendOwnedClassMessage cls' newSelector

-- | initWithEngine:
--
-- Initialize a new object.
--
-- ObjC selector: @- initWithEngine:@
initWithEngine :: (IsPHASEObject phaseObject, IsPHASEEngine engine) => phaseObject -> engine -> IO (Id PHASEObject)
initWithEngine phaseObject engine =
  sendOwnedMessage phaseObject initWithEngineSelector (toPHASEEngine engine)

-- | addChild:error:
--
-- Add a child to this object
--
-- @child@ — The child object
--
-- @error@ — Returns an error if the child already has a parent.
--
-- Returns: YES for success
--
-- ObjC selector: @- addChild:error:@
addChild_error :: (IsPHASEObject phaseObject, IsPHASEObject child, IsNSError error_) => phaseObject -> child -> error_ -> IO Bool
addChild_error phaseObject child error_ =
  sendMessage phaseObject addChild_errorSelector (toPHASEObject child) (toNSError error_)

-- | removeChild:
--
-- Remove a child from this object.
--
-- ObjC selector: @- removeChild:@
removeChild :: (IsPHASEObject phaseObject, IsPHASEObject child) => phaseObject -> child -> IO ()
removeChild phaseObject child =
  sendMessage phaseObject removeChildSelector (toPHASEObject child)

-- | removeChildren
--
-- Remove all the children from this object
--
-- ObjC selector: @- removeChildren@
removeChildren :: IsPHASEObject phaseObject => phaseObject -> IO ()
removeChildren phaseObject =
  sendMessage phaseObject removeChildrenSelector

-- | parent
--
-- The parent of this object, or nil if this object doesn't have a parent object.
--
-- ObjC selector: @- parent@
parent :: IsPHASEObject phaseObject => phaseObject -> IO (Id PHASEObject)
parent phaseObject =
  sendMessage phaseObject parentSelector

-- | children
--
-- The children of this object.
--
-- ObjC selector: @- children@
children :: IsPHASEObject phaseObject => phaseObject -> IO (Id NSArray)
children phaseObject =
  sendMessage phaseObject childrenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEObject)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEObject)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEngine:@
initWithEngineSelector :: Selector '[Id PHASEEngine] (Id PHASEObject)
initWithEngineSelector = mkSelector "initWithEngine:"

-- | @Selector@ for @addChild:error:@
addChild_errorSelector :: Selector '[Id PHASEObject, Id NSError] Bool
addChild_errorSelector = mkSelector "addChild:error:"

-- | @Selector@ for @removeChild:@
removeChildSelector :: Selector '[Id PHASEObject] ()
removeChildSelector = mkSelector "removeChild:"

-- | @Selector@ for @removeChildren@
removeChildrenSelector :: Selector '[] ()
removeChildrenSelector = mkSelector "removeChildren"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id PHASEObject)
parentSelector = mkSelector "parent"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

