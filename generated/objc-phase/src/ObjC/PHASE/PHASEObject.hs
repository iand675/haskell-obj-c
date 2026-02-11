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
  , initSelector
  , newSelector
  , initWithEngineSelector
  , addChild_errorSelector
  , removeChildSelector
  , removeChildrenSelector
  , parentSelector
  , childrenSelector


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
init_ :: IsPHASEObject phaseObject => phaseObject -> IO (Id PHASEObject)
init_ phaseObject  =
  sendMsg phaseObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEObject)
new  =
  do
    cls' <- getRequiredClass "PHASEObject"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithEngine:
--
-- Initialize a new object.
--
-- ObjC selector: @- initWithEngine:@
initWithEngine :: (IsPHASEObject phaseObject, IsPHASEEngine engine) => phaseObject -> engine -> IO (Id PHASEObject)
initWithEngine phaseObject  engine =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseObject (mkSelector "initWithEngine:") (retPtr retVoid) [argPtr (castPtr raw_engine :: Ptr ())] >>= ownedObject . castPtr

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
addChild_error phaseObject  child error_ =
withObjCPtr child $ \raw_child ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg phaseObject (mkSelector "addChild:error:") retCULong [argPtr (castPtr raw_child :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeChild:
--
-- Remove a child from this object.
--
-- ObjC selector: @- removeChild:@
removeChild :: (IsPHASEObject phaseObject, IsPHASEObject child) => phaseObject -> child -> IO ()
removeChild phaseObject  child =
withObjCPtr child $ \raw_child ->
    sendMsg phaseObject (mkSelector "removeChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | removeChildren
--
-- Remove all the children from this object
--
-- ObjC selector: @- removeChildren@
removeChildren :: IsPHASEObject phaseObject => phaseObject -> IO ()
removeChildren phaseObject  =
  sendMsg phaseObject (mkSelector "removeChildren") retVoid []

-- | parent
--
-- The parent of this object, or nil if this object doesn't have a parent object.
--
-- ObjC selector: @- parent@
parent :: IsPHASEObject phaseObject => phaseObject -> IO (Id PHASEObject)
parent phaseObject  =
  sendMsg phaseObject (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | children
--
-- The children of this object.
--
-- ObjC selector: @- children@
children :: IsPHASEObject phaseObject => phaseObject -> IO (Id NSArray)
children phaseObject  =
  sendMsg phaseObject (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @addChild:error:@
addChild_errorSelector :: Selector
addChild_errorSelector = mkSelector "addChild:error:"

-- | @Selector@ for @removeChild:@
removeChildSelector :: Selector
removeChildSelector = mkSelector "removeChild:"

-- | @Selector@ for @removeChildren@
removeChildrenSelector :: Selector
removeChildrenSelector = mkSelector "removeChildren"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

