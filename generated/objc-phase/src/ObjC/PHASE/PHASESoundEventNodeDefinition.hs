{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESoundEventNodeDefinition
--
-- The base class for a sound event node definition.
--
-- Sound event nodes are a hierarchical collection of objects that either generate or control playback of audio content in PHASE.        Generator nodes produce audio. They are always leaf nodes in a node hierarchy. These include samplers and stream nodes.        Control nodes set the logic for how generator nodes are selected, mixed and parameterized before downstream mixer processing.        Control nodes are always parent nodes, and can be organized into hierarchies for complex sound design scenarios.
--
-- Generated bindings for @PHASESoundEventNodeDefinition@.
module ObjC.PHASE.PHASESoundEventNodeDefinition
  ( PHASESoundEventNodeDefinition
  , IsPHASESoundEventNodeDefinition(..)
  , init_
  , new
  , children
  , initSelector
  , newSelector
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
init_ :: IsPHASESoundEventNodeDefinition phaseSoundEventNodeDefinition => phaseSoundEventNodeDefinition -> IO (Id PHASESoundEventNodeDefinition)
init_ phaseSoundEventNodeDefinition  =
  sendMsg phaseSoundEventNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESoundEventNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESoundEventNodeDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | children
--
-- The children of this node definition.
--
-- ObjC selector: @- children@
children :: IsPHASESoundEventNodeDefinition phaseSoundEventNodeDefinition => phaseSoundEventNodeDefinition -> IO (Id NSArray)
children phaseSoundEventNodeDefinition  =
  sendMsg phaseSoundEventNodeDefinition (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

