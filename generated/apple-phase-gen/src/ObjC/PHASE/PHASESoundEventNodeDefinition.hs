{-# LANGUAGE DataKinds #-}
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
  , childrenSelector
  , initSelector
  , newSelector


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
init_ :: IsPHASESoundEventNodeDefinition phaseSoundEventNodeDefinition => phaseSoundEventNodeDefinition -> IO (Id PHASESoundEventNodeDefinition)
init_ phaseSoundEventNodeDefinition =
  sendOwnedMessage phaseSoundEventNodeDefinition initSelector

-- | @+ new@
new :: IO (Id PHASESoundEventNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESoundEventNodeDefinition"
    sendOwnedClassMessage cls' newSelector

-- | children
--
-- The children of this node definition.
--
-- ObjC selector: @- children@
children :: IsPHASESoundEventNodeDefinition phaseSoundEventNodeDefinition => phaseSoundEventNodeDefinition -> IO (Id NSArray)
children phaseSoundEventNodeDefinition =
  sendMessage phaseSoundEventNodeDefinition childrenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESoundEventNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESoundEventNodeDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

