{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An entity is the general purpose object in an entity-component system. Entites have many components but components are associated with only a single entity.
--
-- Note: GKEntity supports NSCopying and NSSecureCoding, but your custom GKComponent's must also support NSCopying and NSSecureCoding
--
-- See: GKComponent
--
-- See: GKComponentSystem
--
-- Generated bindings for @GKEntity@.
module ObjC.GameplayKit.GKEntity
  ( GKEntity
  , IsGKEntity(..)
  , entity
  , init_
  , updateWithDeltaTime
  , addComponent
  , removeComponentForClass
  , componentForClass
  , components
  , addComponentSelector
  , componentForClassSelector
  , componentsSelector
  , entitySelector
  , initSelector
  , removeComponentForClassSelector
  , updateWithDeltaTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new entity ready to have components added to it.
--
-- ObjC selector: @+ entity@
entity :: IO (Id GKEntity)
entity  =
  do
    cls' <- getRequiredClass "GKEntity"
    sendClassMessage cls' entitySelector

-- | Creates a new entity ready to have components added to it.
--
-- ObjC selector: @- init@
init_ :: IsGKEntity gkEntity => gkEntity -> IO (Id GKEntity)
init_ gkEntity =
  sendOwnedMessage gkEntity initSelector

-- | General update loop for this entity, which also updates all components in this entity that are not currently in a dedicated component system.
--
-- Per-entity component updates is a simpler and less flexible option to using per-component updates, however both can not be allowed to occur at the same time for a component. Thus components that are added to dedicated component systems will not be updated here as they have opted for the more powerful feature of per-component systems. Update those components via their system instead.
--
-- See: GKComponentSystem
--
-- @seconds@ — elapsed time, in seconds, since last frame
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKEntity gkEntity => gkEntity -> CDouble -> IO ()
updateWithDeltaTime gkEntity seconds =
  sendMessage gkEntity updateWithDeltaTimeSelector seconds

-- | Adds a component to this entity.  If a component of the same class already exists it is overwritten with the new component.
--
-- @component@ — the component to be added
--
-- See: GKComponent
--
-- ObjC selector: @- addComponent:@
addComponent :: (IsGKEntity gkEntity, IsGKComponent component) => gkEntity -> component -> IO ()
addComponent gkEntity component =
  sendMessage gkEntity addComponentSelector (toGKComponent component)

-- | @- removeComponentForClass:@
removeComponentForClass :: IsGKEntity gkEntity => gkEntity -> Class -> IO ()
removeComponentForClass gkEntity componentClass =
  sendMessage gkEntity removeComponentForClassSelector componentClass

-- | @- componentForClass:@
componentForClass :: IsGKEntity gkEntity => gkEntity -> Class -> IO (Id GKComponent)
componentForClass gkEntity componentClass =
  sendMessage gkEntity componentForClassSelector componentClass

-- | Access the current set of components as an array. Note: this is not the internal array of components, but rather a newly created array of the current component mapping.
--
-- ObjC selector: @- components@
components :: IsGKEntity gkEntity => gkEntity -> IO (Id NSArray)
components gkEntity =
  sendMessage gkEntity componentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id GKEntity)
entitySelector = mkSelector "entity"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKEntity)
initSelector = mkSelector "init"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector '[CDouble] ()
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @addComponent:@
addComponentSelector :: Selector '[Id GKComponent] ()
addComponentSelector = mkSelector "addComponent:"

-- | @Selector@ for @removeComponentForClass:@
removeComponentForClassSelector :: Selector '[Class] ()
removeComponentForClassSelector = mkSelector "removeComponentForClass:"

-- | @Selector@ for @componentForClass:@
componentForClassSelector :: Selector '[Class] (Id GKComponent)
componentForClassSelector = mkSelector "componentForClass:"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] (Id NSArray)
componentsSelector = mkSelector "components"

