{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A component system is a homogeneous collection of components that are intended to be called at the same time. The system is homogeneous, meaning it only allows members of the same class into the system.
--
-- Generated bindings for @GKComponentSystem@.
module ObjC.GameplayKit.GKComponentSystem
  ( GKComponentSystem
  , IsGKComponentSystem(..)
  , objectAtIndexedSubscript
  , initWithComponentClass
  , addComponent
  , addComponentWithEntity
  , removeComponentWithEntity
  , removeComponent
  , updateWithDeltaTime
  , classForGenericArgumentAtIndex
  , componentClass
  , components
  , addComponentSelector
  , addComponentWithEntitySelector
  , classForGenericArgumentAtIndexSelector
  , componentClassSelector
  , componentsSelector
  , initWithComponentClassSelector
  , objectAtIndexedSubscriptSelector
  , removeComponentSelector
  , removeComponentWithEntitySelector
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

-- | Supports getting components via a [] subscript.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> CULong -> IO (Id GKComponent)
objectAtIndexedSubscript gkComponentSystem idx =
  sendMessage gkComponentSystem objectAtIndexedSubscriptSelector idx

-- | Initializes a system for the given component class. The receiver can now only accept components of the given class.
--
-- ObjC selector: @- initWithComponentClass:@
initWithComponentClass :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> Class -> IO (Id GKComponentSystem)
initWithComponentClass gkComponentSystem cls =
  sendOwnedMessage gkComponentSystem initWithComponentClassSelector cls

-- | Adds a component to the system. The component must be of the same class as the system's componentClass. The component is added to the tail of the collection and will be processed after components that were added before it.
--
-- if the component added is not a kind of the system's componentClass.
--
-- ObjC selector: @- addComponent:@
addComponent :: (IsGKComponentSystem gkComponentSystem, IsGKComponent component) => gkComponentSystem -> component -> IO ()
addComponent gkComponentSystem component =
  sendMessage gkComponentSystem addComponentSelector (toGKComponent component)

-- | Adds the supported component from the entity's component collection. This is conceptually the same as the pseudo-code:
--
-- for (GKComponent *component in entity.components)    if (component.class == system.componentClass)        [system addComponent:component]
--
-- See: GKEntity.components
--
-- ObjC selector: @- addComponentWithEntity:@
addComponentWithEntity :: (IsGKComponentSystem gkComponentSystem, IsGKEntity entity) => gkComponentSystem -> entity -> IO ()
addComponentWithEntity gkComponentSystem entity =
  sendMessage gkComponentSystem addComponentWithEntitySelector (toGKEntity entity)

-- | Removes the supported component from the entity's component collection This is conceptually the same as the pseudo-code:
--
-- for (GKComponent *component in entity.components)    if (component.class == system.componentClass)        [system removeComponent:component]
--
-- ObjC selector: @- removeComponentWithEntity:@
removeComponentWithEntity :: (IsGKComponentSystem gkComponentSystem, IsGKEntity entity) => gkComponentSystem -> entity -> IO ()
removeComponentWithEntity gkComponentSystem entity =
  sendMessage gkComponentSystem removeComponentWithEntitySelector (toGKEntity entity)

-- | Removes a component from the system
--
-- Does nothing if the component is not in this system
--
-- ObjC selector: @- removeComponent:@
removeComponent :: (IsGKComponentSystem gkComponentSystem, IsGKComponent component) => gkComponentSystem -> component -> IO ()
removeComponent gkComponentSystem component =
  sendMessage gkComponentSystem removeComponentSelector (toGKComponent component)

-- | Updates each component with the given delta time since the last update. Each component thus performs its time based logic with a single message.
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> CDouble -> IO ()
updateWithDeltaTime gkComponentSystem seconds =
  sendMessage gkComponentSystem updateWithDeltaTimeSelector seconds

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> CULong -> IO Class
classForGenericArgumentAtIndex gkComponentSystem index =
  sendMessage gkComponentSystem classForGenericArgumentAtIndexSelector index

-- | The collection's component class. Any selector the component supports can be called on the system and it will be forwarded to each of the components in the collection.
--
-- ObjC selector: @- componentClass@
componentClass :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> IO Class
componentClass gkComponentSystem =
  sendMessage gkComponentSystem componentClassSelector

-- | The array of components currently in the system.
--
-- ObjC selector: @- components@
components :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> IO (Id NSArray)
components gkComponentSystem =
  sendMessage gkComponentSystem componentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id GKComponent)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @initWithComponentClass:@
initWithComponentClassSelector :: Selector '[Class] (Id GKComponentSystem)
initWithComponentClassSelector = mkSelector "initWithComponentClass:"

-- | @Selector@ for @addComponent:@
addComponentSelector :: Selector '[Id GKComponent] ()
addComponentSelector = mkSelector "addComponent:"

-- | @Selector@ for @addComponentWithEntity:@
addComponentWithEntitySelector :: Selector '[Id GKEntity] ()
addComponentWithEntitySelector = mkSelector "addComponentWithEntity:"

-- | @Selector@ for @removeComponentWithEntity:@
removeComponentWithEntitySelector :: Selector '[Id GKEntity] ()
removeComponentWithEntitySelector = mkSelector "removeComponentWithEntity:"

-- | @Selector@ for @removeComponent:@
removeComponentSelector :: Selector '[Id GKComponent] ()
removeComponentSelector = mkSelector "removeComponent:"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector '[CDouble] ()
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector '[CULong] Class
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @componentClass@
componentClassSelector :: Selector '[] Class
componentClassSelector = mkSelector "componentClass"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] (Id NSArray)
componentsSelector = mkSelector "components"

