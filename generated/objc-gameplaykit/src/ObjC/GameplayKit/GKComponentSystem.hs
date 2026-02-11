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
  , objectAtIndexedSubscriptSelector
  , initWithComponentClassSelector
  , addComponentSelector
  , addComponentWithEntitySelector
  , removeComponentWithEntitySelector
  , removeComponentSelector
  , updateWithDeltaTimeSelector
  , classForGenericArgumentAtIndexSelector
  , componentClassSelector
  , componentsSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Supports getting components via a [] subscript.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> CULong -> IO (Id GKComponent)
objectAtIndexedSubscript gkComponentSystem  idx =
  sendMsg gkComponentSystem (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)] >>= retainedObject . castPtr

-- | Initializes a system for the given component class. The receiver can now only accept components of the given class.
--
-- ObjC selector: @- initWithComponentClass:@
initWithComponentClass :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> Class -> IO (Id GKComponentSystem)
initWithComponentClass gkComponentSystem  cls =
  sendMsg gkComponentSystem (mkSelector "initWithComponentClass:") (retPtr retVoid) [argPtr (unClass cls)] >>= ownedObject . castPtr

-- | Adds a component to the system. The component must be of the same class as the system's componentClass. The component is added to the tail of the collection and will be processed after components that were added before it.
--
-- if the component added is not a kind of the system's componentClass.
--
-- ObjC selector: @- addComponent:@
addComponent :: (IsGKComponentSystem gkComponentSystem, IsGKComponent component) => gkComponentSystem -> component -> IO ()
addComponent gkComponentSystem  component =
withObjCPtr component $ \raw_component ->
    sendMsg gkComponentSystem (mkSelector "addComponent:") retVoid [argPtr (castPtr raw_component :: Ptr ())]

-- | Adds the supported component from the entity's component collection. This is conceptually the same as the pseudo-code:
--
-- for (GKComponent *component in entity.components)    if (component.class == system.componentClass)        [system addComponent:component]
--
-- See: GKEntity.components
--
-- ObjC selector: @- addComponentWithEntity:@
addComponentWithEntity :: (IsGKComponentSystem gkComponentSystem, IsGKEntity entity) => gkComponentSystem -> entity -> IO ()
addComponentWithEntity gkComponentSystem  entity =
withObjCPtr entity $ \raw_entity ->
    sendMsg gkComponentSystem (mkSelector "addComponentWithEntity:") retVoid [argPtr (castPtr raw_entity :: Ptr ())]

-- | Removes the supported component from the entity's component collection This is conceptually the same as the pseudo-code:
--
-- for (GKComponent *component in entity.components)    if (component.class == system.componentClass)        [system removeComponent:component]
--
-- ObjC selector: @- removeComponentWithEntity:@
removeComponentWithEntity :: (IsGKComponentSystem gkComponentSystem, IsGKEntity entity) => gkComponentSystem -> entity -> IO ()
removeComponentWithEntity gkComponentSystem  entity =
withObjCPtr entity $ \raw_entity ->
    sendMsg gkComponentSystem (mkSelector "removeComponentWithEntity:") retVoid [argPtr (castPtr raw_entity :: Ptr ())]

-- | Removes a component from the system
--
-- Does nothing if the component is not in this system
--
-- ObjC selector: @- removeComponent:@
removeComponent :: (IsGKComponentSystem gkComponentSystem, IsGKComponent component) => gkComponentSystem -> component -> IO ()
removeComponent gkComponentSystem  component =
withObjCPtr component $ \raw_component ->
    sendMsg gkComponentSystem (mkSelector "removeComponent:") retVoid [argPtr (castPtr raw_component :: Ptr ())]

-- | Updates each component with the given delta time since the last update. Each component thus performs its time based logic with a single message.
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> CDouble -> IO ()
updateWithDeltaTime gkComponentSystem  seconds =
  sendMsg gkComponentSystem (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral seconds)]

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> CULong -> IO Class
classForGenericArgumentAtIndex gkComponentSystem  index =
  fmap (Class . castPtr) $ sendMsg gkComponentSystem (mkSelector "classForGenericArgumentAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | The collection's component class. Any selector the component supports can be called on the system and it will be forwarded to each of the components in the collection.
--
-- ObjC selector: @- componentClass@
componentClass :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> IO Class
componentClass gkComponentSystem  =
  fmap (Class . castPtr) $ sendMsg gkComponentSystem (mkSelector "componentClass") (retPtr retVoid) []

-- | The array of components currently in the system.
--
-- ObjC selector: @- components@
components :: IsGKComponentSystem gkComponentSystem => gkComponentSystem -> IO (Id NSArray)
components gkComponentSystem  =
  sendMsg gkComponentSystem (mkSelector "components") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @initWithComponentClass:@
initWithComponentClassSelector :: Selector
initWithComponentClassSelector = mkSelector "initWithComponentClass:"

-- | @Selector@ for @addComponent:@
addComponentSelector :: Selector
addComponentSelector = mkSelector "addComponent:"

-- | @Selector@ for @addComponentWithEntity:@
addComponentWithEntitySelector :: Selector
addComponentWithEntitySelector = mkSelector "addComponentWithEntity:"

-- | @Selector@ for @removeComponentWithEntity:@
removeComponentWithEntitySelector :: Selector
removeComponentWithEntitySelector = mkSelector "removeComponentWithEntity:"

-- | @Selector@ for @removeComponent:@
removeComponentSelector :: Selector
removeComponentSelector = mkSelector "removeComponent:"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @componentClass@
componentClassSelector :: Selector
componentClassSelector = mkSelector "componentClass"

-- | @Selector@ for @components@
componentsSelector :: Selector
componentsSelector = mkSelector "components"

