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
  , entitySelector
  , initSelector
  , updateWithDeltaTimeSelector
  , addComponentSelector
  , removeComponentForClassSelector
  , componentForClassSelector
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

-- | Creates a new entity ready to have components added to it.
--
-- ObjC selector: @+ entity@
entity :: IO (Id GKEntity)
entity  =
  do
    cls' <- getRequiredClass "GKEntity"
    sendClassMsg cls' (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates a new entity ready to have components added to it.
--
-- ObjC selector: @- init@
init_ :: IsGKEntity gkEntity => gkEntity -> IO (Id GKEntity)
init_ gkEntity  =
  sendMsg gkEntity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
updateWithDeltaTime gkEntity  seconds =
  sendMsg gkEntity (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral seconds)]

-- | Adds a component to this entity.  If a component of the same class already exists it is overwritten with the new component.
--
-- @component@ — the component to be added
--
-- See: GKComponent
--
-- ObjC selector: @- addComponent:@
addComponent :: (IsGKEntity gkEntity, IsGKComponent component) => gkEntity -> component -> IO ()
addComponent gkEntity  component =
withObjCPtr component $ \raw_component ->
    sendMsg gkEntity (mkSelector "addComponent:") retVoid [argPtr (castPtr raw_component :: Ptr ())]

-- | @- removeComponentForClass:@
removeComponentForClass :: IsGKEntity gkEntity => gkEntity -> Class -> IO ()
removeComponentForClass gkEntity  componentClass =
  sendMsg gkEntity (mkSelector "removeComponentForClass:") retVoid [argPtr (unClass componentClass)]

-- | @- componentForClass:@
componentForClass :: IsGKEntity gkEntity => gkEntity -> Class -> IO (Id GKComponent)
componentForClass gkEntity  componentClass =
  sendMsg gkEntity (mkSelector "componentForClass:") (retPtr retVoid) [argPtr (unClass componentClass)] >>= retainedObject . castPtr

-- | Access the current set of components as an array. Note: this is not the internal array of components, but rather a newly created array of the current component mapping.
--
-- ObjC selector: @- components@
components :: IsGKEntity gkEntity => gkEntity -> IO (Id NSArray)
components gkEntity  =
  sendMsg gkEntity (mkSelector "components") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @addComponent:@
addComponentSelector :: Selector
addComponentSelector = mkSelector "addComponent:"

-- | @Selector@ for @removeComponentForClass:@
removeComponentForClassSelector :: Selector
removeComponentForClassSelector = mkSelector "removeComponentForClass:"

-- | @Selector@ for @componentForClass:@
componentForClassSelector :: Selector
componentForClassSelector = mkSelector "componentForClass:"

-- | @Selector@ for @components@
componentsSelector :: Selector
componentsSelector = mkSelector "components"

