{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A component is the data and logic for one part of an object in an entity-component system. Entities have many components but components are associated with only a single entity.
--
-- Components across entities are best arranged in ComponentSystems, which are homogeneous collections of components that the game logic updates in a deterministic order.
--
-- See: GKComponentSystem
--
-- Generated bindings for @GKComponent@.
module ObjC.GameplayKit.GKComponent
  ( GKComponent
  , IsGKComponent(..)
  , updateWithDeltaTime
  , didAddToEntity
  , willRemoveFromEntity
  , entity
  , didAddToEntitySelector
  , entitySelector
  , updateWithDeltaTimeSelector
  , willRemoveFromEntitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Updates the component with the given delta time since the last update. Each component should perform its time-based logic in this method.
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKComponent gkComponent => gkComponent -> CDouble -> IO ()
updateWithDeltaTime gkComponent seconds =
  sendMessage gkComponent updateWithDeltaTimeSelector seconds

-- | Override this to perform game logic when this component is added to an entity
--
-- ObjC selector: @- didAddToEntity@
didAddToEntity :: IsGKComponent gkComponent => gkComponent -> IO ()
didAddToEntity gkComponent =
  sendMessage gkComponent didAddToEntitySelector

-- | Override this to perform game logic before this entity is removed from it's entity
--
-- ObjC selector: @- willRemoveFromEntity@
willRemoveFromEntity :: IsGKComponent gkComponent => gkComponent -> IO ()
willRemoveFromEntity gkComponent =
  sendMessage gkComponent willRemoveFromEntitySelector

-- | The entity that this component belongs to. Defaults to nil until the component is added to an entity.
--
-- ObjC selector: @- entity@
entity :: IsGKComponent gkComponent => gkComponent -> IO (Id GKEntity)
entity gkComponent =
  sendMessage gkComponent entitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector '[CDouble] ()
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @didAddToEntity@
didAddToEntitySelector :: Selector '[] ()
didAddToEntitySelector = mkSelector "didAddToEntity"

-- | @Selector@ for @willRemoveFromEntity@
willRemoveFromEntitySelector :: Selector '[] ()
willRemoveFromEntitySelector = mkSelector "willRemoveFromEntity"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id GKEntity)
entitySelector = mkSelector "entity"

