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
  , updateWithDeltaTimeSelector
  , didAddToEntitySelector
  , willRemoveFromEntitySelector
  , entitySelector


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

-- | Updates the component with the given delta time since the last update. Each component should perform its time-based logic in this method.
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKComponent gkComponent => gkComponent -> CDouble -> IO ()
updateWithDeltaTime gkComponent  seconds =
  sendMsg gkComponent (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral seconds)]

-- | Override this to perform game logic when this component is added to an entity
--
-- ObjC selector: @- didAddToEntity@
didAddToEntity :: IsGKComponent gkComponent => gkComponent -> IO ()
didAddToEntity gkComponent  =
  sendMsg gkComponent (mkSelector "didAddToEntity") retVoid []

-- | Override this to perform game logic before this entity is removed from it's entity
--
-- ObjC selector: @- willRemoveFromEntity@
willRemoveFromEntity :: IsGKComponent gkComponent => gkComponent -> IO ()
willRemoveFromEntity gkComponent  =
  sendMsg gkComponent (mkSelector "willRemoveFromEntity") retVoid []

-- | The entity that this component belongs to. Defaults to nil until the component is added to an entity.
--
-- ObjC selector: @- entity@
entity :: IsGKComponent gkComponent => gkComponent -> IO (Id GKEntity)
entity gkComponent  =
  sendMsg gkComponent (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @didAddToEntity@
didAddToEntitySelector :: Selector
didAddToEntitySelector = mkSelector "didAddToEntity"

-- | @Selector@ for @willRemoveFromEntity@
willRemoveFromEntitySelector :: Selector
willRemoveFromEntitySelector = mkSelector "willRemoveFromEntity"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

