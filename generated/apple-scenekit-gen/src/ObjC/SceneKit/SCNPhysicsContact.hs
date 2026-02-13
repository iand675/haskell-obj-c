{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsContact
--
-- SCNPhysicsContact contains information about a physics contact.
--
-- Generated bindings for @SCNPhysicsContact@.
module ObjC.SceneKit.SCNPhysicsContact
  ( SCNPhysicsContact
  , IsSCNPhysicsContact(..)
  , nodeA
  , nodeB
  , contactPoint
  , contactNormal
  , collisionImpulse
  , penetrationDistance
  , sweepTestFraction
  , collisionImpulseSelector
  , contactNormalSelector
  , contactPointSelector
  , nodeASelector
  , nodeBSelector
  , penetrationDistanceSelector
  , sweepTestFractionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- nodeA@
nodeA :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO (Id SCNNode)
nodeA scnPhysicsContact =
  sendMessage scnPhysicsContact nodeASelector

-- | @- nodeB@
nodeB :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO (Id SCNNode)
nodeB scnPhysicsContact =
  sendMessage scnPhysicsContact nodeBSelector

-- | @- contactPoint@
contactPoint :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO SCNVector3
contactPoint scnPhysicsContact =
  sendMessage scnPhysicsContact contactPointSelector

-- | @- contactNormal@
contactNormal :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO SCNVector3
contactNormal scnPhysicsContact =
  sendMessage scnPhysicsContact contactNormalSelector

-- | @- collisionImpulse@
collisionImpulse :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO CDouble
collisionImpulse scnPhysicsContact =
  sendMessage scnPhysicsContact collisionImpulseSelector

-- | @- penetrationDistance@
penetrationDistance :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO CDouble
penetrationDistance scnPhysicsContact =
  sendMessage scnPhysicsContact penetrationDistanceSelector

-- | @- sweepTestFraction@
sweepTestFraction :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO CDouble
sweepTestFraction scnPhysicsContact =
  sendMessage scnPhysicsContact sweepTestFractionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeA@
nodeASelector :: Selector '[] (Id SCNNode)
nodeASelector = mkSelector "nodeA"

-- | @Selector@ for @nodeB@
nodeBSelector :: Selector '[] (Id SCNNode)
nodeBSelector = mkSelector "nodeB"

-- | @Selector@ for @contactPoint@
contactPointSelector :: Selector '[] SCNVector3
contactPointSelector = mkSelector "contactPoint"

-- | @Selector@ for @contactNormal@
contactNormalSelector :: Selector '[] SCNVector3
contactNormalSelector = mkSelector "contactNormal"

-- | @Selector@ for @collisionImpulse@
collisionImpulseSelector :: Selector '[] CDouble
collisionImpulseSelector = mkSelector "collisionImpulse"

-- | @Selector@ for @penetrationDistance@
penetrationDistanceSelector :: Selector '[] CDouble
penetrationDistanceSelector = mkSelector "penetrationDistance"

-- | @Selector@ for @sweepTestFraction@
sweepTestFractionSelector :: Selector '[] CDouble
sweepTestFractionSelector = mkSelector "sweepTestFraction"

