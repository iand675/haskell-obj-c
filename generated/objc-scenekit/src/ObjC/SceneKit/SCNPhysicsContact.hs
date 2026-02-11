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
  , nodeASelector
  , nodeBSelector
  , contactPointSelector
  , contactNormalSelector
  , collisionImpulseSelector
  , penetrationDistanceSelector
  , sweepTestFractionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- nodeA@
nodeA :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO (Id SCNNode)
nodeA scnPhysicsContact  =
  sendMsg scnPhysicsContact (mkSelector "nodeA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nodeB@
nodeB :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO (Id SCNNode)
nodeB scnPhysicsContact  =
  sendMsg scnPhysicsContact (mkSelector "nodeB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contactPoint@
contactPoint :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO SCNVector3
contactPoint scnPhysicsContact  =
  sendMsgStret scnPhysicsContact (mkSelector "contactPoint") retSCNVector3 []

-- | @- contactNormal@
contactNormal :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO SCNVector3
contactNormal scnPhysicsContact  =
  sendMsgStret scnPhysicsContact (mkSelector "contactNormal") retSCNVector3 []

-- | @- collisionImpulse@
collisionImpulse :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO CDouble
collisionImpulse scnPhysicsContact  =
  sendMsg scnPhysicsContact (mkSelector "collisionImpulse") retCDouble []

-- | @- penetrationDistance@
penetrationDistance :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO CDouble
penetrationDistance scnPhysicsContact  =
  sendMsg scnPhysicsContact (mkSelector "penetrationDistance") retCDouble []

-- | @- sweepTestFraction@
sweepTestFraction :: IsSCNPhysicsContact scnPhysicsContact => scnPhysicsContact -> IO CDouble
sweepTestFraction scnPhysicsContact  =
  sendMsg scnPhysicsContact (mkSelector "sweepTestFraction") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeA@
nodeASelector :: Selector
nodeASelector = mkSelector "nodeA"

-- | @Selector@ for @nodeB@
nodeBSelector :: Selector
nodeBSelector = mkSelector "nodeB"

-- | @Selector@ for @contactPoint@
contactPointSelector :: Selector
contactPointSelector = mkSelector "contactPoint"

-- | @Selector@ for @contactNormal@
contactNormalSelector :: Selector
contactNormalSelector = mkSelector "contactNormal"

-- | @Selector@ for @collisionImpulse@
collisionImpulseSelector :: Selector
collisionImpulseSelector = mkSelector "collisionImpulse"

-- | @Selector@ for @penetrationDistance@
penetrationDistanceSelector :: Selector
penetrationDistanceSelector = mkSelector "penetrationDistance"

-- | @Selector@ for @sweepTestFraction@
sweepTestFractionSelector :: Selector
sweepTestFractionSelector = mkSelector "sweepTestFraction"

