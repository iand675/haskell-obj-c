{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An obstacle with an impassible radius in 3D space For use with GKAgent3D.  Using this with a GKAgent2D is no different than using GKCircleObstacle.
--
-- Generated bindings for @GKSphereObstacle@.
module ObjC.GameplayKit.GKSphereObstacle
  ( GKSphereObstacle
  , IsGKSphereObstacle(..)
  , obstacleWithRadius
  , initWithRadius
  , radius
  , setRadius
  , initWithRadiusSelector
  , obstacleWithRadiusSelector
  , radiusSelector
  , setRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ obstacleWithRadius:@
obstacleWithRadius :: CFloat -> IO (Id GKSphereObstacle)
obstacleWithRadius radius =
  do
    cls' <- getRequiredClass "GKSphereObstacle"
    sendClassMessage cls' obstacleWithRadiusSelector radius

-- | @- initWithRadius:@
initWithRadius :: IsGKSphereObstacle gkSphereObstacle => gkSphereObstacle -> CFloat -> IO (Id GKSphereObstacle)
initWithRadius gkSphereObstacle radius =
  sendOwnedMessage gkSphereObstacle initWithRadiusSelector radius

-- | Radius of the impassible circle
--
-- ObjC selector: @- radius@
radius :: IsGKSphereObstacle gkSphereObstacle => gkSphereObstacle -> IO CFloat
radius gkSphereObstacle =
  sendMessage gkSphereObstacle radiusSelector

-- | Radius of the impassible circle
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKSphereObstacle gkSphereObstacle => gkSphereObstacle -> CFloat -> IO ()
setRadius gkSphereObstacle value =
  sendMessage gkSphereObstacle setRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @obstacleWithRadius:@
obstacleWithRadiusSelector :: Selector '[CFloat] (Id GKSphereObstacle)
obstacleWithRadiusSelector = mkSelector "obstacleWithRadius:"

-- | @Selector@ for @initWithRadius:@
initWithRadiusSelector :: Selector '[CFloat] (Id GKSphereObstacle)
initWithRadiusSelector = mkSelector "initWithRadius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CFloat
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CFloat] ()
setRadiusSelector = mkSelector "setRadius:"

