{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An obstacle with an impassible radius
--
-- Generated bindings for @GKCircleObstacle@.
module ObjC.GameplayKit.GKCircleObstacle
  ( GKCircleObstacle
  , IsGKCircleObstacle(..)
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
obstacleWithRadius :: CFloat -> IO (Id GKCircleObstacle)
obstacleWithRadius radius =
  do
    cls' <- getRequiredClass "GKCircleObstacle"
    sendClassMessage cls' obstacleWithRadiusSelector radius

-- | @- initWithRadius:@
initWithRadius :: IsGKCircleObstacle gkCircleObstacle => gkCircleObstacle -> CFloat -> IO (Id GKCircleObstacle)
initWithRadius gkCircleObstacle radius =
  sendOwnedMessage gkCircleObstacle initWithRadiusSelector radius

-- | Radius of the impassible circle
--
-- ObjC selector: @- radius@
radius :: IsGKCircleObstacle gkCircleObstacle => gkCircleObstacle -> IO CFloat
radius gkCircleObstacle =
  sendMessage gkCircleObstacle radiusSelector

-- | Radius of the impassible circle
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKCircleObstacle gkCircleObstacle => gkCircleObstacle -> CFloat -> IO ()
setRadius gkCircleObstacle value =
  sendMessage gkCircleObstacle setRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @obstacleWithRadius:@
obstacleWithRadiusSelector :: Selector '[CFloat] (Id GKCircleObstacle)
obstacleWithRadiusSelector = mkSelector "obstacleWithRadius:"

-- | @Selector@ for @initWithRadius:@
initWithRadiusSelector :: Selector '[CFloat] (Id GKCircleObstacle)
initWithRadiusSelector = mkSelector "initWithRadius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CFloat
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CFloat] ()
setRadiusSelector = mkSelector "setRadius:"

