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
  , obstacleWithRadiusSelector
  , initWithRadiusSelector
  , radiusSelector
  , setRadiusSelector


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

-- | @+ obstacleWithRadius:@
obstacleWithRadius :: CFloat -> IO (Id GKSphereObstacle)
obstacleWithRadius radius =
  do
    cls' <- getRequiredClass "GKSphereObstacle"
    sendClassMsg cls' (mkSelector "obstacleWithRadius:") (retPtr retVoid) [argCFloat (fromIntegral radius)] >>= retainedObject . castPtr

-- | @- initWithRadius:@
initWithRadius :: IsGKSphereObstacle gkSphereObstacle => gkSphereObstacle -> CFloat -> IO (Id GKSphereObstacle)
initWithRadius gkSphereObstacle  radius =
  sendMsg gkSphereObstacle (mkSelector "initWithRadius:") (retPtr retVoid) [argCFloat (fromIntegral radius)] >>= ownedObject . castPtr

-- | Radius of the impassible circle
--
-- ObjC selector: @- radius@
radius :: IsGKSphereObstacle gkSphereObstacle => gkSphereObstacle -> IO CFloat
radius gkSphereObstacle  =
  sendMsg gkSphereObstacle (mkSelector "radius") retCFloat []

-- | Radius of the impassible circle
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKSphereObstacle gkSphereObstacle => gkSphereObstacle -> CFloat -> IO ()
setRadius gkSphereObstacle  value =
  sendMsg gkSphereObstacle (mkSelector "setRadius:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @obstacleWithRadius:@
obstacleWithRadiusSelector :: Selector
obstacleWithRadiusSelector = mkSelector "obstacleWithRadius:"

-- | @Selector@ for @initWithRadius:@
initWithRadiusSelector :: Selector
initWithRadiusSelector = mkSelector "initWithRadius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

