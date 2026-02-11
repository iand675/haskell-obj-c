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
obstacleWithRadius :: CFloat -> IO (Id GKCircleObstacle)
obstacleWithRadius radius =
  do
    cls' <- getRequiredClass "GKCircleObstacle"
    sendClassMsg cls' (mkSelector "obstacleWithRadius:") (retPtr retVoid) [argCFloat (fromIntegral radius)] >>= retainedObject . castPtr

-- | @- initWithRadius:@
initWithRadius :: IsGKCircleObstacle gkCircleObstacle => gkCircleObstacle -> CFloat -> IO (Id GKCircleObstacle)
initWithRadius gkCircleObstacle  radius =
  sendMsg gkCircleObstacle (mkSelector "initWithRadius:") (retPtr retVoid) [argCFloat (fromIntegral radius)] >>= ownedObject . castPtr

-- | Radius of the impassible circle
--
-- ObjC selector: @- radius@
radius :: IsGKCircleObstacle gkCircleObstacle => gkCircleObstacle -> IO CFloat
radius gkCircleObstacle  =
  sendMsg gkCircleObstacle (mkSelector "radius") retCFloat []

-- | Radius of the impassible circle
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKCircleObstacle gkCircleObstacle => gkCircleObstacle -> CFloat -> IO ()
setRadius gkCircleObstacle  value =
  sendMsg gkCircleObstacle (mkSelector "setRadius:") retVoid [argCFloat (fromIntegral value)]

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

