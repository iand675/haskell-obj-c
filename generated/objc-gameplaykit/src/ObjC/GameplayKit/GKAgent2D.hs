{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A 2D specalization of an agent that moves on a 2-axis logical coordinate system. This coordinate system does not need to match the visual coordinate system of the delegate. One simple case of that is isometric 2D content where the game model is on a flat 2D plane but the visuals are displayed on an angle where one of the logical axes are used for simulated depth as well as some translation in the display plane.
--
-- Generated bindings for @GKAgent2D@.
module ObjC.GameplayKit.GKAgent2D
  ( GKAgent2D
  , IsGKAgent2D(..)
  , updateWithDeltaTime
  , rotation
  , setRotation
  , updateWithDeltaTimeSelector
  , rotationSelector
  , setRotationSelector


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

-- | Overridden from GKComponent. Updates this agent with the current behavior, generating a force to reach its goals and applying that force.
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKAgent2D gkAgent2D => gkAgent2D -> CDouble -> IO ()
updateWithDeltaTime gkAgent2D  seconds =
  sendMsg gkAgent2D (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral seconds)]

-- | Z rotation of the agent on the logical XY plane
--
-- ObjC selector: @- rotation@
rotation :: IsGKAgent2D gkAgent2D => gkAgent2D -> IO CFloat
rotation gkAgent2D  =
  sendMsg gkAgent2D (mkSelector "rotation") retCFloat []

-- | Z rotation of the agent on the logical XY plane
--
-- ObjC selector: @- setRotation:@
setRotation :: IsGKAgent2D gkAgent2D => gkAgent2D -> CFloat -> IO ()
setRotation gkAgent2D  value =
  sendMsg gkAgent2D (mkSelector "setRotation:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

