{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A 3D specialization of an agent that moves on a 3-axis logical coordinate system.
--
-- Generated bindings for @GKAgent3D@.
module ObjC.GameplayKit.GKAgent3D
  ( GKAgent3D
  , IsGKAgent3D(..)
  , updateWithDeltaTime
  , rightHanded
  , setRightHanded
  , updateWithDeltaTimeSelector
  , rightHandedSelector
  , setRightHandedSelector


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
updateWithDeltaTime :: IsGKAgent3D gkAgent3D => gkAgent3D -> CDouble -> IO ()
updateWithDeltaTime gkAgent3D  seconds =
  sendMsg gkAgent3D (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral seconds)]

-- | Should this vehicle operate in a right-handed coordinate system? NO means it will be left-handed
--
-- ObjC selector: @- rightHanded@
rightHanded :: IsGKAgent3D gkAgent3D => gkAgent3D -> IO Bool
rightHanded gkAgent3D  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkAgent3D (mkSelector "rightHanded") retCULong []

-- | Should this vehicle operate in a right-handed coordinate system? NO means it will be left-handed
--
-- ObjC selector: @- setRightHanded:@
setRightHanded :: IsGKAgent3D gkAgent3D => gkAgent3D -> Bool -> IO ()
setRightHanded gkAgent3D  value =
  sendMsg gkAgent3D (mkSelector "setRightHanded:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @rightHanded@
rightHandedSelector :: Selector
rightHandedSelector = mkSelector "rightHanded"

-- | @Selector@ for @setRightHanded:@
setRightHandedSelector :: Selector
setRightHandedSelector = mkSelector "setRightHanded:"

