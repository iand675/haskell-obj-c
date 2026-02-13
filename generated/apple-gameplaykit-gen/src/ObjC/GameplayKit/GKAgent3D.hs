{-# LANGUAGE DataKinds #-}
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
  , rightHandedSelector
  , setRightHandedSelector
  , updateWithDeltaTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Overridden from GKComponent. Updates this agent with the current behavior, generating a force to reach its goals and applying that force.
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKAgent3D gkAgent3D => gkAgent3D -> CDouble -> IO ()
updateWithDeltaTime gkAgent3D seconds =
  sendMessage gkAgent3D updateWithDeltaTimeSelector seconds

-- | Should this vehicle operate in a right-handed coordinate system? NO means it will be left-handed
--
-- ObjC selector: @- rightHanded@
rightHanded :: IsGKAgent3D gkAgent3D => gkAgent3D -> IO Bool
rightHanded gkAgent3D =
  sendMessage gkAgent3D rightHandedSelector

-- | Should this vehicle operate in a right-handed coordinate system? NO means it will be left-handed
--
-- ObjC selector: @- setRightHanded:@
setRightHanded :: IsGKAgent3D gkAgent3D => gkAgent3D -> Bool -> IO ()
setRightHanded gkAgent3D value =
  sendMessage gkAgent3D setRightHandedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector '[CDouble] ()
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @rightHanded@
rightHandedSelector :: Selector '[] Bool
rightHandedSelector = mkSelector "rightHanded"

-- | @Selector@ for @setRightHanded:@
setRightHandedSelector :: Selector '[Bool] ()
setRightHandedSelector = mkSelector "setRightHanded:"

