{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsJointLimit@.
module ObjC.SpriteKit.SKPhysicsJointLimit
  ( SKPhysicsJointLimit
  , IsSKPhysicsJointLimit(..)
  , maxLength
  , setMaxLength
  , maxLengthSelector
  , setMaxLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- maxLength@
maxLength :: IsSKPhysicsJointLimit skPhysicsJointLimit => skPhysicsJointLimit -> IO CDouble
maxLength skPhysicsJointLimit =
  sendMessage skPhysicsJointLimit maxLengthSelector

-- | @- setMaxLength:@
setMaxLength :: IsSKPhysicsJointLimit skPhysicsJointLimit => skPhysicsJointLimit -> CDouble -> IO ()
setMaxLength skPhysicsJointLimit value =
  sendMessage skPhysicsJointLimit setMaxLengthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxLength@
maxLengthSelector :: Selector '[] CDouble
maxLengthSelector = mkSelector "maxLength"

-- | @Selector@ for @setMaxLength:@
setMaxLengthSelector :: Selector '[CDouble] ()
setMaxLengthSelector = mkSelector "setMaxLength:"

