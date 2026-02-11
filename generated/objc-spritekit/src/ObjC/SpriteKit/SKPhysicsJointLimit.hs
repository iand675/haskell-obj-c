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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- maxLength@
maxLength :: IsSKPhysicsJointLimit skPhysicsJointLimit => skPhysicsJointLimit -> IO CDouble
maxLength skPhysicsJointLimit  =
  sendMsg skPhysicsJointLimit (mkSelector "maxLength") retCDouble []

-- | @- setMaxLength:@
setMaxLength :: IsSKPhysicsJointLimit skPhysicsJointLimit => skPhysicsJointLimit -> CDouble -> IO ()
setMaxLength skPhysicsJointLimit  value =
  sendMsg skPhysicsJointLimit (mkSelector "setMaxLength:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxLength@
maxLengthSelector :: Selector
maxLengthSelector = mkSelector "maxLength"

-- | @Selector@ for @setMaxLength:@
setMaxLengthSelector :: Selector
setMaxLengthSelector = mkSelector "setMaxLength:"

