{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAttitude@.
module ObjC.CoreMotion.CMAttitude
  ( CMAttitude
  , IsCMAttitude(..)
  , multiplyByInverseOfAttitude
  , roll
  , pitch
  , yaw
  , multiplyByInverseOfAttitudeSelector
  , pitchSelector
  , rollSelector
  , yawSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- multiplyByInverseOfAttitude:@
multiplyByInverseOfAttitude :: (IsCMAttitude cmAttitude, IsCMAttitude attitude) => cmAttitude -> attitude -> IO ()
multiplyByInverseOfAttitude cmAttitude attitude =
  sendMessage cmAttitude multiplyByInverseOfAttitudeSelector (toCMAttitude attitude)

-- | @- roll@
roll :: IsCMAttitude cmAttitude => cmAttitude -> IO CDouble
roll cmAttitude =
  sendMessage cmAttitude rollSelector

-- | @- pitch@
pitch :: IsCMAttitude cmAttitude => cmAttitude -> IO CDouble
pitch cmAttitude =
  sendMessage cmAttitude pitchSelector

-- | @- yaw@
yaw :: IsCMAttitude cmAttitude => cmAttitude -> IO CDouble
yaw cmAttitude =
  sendMessage cmAttitude yawSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @multiplyByInverseOfAttitude:@
multiplyByInverseOfAttitudeSelector :: Selector '[Id CMAttitude] ()
multiplyByInverseOfAttitudeSelector = mkSelector "multiplyByInverseOfAttitude:"

-- | @Selector@ for @roll@
rollSelector :: Selector '[] CDouble
rollSelector = mkSelector "roll"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] CDouble
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @yaw@
yawSelector :: Selector '[] CDouble
yawSelector = mkSelector "yaw"

