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
  , rollSelector
  , pitchSelector
  , yawSelector


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

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- multiplyByInverseOfAttitude:@
multiplyByInverseOfAttitude :: (IsCMAttitude cmAttitude, IsCMAttitude attitude) => cmAttitude -> attitude -> IO ()
multiplyByInverseOfAttitude cmAttitude  attitude =
withObjCPtr attitude $ \raw_attitude ->
    sendMsg cmAttitude (mkSelector "multiplyByInverseOfAttitude:") retVoid [argPtr (castPtr raw_attitude :: Ptr ())]

-- | @- roll@
roll :: IsCMAttitude cmAttitude => cmAttitude -> IO CDouble
roll cmAttitude  =
  sendMsg cmAttitude (mkSelector "roll") retCDouble []

-- | @- pitch@
pitch :: IsCMAttitude cmAttitude => cmAttitude -> IO CDouble
pitch cmAttitude  =
  sendMsg cmAttitude (mkSelector "pitch") retCDouble []

-- | @- yaw@
yaw :: IsCMAttitude cmAttitude => cmAttitude -> IO CDouble
yaw cmAttitude  =
  sendMsg cmAttitude (mkSelector "yaw") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @multiplyByInverseOfAttitude:@
multiplyByInverseOfAttitudeSelector :: Selector
multiplyByInverseOfAttitudeSelector = mkSelector "multiplyByInverseOfAttitude:"

-- | @Selector@ for @roll@
rollSelector :: Selector
rollSelector = mkSelector "roll"

-- | @Selector@ for @pitch@
pitchSelector :: Selector
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @yaw@
yawSelector :: Selector
yawSelector = mkSelector "yaw"

