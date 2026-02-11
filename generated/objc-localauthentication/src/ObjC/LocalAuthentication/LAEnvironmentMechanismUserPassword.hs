{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironmentMechanismUserPassword@.
module ObjC.LocalAuthentication.LAEnvironmentMechanismUserPassword
  ( LAEnvironmentMechanismUserPassword
  , IsLAEnvironmentMechanismUserPassword(..)
  , isSet
  , isSetSelector


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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the local user password or passcode is set on this device.
--
-- ObjC selector: @- isSet@
isSet :: IsLAEnvironmentMechanismUserPassword laEnvironmentMechanismUserPassword => laEnvironmentMechanismUserPassword -> IO Bool
isSet laEnvironmentMechanismUserPassword  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laEnvironmentMechanismUserPassword (mkSelector "isSet") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSet@
isSetSelector :: Selector
isSetSelector = mkSelector "isSet"

