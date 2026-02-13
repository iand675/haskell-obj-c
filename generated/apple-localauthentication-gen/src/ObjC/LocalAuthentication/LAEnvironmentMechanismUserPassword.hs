{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the local user password or passcode is set on this device.
--
-- ObjC selector: @- isSet@
isSet :: IsLAEnvironmentMechanismUserPassword laEnvironmentMechanismUserPassword => laEnvironmentMechanismUserPassword -> IO Bool
isSet laEnvironmentMechanismUserPassword =
  sendMessage laEnvironmentMechanismUserPassword isSetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSet@
isSetSelector :: Selector '[] Bool
isSetSelector = mkSelector "isSet"

