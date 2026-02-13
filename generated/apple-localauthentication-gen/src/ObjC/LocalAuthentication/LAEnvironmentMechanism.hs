{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironmentMechanism@.
module ObjC.LocalAuthentication.LAEnvironmentMechanism
  ( LAEnvironmentMechanism
  , IsLAEnvironmentMechanism(..)
  , new
  , init_
  , isUsable
  , localizedName
  , iconSystemName
  , iconSystemNameSelector
  , initSelector
  , isUsableSelector
  , localizedNameSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Clients should only consume environment mechanisms..
--
-- ObjC selector: @+ new@
new :: IO (Id LAEnvironmentMechanism)
new  =
  do
    cls' <- getRequiredClass "LAEnvironmentMechanism"
    sendOwnedClassMessage cls' newSelector

-- | The Clients should only consume environment mechanisms..
--
-- ObjC selector: @- init@
init_ :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO (Id LAEnvironmentMechanism)
init_ laEnvironmentMechanism =
  sendOwnedMessage laEnvironmentMechanism initSelector

-- | Whether the mechanism is available for use, i.e. whether the relevant preflight call of @canEvaluatePolicy@ would succeed.
--
-- Warning: If @isUsable@ reads @NO,@ do not assume that it's because of some particular reason. You should check properties of the subclass to determine why mechanism can't be used.
--
-- ObjC selector: @- isUsable@
isUsable :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO Bool
isUsable laEnvironmentMechanism =
  sendMessage laEnvironmentMechanism isUsableSelector

-- | The localized name of the authentication mechanism, e.g. "Touch ID", "Face ID" etc.
--
-- ObjC selector: @- localizedName@
localizedName :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO (Id NSString)
localizedName laEnvironmentMechanism =
  sendMessage laEnvironmentMechanism localizedNameSelector

-- | Name of the SF Symbol representing this authentication mechanism.
--
-- ObjC selector: @- iconSystemName@
iconSystemName :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO (Id NSString)
iconSystemName laEnvironmentMechanism =
  sendMessage laEnvironmentMechanism iconSystemNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LAEnvironmentMechanism)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LAEnvironmentMechanism)
initSelector = mkSelector "init"

-- | @Selector@ for @isUsable@
isUsableSelector :: Selector '[] Bool
isUsableSelector = mkSelector "isUsable"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @iconSystemName@
iconSystemNameSelector :: Selector '[] (Id NSString)
iconSystemNameSelector = mkSelector "iconSystemName"

