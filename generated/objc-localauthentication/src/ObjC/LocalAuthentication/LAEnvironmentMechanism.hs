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
  , newSelector
  , initSelector
  , isUsableSelector
  , localizedNameSelector
  , iconSystemNameSelector


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

-- | Clients should only consume environment mechanisms..
--
-- ObjC selector: @+ new@
new :: IO (Id LAEnvironmentMechanism)
new  =
  do
    cls' <- getRequiredClass "LAEnvironmentMechanism"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Clients should only consume environment mechanisms..
--
-- ObjC selector: @- init@
init_ :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO (Id LAEnvironmentMechanism)
init_ laEnvironmentMechanism  =
  sendMsg laEnvironmentMechanism (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Whether the mechanism is available for use, i.e. whether the relevant preflight call of @canEvaluatePolicy@ would succeed.
--
-- Warning: If @isUsable@ reads @NO,@ do not assume that it's because of some particular reason. You should check properties of the subclass to determine why mechanism can't be used.
--
-- ObjC selector: @- isUsable@
isUsable :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO Bool
isUsable laEnvironmentMechanism  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laEnvironmentMechanism (mkSelector "isUsable") retCULong []

-- | The localized name of the authentication mechanism, e.g. "Touch ID", "Face ID" etc.
--
-- ObjC selector: @- localizedName@
localizedName :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO (Id NSString)
localizedName laEnvironmentMechanism  =
  sendMsg laEnvironmentMechanism (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of the SF Symbol representing this authentication mechanism.
--
-- ObjC selector: @- iconSystemName@
iconSystemName :: IsLAEnvironmentMechanism laEnvironmentMechanism => laEnvironmentMechanism -> IO (Id NSString)
iconSystemName laEnvironmentMechanism  =
  sendMsg laEnvironmentMechanism (mkSelector "iconSystemName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @isUsable@
isUsableSelector :: Selector
isUsableSelector = mkSelector "isUsable"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @iconSystemName@
iconSystemNameSelector :: Selector
iconSystemNameSelector = mkSelector "iconSystemName"

