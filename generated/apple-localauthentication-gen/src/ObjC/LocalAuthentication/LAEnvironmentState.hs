{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironmentState@.
module ObjC.LocalAuthentication.LAEnvironmentState
  ( LAEnvironmentState
  , IsLAEnvironmentState(..)
  , init_
  , new
  , biometry
  , userPassword
  , companions
  , allMechanisms
  , allMechanismsSelector
  , biometrySelector
  , companionsSelector
  , initSelector
  , newSelector
  , userPasswordSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Clients shall not create environment state.
--
-- ObjC selector: @- init@
init_ :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id LAEnvironmentState)
init_ laEnvironmentState =
  sendOwnedMessage laEnvironmentState initSelector

-- | Clients shall not create environment state.
--
-- ObjC selector: @+ new@
new :: IO (Id LAEnvironmentState)
new  =
  do
    cls' <- getRequiredClass "LAEnvironmentState"
    sendOwnedClassMessage cls' newSelector

-- | Information about biometric authentication (Touch ID, Face ID or Optic ID).
--
-- @nil@ if biometry is not supported by this device.
--
-- ObjC selector: @- biometry@
biometry :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id LAEnvironmentMechanismBiometry)
biometry laEnvironmentState =
  sendMessage laEnvironmentState biometrySelector

-- | Information about local user password (on macOS) or passcode (on embedded platforms).
--
-- @nil@ if user password or passcode is not supported by this device.
--
-- ObjC selector: @- userPassword@
userPassword :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id LAEnvironmentMechanismUserPassword)
userPassword laEnvironmentState =
  sendMessage laEnvironmentState userPasswordSelector

-- | Companion authentication mechanisms.
--
-- Companion mechanisms such as Apple Watch can appear and disappear as they get in and out of reach, but             this property enumerates paired companions, even if they are not reachable at the moment. Check @isUsable@             property to determine if a particular companion type is available for use.              Note that items in this array represent paired companion types, not individual devices. Therefore, even if the user             has paired multiple Apple Watch devices for companion authentication, the array will contain only one             @LAEnvironmentMechanimsCompanion@ instance of type @LACompanionTypeWatch.@
--
-- ObjC selector: @- companions@
companions :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id NSArray)
companions laEnvironmentState =
  sendMessage laEnvironmentState companionsSelector

-- | Information about all authentication mechanisms.
--
-- This property aggregates @biometry,@ @userPassword,@ @companions@ and any future             authentication mechanisms.
--
-- ObjC selector: @- allMechanisms@
allMechanisms :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id NSArray)
allMechanisms laEnvironmentState =
  sendMessage laEnvironmentState allMechanismsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LAEnvironmentState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LAEnvironmentState)
newSelector = mkSelector "new"

-- | @Selector@ for @biometry@
biometrySelector :: Selector '[] (Id LAEnvironmentMechanismBiometry)
biometrySelector = mkSelector "biometry"

-- | @Selector@ for @userPassword@
userPasswordSelector :: Selector '[] (Id LAEnvironmentMechanismUserPassword)
userPasswordSelector = mkSelector "userPassword"

-- | @Selector@ for @companions@
companionsSelector :: Selector '[] (Id NSArray)
companionsSelector = mkSelector "companions"

-- | @Selector@ for @allMechanisms@
allMechanismsSelector :: Selector '[] (Id NSArray)
allMechanismsSelector = mkSelector "allMechanisms"

