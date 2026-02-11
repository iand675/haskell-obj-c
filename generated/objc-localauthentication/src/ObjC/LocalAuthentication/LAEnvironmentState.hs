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
  , initSelector
  , newSelector
  , biometrySelector
  , userPasswordSelector
  , companionsSelector
  , allMechanismsSelector


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

-- | Clients shall not create environment state.
--
-- ObjC selector: @- init@
init_ :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id LAEnvironmentState)
init_ laEnvironmentState  =
  sendMsg laEnvironmentState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Clients shall not create environment state.
--
-- ObjC selector: @+ new@
new :: IO (Id LAEnvironmentState)
new  =
  do
    cls' <- getRequiredClass "LAEnvironmentState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Information about biometric authentication (Touch ID, Face ID or Optic ID).
--
-- @nil@ if biometry is not supported by this device.
--
-- ObjC selector: @- biometry@
biometry :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id LAEnvironmentMechanismBiometry)
biometry laEnvironmentState  =
  sendMsg laEnvironmentState (mkSelector "biometry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Information about local user password (on macOS) or passcode (on embedded platforms).
--
-- @nil@ if user password or passcode is not supported by this device.
--
-- ObjC selector: @- userPassword@
userPassword :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id LAEnvironmentMechanismUserPassword)
userPassword laEnvironmentState  =
  sendMsg laEnvironmentState (mkSelector "userPassword") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Companion authentication mechanisms.
--
-- Companion mechanisms such as Apple Watch can appear and disappear as they get in and out of reach, but             this property enumerates paired companions, even if they are not reachable at the moment. Check @isUsable@             property to determine if a particular companion type is available for use.              Note that items in this array represent paired companion types, not individual devices. Therefore, even if the user             has paired multiple Apple Watch devices for companion authentication, the array will contain only one             @LAEnvironmentMechanimsCompanion@ instance of type @LACompanionTypeWatch.@
--
-- ObjC selector: @- companions@
companions :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id NSArray)
companions laEnvironmentState  =
  sendMsg laEnvironmentState (mkSelector "companions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Information about all authentication mechanisms.
--
-- This property aggregates @biometry,@ @userPassword,@ @companions@ and any future             authentication mechanisms.
--
-- ObjC selector: @- allMechanisms@
allMechanisms :: IsLAEnvironmentState laEnvironmentState => laEnvironmentState -> IO (Id NSArray)
allMechanisms laEnvironmentState  =
  sendMsg laEnvironmentState (mkSelector "allMechanisms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @biometry@
biometrySelector :: Selector
biometrySelector = mkSelector "biometry"

-- | @Selector@ for @userPassword@
userPasswordSelector :: Selector
userPasswordSelector = mkSelector "userPassword"

-- | @Selector@ for @companions@
companionsSelector :: Selector
companionsSelector = mkSelector "companions"

-- | @Selector@ for @allMechanisms@
allMechanismsSelector :: Selector
allMechanismsSelector = mkSelector "allMechanisms"

