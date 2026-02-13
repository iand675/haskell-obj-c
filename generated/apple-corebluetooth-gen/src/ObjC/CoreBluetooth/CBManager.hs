{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CBManager@.
module ObjC.CoreBluetooth.CBManager
  ( CBManager
  , IsCBManager(..)
  , init_
  , state
  , authorization
  , cbManagerAuthorization
  , authorizationSelector
  , cbManagerAuthorizationSelector
  , initSelector
  , stateSelector

  -- * Enum types
  , CBManagerAuthorization(CBManagerAuthorization)
  , pattern CBManagerAuthorizationNotDetermined
  , pattern CBManagerAuthorizationRestricted
  , pattern CBManagerAuthorizationDenied
  , pattern CBManagerAuthorizationAllowedAlways
  , CBManagerState(CBManagerState)
  , pattern CBManagerStateUnknown
  , pattern CBManagerStateResetting
  , pattern CBManagerStateUnsupported
  , pattern CBManagerStateUnauthorized
  , pattern CBManagerStatePoweredOff
  , pattern CBManagerStatePoweredOn

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.CoreBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCBManager cbManager => cbManager -> IO (Id CBManager)
init_ cbManager =
  sendOwnedMessage cbManager initSelector

-- | state
--
-- The current state of the manager, initially set to CBManagerStateUnknown.				Updates are provided by required delegate method {
--
-- managerDidUpdateState:}.
--
-- ObjC selector: @- state@
state :: IsCBManager cbManager => cbManager -> IO CBManagerState
state cbManager =
  sendMessage cbManager stateSelector

-- | authorization
--
-- The current authorization of the manager, initially set to CBManagerAuthorizationNotDetermined.				Updates are provided by required delegate method {
--
-- managerDidUpdateState:}.
-- @seealso	state
--
-- ObjC selector: @- authorization@
authorization :: IsCBManager cbManager => cbManager -> IO CBManagerAuthorization
authorization cbManager =
  sendMessage cbManager authorizationSelector

-- | authorization
--
-- The current authorization of the manager, initially set to CBManagerAuthorizationNotDetermined.  			You can check this in your implementation of required delegate method {
--
-- managerDidUpdateState:}. You can also use it to check authorization status before allocating CBManager.
-- @seealso	state
--
-- ObjC selector: @+ authorization@
cbManagerAuthorization :: IO CBManagerAuthorization
cbManagerAuthorization  =
  do
    cls' <- getRequiredClass "CBManager"
    sendClassMessage cls' cbManagerAuthorizationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CBManager)
initSelector = mkSelector "init"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CBManagerState
stateSelector = mkSelector "state"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector '[] CBManagerAuthorization
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @authorization@
cbManagerAuthorizationSelector :: Selector '[] CBManagerAuthorization
cbManagerAuthorizationSelector = mkSelector "authorization"

