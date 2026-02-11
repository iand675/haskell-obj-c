{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , stateSelector
  , authorizationSelector

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

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.CoreBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCBManager cbManager => cbManager -> IO (Id CBManager)
init_ cbManager  =
  sendMsg cbManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | state
--
-- The current state of the manager, initially set to CBManagerStateUnknown.				Updates are provided by required delegate method {
--
-- managerDidUpdateState:}.
--
-- ObjC selector: @- state@
state :: IsCBManager cbManager => cbManager -> IO CBManagerState
state cbManager  =
  fmap (coerce :: CLong -> CBManagerState) $ sendMsg cbManager (mkSelector "state") retCLong []

-- | authorization
--
-- The current authorization of the manager, initially set to CBManagerAuthorizationNotDetermined.				Updates are provided by required delegate method {
--
-- managerDidUpdateState:}.
-- @seealso	state
--
-- ObjC selector: @- authorization@
authorization :: IsCBManager cbManager => cbManager -> IO CBManagerAuthorization
authorization cbManager  =
  fmap (coerce :: CLong -> CBManagerAuthorization) $ sendMsg cbManager (mkSelector "authorization") retCLong []

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
    fmap (coerce :: CLong -> CBManagerAuthorization) $ sendClassMsg cls' (mkSelector "authorization") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector
authorizationSelector = mkSelector "authorization"

