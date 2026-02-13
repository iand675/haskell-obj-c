{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASCredentialIdentityStoreState@.
module ObjC.AuthenticationServices.ASCredentialIdentityStoreState
  ( ASCredentialIdentityStoreState
  , IsASCredentialIdentityStoreState(..)
  , enabled
  , supportsIncrementalUpdates
  , enabledSelector
  , supportsIncrementalUpdatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Get the enabled state of the credential identity store.
--
-- Returns: YES if the credential identity store is enabled.
--
-- You can only modify the credential identity store when it is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsASCredentialIdentityStoreState asCredentialIdentityStoreState => asCredentialIdentityStoreState -> IO Bool
enabled asCredentialIdentityStoreState =
  sendMessage asCredentialIdentityStoreState enabledSelector

-- | Get whether the credential identity store supports incremental updates.
--
-- Returns: YES if the credential identity store supports incremental updates.
--
-- You should examine the value returned by this property to find out if the credential identity store can accept incremental updates. If incremental updates are supported, you can update the credential identity store with only the new changes since the last time it was updated. Otherwise, you should update the credential identity store by adding all credential identities.
--
-- ObjC selector: @- supportsIncrementalUpdates@
supportsIncrementalUpdates :: IsASCredentialIdentityStoreState asCredentialIdentityStoreState => asCredentialIdentityStoreState -> IO Bool
supportsIncrementalUpdates asCredentialIdentityStoreState =
  sendMessage asCredentialIdentityStoreState supportsIncrementalUpdatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @supportsIncrementalUpdates@
supportsIncrementalUpdatesSelector :: Selector '[] Bool
supportsIncrementalUpdatesSelector = mkSelector "supportsIncrementalUpdates"

