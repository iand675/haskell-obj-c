{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterCredentialStruct@.
module ObjC.Matter.MTRDoorLockClusterCredentialStruct
  ( MTRDoorLockClusterCredentialStruct
  , IsMTRDoorLockClusterCredentialStruct(..)
  , credentialType
  , setCredentialType
  , credentialIndex
  , setCredentialIndex
  , credentialIndexSelector
  , credentialTypeSelector
  , setCredentialIndexSelector
  , setCredentialTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- credentialType@
credentialType :: IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct => mtrDoorLockClusterCredentialStruct -> IO (Id NSNumber)
credentialType mtrDoorLockClusterCredentialStruct =
  sendMessage mtrDoorLockClusterCredentialStruct credentialTypeSelector

-- | @- setCredentialType:@
setCredentialType :: (IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct, IsNSNumber value) => mtrDoorLockClusterCredentialStruct -> value -> IO ()
setCredentialType mtrDoorLockClusterCredentialStruct value =
  sendMessage mtrDoorLockClusterCredentialStruct setCredentialTypeSelector (toNSNumber value)

-- | @- credentialIndex@
credentialIndex :: IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct => mtrDoorLockClusterCredentialStruct -> IO (Id NSNumber)
credentialIndex mtrDoorLockClusterCredentialStruct =
  sendMessage mtrDoorLockClusterCredentialStruct credentialIndexSelector

-- | @- setCredentialIndex:@
setCredentialIndex :: (IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct, IsNSNumber value) => mtrDoorLockClusterCredentialStruct -> value -> IO ()
setCredentialIndex mtrDoorLockClusterCredentialStruct value =
  sendMessage mtrDoorLockClusterCredentialStruct setCredentialIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @credentialType@
credentialTypeSelector :: Selector '[] (Id NSNumber)
credentialTypeSelector = mkSelector "credentialType"

-- | @Selector@ for @setCredentialType:@
setCredentialTypeSelector :: Selector '[Id NSNumber] ()
setCredentialTypeSelector = mkSelector "setCredentialType:"

-- | @Selector@ for @credentialIndex@
credentialIndexSelector :: Selector '[] (Id NSNumber)
credentialIndexSelector = mkSelector "credentialIndex"

-- | @Selector@ for @setCredentialIndex:@
setCredentialIndexSelector :: Selector '[Id NSNumber] ()
setCredentialIndexSelector = mkSelector "setCredentialIndex:"

