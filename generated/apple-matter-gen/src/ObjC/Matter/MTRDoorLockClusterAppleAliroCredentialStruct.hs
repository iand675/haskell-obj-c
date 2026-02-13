{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroCredentialStruct@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroCredentialStruct
  ( MTRDoorLockClusterAppleAliroCredentialStruct
  , IsMTRDoorLockClusterAppleAliroCredentialStruct(..)
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
credentialType :: IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct => mtrDoorLockClusterAppleAliroCredentialStruct -> IO (Id NSNumber)
credentialType mtrDoorLockClusterAppleAliroCredentialStruct =
  sendMessage mtrDoorLockClusterAppleAliroCredentialStruct credentialTypeSelector

-- | @- setCredentialType:@
setCredentialType :: (IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct, IsNSNumber value) => mtrDoorLockClusterAppleAliroCredentialStruct -> value -> IO ()
setCredentialType mtrDoorLockClusterAppleAliroCredentialStruct value =
  sendMessage mtrDoorLockClusterAppleAliroCredentialStruct setCredentialTypeSelector (toNSNumber value)

-- | @- credentialIndex@
credentialIndex :: IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct => mtrDoorLockClusterAppleAliroCredentialStruct -> IO (Id NSNumber)
credentialIndex mtrDoorLockClusterAppleAliroCredentialStruct =
  sendMessage mtrDoorLockClusterAppleAliroCredentialStruct credentialIndexSelector

-- | @- setCredentialIndex:@
setCredentialIndex :: (IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct, IsNSNumber value) => mtrDoorLockClusterAppleAliroCredentialStruct -> value -> IO ()
setCredentialIndex mtrDoorLockClusterAppleAliroCredentialStruct value =
  sendMessage mtrDoorLockClusterAppleAliroCredentialStruct setCredentialIndexSelector (toNSNumber value)

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

