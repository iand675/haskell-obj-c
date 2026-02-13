{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterFabricDescriptor@.
module ObjC.Matter.MTROperationalCredentialsClusterFabricDescriptor
  ( MTROperationalCredentialsClusterFabricDescriptor
  , IsMTROperationalCredentialsClusterFabricDescriptor(..)
  , rootPublicKey
  , setRootPublicKey
  , label
  , setLabel
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
  , labelSelector
  , rootPublicKeySelector
  , setFabricIndexSelector
  , setLabelSelector
  , setRootPublicKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rootPublicKey@
rootPublicKey :: IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor => mtrOperationalCredentialsClusterFabricDescriptor -> IO (Id NSData)
rootPublicKey mtrOperationalCredentialsClusterFabricDescriptor =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptor rootPublicKeySelector

-- | @- setRootPublicKey:@
setRootPublicKey :: (IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor, IsNSData value) => mtrOperationalCredentialsClusterFabricDescriptor -> value -> IO ()
setRootPublicKey mtrOperationalCredentialsClusterFabricDescriptor value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptor setRootPublicKeySelector (toNSData value)

-- | @- label@
label :: IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor => mtrOperationalCredentialsClusterFabricDescriptor -> IO (Id NSString)
label mtrOperationalCredentialsClusterFabricDescriptor =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptor labelSelector

-- | @- setLabel:@
setLabel :: (IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor, IsNSString value) => mtrOperationalCredentialsClusterFabricDescriptor -> value -> IO ()
setLabel mtrOperationalCredentialsClusterFabricDescriptor value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptor setLabelSelector (toNSString value)

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor => mtrOperationalCredentialsClusterFabricDescriptor -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterFabricDescriptor =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptor fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptor -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterFabricDescriptor value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptor setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rootPublicKey@
rootPublicKeySelector :: Selector '[] (Id NSData)
rootPublicKeySelector = mkSelector "rootPublicKey"

-- | @Selector@ for @setRootPublicKey:@
setRootPublicKeySelector :: Selector '[Id NSData] ()
setRootPublicKeySelector = mkSelector "setRootPublicKey:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

