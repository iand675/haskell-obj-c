{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterNOCStruct@.
module ObjC.Matter.MTROperationalCredentialsClusterNOCStruct
  ( MTROperationalCredentialsClusterNOCStruct
  , IsMTROperationalCredentialsClusterNOCStruct(..)
  , noc
  , setNoc
  , icac
  , setIcac
  , vvsc
  , setVvsc
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
  , icacSelector
  , nocSelector
  , setFabricIndexSelector
  , setIcacSelector
  , setNocSelector
  , setVvscSelector
  , vvscSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- noc@
noc :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSData)
noc mtrOperationalCredentialsClusterNOCStruct =
  sendMessage mtrOperationalCredentialsClusterNOCStruct nocSelector

-- | @- setNoc:@
setNoc :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSData value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setNoc mtrOperationalCredentialsClusterNOCStruct value =
  sendMessage mtrOperationalCredentialsClusterNOCStruct setNocSelector (toNSData value)

-- | @- icac@
icac :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSData)
icac mtrOperationalCredentialsClusterNOCStruct =
  sendMessage mtrOperationalCredentialsClusterNOCStruct icacSelector

-- | @- setIcac:@
setIcac :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSData value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setIcac mtrOperationalCredentialsClusterNOCStruct value =
  sendMessage mtrOperationalCredentialsClusterNOCStruct setIcacSelector (toNSData value)

-- | @- vvsc@
vvsc :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSData)
vvsc mtrOperationalCredentialsClusterNOCStruct =
  sendMessage mtrOperationalCredentialsClusterNOCStruct vvscSelector

-- | @- setVvsc:@
setVvsc :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSData value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setVvsc mtrOperationalCredentialsClusterNOCStruct value =
  sendMessage mtrOperationalCredentialsClusterNOCStruct setVvscSelector (toNSData value)

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct => mtrOperationalCredentialsClusterNOCStruct -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterNOCStruct =
  sendMessage mtrOperationalCredentialsClusterNOCStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterNOCStruct mtrOperationalCredentialsClusterNOCStruct, IsNSNumber value) => mtrOperationalCredentialsClusterNOCStruct -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterNOCStruct value =
  sendMessage mtrOperationalCredentialsClusterNOCStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @noc@
nocSelector :: Selector '[] (Id NSData)
nocSelector = mkSelector "noc"

-- | @Selector@ for @setNoc:@
setNocSelector :: Selector '[Id NSData] ()
setNocSelector = mkSelector "setNoc:"

-- | @Selector@ for @icac@
icacSelector :: Selector '[] (Id NSData)
icacSelector = mkSelector "icac"

-- | @Selector@ for @setIcac:@
setIcacSelector :: Selector '[Id NSData] ()
setIcacSelector = mkSelector "setIcac:"

-- | @Selector@ for @vvsc@
vvscSelector :: Selector '[] (Id NSData)
vvscSelector = mkSelector "vvsc"

-- | @Selector@ for @setVvsc:@
setVvscSelector :: Selector '[Id NSData] ()
setVvscSelector = mkSelector "setVvsc:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

