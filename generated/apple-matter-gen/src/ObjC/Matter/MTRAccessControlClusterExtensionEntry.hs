{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterExtensionEntry@.
module ObjC.Matter.MTRAccessControlClusterExtensionEntry
  ( MTRAccessControlClusterExtensionEntry
  , IsMTRAccessControlClusterExtensionEntry(..)
  , data_
  , setData
  , fabricIndex
  , setFabricIndex
  , dataSelector
  , fabricIndexSelector
  , setDataSelector
  , setFabricIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- data@
data_ :: IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry => mtrAccessControlClusterExtensionEntry -> IO (Id NSData)
data_ mtrAccessControlClusterExtensionEntry =
  sendMessage mtrAccessControlClusterExtensionEntry dataSelector

-- | @- setData:@
setData :: (IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry, IsNSData value) => mtrAccessControlClusterExtensionEntry -> value -> IO ()
setData mtrAccessControlClusterExtensionEntry value =
  sendMessage mtrAccessControlClusterExtensionEntry setDataSelector (toNSData value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry => mtrAccessControlClusterExtensionEntry -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterExtensionEntry =
  sendMessage mtrAccessControlClusterExtensionEntry fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry, IsNSNumber value) => mtrAccessControlClusterExtensionEntry -> value -> IO ()
setFabricIndex mtrAccessControlClusterExtensionEntry value =
  sendMessage mtrAccessControlClusterExtensionEntry setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

