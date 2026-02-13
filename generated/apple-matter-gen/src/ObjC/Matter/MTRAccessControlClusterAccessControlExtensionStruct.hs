{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlExtensionStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessControlExtensionStruct
  ( MTRAccessControlClusterAccessControlExtensionStruct
  , IsMTRAccessControlClusterAccessControlExtensionStruct(..)
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
data_ :: IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct => mtrAccessControlClusterAccessControlExtensionStruct -> IO (Id NSData)
data_ mtrAccessControlClusterAccessControlExtensionStruct =
  sendMessage mtrAccessControlClusterAccessControlExtensionStruct dataSelector

-- | @- setData:@
setData :: (IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct, IsNSData value) => mtrAccessControlClusterAccessControlExtensionStruct -> value -> IO ()
setData mtrAccessControlClusterAccessControlExtensionStruct value =
  sendMessage mtrAccessControlClusterAccessControlExtensionStruct setDataSelector (toNSData value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct => mtrAccessControlClusterAccessControlExtensionStruct -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlExtensionStruct =
  sendMessage mtrAccessControlClusterAccessControlExtensionStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionStruct -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlExtensionStruct value =
  sendMessage mtrAccessControlClusterAccessControlExtensionStruct setFabricIndexSelector (toNSNumber value)

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

