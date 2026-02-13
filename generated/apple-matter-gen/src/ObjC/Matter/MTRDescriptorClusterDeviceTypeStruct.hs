{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDescriptorClusterDeviceTypeStruct@.
module ObjC.Matter.MTRDescriptorClusterDeviceTypeStruct
  ( MTRDescriptorClusterDeviceTypeStruct
  , IsMTRDescriptorClusterDeviceTypeStruct(..)
  , deviceType
  , setDeviceType
  , type_
  , setType
  , revision
  , setRevision
  , deviceTypeSelector
  , revisionSelector
  , setDeviceTypeSelector
  , setRevisionSelector
  , setTypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deviceType@
deviceType :: IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct => mtrDescriptorClusterDeviceTypeStruct -> IO (Id NSNumber)
deviceType mtrDescriptorClusterDeviceTypeStruct =
  sendMessage mtrDescriptorClusterDeviceTypeStruct deviceTypeSelector

-- | @- setDeviceType:@
setDeviceType :: (IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct, IsNSNumber value) => mtrDescriptorClusterDeviceTypeStruct -> value -> IO ()
setDeviceType mtrDescriptorClusterDeviceTypeStruct value =
  sendMessage mtrDescriptorClusterDeviceTypeStruct setDeviceTypeSelector (toNSNumber value)

-- | @- type@
type_ :: IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct => mtrDescriptorClusterDeviceTypeStruct -> IO (Id NSNumber)
type_ mtrDescriptorClusterDeviceTypeStruct =
  sendMessage mtrDescriptorClusterDeviceTypeStruct typeSelector

-- | @- setType:@
setType :: (IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct, IsNSNumber value) => mtrDescriptorClusterDeviceTypeStruct -> value -> IO ()
setType mtrDescriptorClusterDeviceTypeStruct value =
  sendMessage mtrDescriptorClusterDeviceTypeStruct setTypeSelector (toNSNumber value)

-- | @- revision@
revision :: IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct => mtrDescriptorClusterDeviceTypeStruct -> IO (Id NSNumber)
revision mtrDescriptorClusterDeviceTypeStruct =
  sendMessage mtrDescriptorClusterDeviceTypeStruct revisionSelector

-- | @- setRevision:@
setRevision :: (IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct, IsNSNumber value) => mtrDescriptorClusterDeviceTypeStruct -> value -> IO ()
setRevision mtrDescriptorClusterDeviceTypeStruct value =
  sendMessage mtrDescriptorClusterDeviceTypeStruct setRevisionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector '[] (Id NSNumber)
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @setDeviceType:@
setDeviceTypeSelector :: Selector '[Id NSNumber] ()
setDeviceTypeSelector = mkSelector "setDeviceType:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSNumber)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSNumber] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] (Id NSNumber)
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector '[Id NSNumber] ()
setRevisionSelector = mkSelector "setRevision:"

