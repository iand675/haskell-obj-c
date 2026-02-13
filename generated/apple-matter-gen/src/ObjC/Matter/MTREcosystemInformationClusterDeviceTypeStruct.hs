{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREcosystemInformationClusterDeviceTypeStruct@.
module ObjC.Matter.MTREcosystemInformationClusterDeviceTypeStruct
  ( MTREcosystemInformationClusterDeviceTypeStruct
  , IsMTREcosystemInformationClusterDeviceTypeStruct(..)
  , deviceType
  , setDeviceType
  , revision
  , setRevision
  , deviceTypeSelector
  , revisionSelector
  , setDeviceTypeSelector
  , setRevisionSelector


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
deviceType :: IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct => mtrEcosystemInformationClusterDeviceTypeStruct -> IO (Id NSNumber)
deviceType mtrEcosystemInformationClusterDeviceTypeStruct =
  sendMessage mtrEcosystemInformationClusterDeviceTypeStruct deviceTypeSelector

-- | @- setDeviceType:@
setDeviceType :: (IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct, IsNSNumber value) => mtrEcosystemInformationClusterDeviceTypeStruct -> value -> IO ()
setDeviceType mtrEcosystemInformationClusterDeviceTypeStruct value =
  sendMessage mtrEcosystemInformationClusterDeviceTypeStruct setDeviceTypeSelector (toNSNumber value)

-- | @- revision@
revision :: IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct => mtrEcosystemInformationClusterDeviceTypeStruct -> IO (Id NSNumber)
revision mtrEcosystemInformationClusterDeviceTypeStruct =
  sendMessage mtrEcosystemInformationClusterDeviceTypeStruct revisionSelector

-- | @- setRevision:@
setRevision :: (IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct, IsNSNumber value) => mtrEcosystemInformationClusterDeviceTypeStruct -> value -> IO ()
setRevision mtrEcosystemInformationClusterDeviceTypeStruct value =
  sendMessage mtrEcosystemInformationClusterDeviceTypeStruct setRevisionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector '[] (Id NSNumber)
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @setDeviceType:@
setDeviceTypeSelector :: Selector '[Id NSNumber] ()
setDeviceTypeSelector = mkSelector "setDeviceType:"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] (Id NSNumber)
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector '[Id NSNumber] ()
setRevisionSelector = mkSelector "setRevision:"

