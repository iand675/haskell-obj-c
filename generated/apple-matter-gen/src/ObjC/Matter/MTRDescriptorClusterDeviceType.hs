{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDescriptorClusterDeviceType@.
module ObjC.Matter.MTRDescriptorClusterDeviceType
  ( MTRDescriptorClusterDeviceType
  , IsMTRDescriptorClusterDeviceType(..)
  , revision
  , setRevision
  , revisionSelector
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

-- | @- revision@
revision :: IsMTRDescriptorClusterDeviceType mtrDescriptorClusterDeviceType => mtrDescriptorClusterDeviceType -> IO (Id NSNumber)
revision mtrDescriptorClusterDeviceType =
  sendMessage mtrDescriptorClusterDeviceType revisionSelector

-- | @- setRevision:@
setRevision :: (IsMTRDescriptorClusterDeviceType mtrDescriptorClusterDeviceType, IsNSNumber value) => mtrDescriptorClusterDeviceType -> value -> IO ()
setRevision mtrDescriptorClusterDeviceType value =
  sendMessage mtrDescriptorClusterDeviceType setRevisionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] (Id NSNumber)
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector '[Id NSNumber] ()
setRevisionSelector = mkSelector "setRevision:"

