{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A default zone that can be use for convenience
--
-- Generated bindings for @MDLMeshBufferZoneDefault@.
module ObjC.ModelIO.MDLMeshBufferZoneDefault
  ( MDLMeshBufferZoneDefault
  , IsMDLMeshBufferZoneDefault(..)
  , capacity
  , allocator
  , allocatorSelector
  , capacitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- capacity@
capacity :: IsMDLMeshBufferZoneDefault mdlMeshBufferZoneDefault => mdlMeshBufferZoneDefault -> IO CULong
capacity mdlMeshBufferZoneDefault =
  sendMessage mdlMeshBufferZoneDefault capacitySelector

-- | @- allocator@
allocator :: IsMDLMeshBufferZoneDefault mdlMeshBufferZoneDefault => mdlMeshBufferZoneDefault -> IO RawId
allocator mdlMeshBufferZoneDefault =
  sendOwnedMessage mdlMeshBufferZoneDefault allocatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capacity@
capacitySelector :: Selector '[] CULong
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @allocator@
allocatorSelector :: Selector '[] RawId
allocatorSelector = mkSelector "allocator"

