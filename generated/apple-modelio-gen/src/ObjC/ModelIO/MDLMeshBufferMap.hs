{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLMeshBufferMap
--
-- Represents a reference to memory of a mapped MeshBuffer
--
-- Generated bindings for @MDLMeshBufferMap@.
module ObjC.ModelIO.MDLMeshBufferMap
  ( MDLMeshBufferMap
  , IsMDLMeshBufferMap(..)
  , initWithBytes_deallocator
  , bytes
  , bytesSelector
  , initWithBytes_deallocatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithBytes:deallocator:
--
-- Called by implementor of MDLMeshBuffer protocol to create the map           and arrange for unmapping on deallocation.
--
-- ObjC selector: @- initWithBytes:deallocator:@
initWithBytes_deallocator :: IsMDLMeshBufferMap mdlMeshBufferMap => mdlMeshBufferMap -> Ptr () -> Ptr () -> IO (Id MDLMeshBufferMap)
initWithBytes_deallocator mdlMeshBufferMap bytes deallocator =
  sendOwnedMessage mdlMeshBufferMap initWithBytes_deallocatorSelector bytes deallocator

-- | bytes
--
-- Mutable pointer to data in a MDLMeshBuffer object.
--
-- ObjC selector: @- bytes@
bytes :: IsMDLMeshBufferMap mdlMeshBufferMap => mdlMeshBufferMap -> IO (Ptr ())
bytes mdlMeshBufferMap =
  sendMessage mdlMeshBufferMap bytesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBytes:deallocator:@
initWithBytes_deallocatorSelector :: Selector '[Ptr (), Ptr ()] (Id MDLMeshBufferMap)
initWithBytes_deallocatorSelector = mkSelector "initWithBytes:deallocator:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector '[] (Ptr ())
bytesSelector = mkSelector "bytes"

