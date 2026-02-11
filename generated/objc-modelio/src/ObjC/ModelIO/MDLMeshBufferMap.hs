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
  , initWithBytes_deallocatorSelector
  , bytesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
initWithBytes_deallocator mdlMeshBufferMap  bytes deallocator =
  sendMsg mdlMeshBufferMap (mkSelector "initWithBytes:deallocator:") (retPtr retVoid) [argPtr bytes, argPtr (castPtr deallocator :: Ptr ())] >>= ownedObject . castPtr

-- | bytes
--
-- Mutable pointer to data in a MDLMeshBuffer object.
--
-- ObjC selector: @- bytes@
bytes :: IsMDLMeshBufferMap mdlMeshBufferMap => mdlMeshBufferMap -> IO (Ptr ())
bytes mdlMeshBufferMap  =
  fmap castPtr $ sendMsg mdlMeshBufferMap (mkSelector "bytes") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBytes:deallocator:@
initWithBytes_deallocatorSelector :: Selector
initWithBytes_deallocatorSelector = mkSelector "initWithBytes:deallocator:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector
bytesSelector = mkSelector "bytes"

