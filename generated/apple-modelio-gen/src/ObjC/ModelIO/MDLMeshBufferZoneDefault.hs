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
  , capacitySelector
  , allocatorSelector


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

-- | @- capacity@
capacity :: IsMDLMeshBufferZoneDefault mdlMeshBufferZoneDefault => mdlMeshBufferZoneDefault -> IO CULong
capacity mdlMeshBufferZoneDefault  =
    sendMsg mdlMeshBufferZoneDefault (mkSelector "capacity") retCULong []

-- | @- allocator@
allocator :: IsMDLMeshBufferZoneDefault mdlMeshBufferZoneDefault => mdlMeshBufferZoneDefault -> IO RawId
allocator mdlMeshBufferZoneDefault  =
    fmap (RawId . castPtr) $ sendMsg mdlMeshBufferZoneDefault (mkSelector "allocator") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capacity@
capacitySelector :: Selector
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @allocator@
allocatorSelector :: Selector
allocatorSelector = mkSelector "allocator"

