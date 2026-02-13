{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GLKMeshBufferAllocator
--
-- Allocator passed to MDLAsset init method to load vertex and index data directly into OpenGL buffer object
--
-- Generated bindings for @GLKMeshBufferAllocator@.
module ObjC.GLKit.GLKMeshBufferAllocator
  ( GLKMeshBufferAllocator
  , IsGLKMeshBufferAllocator(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

