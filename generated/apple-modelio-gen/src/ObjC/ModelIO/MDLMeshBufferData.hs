{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLMeshBufferData
--
-- A CPU memory backed mesh buffer
--
-- Generated bindings for @MDLMeshBufferData@.
module ObjC.ModelIO.MDLMeshBufferData
  ( MDLMeshBufferData
  , IsMDLMeshBufferData(..)
  , initWithType_length
  , initWithType_data
  , data_
  , dataSelector
  , initWithType_dataSelector
  , initWithType_lengthSelector

  -- * Enum types
  , MDLMeshBufferType(MDLMeshBufferType)
  , pattern MDLMeshBufferTypeVertex
  , pattern MDLMeshBufferTypeIndex
  , pattern MDLMeshBufferTypeCustom

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithType:length
--
-- instantiate a new data backed mesh buffer
--
-- @type@ — the intended use of the buffer
--
-- @length@ — the size of buffer to allocate, in bytes
--
-- ObjC selector: @- initWithType:length:@
initWithType_length :: IsMDLMeshBufferData mdlMeshBufferData => mdlMeshBufferData -> MDLMeshBufferType -> CULong -> IO (Id MDLMeshBufferData)
initWithType_length mdlMeshBufferData type_ length_ =
  sendOwnedMessage mdlMeshBufferData initWithType_lengthSelector type_ length_

-- | initWithType:data
--
-- instantiate a new data backed mesh buffer
--
-- @type@ — the intended use of the buffer
--
-- @data@ — the data to be used as a mesh buffer. It will be copied.
--
-- ObjC selector: @- initWithType:data:@
initWithType_data :: (IsMDLMeshBufferData mdlMeshBufferData, IsNSData data_) => mdlMeshBufferData -> MDLMeshBufferType -> data_ -> IO (Id MDLMeshBufferData)
initWithType_data mdlMeshBufferData type_ data_ =
  sendOwnedMessage mdlMeshBufferData initWithType_dataSelector type_ (toNSData data_)

-- | @- data@
data_ :: IsMDLMeshBufferData mdlMeshBufferData => mdlMeshBufferData -> IO (Id NSData)
data_ mdlMeshBufferData =
  sendMessage mdlMeshBufferData dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:length:@
initWithType_lengthSelector :: Selector '[MDLMeshBufferType, CULong] (Id MDLMeshBufferData)
initWithType_lengthSelector = mkSelector "initWithType:length:"

-- | @Selector@ for @initWithType:data:@
initWithType_dataSelector :: Selector '[MDLMeshBufferType, Id NSData] (Id MDLMeshBufferData)
initWithType_dataSelector = mkSelector "initWithType:data:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

