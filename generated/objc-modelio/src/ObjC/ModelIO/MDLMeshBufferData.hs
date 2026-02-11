{-# LANGUAGE PatternSynonyms #-}
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
  , initWithType_lengthSelector
  , initWithType_dataSelector
  , dataSelector

  -- * Enum types
  , MDLMeshBufferType(MDLMeshBufferType)
  , pattern MDLMeshBufferTypeVertex
  , pattern MDLMeshBufferTypeIndex
  , pattern MDLMeshBufferTypeCustom

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
initWithType_length mdlMeshBufferData  type_ length_ =
  sendMsg mdlMeshBufferData (mkSelector "initWithType:length:") (retPtr retVoid) [argCULong (coerce type_), argCULong (fromIntegral length_)] >>= ownedObject . castPtr

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
initWithType_data mdlMeshBufferData  type_ data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg mdlMeshBufferData (mkSelector "initWithType:data:") (retPtr retVoid) [argCULong (coerce type_), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- data@
data_ :: IsMDLMeshBufferData mdlMeshBufferData => mdlMeshBufferData -> IO (Id NSData)
data_ mdlMeshBufferData  =
  sendMsg mdlMeshBufferData (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:length:@
initWithType_lengthSelector :: Selector
initWithType_lengthSelector = mkSelector "initWithType:length:"

-- | @Selector@ for @initWithType:data:@
initWithType_dataSelector :: Selector
initWithType_dataSelector = mkSelector "initWithType:data:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

