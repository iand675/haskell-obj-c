{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLStageInputOutputDescriptor@.
module ObjC.Metal.MTLStageInputOutputDescriptor
  ( MTLStageInputOutputDescriptor
  , IsMTLStageInputOutputDescriptor(..)
  , stageInputOutputDescriptor
  , reset
  , layouts
  , attributes
  , indexType
  , setIndexType
  , indexBufferIndex
  , setIndexBufferIndex
  , stageInputOutputDescriptorSelector
  , resetSelector
  , layoutsSelector
  , attributesSelector
  , indexTypeSelector
  , setIndexTypeSelector
  , indexBufferIndexSelector
  , setIndexBufferIndexSelector

  -- * Enum types
  , MTLIndexType(MTLIndexType)
  , pattern MTLIndexTypeUInt16
  , pattern MTLIndexTypeUInt32

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ stageInputOutputDescriptor@
stageInputOutputDescriptor :: IO (Id MTLStageInputOutputDescriptor)
stageInputOutputDescriptor  =
  do
    cls' <- getRequiredClass "MTLStageInputOutputDescriptor"
    sendClassMsg cls' (mkSelector "stageInputOutputDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reset@
reset :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO ()
reset mtlStageInputOutputDescriptor  =
  sendMsg mtlStageInputOutputDescriptor (mkSelector "reset") retVoid []

-- | @- layouts@
layouts :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO (Id MTLBufferLayoutDescriptorArray)
layouts mtlStageInputOutputDescriptor  =
  sendMsg mtlStageInputOutputDescriptor (mkSelector "layouts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO (Id MTLAttributeDescriptorArray)
attributes mtlStageInputOutputDescriptor  =
  sendMsg mtlStageInputOutputDescriptor (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexType@
indexType :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO MTLIndexType
indexType mtlStageInputOutputDescriptor  =
  fmap (coerce :: CULong -> MTLIndexType) $ sendMsg mtlStageInputOutputDescriptor (mkSelector "indexType") retCULong []

-- | @- setIndexType:@
setIndexType :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> MTLIndexType -> IO ()
setIndexType mtlStageInputOutputDescriptor  value =
  sendMsg mtlStageInputOutputDescriptor (mkSelector "setIndexType:") retVoid [argCULong (coerce value)]

-- | @- indexBufferIndex@
indexBufferIndex :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO CULong
indexBufferIndex mtlStageInputOutputDescriptor  =
  sendMsg mtlStageInputOutputDescriptor (mkSelector "indexBufferIndex") retCULong []

-- | @- setIndexBufferIndex:@
setIndexBufferIndex :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> CULong -> IO ()
setIndexBufferIndex mtlStageInputOutputDescriptor  value =
  sendMsg mtlStageInputOutputDescriptor (mkSelector "setIndexBufferIndex:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stageInputOutputDescriptor@
stageInputOutputDescriptorSelector :: Selector
stageInputOutputDescriptorSelector = mkSelector "stageInputOutputDescriptor"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @layouts@
layoutsSelector :: Selector
layoutsSelector = mkSelector "layouts"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @setIndexType:@
setIndexTypeSelector :: Selector
setIndexTypeSelector = mkSelector "setIndexType:"

-- | @Selector@ for @indexBufferIndex@
indexBufferIndexSelector :: Selector
indexBufferIndexSelector = mkSelector "indexBufferIndex"

-- | @Selector@ for @setIndexBufferIndex:@
setIndexBufferIndexSelector :: Selector
setIndexBufferIndexSelector = mkSelector "setIndexBufferIndex:"

