{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attributesSelector
  , indexBufferIndexSelector
  , indexTypeSelector
  , layoutsSelector
  , resetSelector
  , setIndexBufferIndexSelector
  , setIndexTypeSelector
  , stageInputOutputDescriptorSelector

  -- * Enum types
  , MTLIndexType(MTLIndexType)
  , pattern MTLIndexTypeUInt16
  , pattern MTLIndexTypeUInt32

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' stageInputOutputDescriptorSelector

-- | @- reset@
reset :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO ()
reset mtlStageInputOutputDescriptor =
  sendMessage mtlStageInputOutputDescriptor resetSelector

-- | @- layouts@
layouts :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO (Id MTLBufferLayoutDescriptorArray)
layouts mtlStageInputOutputDescriptor =
  sendMessage mtlStageInputOutputDescriptor layoutsSelector

-- | @- attributes@
attributes :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO (Id MTLAttributeDescriptorArray)
attributes mtlStageInputOutputDescriptor =
  sendMessage mtlStageInputOutputDescriptor attributesSelector

-- | @- indexType@
indexType :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO MTLIndexType
indexType mtlStageInputOutputDescriptor =
  sendMessage mtlStageInputOutputDescriptor indexTypeSelector

-- | @- setIndexType:@
setIndexType :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> MTLIndexType -> IO ()
setIndexType mtlStageInputOutputDescriptor value =
  sendMessage mtlStageInputOutputDescriptor setIndexTypeSelector value

-- | @- indexBufferIndex@
indexBufferIndex :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> IO CULong
indexBufferIndex mtlStageInputOutputDescriptor =
  sendMessage mtlStageInputOutputDescriptor indexBufferIndexSelector

-- | @- setIndexBufferIndex:@
setIndexBufferIndex :: IsMTLStageInputOutputDescriptor mtlStageInputOutputDescriptor => mtlStageInputOutputDescriptor -> CULong -> IO ()
setIndexBufferIndex mtlStageInputOutputDescriptor value =
  sendMessage mtlStageInputOutputDescriptor setIndexBufferIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stageInputOutputDescriptor@
stageInputOutputDescriptorSelector :: Selector '[] (Id MTLStageInputOutputDescriptor)
stageInputOutputDescriptorSelector = mkSelector "stageInputOutputDescriptor"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @layouts@
layoutsSelector :: Selector '[] (Id MTLBufferLayoutDescriptorArray)
layoutsSelector = mkSelector "layouts"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id MTLAttributeDescriptorArray)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector '[] MTLIndexType
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @setIndexType:@
setIndexTypeSelector :: Selector '[MTLIndexType] ()
setIndexTypeSelector = mkSelector "setIndexType:"

-- | @Selector@ for @indexBufferIndex@
indexBufferIndexSelector :: Selector '[] CULong
indexBufferIndexSelector = mkSelector "indexBufferIndex"

-- | @Selector@ for @setIndexBufferIndex:@
setIndexBufferIndexSelector :: Selector '[CULong] ()
setIndexBufferIndexSelector = mkSelector "setIndexBufferIndex:"

