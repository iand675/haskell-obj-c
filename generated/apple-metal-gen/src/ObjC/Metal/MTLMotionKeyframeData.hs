{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLbuffer and description how the data is stored in it.
--
-- Generated bindings for @MTLMotionKeyframeData@.
module ObjC.Metal.MTLMotionKeyframeData
  ( MTLMotionKeyframeData
  , IsMTLMotionKeyframeData(..)
  , data_
  , buffer
  , setBuffer
  , offset
  , setOffset
  , bufferSelector
  , dataSelector
  , offsetSelector
  , setBufferSelector
  , setOffsetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ data@
data_ :: IO (Id MTLMotionKeyframeData)
data_  =
  do
    cls' <- getRequiredClass "MTLMotionKeyframeData"
    sendClassMessage cls' dataSelector

-- | Buffer containing the data of a single keyframe. Multiple keyframes can be interleaved in one MTLBuffer.
--
-- ObjC selector: @- buffer@
buffer :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> IO RawId
buffer mtlMotionKeyframeData =
  sendMessage mtlMotionKeyframeData bufferSelector

-- | Buffer containing the data of a single keyframe. Multiple keyframes can be interleaved in one MTLBuffer.
--
-- ObjC selector: @- setBuffer:@
setBuffer :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> RawId -> IO ()
setBuffer mtlMotionKeyframeData value =
  sendMessage mtlMotionKeyframeData setBufferSelector value

-- | Buffer offset. Must be a multiple of 4 bytes.
--
-- ObjC selector: @- offset@
offset :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> IO CULong
offset mtlMotionKeyframeData =
  sendMessage mtlMotionKeyframeData offsetSelector

-- | Buffer offset. Must be a multiple of 4 bytes.
--
-- ObjC selector: @- setOffset:@
setOffset :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> CULong -> IO ()
setOffset mtlMotionKeyframeData value =
  sendMessage mtlMotionKeyframeData setOffsetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id MTLMotionKeyframeData)
dataSelector = mkSelector "data"

-- | @Selector@ for @buffer@
bufferSelector :: Selector '[] RawId
bufferSelector = mkSelector "buffer"

-- | @Selector@ for @setBuffer:@
setBufferSelector :: Selector '[RawId] ()
setBufferSelector = mkSelector "setBuffer:"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[CULong] ()
setOffsetSelector = mkSelector "setOffset:"

