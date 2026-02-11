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
  , dataSelector
  , bufferSelector
  , setBufferSelector
  , offsetSelector
  , setOffsetSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ data@
data_ :: IO (Id MTLMotionKeyframeData)
data_  =
  do
    cls' <- getRequiredClass "MTLMotionKeyframeData"
    sendClassMsg cls' (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Buffer containing the data of a single keyframe. Multiple keyframes can be interleaved in one MTLBuffer.
--
-- ObjC selector: @- buffer@
buffer :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> IO RawId
buffer mtlMotionKeyframeData  =
    fmap (RawId . castPtr) $ sendMsg mtlMotionKeyframeData (mkSelector "buffer") (retPtr retVoid) []

-- | Buffer containing the data of a single keyframe. Multiple keyframes can be interleaved in one MTLBuffer.
--
-- ObjC selector: @- setBuffer:@
setBuffer :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> RawId -> IO ()
setBuffer mtlMotionKeyframeData  value =
    sendMsg mtlMotionKeyframeData (mkSelector "setBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Buffer offset. Must be a multiple of 4 bytes.
--
-- ObjC selector: @- offset@
offset :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> IO CULong
offset mtlMotionKeyframeData  =
    sendMsg mtlMotionKeyframeData (mkSelector "offset") retCULong []

-- | Buffer offset. Must be a multiple of 4 bytes.
--
-- ObjC selector: @- setOffset:@
setOffset :: IsMTLMotionKeyframeData mtlMotionKeyframeData => mtlMotionKeyframeData -> CULong -> IO ()
setOffset mtlMotionKeyframeData  value =
    sendMsg mtlMotionKeyframeData (mkSelector "setOffset:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @buffer@
bufferSelector :: Selector
bufferSelector = mkSelector "buffer"

-- | @Selector@ for @setBuffer:@
setBufferSelector :: Selector
setBufferSelector = mkSelector "setBuffer:"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

