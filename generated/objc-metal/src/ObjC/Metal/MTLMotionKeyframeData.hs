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
  , offset
  , setOffset
  , dataSelector
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
  sendMsg mtlMotionKeyframeData (mkSelector "setOffset:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

