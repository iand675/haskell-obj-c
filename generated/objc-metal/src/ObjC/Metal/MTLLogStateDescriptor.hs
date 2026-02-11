{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLLogStateDescriptor@.
module ObjC.Metal.MTLLogStateDescriptor
  ( MTLLogStateDescriptor
  , IsMTLLogStateDescriptor(..)
  , level
  , setLevel
  , bufferSize
  , setBufferSize
  , levelSelector
  , setLevelSelector
  , bufferSizeSelector
  , setBufferSizeSelector

  -- * Enum types
  , MTLLogLevel(MTLLogLevel)
  , pattern MTLLogLevelUndefined
  , pattern MTLLogLevelDebug
  , pattern MTLLogLevelInfo
  , pattern MTLLogLevelNotice
  , pattern MTLLogLevelError
  , pattern MTLLogLevelFault

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

-- | level indicates the minimum level of the logs that will be printed.
--
-- All the logs with level less than given level will be skipped on the GPU Side.
--
-- ObjC selector: @- level@
level :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> IO MTLLogLevel
level mtlLogStateDescriptor  =
  fmap (coerce :: CLong -> MTLLogLevel) $ sendMsg mtlLogStateDescriptor (mkSelector "level") retCLong []

-- | level indicates the minimum level of the logs that will be printed.
--
-- All the logs with level less than given level will be skipped on the GPU Side.
--
-- ObjC selector: @- setLevel:@
setLevel :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> MTLLogLevel -> IO ()
setLevel mtlLogStateDescriptor  value =
  sendMsg mtlLogStateDescriptor (mkSelector "setLevel:") retVoid [argCLong (coerce value)]

-- | bufferSize indicates the size of the buffer where GPU will store the logging content from shaders. Minimum value is 1KB
--
-- ObjC selector: @- bufferSize@
bufferSize :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> IO CLong
bufferSize mtlLogStateDescriptor  =
  sendMsg mtlLogStateDescriptor (mkSelector "bufferSize") retCLong []

-- | bufferSize indicates the size of the buffer where GPU will store the logging content from shaders. Minimum value is 1KB
--
-- ObjC selector: @- setBufferSize:@
setBufferSize :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> CLong -> IO ()
setBufferSize mtlLogStateDescriptor  value =
  sendMsg mtlLogStateDescriptor (mkSelector "setBufferSize:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @bufferSize@
bufferSizeSelector :: Selector
bufferSizeSelector = mkSelector "bufferSize"

-- | @Selector@ for @setBufferSize:@
setBufferSizeSelector :: Selector
setBufferSizeSelector = mkSelector "setBufferSize:"

