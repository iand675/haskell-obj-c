{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bufferSizeSelector
  , levelSelector
  , setBufferSizeSelector
  , setLevelSelector

  -- * Enum types
  , MTLLogLevel(MTLLogLevel)
  , pattern MTLLogLevelUndefined
  , pattern MTLLogLevelDebug
  , pattern MTLLogLevelInfo
  , pattern MTLLogLevelNotice
  , pattern MTLLogLevelError
  , pattern MTLLogLevelFault

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

-- | level indicates the minimum level of the logs that will be printed.
--
-- All the logs with level less than given level will be skipped on the GPU Side.
--
-- ObjC selector: @- level@
level :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> IO MTLLogLevel
level mtlLogStateDescriptor =
  sendMessage mtlLogStateDescriptor levelSelector

-- | level indicates the minimum level of the logs that will be printed.
--
-- All the logs with level less than given level will be skipped on the GPU Side.
--
-- ObjC selector: @- setLevel:@
setLevel :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> MTLLogLevel -> IO ()
setLevel mtlLogStateDescriptor value =
  sendMessage mtlLogStateDescriptor setLevelSelector value

-- | bufferSize indicates the size of the buffer where GPU will store the logging content from shaders. Minimum value is 1KB
--
-- ObjC selector: @- bufferSize@
bufferSize :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> IO CLong
bufferSize mtlLogStateDescriptor =
  sendMessage mtlLogStateDescriptor bufferSizeSelector

-- | bufferSize indicates the size of the buffer where GPU will store the logging content from shaders. Minimum value is 1KB
--
-- ObjC selector: @- setBufferSize:@
setBufferSize :: IsMTLLogStateDescriptor mtlLogStateDescriptor => mtlLogStateDescriptor -> CLong -> IO ()
setBufferSize mtlLogStateDescriptor value =
  sendMessage mtlLogStateDescriptor setBufferSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector '[] MTLLogLevel
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector '[MTLLogLevel] ()
setLevelSelector = mkSelector "setLevel:"

-- | @Selector@ for @bufferSize@
bufferSizeSelector :: Selector '[] CLong
bufferSizeSelector = mkSelector "bufferSize"

-- | @Selector@ for @setBufferSize:@
setBufferSizeSelector :: Selector '[CLong] ()
setBufferSizeSelector = mkSelector "setBufferSize:"

