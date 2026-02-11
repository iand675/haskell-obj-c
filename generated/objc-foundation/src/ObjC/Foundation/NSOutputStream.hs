{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOutputStream@.
module ObjC.Foundation.NSOutputStream
  ( NSOutputStream
  , IsNSOutputStream(..)
  , write_maxLength
  , initToMemory
  , initToBuffer_capacity
  , initWithURL_append
  , initToFileAtPath_append
  , outputStreamToMemory
  , outputStreamToBuffer_capacity
  , outputStreamToFileAtPath_append
  , outputStreamWithURL_append
  , hasSpaceAvailable
  , write_maxLengthSelector
  , initToMemorySelector
  , initToBuffer_capacitySelector
  , initWithURL_appendSelector
  , initToFileAtPath_appendSelector
  , outputStreamToMemorySelector
  , outputStreamToBuffer_capacitySelector
  , outputStreamToFileAtPath_appendSelector
  , outputStreamWithURL_appendSelector
  , hasSpaceAvailableSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- write:maxLength:@
write_maxLength :: IsNSOutputStream nsOutputStream => nsOutputStream -> Const (Ptr CUChar) -> CULong -> IO CLong
write_maxLength nsOutputStream  buffer len =
  sendMsg nsOutputStream (mkSelector "write:maxLength:") retCLong [argPtr (unConst buffer), argCULong (fromIntegral len)]

-- | @- initToMemory@
initToMemory :: IsNSOutputStream nsOutputStream => nsOutputStream -> IO (Id NSOutputStream)
initToMemory nsOutputStream  =
  sendMsg nsOutputStream (mkSelector "initToMemory") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initToBuffer:capacity:@
initToBuffer_capacity :: IsNSOutputStream nsOutputStream => nsOutputStream -> Ptr CUChar -> CULong -> IO (Id NSOutputStream)
initToBuffer_capacity nsOutputStream  buffer capacity =
  sendMsg nsOutputStream (mkSelector "initToBuffer:capacity:") (retPtr retVoid) [argPtr buffer, argCULong (fromIntegral capacity)] >>= ownedObject . castPtr

-- | @- initWithURL:append:@
initWithURL_append :: (IsNSOutputStream nsOutputStream, IsNSURL url) => nsOutputStream -> url -> Bool -> IO (Id NSOutputStream)
initWithURL_append nsOutputStream  url shouldAppend =
withObjCPtr url $ \raw_url ->
    sendMsg nsOutputStream (mkSelector "initWithURL:append:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if shouldAppend then 1 else 0)] >>= ownedObject . castPtr

-- | @- initToFileAtPath:append:@
initToFileAtPath_append :: (IsNSOutputStream nsOutputStream, IsNSString path) => nsOutputStream -> path -> Bool -> IO (Id NSOutputStream)
initToFileAtPath_append nsOutputStream  path shouldAppend =
withObjCPtr path $ \raw_path ->
    sendMsg nsOutputStream (mkSelector "initToFileAtPath:append:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if shouldAppend then 1 else 0)] >>= ownedObject . castPtr

-- | @+ outputStreamToMemory@
outputStreamToMemory :: IO (Id NSOutputStream)
outputStreamToMemory  =
  do
    cls' <- getRequiredClass "NSOutputStream"
    sendClassMsg cls' (mkSelector "outputStreamToMemory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ outputStreamToBuffer:capacity:@
outputStreamToBuffer_capacity :: Ptr CUChar -> CULong -> IO (Id NSOutputStream)
outputStreamToBuffer_capacity buffer capacity =
  do
    cls' <- getRequiredClass "NSOutputStream"
    sendClassMsg cls' (mkSelector "outputStreamToBuffer:capacity:") (retPtr retVoid) [argPtr buffer, argCULong (fromIntegral capacity)] >>= retainedObject . castPtr

-- | @+ outputStreamToFileAtPath:append:@
outputStreamToFileAtPath_append :: IsNSString path => path -> Bool -> IO (Id NSOutputStream)
outputStreamToFileAtPath_append path shouldAppend =
  do
    cls' <- getRequiredClass "NSOutputStream"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "outputStreamToFileAtPath:append:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if shouldAppend then 1 else 0)] >>= retainedObject . castPtr

-- | @+ outputStreamWithURL:append:@
outputStreamWithURL_append :: IsNSURL url => url -> Bool -> IO (Id NSOutputStream)
outputStreamWithURL_append url shouldAppend =
  do
    cls' <- getRequiredClass "NSOutputStream"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "outputStreamWithURL:append:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if shouldAppend then 1 else 0)] >>= retainedObject . castPtr

-- | @- hasSpaceAvailable@
hasSpaceAvailable :: IsNSOutputStream nsOutputStream => nsOutputStream -> IO Bool
hasSpaceAvailable nsOutputStream  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutputStream (mkSelector "hasSpaceAvailable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @write:maxLength:@
write_maxLengthSelector :: Selector
write_maxLengthSelector = mkSelector "write:maxLength:"

-- | @Selector@ for @initToMemory@
initToMemorySelector :: Selector
initToMemorySelector = mkSelector "initToMemory"

-- | @Selector@ for @initToBuffer:capacity:@
initToBuffer_capacitySelector :: Selector
initToBuffer_capacitySelector = mkSelector "initToBuffer:capacity:"

-- | @Selector@ for @initWithURL:append:@
initWithURL_appendSelector :: Selector
initWithURL_appendSelector = mkSelector "initWithURL:append:"

-- | @Selector@ for @initToFileAtPath:append:@
initToFileAtPath_appendSelector :: Selector
initToFileAtPath_appendSelector = mkSelector "initToFileAtPath:append:"

-- | @Selector@ for @outputStreamToMemory@
outputStreamToMemorySelector :: Selector
outputStreamToMemorySelector = mkSelector "outputStreamToMemory"

-- | @Selector@ for @outputStreamToBuffer:capacity:@
outputStreamToBuffer_capacitySelector :: Selector
outputStreamToBuffer_capacitySelector = mkSelector "outputStreamToBuffer:capacity:"

-- | @Selector@ for @outputStreamToFileAtPath:append:@
outputStreamToFileAtPath_appendSelector :: Selector
outputStreamToFileAtPath_appendSelector = mkSelector "outputStreamToFileAtPath:append:"

-- | @Selector@ for @outputStreamWithURL:append:@
outputStreamWithURL_appendSelector :: Selector
outputStreamWithURL_appendSelector = mkSelector "outputStreamWithURL:append:"

-- | @Selector@ for @hasSpaceAvailable@
hasSpaceAvailableSelector :: Selector
hasSpaceAvailableSelector = mkSelector "hasSpaceAvailable"

