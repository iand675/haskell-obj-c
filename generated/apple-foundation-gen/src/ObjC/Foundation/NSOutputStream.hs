{-# LANGUAGE DataKinds #-}
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
  , hasSpaceAvailableSelector
  , initToBuffer_capacitySelector
  , initToFileAtPath_appendSelector
  , initToMemorySelector
  , initWithURL_appendSelector
  , outputStreamToBuffer_capacitySelector
  , outputStreamToFileAtPath_appendSelector
  , outputStreamToMemorySelector
  , outputStreamWithURL_appendSelector
  , write_maxLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- write:maxLength:@
write_maxLength :: IsNSOutputStream nsOutputStream => nsOutputStream -> Const (Ptr CUChar) -> CULong -> IO CLong
write_maxLength nsOutputStream buffer len =
  sendMessage nsOutputStream write_maxLengthSelector buffer len

-- | @- initToMemory@
initToMemory :: IsNSOutputStream nsOutputStream => nsOutputStream -> IO (Id NSOutputStream)
initToMemory nsOutputStream =
  sendOwnedMessage nsOutputStream initToMemorySelector

-- | @- initToBuffer:capacity:@
initToBuffer_capacity :: IsNSOutputStream nsOutputStream => nsOutputStream -> Ptr CUChar -> CULong -> IO (Id NSOutputStream)
initToBuffer_capacity nsOutputStream buffer capacity =
  sendOwnedMessage nsOutputStream initToBuffer_capacitySelector buffer capacity

-- | @- initWithURL:append:@
initWithURL_append :: (IsNSOutputStream nsOutputStream, IsNSURL url) => nsOutputStream -> url -> Bool -> IO (Id NSOutputStream)
initWithURL_append nsOutputStream url shouldAppend =
  sendOwnedMessage nsOutputStream initWithURL_appendSelector (toNSURL url) shouldAppend

-- | @- initToFileAtPath:append:@
initToFileAtPath_append :: (IsNSOutputStream nsOutputStream, IsNSString path) => nsOutputStream -> path -> Bool -> IO (Id NSOutputStream)
initToFileAtPath_append nsOutputStream path shouldAppend =
  sendOwnedMessage nsOutputStream initToFileAtPath_appendSelector (toNSString path) shouldAppend

-- | @+ outputStreamToMemory@
outputStreamToMemory :: IO (Id NSOutputStream)
outputStreamToMemory  =
  do
    cls' <- getRequiredClass "NSOutputStream"
    sendClassMessage cls' outputStreamToMemorySelector

-- | @+ outputStreamToBuffer:capacity:@
outputStreamToBuffer_capacity :: Ptr CUChar -> CULong -> IO (Id NSOutputStream)
outputStreamToBuffer_capacity buffer capacity =
  do
    cls' <- getRequiredClass "NSOutputStream"
    sendClassMessage cls' outputStreamToBuffer_capacitySelector buffer capacity

-- | @+ outputStreamToFileAtPath:append:@
outputStreamToFileAtPath_append :: IsNSString path => path -> Bool -> IO (Id NSOutputStream)
outputStreamToFileAtPath_append path shouldAppend =
  do
    cls' <- getRequiredClass "NSOutputStream"
    sendClassMessage cls' outputStreamToFileAtPath_appendSelector (toNSString path) shouldAppend

-- | @+ outputStreamWithURL:append:@
outputStreamWithURL_append :: IsNSURL url => url -> Bool -> IO (Id NSOutputStream)
outputStreamWithURL_append url shouldAppend =
  do
    cls' <- getRequiredClass "NSOutputStream"
    sendClassMessage cls' outputStreamWithURL_appendSelector (toNSURL url) shouldAppend

-- | @- hasSpaceAvailable@
hasSpaceAvailable :: IsNSOutputStream nsOutputStream => nsOutputStream -> IO Bool
hasSpaceAvailable nsOutputStream =
  sendMessage nsOutputStream hasSpaceAvailableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @write:maxLength:@
write_maxLengthSelector :: Selector '[Const (Ptr CUChar), CULong] CLong
write_maxLengthSelector = mkSelector "write:maxLength:"

-- | @Selector@ for @initToMemory@
initToMemorySelector :: Selector '[] (Id NSOutputStream)
initToMemorySelector = mkSelector "initToMemory"

-- | @Selector@ for @initToBuffer:capacity:@
initToBuffer_capacitySelector :: Selector '[Ptr CUChar, CULong] (Id NSOutputStream)
initToBuffer_capacitySelector = mkSelector "initToBuffer:capacity:"

-- | @Selector@ for @initWithURL:append:@
initWithURL_appendSelector :: Selector '[Id NSURL, Bool] (Id NSOutputStream)
initWithURL_appendSelector = mkSelector "initWithURL:append:"

-- | @Selector@ for @initToFileAtPath:append:@
initToFileAtPath_appendSelector :: Selector '[Id NSString, Bool] (Id NSOutputStream)
initToFileAtPath_appendSelector = mkSelector "initToFileAtPath:append:"

-- | @Selector@ for @outputStreamToMemory@
outputStreamToMemorySelector :: Selector '[] (Id NSOutputStream)
outputStreamToMemorySelector = mkSelector "outputStreamToMemory"

-- | @Selector@ for @outputStreamToBuffer:capacity:@
outputStreamToBuffer_capacitySelector :: Selector '[Ptr CUChar, CULong] (Id NSOutputStream)
outputStreamToBuffer_capacitySelector = mkSelector "outputStreamToBuffer:capacity:"

-- | @Selector@ for @outputStreamToFileAtPath:append:@
outputStreamToFileAtPath_appendSelector :: Selector '[Id NSString, Bool] (Id NSOutputStream)
outputStreamToFileAtPath_appendSelector = mkSelector "outputStreamToFileAtPath:append:"

-- | @Selector@ for @outputStreamWithURL:append:@
outputStreamWithURL_appendSelector :: Selector '[Id NSURL, Bool] (Id NSOutputStream)
outputStreamWithURL_appendSelector = mkSelector "outputStreamWithURL:append:"

-- | @Selector@ for @hasSpaceAvailable@
hasSpaceAvailableSelector :: Selector '[] Bool
hasSpaceAvailableSelector = mkSelector "hasSpaceAvailable"

