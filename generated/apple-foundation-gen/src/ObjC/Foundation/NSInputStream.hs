{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInputStream@.
module ObjC.Foundation.NSInputStream
  ( NSInputStream
  , IsNSInputStream(..)
  , read_maxLength
  , getBuffer_length
  , initWithData
  , initWithURL
  , initWithFileAtPath
  , inputStreamWithData
  , inputStreamWithFileAtPath
  , inputStreamWithURL
  , hasBytesAvailable
  , getBuffer_lengthSelector
  , hasBytesAvailableSelector
  , initWithDataSelector
  , initWithFileAtPathSelector
  , initWithURLSelector
  , inputStreamWithDataSelector
  , inputStreamWithFileAtPathSelector
  , inputStreamWithURLSelector
  , read_maxLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- read:maxLength:@
read_maxLength :: IsNSInputStream nsInputStream => nsInputStream -> Ptr CUChar -> CULong -> IO CLong
read_maxLength nsInputStream buffer len =
  sendMessage nsInputStream read_maxLengthSelector buffer len

-- | @- getBuffer:length:@
getBuffer_length :: IsNSInputStream nsInputStream => nsInputStream -> Ptr (Ptr CUChar) -> Ptr CULong -> IO Bool
getBuffer_length nsInputStream buffer len =
  sendMessage nsInputStream getBuffer_lengthSelector buffer len

-- | @- initWithData:@
initWithData :: (IsNSInputStream nsInputStream, IsNSData data_) => nsInputStream -> data_ -> IO (Id NSInputStream)
initWithData nsInputStream data_ =
  sendOwnedMessage nsInputStream initWithDataSelector (toNSData data_)

-- | @- initWithURL:@
initWithURL :: (IsNSInputStream nsInputStream, IsNSURL url) => nsInputStream -> url -> IO (Id NSInputStream)
initWithURL nsInputStream url =
  sendOwnedMessage nsInputStream initWithURLSelector (toNSURL url)

-- | @- initWithFileAtPath:@
initWithFileAtPath :: (IsNSInputStream nsInputStream, IsNSString path) => nsInputStream -> path -> IO (Id NSInputStream)
initWithFileAtPath nsInputStream path =
  sendOwnedMessage nsInputStream initWithFileAtPathSelector (toNSString path)

-- | @+ inputStreamWithData:@
inputStreamWithData :: IsNSData data_ => data_ -> IO (Id NSInputStream)
inputStreamWithData data_ =
  do
    cls' <- getRequiredClass "NSInputStream"
    sendClassMessage cls' inputStreamWithDataSelector (toNSData data_)

-- | @+ inputStreamWithFileAtPath:@
inputStreamWithFileAtPath :: IsNSString path => path -> IO (Id NSInputStream)
inputStreamWithFileAtPath path =
  do
    cls' <- getRequiredClass "NSInputStream"
    sendClassMessage cls' inputStreamWithFileAtPathSelector (toNSString path)

-- | @+ inputStreamWithURL:@
inputStreamWithURL :: IsNSURL url => url -> IO (Id NSInputStream)
inputStreamWithURL url =
  do
    cls' <- getRequiredClass "NSInputStream"
    sendClassMessage cls' inputStreamWithURLSelector (toNSURL url)

-- | @- hasBytesAvailable@
hasBytesAvailable :: IsNSInputStream nsInputStream => nsInputStream -> IO Bool
hasBytesAvailable nsInputStream =
  sendMessage nsInputStream hasBytesAvailableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @read:maxLength:@
read_maxLengthSelector :: Selector '[Ptr CUChar, CULong] CLong
read_maxLengthSelector = mkSelector "read:maxLength:"

-- | @Selector@ for @getBuffer:length:@
getBuffer_lengthSelector :: Selector '[Ptr (Ptr CUChar), Ptr CULong] Bool
getBuffer_lengthSelector = mkSelector "getBuffer:length:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSInputStream)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id NSInputStream)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithFileAtPath:@
initWithFileAtPathSelector :: Selector '[Id NSString] (Id NSInputStream)
initWithFileAtPathSelector = mkSelector "initWithFileAtPath:"

-- | @Selector@ for @inputStreamWithData:@
inputStreamWithDataSelector :: Selector '[Id NSData] (Id NSInputStream)
inputStreamWithDataSelector = mkSelector "inputStreamWithData:"

-- | @Selector@ for @inputStreamWithFileAtPath:@
inputStreamWithFileAtPathSelector :: Selector '[Id NSString] (Id NSInputStream)
inputStreamWithFileAtPathSelector = mkSelector "inputStreamWithFileAtPath:"

-- | @Selector@ for @inputStreamWithURL:@
inputStreamWithURLSelector :: Selector '[Id NSURL] (Id NSInputStream)
inputStreamWithURLSelector = mkSelector "inputStreamWithURL:"

-- | @Selector@ for @hasBytesAvailable@
hasBytesAvailableSelector :: Selector '[] Bool
hasBytesAvailableSelector = mkSelector "hasBytesAvailable"

