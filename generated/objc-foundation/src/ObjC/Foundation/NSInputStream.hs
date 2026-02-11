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
  , read_maxLengthSelector
  , getBuffer_lengthSelector
  , initWithDataSelector
  , initWithURLSelector
  , initWithFileAtPathSelector
  , inputStreamWithDataSelector
  , inputStreamWithFileAtPathSelector
  , inputStreamWithURLSelector
  , hasBytesAvailableSelector


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

-- | @- read:maxLength:@
read_maxLength :: IsNSInputStream nsInputStream => nsInputStream -> Ptr CUChar -> CULong -> IO CLong
read_maxLength nsInputStream  buffer len =
  sendMsg nsInputStream (mkSelector "read:maxLength:") retCLong [argPtr buffer, argCULong (fromIntegral len)]

-- | @- getBuffer:length:@
getBuffer_length :: IsNSInputStream nsInputStream => nsInputStream -> Ptr (Ptr CUChar) -> Ptr CULong -> IO Bool
getBuffer_length nsInputStream  buffer len =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInputStream (mkSelector "getBuffer:length:") retCULong [argPtr buffer, argPtr len]

-- | @- initWithData:@
initWithData :: (IsNSInputStream nsInputStream, IsNSData data_) => nsInputStream -> data_ -> IO (Id NSInputStream)
initWithData nsInputStream  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsInputStream (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithURL:@
initWithURL :: (IsNSInputStream nsInputStream, IsNSURL url) => nsInputStream -> url -> IO (Id NSInputStream)
initWithURL nsInputStream  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsInputStream (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFileAtPath:@
initWithFileAtPath :: (IsNSInputStream nsInputStream, IsNSString path) => nsInputStream -> path -> IO (Id NSInputStream)
initWithFileAtPath nsInputStream  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsInputStream (mkSelector "initWithFileAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @+ inputStreamWithData:@
inputStreamWithData :: IsNSData data_ => data_ -> IO (Id NSInputStream)
inputStreamWithData data_ =
  do
    cls' <- getRequiredClass "NSInputStream"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "inputStreamWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ inputStreamWithFileAtPath:@
inputStreamWithFileAtPath :: IsNSString path => path -> IO (Id NSInputStream)
inputStreamWithFileAtPath path =
  do
    cls' <- getRequiredClass "NSInputStream"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "inputStreamWithFileAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ inputStreamWithURL:@
inputStreamWithURL :: IsNSURL url => url -> IO (Id NSInputStream)
inputStreamWithURL url =
  do
    cls' <- getRequiredClass "NSInputStream"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "inputStreamWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasBytesAvailable@
hasBytesAvailable :: IsNSInputStream nsInputStream => nsInputStream -> IO Bool
hasBytesAvailable nsInputStream  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInputStream (mkSelector "hasBytesAvailable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @read:maxLength:@
read_maxLengthSelector :: Selector
read_maxLengthSelector = mkSelector "read:maxLength:"

-- | @Selector@ for @getBuffer:length:@
getBuffer_lengthSelector :: Selector
getBuffer_lengthSelector = mkSelector "getBuffer:length:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithFileAtPath:@
initWithFileAtPathSelector :: Selector
initWithFileAtPathSelector = mkSelector "initWithFileAtPath:"

-- | @Selector@ for @inputStreamWithData:@
inputStreamWithDataSelector :: Selector
inputStreamWithDataSelector = mkSelector "inputStreamWithData:"

-- | @Selector@ for @inputStreamWithFileAtPath:@
inputStreamWithFileAtPathSelector :: Selector
inputStreamWithFileAtPathSelector = mkSelector "inputStreamWithFileAtPath:"

-- | @Selector@ for @inputStreamWithURL:@
inputStreamWithURLSelector :: Selector
inputStreamWithURLSelector = mkSelector "inputStreamWithURL:"

-- | @Selector@ for @hasBytesAvailable@
hasBytesAvailableSelector :: Selector
hasBytesAvailableSelector = mkSelector "hasBytesAvailable"

