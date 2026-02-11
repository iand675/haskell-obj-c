{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStream@.
module ObjC.Foundation.NSStream
  ( NSStream
  , IsNSStream(..)
  , open
  , close
  , propertyForKey
  , setProperty_forKey
  , scheduleInRunLoop_forMode
  , removeFromRunLoop_forMode
  , getBoundStreamsWithBufferSize_inputStream_outputStream
  , getStreamsToHostWithName_port_inputStream_outputStream
  , getStreamsToHost_port_inputStream_outputStream
  , delegate
  , setDelegate
  , streamStatus
  , streamError
  , openSelector
  , closeSelector
  , propertyForKeySelector
  , setProperty_forKeySelector
  , scheduleInRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , getBoundStreamsWithBufferSize_inputStream_outputStreamSelector
  , getStreamsToHostWithName_port_inputStream_outputStreamSelector
  , getStreamsToHost_port_inputStream_outputStreamSelector
  , delegateSelector
  , setDelegateSelector
  , streamStatusSelector
  , streamErrorSelector

  -- * Enum types
  , NSStreamStatus(NSStreamStatus)
  , pattern NSStreamStatusNotOpen
  , pattern NSStreamStatusOpening
  , pattern NSStreamStatusOpen
  , pattern NSStreamStatusReading
  , pattern NSStreamStatusWriting
  , pattern NSStreamStatusAtEnd
  , pattern NSStreamStatusClosed
  , pattern NSStreamStatusError

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
import ObjC.Foundation.Internal.Enums

-- | @- open@
open :: IsNSStream nsStream => nsStream -> IO ()
open nsStream  =
    sendMsg nsStream (mkSelector "open") retVoid []

-- | @- close@
close :: IsNSStream nsStream => nsStream -> IO ()
close nsStream  =
    sendMsg nsStream (mkSelector "close") retVoid []

-- | @- propertyForKey:@
propertyForKey :: (IsNSStream nsStream, IsNSString key) => nsStream -> key -> IO RawId
propertyForKey nsStream  key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsStream (mkSelector "propertyForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setProperty:forKey:@
setProperty_forKey :: (IsNSStream nsStream, IsNSString key) => nsStream -> RawId -> key -> IO Bool
setProperty_forKey nsStream  property key =
  withObjCPtr key $ \raw_key ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStream (mkSelector "setProperty:forKey:") retCULong [argPtr (castPtr (unRawId property) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSStream nsStream, IsNSRunLoop aRunLoop, IsNSString mode) => nsStream -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsStream  aRunLoop mode =
  withObjCPtr aRunLoop $ \raw_aRunLoop ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsStream (mkSelector "scheduleInRunLoop:forMode:") retVoid [argPtr (castPtr raw_aRunLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSStream nsStream, IsNSRunLoop aRunLoop, IsNSString mode) => nsStream -> aRunLoop -> mode -> IO ()
removeFromRunLoop_forMode nsStream  aRunLoop mode =
  withObjCPtr aRunLoop $ \raw_aRunLoop ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsStream (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_aRunLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @+ getBoundStreamsWithBufferSize:inputStream:outputStream:@
getBoundStreamsWithBufferSize_inputStream_outputStream :: (IsNSInputStream inputStream, IsNSOutputStream outputStream) => CULong -> inputStream -> outputStream -> IO ()
getBoundStreamsWithBufferSize_inputStream_outputStream bufferSize inputStream outputStream =
  do
    cls' <- getRequiredClass "NSStream"
    withObjCPtr inputStream $ \raw_inputStream ->
      withObjCPtr outputStream $ \raw_outputStream ->
        sendClassMsg cls' (mkSelector "getBoundStreamsWithBufferSize:inputStream:outputStream:") retVoid [argCULong bufferSize, argPtr (castPtr raw_inputStream :: Ptr ()), argPtr (castPtr raw_outputStream :: Ptr ())]

-- | @+ getStreamsToHostWithName:port:inputStream:outputStream:@
getStreamsToHostWithName_port_inputStream_outputStream :: (IsNSString hostname, IsNSInputStream inputStream, IsNSOutputStream outputStream) => hostname -> CLong -> inputStream -> outputStream -> IO ()
getStreamsToHostWithName_port_inputStream_outputStream hostname port inputStream outputStream =
  do
    cls' <- getRequiredClass "NSStream"
    withObjCPtr hostname $ \raw_hostname ->
      withObjCPtr inputStream $ \raw_inputStream ->
        withObjCPtr outputStream $ \raw_outputStream ->
          sendClassMsg cls' (mkSelector "getStreamsToHostWithName:port:inputStream:outputStream:") retVoid [argPtr (castPtr raw_hostname :: Ptr ()), argCLong port, argPtr (castPtr raw_inputStream :: Ptr ()), argPtr (castPtr raw_outputStream :: Ptr ())]

-- | @+ getStreamsToHost:port:inputStream:outputStream:@
getStreamsToHost_port_inputStream_outputStream :: (IsNSHost host, IsNSInputStream inputStream, IsNSOutputStream outputStream) => host -> CLong -> inputStream -> outputStream -> IO ()
getStreamsToHost_port_inputStream_outputStream host port inputStream outputStream =
  do
    cls' <- getRequiredClass "NSStream"
    withObjCPtr host $ \raw_host ->
      withObjCPtr inputStream $ \raw_inputStream ->
        withObjCPtr outputStream $ \raw_outputStream ->
          sendClassMsg cls' (mkSelector "getStreamsToHost:port:inputStream:outputStream:") retVoid [argPtr (castPtr raw_host :: Ptr ()), argCLong port, argPtr (castPtr raw_inputStream :: Ptr ()), argPtr (castPtr raw_outputStream :: Ptr ())]

-- | @- delegate@
delegate :: IsNSStream nsStream => nsStream -> IO RawId
delegate nsStream  =
    fmap (RawId . castPtr) $ sendMsg nsStream (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSStream nsStream => nsStream -> RawId -> IO ()
setDelegate nsStream  value =
    sendMsg nsStream (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- streamStatus@
streamStatus :: IsNSStream nsStream => nsStream -> IO NSStreamStatus
streamStatus nsStream  =
    fmap (coerce :: CULong -> NSStreamStatus) $ sendMsg nsStream (mkSelector "streamStatus") retCULong []

-- | @- streamError@
streamError :: IsNSStream nsStream => nsStream -> IO (Id NSError)
streamError nsStream  =
    sendMsg nsStream (mkSelector "streamError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @open@
openSelector :: Selector
openSelector = mkSelector "open"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @setProperty:forKey:@
setProperty_forKeySelector :: Selector
setProperty_forKeySelector = mkSelector "setProperty:forKey:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @getBoundStreamsWithBufferSize:inputStream:outputStream:@
getBoundStreamsWithBufferSize_inputStream_outputStreamSelector :: Selector
getBoundStreamsWithBufferSize_inputStream_outputStreamSelector = mkSelector "getBoundStreamsWithBufferSize:inputStream:outputStream:"

-- | @Selector@ for @getStreamsToHostWithName:port:inputStream:outputStream:@
getStreamsToHostWithName_port_inputStream_outputStreamSelector :: Selector
getStreamsToHostWithName_port_inputStream_outputStreamSelector = mkSelector "getStreamsToHostWithName:port:inputStream:outputStream:"

-- | @Selector@ for @getStreamsToHost:port:inputStream:outputStream:@
getStreamsToHost_port_inputStream_outputStreamSelector :: Selector
getStreamsToHost_port_inputStream_outputStreamSelector = mkSelector "getStreamsToHost:port:inputStream:outputStream:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @streamStatus@
streamStatusSelector :: Selector
streamStatusSelector = mkSelector "streamStatus"

-- | @Selector@ for @streamError@
streamErrorSelector :: Selector
streamErrorSelector = mkSelector "streamError"

