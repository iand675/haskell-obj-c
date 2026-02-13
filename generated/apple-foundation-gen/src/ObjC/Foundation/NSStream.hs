{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , closeSelector
  , delegateSelector
  , getBoundStreamsWithBufferSize_inputStream_outputStreamSelector
  , getStreamsToHostWithName_port_inputStream_outputStreamSelector
  , getStreamsToHost_port_inputStream_outputStreamSelector
  , openSelector
  , propertyForKeySelector
  , removeFromRunLoop_forModeSelector
  , scheduleInRunLoop_forModeSelector
  , setDelegateSelector
  , setProperty_forKeySelector
  , streamErrorSelector
  , streamStatusSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- open@
open :: IsNSStream nsStream => nsStream -> IO ()
open nsStream =
  sendMessage nsStream openSelector

-- | @- close@
close :: IsNSStream nsStream => nsStream -> IO ()
close nsStream =
  sendMessage nsStream closeSelector

-- | @- propertyForKey:@
propertyForKey :: (IsNSStream nsStream, IsNSString key) => nsStream -> key -> IO RawId
propertyForKey nsStream key =
  sendMessage nsStream propertyForKeySelector (toNSString key)

-- | @- setProperty:forKey:@
setProperty_forKey :: (IsNSStream nsStream, IsNSString key) => nsStream -> RawId -> key -> IO Bool
setProperty_forKey nsStream property key =
  sendMessage nsStream setProperty_forKeySelector property (toNSString key)

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSStream nsStream, IsNSRunLoop aRunLoop, IsNSString mode) => nsStream -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsStream aRunLoop mode =
  sendMessage nsStream scheduleInRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSStream nsStream, IsNSRunLoop aRunLoop, IsNSString mode) => nsStream -> aRunLoop -> mode -> IO ()
removeFromRunLoop_forMode nsStream aRunLoop mode =
  sendMessage nsStream removeFromRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @+ getBoundStreamsWithBufferSize:inputStream:outputStream:@
getBoundStreamsWithBufferSize_inputStream_outputStream :: (IsNSInputStream inputStream, IsNSOutputStream outputStream) => CULong -> inputStream -> outputStream -> IO ()
getBoundStreamsWithBufferSize_inputStream_outputStream bufferSize inputStream outputStream =
  do
    cls' <- getRequiredClass "NSStream"
    sendClassMessage cls' getBoundStreamsWithBufferSize_inputStream_outputStreamSelector bufferSize (toNSInputStream inputStream) (toNSOutputStream outputStream)

-- | @+ getStreamsToHostWithName:port:inputStream:outputStream:@
getStreamsToHostWithName_port_inputStream_outputStream :: (IsNSString hostname, IsNSInputStream inputStream, IsNSOutputStream outputStream) => hostname -> CLong -> inputStream -> outputStream -> IO ()
getStreamsToHostWithName_port_inputStream_outputStream hostname port inputStream outputStream =
  do
    cls' <- getRequiredClass "NSStream"
    sendClassMessage cls' getStreamsToHostWithName_port_inputStream_outputStreamSelector (toNSString hostname) port (toNSInputStream inputStream) (toNSOutputStream outputStream)

-- | @+ getStreamsToHost:port:inputStream:outputStream:@
getStreamsToHost_port_inputStream_outputStream :: (IsNSHost host, IsNSInputStream inputStream, IsNSOutputStream outputStream) => host -> CLong -> inputStream -> outputStream -> IO ()
getStreamsToHost_port_inputStream_outputStream host port inputStream outputStream =
  do
    cls' <- getRequiredClass "NSStream"
    sendClassMessage cls' getStreamsToHost_port_inputStream_outputStreamSelector (toNSHost host) port (toNSInputStream inputStream) (toNSOutputStream outputStream)

-- | @- delegate@
delegate :: IsNSStream nsStream => nsStream -> IO RawId
delegate nsStream =
  sendMessage nsStream delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSStream nsStream => nsStream -> RawId -> IO ()
setDelegate nsStream value =
  sendMessage nsStream setDelegateSelector value

-- | @- streamStatus@
streamStatus :: IsNSStream nsStream => nsStream -> IO NSStreamStatus
streamStatus nsStream =
  sendMessage nsStream streamStatusSelector

-- | @- streamError@
streamError :: IsNSStream nsStream => nsStream -> IO (Id NSError)
streamError nsStream =
  sendMessage nsStream streamErrorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @open@
openSelector :: Selector '[] ()
openSelector = mkSelector "open"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector '[Id NSString] RawId
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @setProperty:forKey:@
setProperty_forKeySelector :: Selector '[RawId, Id NSString] Bool
setProperty_forKeySelector = mkSelector "setProperty:forKey:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @getBoundStreamsWithBufferSize:inputStream:outputStream:@
getBoundStreamsWithBufferSize_inputStream_outputStreamSelector :: Selector '[CULong, Id NSInputStream, Id NSOutputStream] ()
getBoundStreamsWithBufferSize_inputStream_outputStreamSelector = mkSelector "getBoundStreamsWithBufferSize:inputStream:outputStream:"

-- | @Selector@ for @getStreamsToHostWithName:port:inputStream:outputStream:@
getStreamsToHostWithName_port_inputStream_outputStreamSelector :: Selector '[Id NSString, CLong, Id NSInputStream, Id NSOutputStream] ()
getStreamsToHostWithName_port_inputStream_outputStreamSelector = mkSelector "getStreamsToHostWithName:port:inputStream:outputStream:"

-- | @Selector@ for @getStreamsToHost:port:inputStream:outputStream:@
getStreamsToHost_port_inputStream_outputStreamSelector :: Selector '[Id NSHost, CLong, Id NSInputStream, Id NSOutputStream] ()
getStreamsToHost_port_inputStream_outputStreamSelector = mkSelector "getStreamsToHost:port:inputStream:outputStream:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @streamStatus@
streamStatusSelector :: Selector '[] NSStreamStatus
streamStatusSelector = mkSelector "streamStatus"

-- | @Selector@ for @streamError@
streamErrorSelector :: Selector '[] (Id NSError)
streamErrorSelector = mkSelector "streamError"

