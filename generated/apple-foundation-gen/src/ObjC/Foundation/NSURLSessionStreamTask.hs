{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionStreamTask@.
module ObjC.Foundation.NSURLSessionStreamTask
  ( NSURLSessionStreamTask
  , IsNSURLSessionStreamTask(..)
  , readDataOfMinLength_maxLength_timeout_completionHandler
  , writeData_timeout_completionHandler
  , captureStreams
  , closeWrite
  , closeRead
  , startSecureConnection
  , stopSecureConnection
  , init_
  , new
  , captureStreamsSelector
  , closeReadSelector
  , closeWriteSelector
  , initSelector
  , newSelector
  , readDataOfMinLength_maxLength_timeout_completionHandlerSelector
  , startSecureConnectionSelector
  , stopSecureConnectionSelector
  , writeData_timeout_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- readDataOfMinLength:maxLength:timeout:completionHandler:@
readDataOfMinLength_maxLength_timeout_completionHandler :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> CULong -> CULong -> CDouble -> Ptr () -> IO ()
readDataOfMinLength_maxLength_timeout_completionHandler nsurlSessionStreamTask minBytes maxBytes timeout completionHandler =
  sendMessage nsurlSessionStreamTask readDataOfMinLength_maxLength_timeout_completionHandlerSelector minBytes maxBytes timeout completionHandler

-- | @- writeData:timeout:completionHandler:@
writeData_timeout_completionHandler :: (IsNSURLSessionStreamTask nsurlSessionStreamTask, IsNSData data_) => nsurlSessionStreamTask -> data_ -> CDouble -> Ptr () -> IO ()
writeData_timeout_completionHandler nsurlSessionStreamTask data_ timeout completionHandler =
  sendMessage nsurlSessionStreamTask writeData_timeout_completionHandlerSelector (toNSData data_) timeout completionHandler

-- | @- captureStreams@
captureStreams :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
captureStreams nsurlSessionStreamTask =
  sendMessage nsurlSessionStreamTask captureStreamsSelector

-- | @- closeWrite@
closeWrite :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
closeWrite nsurlSessionStreamTask =
  sendMessage nsurlSessionStreamTask closeWriteSelector

-- | @- closeRead@
closeRead :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
closeRead nsurlSessionStreamTask =
  sendMessage nsurlSessionStreamTask closeReadSelector

-- | @- startSecureConnection@
startSecureConnection :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
startSecureConnection nsurlSessionStreamTask =
  sendMessage nsurlSessionStreamTask startSecureConnectionSelector

-- | @- stopSecureConnection@
stopSecureConnection :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
stopSecureConnection nsurlSessionStreamTask =
  sendMessage nsurlSessionStreamTask stopSecureConnectionSelector

-- | @- init@
init_ :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO (Id NSURLSessionStreamTask)
init_ nsurlSessionStreamTask =
  sendOwnedMessage nsurlSessionStreamTask initSelector

-- | @+ new@
new :: IO (Id NSURLSessionStreamTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionStreamTask"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readDataOfMinLength:maxLength:timeout:completionHandler:@
readDataOfMinLength_maxLength_timeout_completionHandlerSelector :: Selector '[CULong, CULong, CDouble, Ptr ()] ()
readDataOfMinLength_maxLength_timeout_completionHandlerSelector = mkSelector "readDataOfMinLength:maxLength:timeout:completionHandler:"

-- | @Selector@ for @writeData:timeout:completionHandler:@
writeData_timeout_completionHandlerSelector :: Selector '[Id NSData, CDouble, Ptr ()] ()
writeData_timeout_completionHandlerSelector = mkSelector "writeData:timeout:completionHandler:"

-- | @Selector@ for @captureStreams@
captureStreamsSelector :: Selector '[] ()
captureStreamsSelector = mkSelector "captureStreams"

-- | @Selector@ for @closeWrite@
closeWriteSelector :: Selector '[] ()
closeWriteSelector = mkSelector "closeWrite"

-- | @Selector@ for @closeRead@
closeReadSelector :: Selector '[] ()
closeReadSelector = mkSelector "closeRead"

-- | @Selector@ for @startSecureConnection@
startSecureConnectionSelector :: Selector '[] ()
startSecureConnectionSelector = mkSelector "startSecureConnection"

-- | @Selector@ for @stopSecureConnection@
stopSecureConnectionSelector :: Selector '[] ()
stopSecureConnectionSelector = mkSelector "stopSecureConnection"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionStreamTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionStreamTask)
newSelector = mkSelector "new"

