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
  , readDataOfMinLength_maxLength_timeout_completionHandlerSelector
  , writeData_timeout_completionHandlerSelector
  , captureStreamsSelector
  , closeWriteSelector
  , closeReadSelector
  , startSecureConnectionSelector
  , stopSecureConnectionSelector
  , initSelector
  , newSelector


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

-- | @- readDataOfMinLength:maxLength:timeout:completionHandler:@
readDataOfMinLength_maxLength_timeout_completionHandler :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> CULong -> CULong -> CDouble -> Ptr () -> IO ()
readDataOfMinLength_maxLength_timeout_completionHandler nsurlSessionStreamTask  minBytes maxBytes timeout completionHandler =
  sendMsg nsurlSessionStreamTask (mkSelector "readDataOfMinLength:maxLength:timeout:completionHandler:") retVoid [argCULong (fromIntegral minBytes), argCULong (fromIntegral maxBytes), argCDouble (fromIntegral timeout), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeData:timeout:completionHandler:@
writeData_timeout_completionHandler :: (IsNSURLSessionStreamTask nsurlSessionStreamTask, IsNSData data_) => nsurlSessionStreamTask -> data_ -> CDouble -> Ptr () -> IO ()
writeData_timeout_completionHandler nsurlSessionStreamTask  data_ timeout completionHandler =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsurlSessionStreamTask (mkSelector "writeData:timeout:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argCDouble (fromIntegral timeout), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- captureStreams@
captureStreams :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
captureStreams nsurlSessionStreamTask  =
  sendMsg nsurlSessionStreamTask (mkSelector "captureStreams") retVoid []

-- | @- closeWrite@
closeWrite :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
closeWrite nsurlSessionStreamTask  =
  sendMsg nsurlSessionStreamTask (mkSelector "closeWrite") retVoid []

-- | @- closeRead@
closeRead :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
closeRead nsurlSessionStreamTask  =
  sendMsg nsurlSessionStreamTask (mkSelector "closeRead") retVoid []

-- | @- startSecureConnection@
startSecureConnection :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
startSecureConnection nsurlSessionStreamTask  =
  sendMsg nsurlSessionStreamTask (mkSelector "startSecureConnection") retVoid []

-- | @- stopSecureConnection@
stopSecureConnection :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO ()
stopSecureConnection nsurlSessionStreamTask  =
  sendMsg nsurlSessionStreamTask (mkSelector "stopSecureConnection") retVoid []

-- | @- init@
init_ :: IsNSURLSessionStreamTask nsurlSessionStreamTask => nsurlSessionStreamTask -> IO (Id NSURLSessionStreamTask)
init_ nsurlSessionStreamTask  =
  sendMsg nsurlSessionStreamTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionStreamTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionStreamTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readDataOfMinLength:maxLength:timeout:completionHandler:@
readDataOfMinLength_maxLength_timeout_completionHandlerSelector :: Selector
readDataOfMinLength_maxLength_timeout_completionHandlerSelector = mkSelector "readDataOfMinLength:maxLength:timeout:completionHandler:"

-- | @Selector@ for @writeData:timeout:completionHandler:@
writeData_timeout_completionHandlerSelector :: Selector
writeData_timeout_completionHandlerSelector = mkSelector "writeData:timeout:completionHandler:"

-- | @Selector@ for @captureStreams@
captureStreamsSelector :: Selector
captureStreamsSelector = mkSelector "captureStreams"

-- | @Selector@ for @closeWrite@
closeWriteSelector :: Selector
closeWriteSelector = mkSelector "closeWrite"

-- | @Selector@ for @closeRead@
closeReadSelector :: Selector
closeReadSelector = mkSelector "closeRead"

-- | @Selector@ for @startSecureConnection@
startSecureConnectionSelector :: Selector
startSecureConnectionSelector = mkSelector "startSecureConnection"

-- | @Selector@ for @stopSecureConnection@
stopSecureConnectionSelector :: Selector
stopSecureConnectionSelector = mkSelector "stopSecureConnection"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

