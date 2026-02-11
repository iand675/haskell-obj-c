{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFilePromiseReceiver@.
module ObjC.AppKit.NSFilePromiseReceiver
  ( NSFilePromiseReceiver
  , IsNSFilePromiseReceiver(..)
  , receivePromisedFilesAtDestination_options_operationQueue_reader
  , readableDraggedTypes
  , fileTypes
  , fileNames
  , receivePromisedFilesAtDestination_options_operationQueue_readerSelector
  , readableDraggedTypesSelector
  , fileTypesSelector
  , fileNamesSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- receivePromisedFilesAtDestination:options:operationQueue:reader:@
receivePromisedFilesAtDestination_options_operationQueue_reader :: (IsNSFilePromiseReceiver nsFilePromiseReceiver, IsNSURL destinationDir, IsNSDictionary options, IsNSOperationQueue operationQueue) => nsFilePromiseReceiver -> destinationDir -> options -> operationQueue -> Ptr () -> IO ()
receivePromisedFilesAtDestination_options_operationQueue_reader nsFilePromiseReceiver  destinationDir options operationQueue reader =
withObjCPtr destinationDir $ \raw_destinationDir ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr operationQueue $ \raw_operationQueue ->
        sendMsg nsFilePromiseReceiver (mkSelector "receivePromisedFilesAtDestination:options:operationQueue:reader:") retVoid [argPtr (castPtr raw_destinationDir :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_operationQueue :: Ptr ()), argPtr (castPtr reader :: Ptr ())]

-- | @+ readableDraggedTypes@
readableDraggedTypes :: IO (Id NSArray)
readableDraggedTypes  =
  do
    cls' <- getRequiredClass "NSFilePromiseReceiver"
    sendClassMsg cls' (mkSelector "readableDraggedTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileTypes@
fileTypes :: IsNSFilePromiseReceiver nsFilePromiseReceiver => nsFilePromiseReceiver -> IO (Id NSArray)
fileTypes nsFilePromiseReceiver  =
  sendMsg nsFilePromiseReceiver (mkSelector "fileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileNames@
fileNames :: IsNSFilePromiseReceiver nsFilePromiseReceiver => nsFilePromiseReceiver -> IO (Id NSArray)
fileNames nsFilePromiseReceiver  =
  sendMsg nsFilePromiseReceiver (mkSelector "fileNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @receivePromisedFilesAtDestination:options:operationQueue:reader:@
receivePromisedFilesAtDestination_options_operationQueue_readerSelector :: Selector
receivePromisedFilesAtDestination_options_operationQueue_readerSelector = mkSelector "receivePromisedFilesAtDestination:options:operationQueue:reader:"

-- | @Selector@ for @readableDraggedTypes@
readableDraggedTypesSelector :: Selector
readableDraggedTypesSelector = mkSelector "readableDraggedTypes"

-- | @Selector@ for @fileTypes@
fileTypesSelector :: Selector
fileTypesSelector = mkSelector "fileTypes"

-- | @Selector@ for @fileNames@
fileNamesSelector :: Selector
fileNamesSelector = mkSelector "fileNames"

