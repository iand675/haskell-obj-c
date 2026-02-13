{-# LANGUAGE DataKinds #-}
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
  , fileNamesSelector
  , fileTypesSelector
  , readableDraggedTypesSelector
  , receivePromisedFilesAtDestination_options_operationQueue_readerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- receivePromisedFilesAtDestination:options:operationQueue:reader:@
receivePromisedFilesAtDestination_options_operationQueue_reader :: (IsNSFilePromiseReceiver nsFilePromiseReceiver, IsNSURL destinationDir, IsNSDictionary options, IsNSOperationQueue operationQueue) => nsFilePromiseReceiver -> destinationDir -> options -> operationQueue -> Ptr () -> IO ()
receivePromisedFilesAtDestination_options_operationQueue_reader nsFilePromiseReceiver destinationDir options operationQueue reader =
  sendMessage nsFilePromiseReceiver receivePromisedFilesAtDestination_options_operationQueue_readerSelector (toNSURL destinationDir) (toNSDictionary options) (toNSOperationQueue operationQueue) reader

-- | @+ readableDraggedTypes@
readableDraggedTypes :: IO (Id NSArray)
readableDraggedTypes  =
  do
    cls' <- getRequiredClass "NSFilePromiseReceiver"
    sendClassMessage cls' readableDraggedTypesSelector

-- | @- fileTypes@
fileTypes :: IsNSFilePromiseReceiver nsFilePromiseReceiver => nsFilePromiseReceiver -> IO (Id NSArray)
fileTypes nsFilePromiseReceiver =
  sendMessage nsFilePromiseReceiver fileTypesSelector

-- | @- fileNames@
fileNames :: IsNSFilePromiseReceiver nsFilePromiseReceiver => nsFilePromiseReceiver -> IO (Id NSArray)
fileNames nsFilePromiseReceiver =
  sendMessage nsFilePromiseReceiver fileNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @receivePromisedFilesAtDestination:options:operationQueue:reader:@
receivePromisedFilesAtDestination_options_operationQueue_readerSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSOperationQueue, Ptr ()] ()
receivePromisedFilesAtDestination_options_operationQueue_readerSelector = mkSelector "receivePromisedFilesAtDestination:options:operationQueue:reader:"

-- | @Selector@ for @readableDraggedTypes@
readableDraggedTypesSelector :: Selector '[] (Id NSArray)
readableDraggedTypesSelector = mkSelector "readableDraggedTypes"

-- | @Selector@ for @fileTypes@
fileTypesSelector :: Selector '[] (Id NSArray)
fileTypesSelector = mkSelector "fileTypes"

-- | @Selector@ for @fileNames@
fileNamesSelector :: Selector '[] (Id NSArray)
fileNamesSelector = mkSelector "fileNames"

