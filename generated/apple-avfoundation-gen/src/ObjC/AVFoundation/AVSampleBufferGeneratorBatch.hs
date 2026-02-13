{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSampleBufferGeneratorBatch
--
-- An AVSampleBufferGeneratorBatch provides an optimized way to load sample data asynchronously for multiple CMSampleBuffers in an asset.
--
-- The AVSampleBufferGeneratorBatch loads sample data asynchronously, by aggregating adjacent I/O requests and overlapping them when possible for all CMSampleBuffers within a batch.		An AVSampleBufferGeneratorBatch is associated with an AVSampleBufferGenerator. See -[AVSampleBufferGenerator makeBatch] to create an AVSampleBufferGeneratorBatch.		See -[AVSampleBufferGeneratorBatch createSampleBufferForRequest: addingToBatch: error:] to create a CMSampleBuffer, defer I/O for its data, and build up a batch.		Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVSampleBufferGeneratorBatch@.
module ObjC.AVFoundation.AVSampleBufferGeneratorBatch
  ( AVSampleBufferGeneratorBatch
  , IsAVSampleBufferGeneratorBatch(..)
  , init_
  , new
  , makeDataReadyWithCompletionHandler
  , cancel
  , cancelSelector
  , initSelector
  , makeDataReadyWithCompletionHandlerSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSampleBufferGeneratorBatch avSampleBufferGeneratorBatch => avSampleBufferGeneratorBatch -> IO (Id AVSampleBufferGeneratorBatch)
init_ avSampleBufferGeneratorBatch =
  sendOwnedMessage avSampleBufferGeneratorBatch initSelector

-- | @+ new@
new :: IO (Id AVSampleBufferGeneratorBatch)
new  =
  do
    cls' <- getRequiredClass "AVSampleBufferGeneratorBatch"
    sendOwnedClassMessage cls' newSelector

-- | makeDataReadyWithCompletionHandler:
--
-- Loads sample data asynchronously for all CMSampleBuffers within a batch.				This can only be called once on a batch, an exception will be thrown otherwise.
--
-- @completionHandler@ — The completionHandler is called once, when all CMSampleBuffers in the batch are data-ready, or as soon as an error has occurred.
--
-- ObjC selector: @- makeDataReadyWithCompletionHandler:@
makeDataReadyWithCompletionHandler :: IsAVSampleBufferGeneratorBatch avSampleBufferGeneratorBatch => avSampleBufferGeneratorBatch -> Ptr () -> IO ()
makeDataReadyWithCompletionHandler avSampleBufferGeneratorBatch completionHandler =
  sendMessage avSampleBufferGeneratorBatch makeDataReadyWithCompletionHandlerSelector completionHandler

-- | cancel
--
-- Attempt to cancel any I/O for this batch. The associated sample buffers will have their data ready handler invoked with an error.
--
-- ObjC selector: @- cancel@
cancel :: IsAVSampleBufferGeneratorBatch avSampleBufferGeneratorBatch => avSampleBufferGeneratorBatch -> IO ()
cancel avSampleBufferGeneratorBatch =
  sendMessage avSampleBufferGeneratorBatch cancelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSampleBufferGeneratorBatch)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSampleBufferGeneratorBatch)
newSelector = mkSelector "new"

-- | @Selector@ for @makeDataReadyWithCompletionHandler:@
makeDataReadyWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
makeDataReadyWithCompletionHandlerSelector = mkSelector "makeDataReadyWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

