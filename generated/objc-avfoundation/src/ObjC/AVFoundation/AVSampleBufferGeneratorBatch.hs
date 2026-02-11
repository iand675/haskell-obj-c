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
  , initSelector
  , newSelector
  , makeDataReadyWithCompletionHandlerSelector
  , cancelSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSampleBufferGeneratorBatch avSampleBufferGeneratorBatch => avSampleBufferGeneratorBatch -> IO (Id AVSampleBufferGeneratorBatch)
init_ avSampleBufferGeneratorBatch  =
  sendMsg avSampleBufferGeneratorBatch (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSampleBufferGeneratorBatch)
new  =
  do
    cls' <- getRequiredClass "AVSampleBufferGeneratorBatch"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | makeDataReadyWithCompletionHandler:
--
-- Loads sample data asynchronously for all CMSampleBuffers within a batch.				This can only be called once on a batch, an exception will be thrown otherwise.
--
-- @completionHandler@ — The completionHandler is called once, when all CMSampleBuffers in the batch are data-ready, or as soon as an error has occurred.
--
-- ObjC selector: @- makeDataReadyWithCompletionHandler:@
makeDataReadyWithCompletionHandler :: IsAVSampleBufferGeneratorBatch avSampleBufferGeneratorBatch => avSampleBufferGeneratorBatch -> Ptr () -> IO ()
makeDataReadyWithCompletionHandler avSampleBufferGeneratorBatch  completionHandler =
  sendMsg avSampleBufferGeneratorBatch (mkSelector "makeDataReadyWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | cancel
--
-- Attempt to cancel any I/O for this batch. The associated sample buffers will have their data ready handler invoked with an error.
--
-- ObjC selector: @- cancel@
cancel :: IsAVSampleBufferGeneratorBatch avSampleBufferGeneratorBatch => avSampleBufferGeneratorBatch -> IO ()
cancel avSampleBufferGeneratorBatch  =
  sendMsg avSampleBufferGeneratorBatch (mkSelector "cancel") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @makeDataReadyWithCompletionHandler:@
makeDataReadyWithCompletionHandlerSelector :: Selector
makeDataReadyWithCompletionHandlerSelector = mkSelector "makeDataReadyWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

