{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MESampleCursorChunk
--
-- Provides information about the chunk of media where a sample is located.
--
-- An instance of this class is returned by calls to the MESampleCursor method chunkDetails.
--
-- Generated bindings for @MESampleCursorChunk@.
module ObjC.MediaExtension.MESampleCursorChunk
  ( MESampleCursorChunk
  , IsMESampleCursorChunk(..)
  , new
  , init_
  , byteSource
  , sampleIndexWithinChunk
  , byteSourceSelector
  , initSelector
  , newSelector
  , sampleIndexWithinChunkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MESampleCursorChunk)
new  =
  do
    cls' <- getRequiredClass "MESampleCursorChunk"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMESampleCursorChunk meSampleCursorChunk => meSampleCursorChunk -> IO (Id MESampleCursorChunk)
init_ meSampleCursorChunk =
  sendOwnedMessage meSampleCursorChunk initSelector

-- | byteSource
--
-- The MEByteSource to be used to read the data for the sample.
--
-- ObjC selector: @- byteSource@
byteSource :: IsMESampleCursorChunk meSampleCursorChunk => meSampleCursorChunk -> IO (Id MEByteSource)
byteSource meSampleCursorChunk =
  sendMessage meSampleCursorChunk byteSourceSelector

-- | sampleIndexWithinChunk
--
-- The offset of the sample within the chunk, in samples.
--
-- Index value 0 corresponds to the start of the chunk. You would step back this many samples to position the cursor at the start of the chunk. Subtract from the chunkInfo.chunkSampleCount field to obtain the number of samples to the end of the chunk.
--
-- ObjC selector: @- sampleIndexWithinChunk@
sampleIndexWithinChunk :: IsMESampleCursorChunk meSampleCursorChunk => meSampleCursorChunk -> IO CLong
sampleIndexWithinChunk meSampleCursorChunk =
  sendMessage meSampleCursorChunk sampleIndexWithinChunkSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MESampleCursorChunk)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MESampleCursorChunk)
initSelector = mkSelector "init"

-- | @Selector@ for @byteSource@
byteSourceSelector :: Selector '[] (Id MEByteSource)
byteSourceSelector = mkSelector "byteSource"

-- | @Selector@ for @sampleIndexWithinChunk@
sampleIndexWithinChunkSelector :: Selector '[] CLong
sampleIndexWithinChunkSelector = mkSelector "sampleIndexWithinChunk"

