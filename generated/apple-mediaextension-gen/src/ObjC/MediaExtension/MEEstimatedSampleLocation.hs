{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEEstimatedSampleLocation
--
-- Provides information about the estimated sample location with the media.
--
-- An instance of this class is returned by calls to the MESampleCursor method estimatedSampleLocationReturningError.
--
-- Generated bindings for @MEEstimatedSampleLocation@.
module ObjC.MediaExtension.MEEstimatedSampleLocation
  ( MEEstimatedSampleLocation
  , IsMEEstimatedSampleLocation(..)
  , new
  , init_
  , byteSource
  , byteSourceSelector
  , initSelector
  , newSelector


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
new :: IO (Id MEEstimatedSampleLocation)
new  =
  do
    cls' <- getRequiredClass "MEEstimatedSampleLocation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEEstimatedSampleLocation meEstimatedSampleLocation => meEstimatedSampleLocation -> IO (Id MEEstimatedSampleLocation)
init_ meEstimatedSampleLocation =
  sendOwnedMessage meEstimatedSampleLocation initSelector

-- | byteSource
--
-- The MEByteSource to be used to read the data for the sample.
--
-- ObjC selector: @- byteSource@
byteSource :: IsMEEstimatedSampleLocation meEstimatedSampleLocation => meEstimatedSampleLocation -> IO (Id MEByteSource)
byteSource meEstimatedSampleLocation =
  sendMessage meEstimatedSampleLocation byteSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEEstimatedSampleLocation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEEstimatedSampleLocation)
initSelector = mkSelector "init"

-- | @Selector@ for @byteSource@
byteSourceSelector :: Selector '[] (Id MEByteSource)
byteSourceSelector = mkSelector "byteSource"

