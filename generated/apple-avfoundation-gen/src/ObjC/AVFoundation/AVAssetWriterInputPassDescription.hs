{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines an interface for querying information about the requirements of the current pass, such as the time ranges of media data to append.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetWriterInputPassDescription@.
module ObjC.AVFoundation.AVAssetWriterInputPassDescription
  ( AVAssetWriterInputPassDescription
  , IsAVAssetWriterInputPassDescription(..)
  , init_
  , new
  , sourceTimeRanges
  , initSelector
  , newSelector
  , sourceTimeRangesSelector


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
init_ :: IsAVAssetWriterInputPassDescription avAssetWriterInputPassDescription => avAssetWriterInputPassDescription -> IO (Id AVAssetWriterInputPassDescription)
init_ avAssetWriterInputPassDescription =
  sendOwnedMessage avAssetWriterInputPassDescription initSelector

-- | @+ new@
new :: IO (Id AVAssetWriterInputPassDescription)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputPassDescription"
    sendOwnedClassMessage cls' newSelector

-- | An NSArray of NSValue objects wrapping CMTimeRange structures, each representing one source time range.
--
-- The value of this property is suitable for using as a parameter for -[AVAssetReaderOutput resetForReadingTimeRanges:].
--
-- ObjC selector: @- sourceTimeRanges@
sourceTimeRanges :: IsAVAssetWriterInputPassDescription avAssetWriterInputPassDescription => avAssetWriterInputPassDescription -> IO (Id NSArray)
sourceTimeRanges avAssetWriterInputPassDescription =
  sendMessage avAssetWriterInputPassDescription sourceTimeRangesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriterInputPassDescription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriterInputPassDescription)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceTimeRanges@
sourceTimeRangesSelector :: Selector '[] (Id NSArray)
sourceTimeRangesSelector = mkSelector "sourceTimeRanges"

