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
init_ :: IsAVAssetWriterInputPassDescription avAssetWriterInputPassDescription => avAssetWriterInputPassDescription -> IO (Id AVAssetWriterInputPassDescription)
init_ avAssetWriterInputPassDescription  =
  sendMsg avAssetWriterInputPassDescription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetWriterInputPassDescription)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputPassDescription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An NSArray of NSValue objects wrapping CMTimeRange structures, each representing one source time range.
--
-- The value of this property is suitable for using as a parameter for -[AVAssetReaderOutput resetForReadingTimeRanges:].
--
-- ObjC selector: @- sourceTimeRanges@
sourceTimeRanges :: IsAVAssetWriterInputPassDescription avAssetWriterInputPassDescription => avAssetWriterInputPassDescription -> IO (Id NSArray)
sourceTimeRanges avAssetWriterInputPassDescription  =
  sendMsg avAssetWriterInputPassDescription (mkSelector "sourceTimeRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sourceTimeRanges@
sourceTimeRangesSelector :: Selector
sourceTimeRangesSelector = mkSelector "sourceTimeRanges"

