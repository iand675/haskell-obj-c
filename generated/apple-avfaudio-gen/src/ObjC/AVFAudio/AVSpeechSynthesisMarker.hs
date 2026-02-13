{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSpeechSynthesisMarker@.
module ObjC.AVFAudio.AVSpeechSynthesisMarker
  ( AVSpeechSynthesisMarker
  , IsAVSpeechSynthesisMarker(..)
  , initWithMarkerType_forTextRange_atByteSampleOffset
  , initWithWordRange_atByteSampleOffset
  , initWithSentenceRange_atByteSampleOffset
  , initWithParagraphRange_atByteSampleOffset
  , initWithPhonemeString_atByteSampleOffset
  , initWithBookmarkName_atByteSampleOffset
  , mark
  , setMark
  , byteSampleOffset
  , setByteSampleOffset
  , textRange
  , setTextRange
  , bookmarkName
  , setBookmarkName
  , phoneme
  , setPhoneme
  , bookmarkNameSelector
  , byteSampleOffsetSelector
  , initWithBookmarkName_atByteSampleOffsetSelector
  , initWithMarkerType_forTextRange_atByteSampleOffsetSelector
  , initWithParagraphRange_atByteSampleOffsetSelector
  , initWithPhonemeString_atByteSampleOffsetSelector
  , initWithSentenceRange_atByteSampleOffsetSelector
  , initWithWordRange_atByteSampleOffsetSelector
  , markSelector
  , phonemeSelector
  , setBookmarkNameSelector
  , setByteSampleOffsetSelector
  , setMarkSelector
  , setPhonemeSelector
  , setTextRangeSelector
  , textRangeSelector

  -- * Enum types
  , AVSpeechSynthesisMarkerMark(AVSpeechSynthesisMarkerMark)
  , pattern AVSpeechSynthesisMarkerMarkPhoneme
  , pattern AVSpeechSynthesisMarkerMarkWord
  , pattern AVSpeechSynthesisMarkerMarkSentence
  , pattern AVSpeechSynthesisMarkerMarkParagraph
  , pattern AVSpeechSynthesisMarkerMarkBookmark

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMarkerType:forTextRange:atByteSampleOffset:@
initWithMarkerType_forTextRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> AVSpeechSynthesisMarkerMark -> NSRange -> CULong -> IO (Id AVSpeechSynthesisMarker)
initWithMarkerType_forTextRange_atByteSampleOffset avSpeechSynthesisMarker type_ range byteSampleOffset =
  sendOwnedMessage avSpeechSynthesisMarker initWithMarkerType_forTextRange_atByteSampleOffsetSelector type_ range byteSampleOffset

-- | @- initWithWordRange:atByteSampleOffset:@
initWithWordRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithWordRange_atByteSampleOffset avSpeechSynthesisMarker range byteSampleOffset =
  sendOwnedMessage avSpeechSynthesisMarker initWithWordRange_atByteSampleOffsetSelector range byteSampleOffset

-- | @- initWithSentenceRange:atByteSampleOffset:@
initWithSentenceRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithSentenceRange_atByteSampleOffset avSpeechSynthesisMarker range byteSampleOffset =
  sendOwnedMessage avSpeechSynthesisMarker initWithSentenceRange_atByteSampleOffsetSelector range byteSampleOffset

-- | @- initWithParagraphRange:atByteSampleOffset:@
initWithParagraphRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithParagraphRange_atByteSampleOffset avSpeechSynthesisMarker range byteSampleOffset =
  sendOwnedMessage avSpeechSynthesisMarker initWithParagraphRange_atByteSampleOffsetSelector range byteSampleOffset

-- | @- initWithPhonemeString:atByteSampleOffset:@
initWithPhonemeString_atByteSampleOffset :: (IsAVSpeechSynthesisMarker avSpeechSynthesisMarker, IsNSString phoneme) => avSpeechSynthesisMarker -> phoneme -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithPhonemeString_atByteSampleOffset avSpeechSynthesisMarker phoneme byteSampleOffset =
  sendOwnedMessage avSpeechSynthesisMarker initWithPhonemeString_atByteSampleOffsetSelector (toNSString phoneme) byteSampleOffset

-- | @- initWithBookmarkName:atByteSampleOffset:@
initWithBookmarkName_atByteSampleOffset :: (IsAVSpeechSynthesisMarker avSpeechSynthesisMarker, IsNSString mark) => avSpeechSynthesisMarker -> mark -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithBookmarkName_atByteSampleOffset avSpeechSynthesisMarker mark byteSampleOffset =
  sendOwnedMessage avSpeechSynthesisMarker initWithBookmarkName_atByteSampleOffsetSelector (toNSString mark) byteSampleOffset

-- | @- mark@
mark :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO AVSpeechSynthesisMarkerMark
mark avSpeechSynthesisMarker =
  sendMessage avSpeechSynthesisMarker markSelector

-- | @- setMark:@
setMark :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> AVSpeechSynthesisMarkerMark -> IO ()
setMark avSpeechSynthesisMarker value =
  sendMessage avSpeechSynthesisMarker setMarkSelector value

-- | Byte offset into the associated audio buffer
--
-- ObjC selector: @- byteSampleOffset@
byteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO CULong
byteSampleOffset avSpeechSynthesisMarker =
  sendMessage avSpeechSynthesisMarker byteSampleOffsetSelector

-- | Byte offset into the associated audio buffer
--
-- ObjC selector: @- setByteSampleOffset:@
setByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> CULong -> IO ()
setByteSampleOffset avSpeechSynthesisMarker value =
  sendMessage avSpeechSynthesisMarker setByteSampleOffsetSelector value

-- | The location and length of the pertaining speech request's SSML text. This marker applies to the range of characters represented by the NSString.
--
-- ObjC selector: @- textRange@
textRange :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO NSRange
textRange avSpeechSynthesisMarker =
  sendMessage avSpeechSynthesisMarker textRangeSelector

-- | The location and length of the pertaining speech request's SSML text. This marker applies to the range of characters represented by the NSString.
--
-- ObjC selector: @- setTextRange:@
setTextRange :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> IO ()
setTextRange avSpeechSynthesisMarker value =
  sendMessage avSpeechSynthesisMarker setTextRangeSelector value

-- | @- bookmarkName@
bookmarkName :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO RawId
bookmarkName avSpeechSynthesisMarker =
  sendMessage avSpeechSynthesisMarker bookmarkNameSelector

-- | @- setBookmarkName:@
setBookmarkName :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> RawId -> IO ()
setBookmarkName avSpeechSynthesisMarker value =
  sendMessage avSpeechSynthesisMarker setBookmarkNameSelector value

-- | @- phoneme@
phoneme :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO RawId
phoneme avSpeechSynthesisMarker =
  sendMessage avSpeechSynthesisMarker phonemeSelector

-- | @- setPhoneme:@
setPhoneme :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> RawId -> IO ()
setPhoneme avSpeechSynthesisMarker value =
  sendMessage avSpeechSynthesisMarker setPhonemeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMarkerType:forTextRange:atByteSampleOffset:@
initWithMarkerType_forTextRange_atByteSampleOffsetSelector :: Selector '[AVSpeechSynthesisMarkerMark, NSRange, CULong] (Id AVSpeechSynthesisMarker)
initWithMarkerType_forTextRange_atByteSampleOffsetSelector = mkSelector "initWithMarkerType:forTextRange:atByteSampleOffset:"

-- | @Selector@ for @initWithWordRange:atByteSampleOffset:@
initWithWordRange_atByteSampleOffsetSelector :: Selector '[NSRange, CLong] (Id AVSpeechSynthesisMarker)
initWithWordRange_atByteSampleOffsetSelector = mkSelector "initWithWordRange:atByteSampleOffset:"

-- | @Selector@ for @initWithSentenceRange:atByteSampleOffset:@
initWithSentenceRange_atByteSampleOffsetSelector :: Selector '[NSRange, CLong] (Id AVSpeechSynthesisMarker)
initWithSentenceRange_atByteSampleOffsetSelector = mkSelector "initWithSentenceRange:atByteSampleOffset:"

-- | @Selector@ for @initWithParagraphRange:atByteSampleOffset:@
initWithParagraphRange_atByteSampleOffsetSelector :: Selector '[NSRange, CLong] (Id AVSpeechSynthesisMarker)
initWithParagraphRange_atByteSampleOffsetSelector = mkSelector "initWithParagraphRange:atByteSampleOffset:"

-- | @Selector@ for @initWithPhonemeString:atByteSampleOffset:@
initWithPhonemeString_atByteSampleOffsetSelector :: Selector '[Id NSString, CLong] (Id AVSpeechSynthesisMarker)
initWithPhonemeString_atByteSampleOffsetSelector = mkSelector "initWithPhonemeString:atByteSampleOffset:"

-- | @Selector@ for @initWithBookmarkName:atByteSampleOffset:@
initWithBookmarkName_atByteSampleOffsetSelector :: Selector '[Id NSString, CLong] (Id AVSpeechSynthesisMarker)
initWithBookmarkName_atByteSampleOffsetSelector = mkSelector "initWithBookmarkName:atByteSampleOffset:"

-- | @Selector@ for @mark@
markSelector :: Selector '[] AVSpeechSynthesisMarkerMark
markSelector = mkSelector "mark"

-- | @Selector@ for @setMark:@
setMarkSelector :: Selector '[AVSpeechSynthesisMarkerMark] ()
setMarkSelector = mkSelector "setMark:"

-- | @Selector@ for @byteSampleOffset@
byteSampleOffsetSelector :: Selector '[] CULong
byteSampleOffsetSelector = mkSelector "byteSampleOffset"

-- | @Selector@ for @setByteSampleOffset:@
setByteSampleOffsetSelector :: Selector '[CULong] ()
setByteSampleOffsetSelector = mkSelector "setByteSampleOffset:"

-- | @Selector@ for @textRange@
textRangeSelector :: Selector '[] NSRange
textRangeSelector = mkSelector "textRange"

-- | @Selector@ for @setTextRange:@
setTextRangeSelector :: Selector '[NSRange] ()
setTextRangeSelector = mkSelector "setTextRange:"

-- | @Selector@ for @bookmarkName@
bookmarkNameSelector :: Selector '[] RawId
bookmarkNameSelector = mkSelector "bookmarkName"

-- | @Selector@ for @setBookmarkName:@
setBookmarkNameSelector :: Selector '[RawId] ()
setBookmarkNameSelector = mkSelector "setBookmarkName:"

-- | @Selector@ for @phoneme@
phonemeSelector :: Selector '[] RawId
phonemeSelector = mkSelector "phoneme"

-- | @Selector@ for @setPhoneme:@
setPhonemeSelector :: Selector '[RawId] ()
setPhonemeSelector = mkSelector "setPhoneme:"

