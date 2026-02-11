{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMarkerType_forTextRange_atByteSampleOffsetSelector
  , initWithWordRange_atByteSampleOffsetSelector
  , initWithSentenceRange_atByteSampleOffsetSelector
  , initWithParagraphRange_atByteSampleOffsetSelector
  , initWithPhonemeString_atByteSampleOffsetSelector
  , initWithBookmarkName_atByteSampleOffsetSelector
  , markSelector
  , setMarkSelector
  , byteSampleOffsetSelector
  , setByteSampleOffsetSelector
  , textRangeSelector
  , setTextRangeSelector

  -- * Enum types
  , AVSpeechSynthesisMarkerMark(AVSpeechSynthesisMarkerMark)
  , pattern AVSpeechSynthesisMarkerMarkPhoneme
  , pattern AVSpeechSynthesisMarkerMarkWord
  , pattern AVSpeechSynthesisMarkerMarkSentence
  , pattern AVSpeechSynthesisMarkerMarkParagraph
  , pattern AVSpeechSynthesisMarkerMarkBookmark

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMarkerType:forTextRange:atByteSampleOffset:@
initWithMarkerType_forTextRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> AVSpeechSynthesisMarkerMark -> NSRange -> CULong -> IO (Id AVSpeechSynthesisMarker)
initWithMarkerType_forTextRange_atByteSampleOffset avSpeechSynthesisMarker  type_ range byteSampleOffset =
  sendMsg avSpeechSynthesisMarker (mkSelector "initWithMarkerType:forTextRange:atByteSampleOffset:") (retPtr retVoid) [argCLong (coerce type_), argNSRange range, argCULong (fromIntegral byteSampleOffset)] >>= ownedObject . castPtr

-- | @- initWithWordRange:atByteSampleOffset:@
initWithWordRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithWordRange_atByteSampleOffset avSpeechSynthesisMarker  range byteSampleOffset =
  sendMsg avSpeechSynthesisMarker (mkSelector "initWithWordRange:atByteSampleOffset:") (retPtr retVoid) [argNSRange range, argCLong (fromIntegral byteSampleOffset)] >>= ownedObject . castPtr

-- | @- initWithSentenceRange:atByteSampleOffset:@
initWithSentenceRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithSentenceRange_atByteSampleOffset avSpeechSynthesisMarker  range byteSampleOffset =
  sendMsg avSpeechSynthesisMarker (mkSelector "initWithSentenceRange:atByteSampleOffset:") (retPtr retVoid) [argNSRange range, argCLong (fromIntegral byteSampleOffset)] >>= ownedObject . castPtr

-- | @- initWithParagraphRange:atByteSampleOffset:@
initWithParagraphRange_atByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithParagraphRange_atByteSampleOffset avSpeechSynthesisMarker  range byteSampleOffset =
  sendMsg avSpeechSynthesisMarker (mkSelector "initWithParagraphRange:atByteSampleOffset:") (retPtr retVoid) [argNSRange range, argCLong (fromIntegral byteSampleOffset)] >>= ownedObject . castPtr

-- | @- initWithPhonemeString:atByteSampleOffset:@
initWithPhonemeString_atByteSampleOffset :: (IsAVSpeechSynthesisMarker avSpeechSynthesisMarker, IsNSString phoneme) => avSpeechSynthesisMarker -> phoneme -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithPhonemeString_atByteSampleOffset avSpeechSynthesisMarker  phoneme byteSampleOffset =
withObjCPtr phoneme $ \raw_phoneme ->
    sendMsg avSpeechSynthesisMarker (mkSelector "initWithPhonemeString:atByteSampleOffset:") (retPtr retVoid) [argPtr (castPtr raw_phoneme :: Ptr ()), argCLong (fromIntegral byteSampleOffset)] >>= ownedObject . castPtr

-- | @- initWithBookmarkName:atByteSampleOffset:@
initWithBookmarkName_atByteSampleOffset :: (IsAVSpeechSynthesisMarker avSpeechSynthesisMarker, IsNSString mark) => avSpeechSynthesisMarker -> mark -> CLong -> IO (Id AVSpeechSynthesisMarker)
initWithBookmarkName_atByteSampleOffset avSpeechSynthesisMarker  mark byteSampleOffset =
withObjCPtr mark $ \raw_mark ->
    sendMsg avSpeechSynthesisMarker (mkSelector "initWithBookmarkName:atByteSampleOffset:") (retPtr retVoid) [argPtr (castPtr raw_mark :: Ptr ()), argCLong (fromIntegral byteSampleOffset)] >>= ownedObject . castPtr

-- | @- mark@
mark :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO AVSpeechSynthesisMarkerMark
mark avSpeechSynthesisMarker  =
  fmap (coerce :: CLong -> AVSpeechSynthesisMarkerMark) $ sendMsg avSpeechSynthesisMarker (mkSelector "mark") retCLong []

-- | @- setMark:@
setMark :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> AVSpeechSynthesisMarkerMark -> IO ()
setMark avSpeechSynthesisMarker  value =
  sendMsg avSpeechSynthesisMarker (mkSelector "setMark:") retVoid [argCLong (coerce value)]

-- | Byte offset into the associated audio buffer
--
-- ObjC selector: @- byteSampleOffset@
byteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO CULong
byteSampleOffset avSpeechSynthesisMarker  =
  sendMsg avSpeechSynthesisMarker (mkSelector "byteSampleOffset") retCULong []

-- | Byte offset into the associated audio buffer
--
-- ObjC selector: @- setByteSampleOffset:@
setByteSampleOffset :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> CULong -> IO ()
setByteSampleOffset avSpeechSynthesisMarker  value =
  sendMsg avSpeechSynthesisMarker (mkSelector "setByteSampleOffset:") retVoid [argCULong (fromIntegral value)]

-- | The location and length of the pertaining speech request's SSML text. This marker applies to the range of characters represented by the NSString.
--
-- ObjC selector: @- textRange@
textRange :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> IO NSRange
textRange avSpeechSynthesisMarker  =
  sendMsgStret avSpeechSynthesisMarker (mkSelector "textRange") retNSRange []

-- | The location and length of the pertaining speech request's SSML text. This marker applies to the range of characters represented by the NSString.
--
-- ObjC selector: @- setTextRange:@
setTextRange :: IsAVSpeechSynthesisMarker avSpeechSynthesisMarker => avSpeechSynthesisMarker -> NSRange -> IO ()
setTextRange avSpeechSynthesisMarker  value =
  sendMsg avSpeechSynthesisMarker (mkSelector "setTextRange:") retVoid [argNSRange value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMarkerType:forTextRange:atByteSampleOffset:@
initWithMarkerType_forTextRange_atByteSampleOffsetSelector :: Selector
initWithMarkerType_forTextRange_atByteSampleOffsetSelector = mkSelector "initWithMarkerType:forTextRange:atByteSampleOffset:"

-- | @Selector@ for @initWithWordRange:atByteSampleOffset:@
initWithWordRange_atByteSampleOffsetSelector :: Selector
initWithWordRange_atByteSampleOffsetSelector = mkSelector "initWithWordRange:atByteSampleOffset:"

-- | @Selector@ for @initWithSentenceRange:atByteSampleOffset:@
initWithSentenceRange_atByteSampleOffsetSelector :: Selector
initWithSentenceRange_atByteSampleOffsetSelector = mkSelector "initWithSentenceRange:atByteSampleOffset:"

-- | @Selector@ for @initWithParagraphRange:atByteSampleOffset:@
initWithParagraphRange_atByteSampleOffsetSelector :: Selector
initWithParagraphRange_atByteSampleOffsetSelector = mkSelector "initWithParagraphRange:atByteSampleOffset:"

-- | @Selector@ for @initWithPhonemeString:atByteSampleOffset:@
initWithPhonemeString_atByteSampleOffsetSelector :: Selector
initWithPhonemeString_atByteSampleOffsetSelector = mkSelector "initWithPhonemeString:atByteSampleOffset:"

-- | @Selector@ for @initWithBookmarkName:atByteSampleOffset:@
initWithBookmarkName_atByteSampleOffsetSelector :: Selector
initWithBookmarkName_atByteSampleOffsetSelector = mkSelector "initWithBookmarkName:atByteSampleOffset:"

-- | @Selector@ for @mark@
markSelector :: Selector
markSelector = mkSelector "mark"

-- | @Selector@ for @setMark:@
setMarkSelector :: Selector
setMarkSelector = mkSelector "setMark:"

-- | @Selector@ for @byteSampleOffset@
byteSampleOffsetSelector :: Selector
byteSampleOffsetSelector = mkSelector "byteSampleOffset"

-- | @Selector@ for @setByteSampleOffset:@
setByteSampleOffsetSelector :: Selector
setByteSampleOffsetSelector = mkSelector "setByteSampleOffset:"

-- | @Selector@ for @textRange@
textRangeSelector :: Selector
textRangeSelector = mkSelector "textRange"

-- | @Selector@ for @setTextRange:@
setTextRangeSelector :: Selector
setTextRangeSelector = mkSelector "setTextRange:"

