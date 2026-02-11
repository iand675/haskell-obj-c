{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A discrete part of an entire transcription, as identified by the speech recognizer.
--
-- Use ``SFTranscriptionSegment`` to get details about a part of an overall ``SFTranscription``. An ``SFTranscriptionSegment`` represents an utterance, which is a vocalized word or group of words that represent a single meaning to the speech recognizer (``SFSpeechRecognizer``).
--
-- You don't create transcription object segments directly. Instead, you access them from a transcription's ``SFTranscription/segments`` property.
--
-- A transcription segment includes the following information:
--
-- - The text of the utterance, plus any alternative interpretations of the spoken word. - The character range of the segment within the ``SFTranscription/formattedString`` of its parent ``SFTranscription``. - A ``confidence`` value, indicating how likely it is that the specified string matches the audible speech. - A ``timestamp`` and ``duration`` value, indicating the position of the segment within the provided audio stream.
--
-- Generated bindings for @SFTranscriptionSegment@.
module ObjC.Speech.SFTranscriptionSegment
  ( SFTranscriptionSegment
  , IsSFTranscriptionSegment(..)
  , substring
  , substringRange
  , timestamp
  , duration
  , confidence
  , alternativeSubstrings
  , substringSelector
  , substringRangeSelector
  , timestampSelector
  , durationSelector
  , confidenceSelector
  , alternativeSubstringsSelector


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

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | The string representation of the utterance in the transcription segment.
--
-- ObjC selector: @- substring@
substring :: IsSFTranscriptionSegment sfTranscriptionSegment => sfTranscriptionSegment -> IO (Id NSString)
substring sfTranscriptionSegment  =
  sendMsg sfTranscriptionSegment (mkSelector "substring") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The range information for the transcription segment's substring, relative to the overall transcription.
--
-- Use the range information to find the position of the segment within the ``SFTranscription/formattedString`` property of the ``SFTranscription`` object containing this segment.
--
-- ObjC selector: @- substringRange@
substringRange :: IsSFTranscriptionSegment sfTranscriptionSegment => sfTranscriptionSegment -> IO NSRange
substringRange sfTranscriptionSegment  =
  sendMsgStret sfTranscriptionSegment (mkSelector "substringRange") retNSRange []

-- | The start time of the segment in the processed audio stream.
--
-- The ``timestamp`` is the number of seconds between the beginning of the audio content and when the user spoke the word represented by the segment. For example, if the user said the word "time" one second into the transcription "What time is it", the timestamp would be equal to @1.0@.
--
-- ObjC selector: @- timestamp@
timestamp :: IsSFTranscriptionSegment sfTranscriptionSegment => sfTranscriptionSegment -> IO CDouble
timestamp sfTranscriptionSegment  =
  sendMsg sfTranscriptionSegment (mkSelector "timestamp") retCDouble []

-- | The number of seconds it took for the user to speak the utterance represented by the segment.
--
-- The ``duration`` contains the number of seconds it took for the user to speak the one or more words (utterance) represented by the segment. For example, the ``SFSpeechRecognizer`` sets ``duration`` to @0.6@ if the user took @0.6@ seconds to say @“time”@ in the transcription of @“What time is it?"@.
--
-- ObjC selector: @- duration@
duration :: IsSFTranscriptionSegment sfTranscriptionSegment => sfTranscriptionSegment -> IO CDouble
duration sfTranscriptionSegment  =
  sendMsg sfTranscriptionSegment (mkSelector "duration") retCDouble []

-- | The level of confidence the speech recognizer has in its recognition of the speech transcribed for the segment.
--
-- This property reflects the overall confidence in the recognition of the entire phrase. The value is @0@ if there was no recognition, and it is closer to @1@ when there is a high certainty that a transcription matches the user's speech exactly. For example, a confidence value of @0.94@ represents a very high confidence level, and is more likely to be correct than a transcription with a confidence value of @0.72@.
--
-- ObjC selector: @- confidence@
confidence :: IsSFTranscriptionSegment sfTranscriptionSegment => sfTranscriptionSegment -> IO CFloat
confidence sfTranscriptionSegment  =
  sendMsg sfTranscriptionSegment (mkSelector "confidence") retCFloat []

-- | An array of alternate interpretations of the utterance in the transcription segment.
--
-- ObjC selector: @- alternativeSubstrings@
alternativeSubstrings :: IsSFTranscriptionSegment sfTranscriptionSegment => sfTranscriptionSegment -> IO (Id NSArray)
alternativeSubstrings sfTranscriptionSegment  =
  sendMsg sfTranscriptionSegment (mkSelector "alternativeSubstrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @substring@
substringSelector :: Selector
substringSelector = mkSelector "substring"

-- | @Selector@ for @substringRange@
substringRangeSelector :: Selector
substringRangeSelector = mkSelector "substringRange"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @alternativeSubstrings@
alternativeSubstringsSelector :: Selector
alternativeSubstringsSelector = mkSelector "alternativeSubstrings"

