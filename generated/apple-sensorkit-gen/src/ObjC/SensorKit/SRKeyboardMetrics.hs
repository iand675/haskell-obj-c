{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRKeyboardMetrics@.
module ObjC.SensorKit.SRKeyboardMetrics
  ( SRKeyboardMetrics
  , IsSRKeyboardMetrics(..)
  , wordCountForSentimentCategory
  , emojiCountForSentimentCategory
  , duration
  , keyboardIdentifier
  , version
  , width
  , height
  , inputModes
  , sessionIdentifiers
  , totalPauses
  , totalPathPauses
  , typingSpeed
  , pathTypingSpeed
  , totalTypingEpisodes
  , longWordUpErrorDistance
  , longWordDownErrorDistance
  , longWordTouchDownUp
  , longWordTouchDownDown
  , longWordTouchUpDown
  , deleteToDeletes
  , upErrorDistance
  , downErrorDistance
  , spaceUpErrorDistance
  , spaceDownErrorDistance
  , deleteUpErrorDistance
  , deleteDownErrorDistance
  , shortWordCharKeyUpErrorDistance
  , shortWordCharKeyDownErrorDistance
  , touchDownUp
  , spaceTouchDownUp
  , deleteTouchDownUp
  , shortWordCharKeyTouchDownUp
  , touchDownDown
  , touchUpDown
  , charKeyToPrediction
  , shortWordCharKeyToCharKey
  , charKeyToAnyTapKey
  , anyTapToCharKey
  , spaceToCharKey
  , charKeyToSpaceKey
  , spaceToDeleteKey
  , deleteToSpaceKey
  , spaceToSpaceKey
  , spaceToShiftKey
  , spaceToPlaneChangeKey
  , spaceToPredictionKey
  , deleteToCharKey
  , charKeyToDelete
  , deleteToDelete
  , deleteToShiftKey
  , deleteToPlaneChangeKey
  , anyTapToPlaneChangeKey
  , planeChangeToAnyTap
  , charKeyToPlaneChangeKey
  , planeChangeKeyToCharKey
  , pathErrorDistanceRatio
  , deleteToPath
  , pathToDelete
  , spaceToPath
  , pathToSpace
  , pathToPath
  , totalWords
  , totalAlteredWords
  , totalTaps
  , totalDrags
  , totalDeletes
  , totalEmojis
  , totalPaths
  , totalPathTime
  , totalPathLength
  , totalAutoCorrections
  , totalSpaceCorrections
  , totalRetroCorrections
  , totalTranspositionCorrections
  , totalInsertKeyCorrections
  , totalSkipTouchCorrections
  , totalNearKeyCorrections
  , totalSubstitutionCorrections
  , totalHitTestCorrections
  , totalTypingDuration
  , wordCountForSentimentCategorySelector
  , emojiCountForSentimentCategorySelector
  , durationSelector
  , keyboardIdentifierSelector
  , versionSelector
  , widthSelector
  , heightSelector
  , inputModesSelector
  , sessionIdentifiersSelector
  , totalPausesSelector
  , totalPathPausesSelector
  , typingSpeedSelector
  , pathTypingSpeedSelector
  , totalTypingEpisodesSelector
  , longWordUpErrorDistanceSelector
  , longWordDownErrorDistanceSelector
  , longWordTouchDownUpSelector
  , longWordTouchDownDownSelector
  , longWordTouchUpDownSelector
  , deleteToDeletesSelector
  , upErrorDistanceSelector
  , downErrorDistanceSelector
  , spaceUpErrorDistanceSelector
  , spaceDownErrorDistanceSelector
  , deleteUpErrorDistanceSelector
  , deleteDownErrorDistanceSelector
  , shortWordCharKeyUpErrorDistanceSelector
  , shortWordCharKeyDownErrorDistanceSelector
  , touchDownUpSelector
  , spaceTouchDownUpSelector
  , deleteTouchDownUpSelector
  , shortWordCharKeyTouchDownUpSelector
  , touchDownDownSelector
  , touchUpDownSelector
  , charKeyToPredictionSelector
  , shortWordCharKeyToCharKeySelector
  , charKeyToAnyTapKeySelector
  , anyTapToCharKeySelector
  , spaceToCharKeySelector
  , charKeyToSpaceKeySelector
  , spaceToDeleteKeySelector
  , deleteToSpaceKeySelector
  , spaceToSpaceKeySelector
  , spaceToShiftKeySelector
  , spaceToPlaneChangeKeySelector
  , spaceToPredictionKeySelector
  , deleteToCharKeySelector
  , charKeyToDeleteSelector
  , deleteToDeleteSelector
  , deleteToShiftKeySelector
  , deleteToPlaneChangeKeySelector
  , anyTapToPlaneChangeKeySelector
  , planeChangeToAnyTapSelector
  , charKeyToPlaneChangeKeySelector
  , planeChangeKeyToCharKeySelector
  , pathErrorDistanceRatioSelector
  , deleteToPathSelector
  , pathToDeleteSelector
  , spaceToPathSelector
  , pathToSpaceSelector
  , pathToPathSelector
  , totalWordsSelector
  , totalAlteredWordsSelector
  , totalTapsSelector
  , totalDragsSelector
  , totalDeletesSelector
  , totalEmojisSelector
  , totalPathsSelector
  , totalPathTimeSelector
  , totalPathLengthSelector
  , totalAutoCorrectionsSelector
  , totalSpaceCorrectionsSelector
  , totalRetroCorrectionsSelector
  , totalTranspositionCorrectionsSelector
  , totalInsertKeyCorrectionsSelector
  , totalSkipTouchCorrectionsSelector
  , totalNearKeyCorrectionsSelector
  , totalSubstitutionCorrectionsSelector
  , totalHitTestCorrectionsSelector
  , totalTypingDurationSelector

  -- * Enum types
  , SRKeyboardMetricsSentimentCategory(SRKeyboardMetricsSentimentCategory)
  , pattern SRKeyboardMetricsSentimentCategoryAbsolutist
  , pattern SRKeyboardMetricsSentimentCategoryDown
  , pattern SRKeyboardMetricsSentimentCategoryDeath
  , pattern SRKeyboardMetricsSentimentCategoryAnxiety
  , pattern SRKeyboardMetricsSentimentCategoryAnger
  , pattern SRKeyboardMetricsSentimentCategoryHealth
  , pattern SRKeyboardMetricsSentimentCategoryPositive
  , pattern SRKeyboardMetricsSentimentCategorySad
  , pattern SRKeyboardMetricsSentimentCategoryLowEnergy
  , pattern SRKeyboardMetricsSentimentCategoryConfused

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The count of words typed per category in the session
--
-- ObjC selector: @- wordCountForSentimentCategory:@
wordCountForSentimentCategory :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> SRKeyboardMetricsSentimentCategory -> IO CLong
wordCountForSentimentCategory srKeyboardMetrics  category =
    sendMsg srKeyboardMetrics (mkSelector "wordCountForSentimentCategory:") retCLong [argCLong (coerce category)]

-- | The count of emoji typed per category in the session
--
-- ObjC selector: @- emojiCountForSentimentCategory:@
emojiCountForSentimentCategory :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> SRKeyboardMetricsSentimentCategory -> IO CLong
emojiCountForSentimentCategory srKeyboardMetrics  category =
    sendMsg srKeyboardMetrics (mkSelector "emojiCountForSentimentCategory:") retCLong [argCLong (coerce category)]

-- | The duration over which these metrics were calculated
--
-- ObjC selector: @- duration@
duration :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
duration srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "duration") retCDouble []

-- | The identifier of the keyboard in the keyboard list
--
-- ObjC selector: @- keyboardIdentifier@
keyboardIdentifier :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSString)
keyboardIdentifier srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "keyboardIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of keyboard metrics
--
-- ObjC selector: @- version@
version :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSString)
version srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The width of the keyboard in mm in the session
--
-- ObjC selector: @- width@
width :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSMeasurement)
width srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The height of the keyboard in mm in the session
--
-- ObjC selector: @- height@
height :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSMeasurement)
height srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The input modes used during a keyboard session
--
-- ObjC selector: @- inputModes@
inputModes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
inputModes srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "inputModes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The keyboard session identifiers. These are the identifiers of the keyboard sessions that contributed to keyboard metrics sample to correlate current stream with another stream using the same keyboard session indentifiers
--
-- ObjC selector: @- sessionIdentifiers@
sessionIdentifiers :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
sessionIdentifiers srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "sessionIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The total number of pauses during the session
--
-- ObjC selector: @- totalPauses@
totalPauses :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalPauses srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalPauses") retCLong []

-- | The total number of pauses made while entering the path for any words composed using continuous path during the session
--
-- ObjC selector: @- totalPathPauses@
totalPathPauses :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalPathPauses srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalPathPauses") retCLong []

-- | The words per minute typed during the session
--
-- ObjC selector: @- typingSpeed@
typingSpeed :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
typingSpeed srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "typingSpeed") retCDouble []

-- | The words per minute typed using continuous path during the session
--
-- ObjC selector: @- pathTypingSpeed@
pathTypingSpeed :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
pathTypingSpeed srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "pathTypingSpeed") retCDouble []

-- | Total number of continuous typing episodes during the session
--
-- ObjC selector: @- totalTypingEpisodes@
totalTypingEpisodes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalTypingEpisodes srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalTypingEpisodes") retCLong []

-- | The distance from the touch up to the center of the intended key of the characters of a long word
--
-- ObjC selector: @- longWordUpErrorDistance@
longWordUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordUpErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "longWordUpErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch down to the center of the intended key of the characters of a long word
--
-- ObjC selector: @- longWordDownErrorDistance@
longWordDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordDownErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "longWordDownErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down and touchup of the character keys of all the long words in the session.
--
-- ObjC selector: @- longWordTouchDownUp@
longWordTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordTouchDownUp srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "longWordTouchDownUp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down and touch down of the character keys of all the long words in the session.
--
-- ObjC selector: @- longWordTouchDownDown@
longWordTouchDownDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordTouchDownDown srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "longWordTouchDownDown") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch up and touch down of the character keys of all the long words in the session.
--
-- ObjC selector: @- longWordTouchUpDown@
longWordTouchUpDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordTouchUpDown srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "longWordTouchUpDown") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of the delete key and touch down of a sequential delete key
--
-- ObjC selector: @- deleteToDeletes@
deleteToDeletes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
deleteToDeletes srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToDeletes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch up to the center of any key
--
-- ObjC selector: @- upErrorDistance@
upErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
upErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "upErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch down to the center of any key
--
-- ObjC selector: @- downErrorDistance@
downErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
downErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "downErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch up to the right centroid of the space key
--
-- ObjC selector: @- spaceUpErrorDistance@
spaceUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceUpErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceUpErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch down to the right centroid of the space key
--
-- ObjC selector: @- spaceDownErrorDistance@
spaceDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceDownErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceDownErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch up to the center of the delete key
--
-- ObjC selector: @- deleteUpErrorDistance@
deleteUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteUpErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteUpErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch down to the center of the delete key
--
-- ObjC selector: @- deleteDownErrorDistance@
deleteDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteDownErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteDownErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch up to the center of the intended key of a character in a short word
--
-- ObjC selector: @- shortWordCharKeyUpErrorDistance@
shortWordCharKeyUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyUpErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "shortWordCharKeyUpErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance from the touch down to the center of the intended key of a character in a short word
--
-- ObjC selector: @- shortWordCharKeyDownErrorDistance@
shortWordCharKeyDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyDownErrorDistance srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "shortWordCharKeyDownErrorDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down to touchup for any key
--
-- ObjC selector: @- touchDownUp@
touchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
touchDownUp srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "touchDownUp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down and touchup of all space key events in the session.
--
-- ObjC selector: @- spaceTouchDownUp@
spaceTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceTouchDownUp srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceTouchDownUp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down and touchup of all delete key events in the session.
--
-- ObjC selector: @- deleteTouchDownUp@
deleteTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteTouchDownUp srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteTouchDownUp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down and touchup of all character keys in short words in the session.
--
-- ObjC selector: @- shortWordCharKeyTouchDownUp@
shortWordCharKeyTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyTouchDownUp srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "shortWordCharKeyTouchDownUp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch down to touch down for any key
--
-- ObjC selector: @- touchDownDown@
touchDownDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
touchDownDown srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "touchDownDown") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touch up and touch down for any key
--
-- ObjC selector: @- touchUpDown@
touchUpDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
touchUpDown srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "touchUpDown") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup on a character key and touch down on a word in the prediction bar
--
-- ObjC selector: @- charKeyToPrediction@
charKeyToPrediction :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToPrediction srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "charKeyToPrediction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup on a character key and touch down on any sequential character key in a short word
--
-- ObjC selector: @- shortWordCharKeyToCharKey@
shortWordCharKeyToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyToCharKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "shortWordCharKeyToCharKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup on a character key and touch down on the next sequential key (any key)
--
-- ObjC selector: @- charKeyToAnyTapKey@
charKeyToAnyTapKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToAnyTapKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "charKeyToAnyTapKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of any key and touch down on a sequential character key
--
-- ObjC selector: @- anyTapToCharKey@
anyTapToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
anyTapToCharKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "anyTapToCharKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a space key and touch down of a sequential character key
--
-- ObjC selector: @- spaceToCharKey@
spaceToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToCharKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToCharKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a character key and touch down of a sequential space key
--
-- ObjC selector: @- charKeyToSpaceKey@
charKeyToSpaceKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToSpaceKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "charKeyToSpaceKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a space key and touch down of a sequential delete key
--
-- ObjC selector: @- spaceToDeleteKey@
spaceToDeleteKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToDeleteKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToDeleteKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential space key
--
-- ObjC selector: @- deleteToSpaceKey@
deleteToSpaceKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToSpaceKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToSpaceKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a space key and touch down of a sequential space key
--
-- ObjC selector: @- spaceToSpaceKey@
spaceToSpaceKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToSpaceKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToSpaceKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a space key and touch down of a sequential Shift key
--
-- ObjC selector: @- spaceToShiftKey@
spaceToShiftKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToShiftKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToShiftKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a space key and touch down of a sequential plane change key
--
-- ObjC selector: @- spaceToPlaneChangeKey@
spaceToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToPlaneChangeKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToPlaneChangeKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup on the space key and touch down of a sequential selection from the prediction bar
--
-- ObjC selector: @- spaceToPredictionKey@
spaceToPredictionKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToPredictionKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToPredictionKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential character key
--
-- ObjC selector: @- deleteToCharKey@
deleteToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToCharKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToCharKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a character key and touch down of a sequential delete key
--
-- ObjC selector: @- charKeyToDelete@
charKeyToDelete :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToDelete srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "charKeyToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential delete key
--
-- ObjC selector: @- deleteToDelete@
deleteToDelete :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToDelete srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential Shift key
--
-- ObjC selector: @- deleteToShiftKey@
deleteToShiftKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToShiftKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToShiftKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential plane change key
--
-- ObjC selector: @- deleteToPlaneChangeKey@
deleteToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToPlaneChangeKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToPlaneChangeKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of any key and touch down on a plane change key
--
-- ObjC selector: @- anyTapToPlaneChangeKey@
anyTapToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
anyTapToPlaneChangeKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "anyTapToPlaneChangeKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup on a plane change key and touch down on the next sequential key
--
-- ObjC selector: @- planeChangeToAnyTap@
planeChangeToAnyTap :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
planeChangeToAnyTap srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "planeChangeToAnyTap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a character key and touch down of a sequential plane change key
--
-- ObjC selector: @- charKeyToPlaneChangeKey@
charKeyToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToPlaneChangeKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "charKeyToPlaneChangeKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a plane change key and touch down of any key
--
-- ObjC selector: @- planeChangeKeyToCharKey@
planeChangeKeyToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
planeChangeKeyToCharKey srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "planeChangeKeyToCharKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sample values of the ratio of error distance between intended and actual path
--
-- ObjC selector: @- pathErrorDistanceRatio@
pathErrorDistanceRatio :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
pathErrorDistanceRatio srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "pathErrorDistanceRatio") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential path
--
-- ObjC selector: @- deleteToPath@
deleteToPath :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToPath srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "deleteToPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a delete key and touch down of a sequential path (ie. Continuous Path)
--
-- ObjC selector: @- pathToDelete@
pathToDelete :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
pathToDelete srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "pathToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup on the space key and touch down to begin a sequential path
--
-- ObjC selector: @- spaceToPath@
spaceToPath :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToPath srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "spaceToPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a path and touch down of a sequential space key
--
-- ObjC selector: @- pathToSpace@
pathToSpace :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
pathToSpace srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "pathToSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration between touchup of a path and touch down of a sequential path
--
-- ObjC selector: @- pathToPath@
pathToPath :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
pathToPath srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "pathToPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The total number of words typed during the session
--
-- ObjC selector: @- totalWords@
totalWords :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalWords srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalWords") retCLong []

-- | The total number of altered words during the session
--
-- ObjC selector: @- totalAlteredWords@
totalAlteredWords :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalAlteredWords srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalAlteredWords") retCLong []

-- | The total number of taps during the session
--
-- ObjC selector: @- totalTaps@
totalTaps :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalTaps srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalTaps") retCLong []

-- | The total number of drags during the session
--
-- ObjC selector: @- totalDrags@
totalDrags :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalDrags srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalDrags") retCLong []

-- | The total number of deletes during the session
--
-- ObjC selector: @- totalDeletes@
totalDeletes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalDeletes srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalDeletes") retCLong []

-- | The total number of emojis used during the session
--
-- ObjC selector: @- totalEmojis@
totalEmojis :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalEmojis srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalEmojis") retCLong []

-- | The total number of paths used during the sesion
--
-- ObjC selector: @- totalPaths@
totalPaths :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalPaths srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalPaths") retCLong []

-- | The total time taken to complete paths in the session
--
-- ObjC selector: @- totalPathTime@
totalPathTime :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
totalPathTime srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalPathTime") retCDouble []

-- | The total length of paths completed in the session
--
-- ObjC selector: @- totalPathLength@
totalPathLength :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSMeasurement)
totalPathLength srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalPathLength") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The total number of autocorrections in the session
--
-- ObjC selector: @- totalAutoCorrections@
totalAutoCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalAutoCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalAutoCorrections") retCLong []

-- | The total number of space corrections in the session
--
-- ObjC selector: @- totalSpaceCorrections@
totalSpaceCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalSpaceCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalSpaceCorrections") retCLong []

-- | The total number of retro corrections in the session
--
-- ObjC selector: @- totalRetroCorrections@
totalRetroCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalRetroCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalRetroCorrections") retCLong []

-- | The total number of transposition corrections in the session
--
-- ObjC selector: @- totalTranspositionCorrections@
totalTranspositionCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalTranspositionCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalTranspositionCorrections") retCLong []

-- | The total number of insert key corrections in the session
--
-- ObjC selector: @- totalInsertKeyCorrections@
totalInsertKeyCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalInsertKeyCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalInsertKeyCorrections") retCLong []

-- | The total number of skip touch corrections in the session
--
-- ObjC selector: @- totalSkipTouchCorrections@
totalSkipTouchCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalSkipTouchCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalSkipTouchCorrections") retCLong []

-- | The total number of near key corrections in the session
--
-- ObjC selector: @- totalNearKeyCorrections@
totalNearKeyCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalNearKeyCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalNearKeyCorrections") retCLong []

-- | The total number of substitution corrections in the session
--
-- ObjC selector: @- totalSubstitutionCorrections@
totalSubstitutionCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalSubstitutionCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalSubstitutionCorrections") retCLong []

-- | The total number of hit test corrections in the session
--
-- ObjC selector: @- totalHitTestCorrections@
totalHitTestCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalHitTestCorrections srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalHitTestCorrections") retCLong []

-- | The total amount of time typing during the session
--
-- ObjC selector: @- totalTypingDuration@
totalTypingDuration :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
totalTypingDuration srKeyboardMetrics  =
    sendMsg srKeyboardMetrics (mkSelector "totalTypingDuration") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wordCountForSentimentCategory:@
wordCountForSentimentCategorySelector :: Selector
wordCountForSentimentCategorySelector = mkSelector "wordCountForSentimentCategory:"

-- | @Selector@ for @emojiCountForSentimentCategory:@
emojiCountForSentimentCategorySelector :: Selector
emojiCountForSentimentCategorySelector = mkSelector "emojiCountForSentimentCategory:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @keyboardIdentifier@
keyboardIdentifierSelector :: Selector
keyboardIdentifierSelector = mkSelector "keyboardIdentifier"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @inputModes@
inputModesSelector :: Selector
inputModesSelector = mkSelector "inputModes"

-- | @Selector@ for @sessionIdentifiers@
sessionIdentifiersSelector :: Selector
sessionIdentifiersSelector = mkSelector "sessionIdentifiers"

-- | @Selector@ for @totalPauses@
totalPausesSelector :: Selector
totalPausesSelector = mkSelector "totalPauses"

-- | @Selector@ for @totalPathPauses@
totalPathPausesSelector :: Selector
totalPathPausesSelector = mkSelector "totalPathPauses"

-- | @Selector@ for @typingSpeed@
typingSpeedSelector :: Selector
typingSpeedSelector = mkSelector "typingSpeed"

-- | @Selector@ for @pathTypingSpeed@
pathTypingSpeedSelector :: Selector
pathTypingSpeedSelector = mkSelector "pathTypingSpeed"

-- | @Selector@ for @totalTypingEpisodes@
totalTypingEpisodesSelector :: Selector
totalTypingEpisodesSelector = mkSelector "totalTypingEpisodes"

-- | @Selector@ for @longWordUpErrorDistance@
longWordUpErrorDistanceSelector :: Selector
longWordUpErrorDistanceSelector = mkSelector "longWordUpErrorDistance"

-- | @Selector@ for @longWordDownErrorDistance@
longWordDownErrorDistanceSelector :: Selector
longWordDownErrorDistanceSelector = mkSelector "longWordDownErrorDistance"

-- | @Selector@ for @longWordTouchDownUp@
longWordTouchDownUpSelector :: Selector
longWordTouchDownUpSelector = mkSelector "longWordTouchDownUp"

-- | @Selector@ for @longWordTouchDownDown@
longWordTouchDownDownSelector :: Selector
longWordTouchDownDownSelector = mkSelector "longWordTouchDownDown"

-- | @Selector@ for @longWordTouchUpDown@
longWordTouchUpDownSelector :: Selector
longWordTouchUpDownSelector = mkSelector "longWordTouchUpDown"

-- | @Selector@ for @deleteToDeletes@
deleteToDeletesSelector :: Selector
deleteToDeletesSelector = mkSelector "deleteToDeletes"

-- | @Selector@ for @upErrorDistance@
upErrorDistanceSelector :: Selector
upErrorDistanceSelector = mkSelector "upErrorDistance"

-- | @Selector@ for @downErrorDistance@
downErrorDistanceSelector :: Selector
downErrorDistanceSelector = mkSelector "downErrorDistance"

-- | @Selector@ for @spaceUpErrorDistance@
spaceUpErrorDistanceSelector :: Selector
spaceUpErrorDistanceSelector = mkSelector "spaceUpErrorDistance"

-- | @Selector@ for @spaceDownErrorDistance@
spaceDownErrorDistanceSelector :: Selector
spaceDownErrorDistanceSelector = mkSelector "spaceDownErrorDistance"

-- | @Selector@ for @deleteUpErrorDistance@
deleteUpErrorDistanceSelector :: Selector
deleteUpErrorDistanceSelector = mkSelector "deleteUpErrorDistance"

-- | @Selector@ for @deleteDownErrorDistance@
deleteDownErrorDistanceSelector :: Selector
deleteDownErrorDistanceSelector = mkSelector "deleteDownErrorDistance"

-- | @Selector@ for @shortWordCharKeyUpErrorDistance@
shortWordCharKeyUpErrorDistanceSelector :: Selector
shortWordCharKeyUpErrorDistanceSelector = mkSelector "shortWordCharKeyUpErrorDistance"

-- | @Selector@ for @shortWordCharKeyDownErrorDistance@
shortWordCharKeyDownErrorDistanceSelector :: Selector
shortWordCharKeyDownErrorDistanceSelector = mkSelector "shortWordCharKeyDownErrorDistance"

-- | @Selector@ for @touchDownUp@
touchDownUpSelector :: Selector
touchDownUpSelector = mkSelector "touchDownUp"

-- | @Selector@ for @spaceTouchDownUp@
spaceTouchDownUpSelector :: Selector
spaceTouchDownUpSelector = mkSelector "spaceTouchDownUp"

-- | @Selector@ for @deleteTouchDownUp@
deleteTouchDownUpSelector :: Selector
deleteTouchDownUpSelector = mkSelector "deleteTouchDownUp"

-- | @Selector@ for @shortWordCharKeyTouchDownUp@
shortWordCharKeyTouchDownUpSelector :: Selector
shortWordCharKeyTouchDownUpSelector = mkSelector "shortWordCharKeyTouchDownUp"

-- | @Selector@ for @touchDownDown@
touchDownDownSelector :: Selector
touchDownDownSelector = mkSelector "touchDownDown"

-- | @Selector@ for @touchUpDown@
touchUpDownSelector :: Selector
touchUpDownSelector = mkSelector "touchUpDown"

-- | @Selector@ for @charKeyToPrediction@
charKeyToPredictionSelector :: Selector
charKeyToPredictionSelector = mkSelector "charKeyToPrediction"

-- | @Selector@ for @shortWordCharKeyToCharKey@
shortWordCharKeyToCharKeySelector :: Selector
shortWordCharKeyToCharKeySelector = mkSelector "shortWordCharKeyToCharKey"

-- | @Selector@ for @charKeyToAnyTapKey@
charKeyToAnyTapKeySelector :: Selector
charKeyToAnyTapKeySelector = mkSelector "charKeyToAnyTapKey"

-- | @Selector@ for @anyTapToCharKey@
anyTapToCharKeySelector :: Selector
anyTapToCharKeySelector = mkSelector "anyTapToCharKey"

-- | @Selector@ for @spaceToCharKey@
spaceToCharKeySelector :: Selector
spaceToCharKeySelector = mkSelector "spaceToCharKey"

-- | @Selector@ for @charKeyToSpaceKey@
charKeyToSpaceKeySelector :: Selector
charKeyToSpaceKeySelector = mkSelector "charKeyToSpaceKey"

-- | @Selector@ for @spaceToDeleteKey@
spaceToDeleteKeySelector :: Selector
spaceToDeleteKeySelector = mkSelector "spaceToDeleteKey"

-- | @Selector@ for @deleteToSpaceKey@
deleteToSpaceKeySelector :: Selector
deleteToSpaceKeySelector = mkSelector "deleteToSpaceKey"

-- | @Selector@ for @spaceToSpaceKey@
spaceToSpaceKeySelector :: Selector
spaceToSpaceKeySelector = mkSelector "spaceToSpaceKey"

-- | @Selector@ for @spaceToShiftKey@
spaceToShiftKeySelector :: Selector
spaceToShiftKeySelector = mkSelector "spaceToShiftKey"

-- | @Selector@ for @spaceToPlaneChangeKey@
spaceToPlaneChangeKeySelector :: Selector
spaceToPlaneChangeKeySelector = mkSelector "spaceToPlaneChangeKey"

-- | @Selector@ for @spaceToPredictionKey@
spaceToPredictionKeySelector :: Selector
spaceToPredictionKeySelector = mkSelector "spaceToPredictionKey"

-- | @Selector@ for @deleteToCharKey@
deleteToCharKeySelector :: Selector
deleteToCharKeySelector = mkSelector "deleteToCharKey"

-- | @Selector@ for @charKeyToDelete@
charKeyToDeleteSelector :: Selector
charKeyToDeleteSelector = mkSelector "charKeyToDelete"

-- | @Selector@ for @deleteToDelete@
deleteToDeleteSelector :: Selector
deleteToDeleteSelector = mkSelector "deleteToDelete"

-- | @Selector@ for @deleteToShiftKey@
deleteToShiftKeySelector :: Selector
deleteToShiftKeySelector = mkSelector "deleteToShiftKey"

-- | @Selector@ for @deleteToPlaneChangeKey@
deleteToPlaneChangeKeySelector :: Selector
deleteToPlaneChangeKeySelector = mkSelector "deleteToPlaneChangeKey"

-- | @Selector@ for @anyTapToPlaneChangeKey@
anyTapToPlaneChangeKeySelector :: Selector
anyTapToPlaneChangeKeySelector = mkSelector "anyTapToPlaneChangeKey"

-- | @Selector@ for @planeChangeToAnyTap@
planeChangeToAnyTapSelector :: Selector
planeChangeToAnyTapSelector = mkSelector "planeChangeToAnyTap"

-- | @Selector@ for @charKeyToPlaneChangeKey@
charKeyToPlaneChangeKeySelector :: Selector
charKeyToPlaneChangeKeySelector = mkSelector "charKeyToPlaneChangeKey"

-- | @Selector@ for @planeChangeKeyToCharKey@
planeChangeKeyToCharKeySelector :: Selector
planeChangeKeyToCharKeySelector = mkSelector "planeChangeKeyToCharKey"

-- | @Selector@ for @pathErrorDistanceRatio@
pathErrorDistanceRatioSelector :: Selector
pathErrorDistanceRatioSelector = mkSelector "pathErrorDistanceRatio"

-- | @Selector@ for @deleteToPath@
deleteToPathSelector :: Selector
deleteToPathSelector = mkSelector "deleteToPath"

-- | @Selector@ for @pathToDelete@
pathToDeleteSelector :: Selector
pathToDeleteSelector = mkSelector "pathToDelete"

-- | @Selector@ for @spaceToPath@
spaceToPathSelector :: Selector
spaceToPathSelector = mkSelector "spaceToPath"

-- | @Selector@ for @pathToSpace@
pathToSpaceSelector :: Selector
pathToSpaceSelector = mkSelector "pathToSpace"

-- | @Selector@ for @pathToPath@
pathToPathSelector :: Selector
pathToPathSelector = mkSelector "pathToPath"

-- | @Selector@ for @totalWords@
totalWordsSelector :: Selector
totalWordsSelector = mkSelector "totalWords"

-- | @Selector@ for @totalAlteredWords@
totalAlteredWordsSelector :: Selector
totalAlteredWordsSelector = mkSelector "totalAlteredWords"

-- | @Selector@ for @totalTaps@
totalTapsSelector :: Selector
totalTapsSelector = mkSelector "totalTaps"

-- | @Selector@ for @totalDrags@
totalDragsSelector :: Selector
totalDragsSelector = mkSelector "totalDrags"

-- | @Selector@ for @totalDeletes@
totalDeletesSelector :: Selector
totalDeletesSelector = mkSelector "totalDeletes"

-- | @Selector@ for @totalEmojis@
totalEmojisSelector :: Selector
totalEmojisSelector = mkSelector "totalEmojis"

-- | @Selector@ for @totalPaths@
totalPathsSelector :: Selector
totalPathsSelector = mkSelector "totalPaths"

-- | @Selector@ for @totalPathTime@
totalPathTimeSelector :: Selector
totalPathTimeSelector = mkSelector "totalPathTime"

-- | @Selector@ for @totalPathLength@
totalPathLengthSelector :: Selector
totalPathLengthSelector = mkSelector "totalPathLength"

-- | @Selector@ for @totalAutoCorrections@
totalAutoCorrectionsSelector :: Selector
totalAutoCorrectionsSelector = mkSelector "totalAutoCorrections"

-- | @Selector@ for @totalSpaceCorrections@
totalSpaceCorrectionsSelector :: Selector
totalSpaceCorrectionsSelector = mkSelector "totalSpaceCorrections"

-- | @Selector@ for @totalRetroCorrections@
totalRetroCorrectionsSelector :: Selector
totalRetroCorrectionsSelector = mkSelector "totalRetroCorrections"

-- | @Selector@ for @totalTranspositionCorrections@
totalTranspositionCorrectionsSelector :: Selector
totalTranspositionCorrectionsSelector = mkSelector "totalTranspositionCorrections"

-- | @Selector@ for @totalInsertKeyCorrections@
totalInsertKeyCorrectionsSelector :: Selector
totalInsertKeyCorrectionsSelector = mkSelector "totalInsertKeyCorrections"

-- | @Selector@ for @totalSkipTouchCorrections@
totalSkipTouchCorrectionsSelector :: Selector
totalSkipTouchCorrectionsSelector = mkSelector "totalSkipTouchCorrections"

-- | @Selector@ for @totalNearKeyCorrections@
totalNearKeyCorrectionsSelector :: Selector
totalNearKeyCorrectionsSelector = mkSelector "totalNearKeyCorrections"

-- | @Selector@ for @totalSubstitutionCorrections@
totalSubstitutionCorrectionsSelector :: Selector
totalSubstitutionCorrectionsSelector = mkSelector "totalSubstitutionCorrections"

-- | @Selector@ for @totalHitTestCorrections@
totalHitTestCorrectionsSelector :: Selector
totalHitTestCorrectionsSelector = mkSelector "totalHitTestCorrections"

-- | @Selector@ for @totalTypingDuration@
totalTypingDurationSelector :: Selector
totalTypingDurationSelector = mkSelector "totalTypingDuration"

