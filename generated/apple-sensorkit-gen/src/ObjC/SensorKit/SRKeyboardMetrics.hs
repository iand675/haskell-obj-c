{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , anyTapToCharKeySelector
  , anyTapToPlaneChangeKeySelector
  , charKeyToAnyTapKeySelector
  , charKeyToDeleteSelector
  , charKeyToPlaneChangeKeySelector
  , charKeyToPredictionSelector
  , charKeyToSpaceKeySelector
  , deleteDownErrorDistanceSelector
  , deleteToCharKeySelector
  , deleteToDeleteSelector
  , deleteToDeletesSelector
  , deleteToPathSelector
  , deleteToPlaneChangeKeySelector
  , deleteToShiftKeySelector
  , deleteToSpaceKeySelector
  , deleteTouchDownUpSelector
  , deleteUpErrorDistanceSelector
  , downErrorDistanceSelector
  , durationSelector
  , emojiCountForSentimentCategorySelector
  , heightSelector
  , inputModesSelector
  , keyboardIdentifierSelector
  , longWordDownErrorDistanceSelector
  , longWordTouchDownDownSelector
  , longWordTouchDownUpSelector
  , longWordTouchUpDownSelector
  , longWordUpErrorDistanceSelector
  , pathErrorDistanceRatioSelector
  , pathToDeleteSelector
  , pathToPathSelector
  , pathToSpaceSelector
  , pathTypingSpeedSelector
  , planeChangeKeyToCharKeySelector
  , planeChangeToAnyTapSelector
  , sessionIdentifiersSelector
  , shortWordCharKeyDownErrorDistanceSelector
  , shortWordCharKeyToCharKeySelector
  , shortWordCharKeyTouchDownUpSelector
  , shortWordCharKeyUpErrorDistanceSelector
  , spaceDownErrorDistanceSelector
  , spaceToCharKeySelector
  , spaceToDeleteKeySelector
  , spaceToPathSelector
  , spaceToPlaneChangeKeySelector
  , spaceToPredictionKeySelector
  , spaceToShiftKeySelector
  , spaceToSpaceKeySelector
  , spaceTouchDownUpSelector
  , spaceUpErrorDistanceSelector
  , totalAlteredWordsSelector
  , totalAutoCorrectionsSelector
  , totalDeletesSelector
  , totalDragsSelector
  , totalEmojisSelector
  , totalHitTestCorrectionsSelector
  , totalInsertKeyCorrectionsSelector
  , totalNearKeyCorrectionsSelector
  , totalPathLengthSelector
  , totalPathPausesSelector
  , totalPathTimeSelector
  , totalPathsSelector
  , totalPausesSelector
  , totalRetroCorrectionsSelector
  , totalSkipTouchCorrectionsSelector
  , totalSpaceCorrectionsSelector
  , totalSubstitutionCorrectionsSelector
  , totalTapsSelector
  , totalTranspositionCorrectionsSelector
  , totalTypingDurationSelector
  , totalTypingEpisodesSelector
  , totalWordsSelector
  , touchDownDownSelector
  , touchDownUpSelector
  , touchUpDownSelector
  , typingSpeedSelector
  , upErrorDistanceSelector
  , versionSelector
  , widthSelector
  , wordCountForSentimentCategorySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The count of words typed per category in the session
--
-- ObjC selector: @- wordCountForSentimentCategory:@
wordCountForSentimentCategory :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> SRKeyboardMetricsSentimentCategory -> IO CLong
wordCountForSentimentCategory srKeyboardMetrics category =
  sendMessage srKeyboardMetrics wordCountForSentimentCategorySelector category

-- | The count of emoji typed per category in the session
--
-- ObjC selector: @- emojiCountForSentimentCategory:@
emojiCountForSentimentCategory :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> SRKeyboardMetricsSentimentCategory -> IO CLong
emojiCountForSentimentCategory srKeyboardMetrics category =
  sendMessage srKeyboardMetrics emojiCountForSentimentCategorySelector category

-- | The duration over which these metrics were calculated
--
-- ObjC selector: @- duration@
duration :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
duration srKeyboardMetrics =
  sendMessage srKeyboardMetrics durationSelector

-- | The identifier of the keyboard in the keyboard list
--
-- ObjC selector: @- keyboardIdentifier@
keyboardIdentifier :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSString)
keyboardIdentifier srKeyboardMetrics =
  sendMessage srKeyboardMetrics keyboardIdentifierSelector

-- | The version of keyboard metrics
--
-- ObjC selector: @- version@
version :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSString)
version srKeyboardMetrics =
  sendMessage srKeyboardMetrics versionSelector

-- | The width of the keyboard in mm in the session
--
-- ObjC selector: @- width@
width :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSMeasurement)
width srKeyboardMetrics =
  sendMessage srKeyboardMetrics widthSelector

-- | The height of the keyboard in mm in the session
--
-- ObjC selector: @- height@
height :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSMeasurement)
height srKeyboardMetrics =
  sendMessage srKeyboardMetrics heightSelector

-- | The input modes used during a keyboard session
--
-- ObjC selector: @- inputModes@
inputModes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
inputModes srKeyboardMetrics =
  sendMessage srKeyboardMetrics inputModesSelector

-- | The keyboard session identifiers. These are the identifiers of the keyboard sessions that contributed to keyboard metrics sample to correlate current stream with another stream using the same keyboard session indentifiers
--
-- ObjC selector: @- sessionIdentifiers@
sessionIdentifiers :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
sessionIdentifiers srKeyboardMetrics =
  sendMessage srKeyboardMetrics sessionIdentifiersSelector

-- | The total number of pauses during the session
--
-- ObjC selector: @- totalPauses@
totalPauses :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalPauses srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalPausesSelector

-- | The total number of pauses made while entering the path for any words composed using continuous path during the session
--
-- ObjC selector: @- totalPathPauses@
totalPathPauses :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalPathPauses srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalPathPausesSelector

-- | The words per minute typed during the session
--
-- ObjC selector: @- typingSpeed@
typingSpeed :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
typingSpeed srKeyboardMetrics =
  sendMessage srKeyboardMetrics typingSpeedSelector

-- | The words per minute typed using continuous path during the session
--
-- ObjC selector: @- pathTypingSpeed@
pathTypingSpeed :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
pathTypingSpeed srKeyboardMetrics =
  sendMessage srKeyboardMetrics pathTypingSpeedSelector

-- | Total number of continuous typing episodes during the session
--
-- ObjC selector: @- totalTypingEpisodes@
totalTypingEpisodes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalTypingEpisodes srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalTypingEpisodesSelector

-- | The distance from the touch up to the center of the intended key of the characters of a long word
--
-- ObjC selector: @- longWordUpErrorDistance@
longWordUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordUpErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics longWordUpErrorDistanceSelector

-- | The distance from the touch down to the center of the intended key of the characters of a long word
--
-- ObjC selector: @- longWordDownErrorDistance@
longWordDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordDownErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics longWordDownErrorDistanceSelector

-- | The duration between touch down and touchup of the character keys of all the long words in the session.
--
-- ObjC selector: @- longWordTouchDownUp@
longWordTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordTouchDownUp srKeyboardMetrics =
  sendMessage srKeyboardMetrics longWordTouchDownUpSelector

-- | The duration between touch down and touch down of the character keys of all the long words in the session.
--
-- ObjC selector: @- longWordTouchDownDown@
longWordTouchDownDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordTouchDownDown srKeyboardMetrics =
  sendMessage srKeyboardMetrics longWordTouchDownDownSelector

-- | The duration between touch up and touch down of the character keys of all the long words in the session.
--
-- ObjC selector: @- longWordTouchUpDown@
longWordTouchUpDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
longWordTouchUpDown srKeyboardMetrics =
  sendMessage srKeyboardMetrics longWordTouchUpDownSelector

-- | The duration between touchup of the delete key and touch down of a sequential delete key
--
-- ObjC selector: @- deleteToDeletes@
deleteToDeletes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
deleteToDeletes srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToDeletesSelector

-- | The distance from the touch up to the center of any key
--
-- ObjC selector: @- upErrorDistance@
upErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
upErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics upErrorDistanceSelector

-- | The distance from the touch down to the center of any key
--
-- ObjC selector: @- downErrorDistance@
downErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
downErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics downErrorDistanceSelector

-- | The distance from the touch up to the right centroid of the space key
--
-- ObjC selector: @- spaceUpErrorDistance@
spaceUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceUpErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceUpErrorDistanceSelector

-- | The distance from the touch down to the right centroid of the space key
--
-- ObjC selector: @- spaceDownErrorDistance@
spaceDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceDownErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceDownErrorDistanceSelector

-- | The distance from the touch up to the center of the delete key
--
-- ObjC selector: @- deleteUpErrorDistance@
deleteUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteUpErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteUpErrorDistanceSelector

-- | The distance from the touch down to the center of the delete key
--
-- ObjC selector: @- deleteDownErrorDistance@
deleteDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteDownErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteDownErrorDistanceSelector

-- | The distance from the touch up to the center of the intended key of a character in a short word
--
-- ObjC selector: @- shortWordCharKeyUpErrorDistance@
shortWordCharKeyUpErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyUpErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics shortWordCharKeyUpErrorDistanceSelector

-- | The distance from the touch down to the center of the intended key of a character in a short word
--
-- ObjC selector: @- shortWordCharKeyDownErrorDistance@
shortWordCharKeyDownErrorDistance :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyDownErrorDistance srKeyboardMetrics =
  sendMessage srKeyboardMetrics shortWordCharKeyDownErrorDistanceSelector

-- | The duration between touch down to touchup for any key
--
-- ObjC selector: @- touchDownUp@
touchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
touchDownUp srKeyboardMetrics =
  sendMessage srKeyboardMetrics touchDownUpSelector

-- | The duration between touch down and touchup of all space key events in the session.
--
-- ObjC selector: @- spaceTouchDownUp@
spaceTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceTouchDownUp srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceTouchDownUpSelector

-- | The duration between touch down and touchup of all delete key events in the session.
--
-- ObjC selector: @- deleteTouchDownUp@
deleteTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteTouchDownUp srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteTouchDownUpSelector

-- | The duration between touch down and touchup of all character keys in short words in the session.
--
-- ObjC selector: @- shortWordCharKeyTouchDownUp@
shortWordCharKeyTouchDownUp :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyTouchDownUp srKeyboardMetrics =
  sendMessage srKeyboardMetrics shortWordCharKeyTouchDownUpSelector

-- | The duration between touch down to touch down for any key
--
-- ObjC selector: @- touchDownDown@
touchDownDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
touchDownDown srKeyboardMetrics =
  sendMessage srKeyboardMetrics touchDownDownSelector

-- | The duration between touch up and touch down for any key
--
-- ObjC selector: @- touchUpDown@
touchUpDown :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
touchUpDown srKeyboardMetrics =
  sendMessage srKeyboardMetrics touchUpDownSelector

-- | The duration between touchup on a character key and touch down on a word in the prediction bar
--
-- ObjC selector: @- charKeyToPrediction@
charKeyToPrediction :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToPrediction srKeyboardMetrics =
  sendMessage srKeyboardMetrics charKeyToPredictionSelector

-- | The duration between touchup on a character key and touch down on any sequential character key in a short word
--
-- ObjC selector: @- shortWordCharKeyToCharKey@
shortWordCharKeyToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
shortWordCharKeyToCharKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics shortWordCharKeyToCharKeySelector

-- | The duration between touchup on a character key and touch down on the next sequential key (any key)
--
-- ObjC selector: @- charKeyToAnyTapKey@
charKeyToAnyTapKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToAnyTapKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics charKeyToAnyTapKeySelector

-- | The duration between touchup of any key and touch down on a sequential character key
--
-- ObjC selector: @- anyTapToCharKey@
anyTapToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
anyTapToCharKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics anyTapToCharKeySelector

-- | The duration between touchup of a space key and touch down of a sequential character key
--
-- ObjC selector: @- spaceToCharKey@
spaceToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToCharKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToCharKeySelector

-- | The duration between touchup of a character key and touch down of a sequential space key
--
-- ObjC selector: @- charKeyToSpaceKey@
charKeyToSpaceKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToSpaceKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics charKeyToSpaceKeySelector

-- | The duration between touchup of a space key and touch down of a sequential delete key
--
-- ObjC selector: @- spaceToDeleteKey@
spaceToDeleteKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToDeleteKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToDeleteKeySelector

-- | The duration between touchup of a delete key and touch down of a sequential space key
--
-- ObjC selector: @- deleteToSpaceKey@
deleteToSpaceKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToSpaceKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToSpaceKeySelector

-- | The duration between touchup of a space key and touch down of a sequential space key
--
-- ObjC selector: @- spaceToSpaceKey@
spaceToSpaceKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToSpaceKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToSpaceKeySelector

-- | The duration between touchup of a space key and touch down of a sequential Shift key
--
-- ObjC selector: @- spaceToShiftKey@
spaceToShiftKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToShiftKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToShiftKeySelector

-- | The duration between touchup of a space key and touch down of a sequential plane change key
--
-- ObjC selector: @- spaceToPlaneChangeKey@
spaceToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToPlaneChangeKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToPlaneChangeKeySelector

-- | The duration between touchup on the space key and touch down of a sequential selection from the prediction bar
--
-- ObjC selector: @- spaceToPredictionKey@
spaceToPredictionKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToPredictionKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToPredictionKeySelector

-- | The duration between touchup of a delete key and touch down of a sequential character key
--
-- ObjC selector: @- deleteToCharKey@
deleteToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToCharKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToCharKeySelector

-- | The duration between touchup of a character key and touch down of a sequential delete key
--
-- ObjC selector: @- charKeyToDelete@
charKeyToDelete :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToDelete srKeyboardMetrics =
  sendMessage srKeyboardMetrics charKeyToDeleteSelector

-- | The duration between touchup of a delete key and touch down of a sequential delete key
--
-- ObjC selector: @- deleteToDelete@
deleteToDelete :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToDelete srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToDeleteSelector

-- | The duration between touchup of a delete key and touch down of a sequential Shift key
--
-- ObjC selector: @- deleteToShiftKey@
deleteToShiftKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToShiftKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToShiftKeySelector

-- | The duration between touchup of a delete key and touch down of a sequential plane change key
--
-- ObjC selector: @- deleteToPlaneChangeKey@
deleteToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToPlaneChangeKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToPlaneChangeKeySelector

-- | The duration between touchup of any key and touch down on a plane change key
--
-- ObjC selector: @- anyTapToPlaneChangeKey@
anyTapToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
anyTapToPlaneChangeKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics anyTapToPlaneChangeKeySelector

-- | The duration between touchup on a plane change key and touch down on the next sequential key
--
-- ObjC selector: @- planeChangeToAnyTap@
planeChangeToAnyTap :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
planeChangeToAnyTap srKeyboardMetrics =
  sendMessage srKeyboardMetrics planeChangeToAnyTapSelector

-- | The duration between touchup of a character key and touch down of a sequential plane change key
--
-- ObjC selector: @- charKeyToPlaneChangeKey@
charKeyToPlaneChangeKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
charKeyToPlaneChangeKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics charKeyToPlaneChangeKeySelector

-- | The duration between touchup of a plane change key and touch down of any key
--
-- ObjC selector: @- planeChangeKeyToCharKey@
planeChangeKeyToCharKey :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
planeChangeKeyToCharKey srKeyboardMetrics =
  sendMessage srKeyboardMetrics planeChangeKeyToCharKeySelector

-- | sample values of the ratio of error distance between intended and actual path
--
-- ObjC selector: @- pathErrorDistanceRatio@
pathErrorDistanceRatio :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSArray)
pathErrorDistanceRatio srKeyboardMetrics =
  sendMessage srKeyboardMetrics pathErrorDistanceRatioSelector

-- | The duration between touchup of a delete key and touch down of a sequential path
--
-- ObjC selector: @- deleteToPath@
deleteToPath :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
deleteToPath srKeyboardMetrics =
  sendMessage srKeyboardMetrics deleteToPathSelector

-- | The duration between touchup of a delete key and touch down of a sequential path (ie. Continuous Path)
--
-- ObjC selector: @- pathToDelete@
pathToDelete :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
pathToDelete srKeyboardMetrics =
  sendMessage srKeyboardMetrics pathToDeleteSelector

-- | The duration between touchup on the space key and touch down to begin a sequential path
--
-- ObjC selector: @- spaceToPath@
spaceToPath :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
spaceToPath srKeyboardMetrics =
  sendMessage srKeyboardMetrics spaceToPathSelector

-- | The duration between touchup of a path and touch down of a sequential space key
--
-- ObjC selector: @- pathToSpace@
pathToSpace :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
pathToSpace srKeyboardMetrics =
  sendMessage srKeyboardMetrics pathToSpaceSelector

-- | The duration between touchup of a path and touch down of a sequential path
--
-- ObjC selector: @- pathToPath@
pathToPath :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id SRKeyboardProbabilityMetric)
pathToPath srKeyboardMetrics =
  sendMessage srKeyboardMetrics pathToPathSelector

-- | The total number of words typed during the session
--
-- ObjC selector: @- totalWords@
totalWords :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalWords srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalWordsSelector

-- | The total number of altered words during the session
--
-- ObjC selector: @- totalAlteredWords@
totalAlteredWords :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalAlteredWords srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalAlteredWordsSelector

-- | The total number of taps during the session
--
-- ObjC selector: @- totalTaps@
totalTaps :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalTaps srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalTapsSelector

-- | The total number of drags during the session
--
-- ObjC selector: @- totalDrags@
totalDrags :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalDrags srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalDragsSelector

-- | The total number of deletes during the session
--
-- ObjC selector: @- totalDeletes@
totalDeletes :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalDeletes srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalDeletesSelector

-- | The total number of emojis used during the session
--
-- ObjC selector: @- totalEmojis@
totalEmojis :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalEmojis srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalEmojisSelector

-- | The total number of paths used during the sesion
--
-- ObjC selector: @- totalPaths@
totalPaths :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalPaths srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalPathsSelector

-- | The total time taken to complete paths in the session
--
-- ObjC selector: @- totalPathTime@
totalPathTime :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
totalPathTime srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalPathTimeSelector

-- | The total length of paths completed in the session
--
-- ObjC selector: @- totalPathLength@
totalPathLength :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO (Id NSMeasurement)
totalPathLength srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalPathLengthSelector

-- | The total number of autocorrections in the session
--
-- ObjC selector: @- totalAutoCorrections@
totalAutoCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalAutoCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalAutoCorrectionsSelector

-- | The total number of space corrections in the session
--
-- ObjC selector: @- totalSpaceCorrections@
totalSpaceCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalSpaceCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalSpaceCorrectionsSelector

-- | The total number of retro corrections in the session
--
-- ObjC selector: @- totalRetroCorrections@
totalRetroCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalRetroCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalRetroCorrectionsSelector

-- | The total number of transposition corrections in the session
--
-- ObjC selector: @- totalTranspositionCorrections@
totalTranspositionCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalTranspositionCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalTranspositionCorrectionsSelector

-- | The total number of insert key corrections in the session
--
-- ObjC selector: @- totalInsertKeyCorrections@
totalInsertKeyCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalInsertKeyCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalInsertKeyCorrectionsSelector

-- | The total number of skip touch corrections in the session
--
-- ObjC selector: @- totalSkipTouchCorrections@
totalSkipTouchCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalSkipTouchCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalSkipTouchCorrectionsSelector

-- | The total number of near key corrections in the session
--
-- ObjC selector: @- totalNearKeyCorrections@
totalNearKeyCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalNearKeyCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalNearKeyCorrectionsSelector

-- | The total number of substitution corrections in the session
--
-- ObjC selector: @- totalSubstitutionCorrections@
totalSubstitutionCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalSubstitutionCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalSubstitutionCorrectionsSelector

-- | The total number of hit test corrections in the session
--
-- ObjC selector: @- totalHitTestCorrections@
totalHitTestCorrections :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CLong
totalHitTestCorrections srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalHitTestCorrectionsSelector

-- | The total amount of time typing during the session
--
-- ObjC selector: @- totalTypingDuration@
totalTypingDuration :: IsSRKeyboardMetrics srKeyboardMetrics => srKeyboardMetrics -> IO CDouble
totalTypingDuration srKeyboardMetrics =
  sendMessage srKeyboardMetrics totalTypingDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wordCountForSentimentCategory:@
wordCountForSentimentCategorySelector :: Selector '[SRKeyboardMetricsSentimentCategory] CLong
wordCountForSentimentCategorySelector = mkSelector "wordCountForSentimentCategory:"

-- | @Selector@ for @emojiCountForSentimentCategory:@
emojiCountForSentimentCategorySelector :: Selector '[SRKeyboardMetricsSentimentCategory] CLong
emojiCountForSentimentCategorySelector = mkSelector "emojiCountForSentimentCategory:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @keyboardIdentifier@
keyboardIdentifierSelector :: Selector '[] (Id NSString)
keyboardIdentifierSelector = mkSelector "keyboardIdentifier"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSMeasurement)
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSMeasurement)
heightSelector = mkSelector "height"

-- | @Selector@ for @inputModes@
inputModesSelector :: Selector '[] (Id NSArray)
inputModesSelector = mkSelector "inputModes"

-- | @Selector@ for @sessionIdentifiers@
sessionIdentifiersSelector :: Selector '[] (Id NSArray)
sessionIdentifiersSelector = mkSelector "sessionIdentifiers"

-- | @Selector@ for @totalPauses@
totalPausesSelector :: Selector '[] CLong
totalPausesSelector = mkSelector "totalPauses"

-- | @Selector@ for @totalPathPauses@
totalPathPausesSelector :: Selector '[] CLong
totalPathPausesSelector = mkSelector "totalPathPauses"

-- | @Selector@ for @typingSpeed@
typingSpeedSelector :: Selector '[] CDouble
typingSpeedSelector = mkSelector "typingSpeed"

-- | @Selector@ for @pathTypingSpeed@
pathTypingSpeedSelector :: Selector '[] CDouble
pathTypingSpeedSelector = mkSelector "pathTypingSpeed"

-- | @Selector@ for @totalTypingEpisodes@
totalTypingEpisodesSelector :: Selector '[] CLong
totalTypingEpisodesSelector = mkSelector "totalTypingEpisodes"

-- | @Selector@ for @longWordUpErrorDistance@
longWordUpErrorDistanceSelector :: Selector '[] (Id NSArray)
longWordUpErrorDistanceSelector = mkSelector "longWordUpErrorDistance"

-- | @Selector@ for @longWordDownErrorDistance@
longWordDownErrorDistanceSelector :: Selector '[] (Id NSArray)
longWordDownErrorDistanceSelector = mkSelector "longWordDownErrorDistance"

-- | @Selector@ for @longWordTouchDownUp@
longWordTouchDownUpSelector :: Selector '[] (Id NSArray)
longWordTouchDownUpSelector = mkSelector "longWordTouchDownUp"

-- | @Selector@ for @longWordTouchDownDown@
longWordTouchDownDownSelector :: Selector '[] (Id NSArray)
longWordTouchDownDownSelector = mkSelector "longWordTouchDownDown"

-- | @Selector@ for @longWordTouchUpDown@
longWordTouchUpDownSelector :: Selector '[] (Id NSArray)
longWordTouchUpDownSelector = mkSelector "longWordTouchUpDown"

-- | @Selector@ for @deleteToDeletes@
deleteToDeletesSelector :: Selector '[] (Id NSArray)
deleteToDeletesSelector = mkSelector "deleteToDeletes"

-- | @Selector@ for @upErrorDistance@
upErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
upErrorDistanceSelector = mkSelector "upErrorDistance"

-- | @Selector@ for @downErrorDistance@
downErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
downErrorDistanceSelector = mkSelector "downErrorDistance"

-- | @Selector@ for @spaceUpErrorDistance@
spaceUpErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceUpErrorDistanceSelector = mkSelector "spaceUpErrorDistance"

-- | @Selector@ for @spaceDownErrorDistance@
spaceDownErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceDownErrorDistanceSelector = mkSelector "spaceDownErrorDistance"

-- | @Selector@ for @deleteUpErrorDistance@
deleteUpErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteUpErrorDistanceSelector = mkSelector "deleteUpErrorDistance"

-- | @Selector@ for @deleteDownErrorDistance@
deleteDownErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteDownErrorDistanceSelector = mkSelector "deleteDownErrorDistance"

-- | @Selector@ for @shortWordCharKeyUpErrorDistance@
shortWordCharKeyUpErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
shortWordCharKeyUpErrorDistanceSelector = mkSelector "shortWordCharKeyUpErrorDistance"

-- | @Selector@ for @shortWordCharKeyDownErrorDistance@
shortWordCharKeyDownErrorDistanceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
shortWordCharKeyDownErrorDistanceSelector = mkSelector "shortWordCharKeyDownErrorDistance"

-- | @Selector@ for @touchDownUp@
touchDownUpSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
touchDownUpSelector = mkSelector "touchDownUp"

-- | @Selector@ for @spaceTouchDownUp@
spaceTouchDownUpSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceTouchDownUpSelector = mkSelector "spaceTouchDownUp"

-- | @Selector@ for @deleteTouchDownUp@
deleteTouchDownUpSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteTouchDownUpSelector = mkSelector "deleteTouchDownUp"

-- | @Selector@ for @shortWordCharKeyTouchDownUp@
shortWordCharKeyTouchDownUpSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
shortWordCharKeyTouchDownUpSelector = mkSelector "shortWordCharKeyTouchDownUp"

-- | @Selector@ for @touchDownDown@
touchDownDownSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
touchDownDownSelector = mkSelector "touchDownDown"

-- | @Selector@ for @touchUpDown@
touchUpDownSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
touchUpDownSelector = mkSelector "touchUpDown"

-- | @Selector@ for @charKeyToPrediction@
charKeyToPredictionSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
charKeyToPredictionSelector = mkSelector "charKeyToPrediction"

-- | @Selector@ for @shortWordCharKeyToCharKey@
shortWordCharKeyToCharKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
shortWordCharKeyToCharKeySelector = mkSelector "shortWordCharKeyToCharKey"

-- | @Selector@ for @charKeyToAnyTapKey@
charKeyToAnyTapKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
charKeyToAnyTapKeySelector = mkSelector "charKeyToAnyTapKey"

-- | @Selector@ for @anyTapToCharKey@
anyTapToCharKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
anyTapToCharKeySelector = mkSelector "anyTapToCharKey"

-- | @Selector@ for @spaceToCharKey@
spaceToCharKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToCharKeySelector = mkSelector "spaceToCharKey"

-- | @Selector@ for @charKeyToSpaceKey@
charKeyToSpaceKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
charKeyToSpaceKeySelector = mkSelector "charKeyToSpaceKey"

-- | @Selector@ for @spaceToDeleteKey@
spaceToDeleteKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToDeleteKeySelector = mkSelector "spaceToDeleteKey"

-- | @Selector@ for @deleteToSpaceKey@
deleteToSpaceKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteToSpaceKeySelector = mkSelector "deleteToSpaceKey"

-- | @Selector@ for @spaceToSpaceKey@
spaceToSpaceKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToSpaceKeySelector = mkSelector "spaceToSpaceKey"

-- | @Selector@ for @spaceToShiftKey@
spaceToShiftKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToShiftKeySelector = mkSelector "spaceToShiftKey"

-- | @Selector@ for @spaceToPlaneChangeKey@
spaceToPlaneChangeKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToPlaneChangeKeySelector = mkSelector "spaceToPlaneChangeKey"

-- | @Selector@ for @spaceToPredictionKey@
spaceToPredictionKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToPredictionKeySelector = mkSelector "spaceToPredictionKey"

-- | @Selector@ for @deleteToCharKey@
deleteToCharKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteToCharKeySelector = mkSelector "deleteToCharKey"

-- | @Selector@ for @charKeyToDelete@
charKeyToDeleteSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
charKeyToDeleteSelector = mkSelector "charKeyToDelete"

-- | @Selector@ for @deleteToDelete@
deleteToDeleteSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteToDeleteSelector = mkSelector "deleteToDelete"

-- | @Selector@ for @deleteToShiftKey@
deleteToShiftKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteToShiftKeySelector = mkSelector "deleteToShiftKey"

-- | @Selector@ for @deleteToPlaneChangeKey@
deleteToPlaneChangeKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteToPlaneChangeKeySelector = mkSelector "deleteToPlaneChangeKey"

-- | @Selector@ for @anyTapToPlaneChangeKey@
anyTapToPlaneChangeKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
anyTapToPlaneChangeKeySelector = mkSelector "anyTapToPlaneChangeKey"

-- | @Selector@ for @planeChangeToAnyTap@
planeChangeToAnyTapSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
planeChangeToAnyTapSelector = mkSelector "planeChangeToAnyTap"

-- | @Selector@ for @charKeyToPlaneChangeKey@
charKeyToPlaneChangeKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
charKeyToPlaneChangeKeySelector = mkSelector "charKeyToPlaneChangeKey"

-- | @Selector@ for @planeChangeKeyToCharKey@
planeChangeKeyToCharKeySelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
planeChangeKeyToCharKeySelector = mkSelector "planeChangeKeyToCharKey"

-- | @Selector@ for @pathErrorDistanceRatio@
pathErrorDistanceRatioSelector :: Selector '[] (Id NSArray)
pathErrorDistanceRatioSelector = mkSelector "pathErrorDistanceRatio"

-- | @Selector@ for @deleteToPath@
deleteToPathSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
deleteToPathSelector = mkSelector "deleteToPath"

-- | @Selector@ for @pathToDelete@
pathToDeleteSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
pathToDeleteSelector = mkSelector "pathToDelete"

-- | @Selector@ for @spaceToPath@
spaceToPathSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
spaceToPathSelector = mkSelector "spaceToPath"

-- | @Selector@ for @pathToSpace@
pathToSpaceSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
pathToSpaceSelector = mkSelector "pathToSpace"

-- | @Selector@ for @pathToPath@
pathToPathSelector :: Selector '[] (Id SRKeyboardProbabilityMetric)
pathToPathSelector = mkSelector "pathToPath"

-- | @Selector@ for @totalWords@
totalWordsSelector :: Selector '[] CLong
totalWordsSelector = mkSelector "totalWords"

-- | @Selector@ for @totalAlteredWords@
totalAlteredWordsSelector :: Selector '[] CLong
totalAlteredWordsSelector = mkSelector "totalAlteredWords"

-- | @Selector@ for @totalTaps@
totalTapsSelector :: Selector '[] CLong
totalTapsSelector = mkSelector "totalTaps"

-- | @Selector@ for @totalDrags@
totalDragsSelector :: Selector '[] CLong
totalDragsSelector = mkSelector "totalDrags"

-- | @Selector@ for @totalDeletes@
totalDeletesSelector :: Selector '[] CLong
totalDeletesSelector = mkSelector "totalDeletes"

-- | @Selector@ for @totalEmojis@
totalEmojisSelector :: Selector '[] CLong
totalEmojisSelector = mkSelector "totalEmojis"

-- | @Selector@ for @totalPaths@
totalPathsSelector :: Selector '[] CLong
totalPathsSelector = mkSelector "totalPaths"

-- | @Selector@ for @totalPathTime@
totalPathTimeSelector :: Selector '[] CDouble
totalPathTimeSelector = mkSelector "totalPathTime"

-- | @Selector@ for @totalPathLength@
totalPathLengthSelector :: Selector '[] (Id NSMeasurement)
totalPathLengthSelector = mkSelector "totalPathLength"

-- | @Selector@ for @totalAutoCorrections@
totalAutoCorrectionsSelector :: Selector '[] CLong
totalAutoCorrectionsSelector = mkSelector "totalAutoCorrections"

-- | @Selector@ for @totalSpaceCorrections@
totalSpaceCorrectionsSelector :: Selector '[] CLong
totalSpaceCorrectionsSelector = mkSelector "totalSpaceCorrections"

-- | @Selector@ for @totalRetroCorrections@
totalRetroCorrectionsSelector :: Selector '[] CLong
totalRetroCorrectionsSelector = mkSelector "totalRetroCorrections"

-- | @Selector@ for @totalTranspositionCorrections@
totalTranspositionCorrectionsSelector :: Selector '[] CLong
totalTranspositionCorrectionsSelector = mkSelector "totalTranspositionCorrections"

-- | @Selector@ for @totalInsertKeyCorrections@
totalInsertKeyCorrectionsSelector :: Selector '[] CLong
totalInsertKeyCorrectionsSelector = mkSelector "totalInsertKeyCorrections"

-- | @Selector@ for @totalSkipTouchCorrections@
totalSkipTouchCorrectionsSelector :: Selector '[] CLong
totalSkipTouchCorrectionsSelector = mkSelector "totalSkipTouchCorrections"

-- | @Selector@ for @totalNearKeyCorrections@
totalNearKeyCorrectionsSelector :: Selector '[] CLong
totalNearKeyCorrectionsSelector = mkSelector "totalNearKeyCorrections"

-- | @Selector@ for @totalSubstitutionCorrections@
totalSubstitutionCorrectionsSelector :: Selector '[] CLong
totalSubstitutionCorrectionsSelector = mkSelector "totalSubstitutionCorrections"

-- | @Selector@ for @totalHitTestCorrections@
totalHitTestCorrectionsSelector :: Selector '[] CLong
totalHitTestCorrectionsSelector = mkSelector "totalHitTestCorrections"

-- | @Selector@ for @totalTypingDuration@
totalTypingDurationSelector :: Selector '[] CDouble
totalTypingDurationSelector = mkSelector "totalTypingDuration"

