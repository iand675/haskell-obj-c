{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SoundAnalysis.Internal.Classes (
    module ObjC.SoundAnalysis.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.CoreML.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SNAudioFileAnalyzer ----------

-- | Analyzes an audio file and provides analysis results to the client
-- 
-- Phantom type for @SNAudioFileAnalyzer@.
data SNAudioFileAnalyzer

instance IsObjCObject (Id SNAudioFileAnalyzer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SNAudioFileAnalyzer"

class IsNSObject a => IsSNAudioFileAnalyzer a where
  toSNAudioFileAnalyzer :: a -> Id SNAudioFileAnalyzer

instance IsSNAudioFileAnalyzer (Id SNAudioFileAnalyzer) where
  toSNAudioFileAnalyzer = unsafeCastId

instance IsNSObject (Id SNAudioFileAnalyzer) where
  toNSObject = unsafeCastId

-- ---------- SNAudioStreamAnalyzer ----------

-- | Analyzes a stream of audio data and provides analysis results to the client
--
-- SNAudioStreamAnalyzer should be used to analyze a stream of audio, represented by a sequence of audio buffers over time.
-- 
-- Phantom type for @SNAudioStreamAnalyzer@.
data SNAudioStreamAnalyzer

instance IsObjCObject (Id SNAudioStreamAnalyzer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SNAudioStreamAnalyzer"

class IsNSObject a => IsSNAudioStreamAnalyzer a where
  toSNAudioStreamAnalyzer :: a -> Id SNAudioStreamAnalyzer

instance IsSNAudioStreamAnalyzer (Id SNAudioStreamAnalyzer) where
  toSNAudioStreamAnalyzer = unsafeCastId

instance IsNSObject (Id SNAudioStreamAnalyzer) where
  toNSObject = unsafeCastId

-- ---------- SNClassification ----------

-- | The likelihood of a sound belonging to identified class
-- 
-- Phantom type for @SNClassification@.
data SNClassification

instance IsObjCObject (Id SNClassification) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SNClassification"

class IsNSObject a => IsSNClassification a where
  toSNClassification :: a -> Id SNClassification

instance IsSNClassification (Id SNClassification) where
  toSNClassification = unsafeCastId

instance IsNSObject (Id SNClassification) where
  toNSObject = unsafeCastId

-- ---------- SNClassificationResult ----------

-- | A result containing the most likely classification candidates in the time range specified
-- 
-- Phantom type for @SNClassificationResult@.
data SNClassificationResult

instance IsObjCObject (Id SNClassificationResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SNClassificationResult"

class IsNSObject a => IsSNClassificationResult a where
  toSNClassificationResult :: a -> Id SNClassificationResult

instance IsSNClassificationResult (Id SNClassificationResult) where
  toSNClassificationResult = unsafeCastId

instance IsNSObject (Id SNClassificationResult) where
  toNSObject = unsafeCastId

-- ---------- SNClassifySoundRequest ----------

-- | Configure an analyzer to perform sound classification using the provided MLModel.
--
-- When a new classification result is produced, the results observer will be called with an SNClassificationResult. Audio buffers provided to SNAudioStreamAnalyzer may vary in size, and the analyzer will reblock the audio data to the block size expected by the MLModel. By default, analysis will occur on the first audio channel in the audio stream, and the analyzer will apply sample rate conversion if the provided audio does not match the sample rate required by the MLModel.
-- 
-- Phantom type for @SNClassifySoundRequest@.
data SNClassifySoundRequest

instance IsObjCObject (Id SNClassifySoundRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SNClassifySoundRequest"

class IsNSObject a => IsSNClassifySoundRequest a where
  toSNClassifySoundRequest :: a -> Id SNClassifySoundRequest

instance IsSNClassifySoundRequest (Id SNClassifySoundRequest) where
  toSNClassifySoundRequest = unsafeCastId

instance IsNSObject (Id SNClassifySoundRequest) where
  toNSObject = unsafeCastId

-- ---------- SNTimeDurationConstraint ----------

-- | Constrains CMTime durations to a subset of legal values.
--
-- @SNTimeDurationConstraint@ is a union type, which, based on the value of its @type@ property, may assume one of several forms. Instance properties may be used to extract information from an object, but certain properties are only valid to exercise under certain circumstances. Before accessing a particular property, refer to its documentation to understand what @type@ value is required in order for that property to be valid.
-- 
-- Phantom type for @SNTimeDurationConstraint@.
data SNTimeDurationConstraint

instance IsObjCObject (Id SNTimeDurationConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SNTimeDurationConstraint"

class IsNSObject a => IsSNTimeDurationConstraint a where
  toSNTimeDurationConstraint :: a -> Id SNTimeDurationConstraint

instance IsSNTimeDurationConstraint (Id SNTimeDurationConstraint) where
  toSNTimeDurationConstraint = unsafeCastId

instance IsNSObject (Id SNTimeDurationConstraint) where
  toNSObject = unsafeCastId
