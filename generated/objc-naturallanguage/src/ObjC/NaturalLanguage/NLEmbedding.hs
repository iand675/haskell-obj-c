{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLEmbedding@.
module ObjC.NaturalLanguage.NLEmbedding
  ( NLEmbedding
  , IsNLEmbedding(..)
  , wordEmbeddingForLanguage
  , wordEmbeddingForLanguage_revision
  , sentenceEmbeddingForLanguage
  , sentenceEmbeddingForLanguage_revision
  , embeddingWithContentsOfURL_error
  , containsString
  , distanceBetweenString_andString_distanceType
  , enumerateNeighborsForString_maximumCount_distanceType_usingBlock
  , enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlock
  , neighborsForString_maximumCount_distanceType
  , neighborsForString_maximumCount_maximumDistance_distanceType
  , vectorForString
  , getVector_forString
  , enumerateNeighborsForVector_maximumCount_distanceType_usingBlock
  , enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlock
  , neighborsForVector_maximumCount_distanceType
  , neighborsForVector_maximumCount_maximumDistance_distanceType
  , supportedRevisionsForLanguage
  , currentRevisionForLanguage
  , supportedSentenceEmbeddingRevisionsForLanguage
  , currentSentenceEmbeddingRevisionForLanguage
  , writeEmbeddingForDictionary_language_revision_toURL_error
  , dimension
  , vocabularySize
  , language
  , revision
  , wordEmbeddingForLanguageSelector
  , wordEmbeddingForLanguage_revisionSelector
  , sentenceEmbeddingForLanguageSelector
  , sentenceEmbeddingForLanguage_revisionSelector
  , embeddingWithContentsOfURL_errorSelector
  , containsStringSelector
  , distanceBetweenString_andString_distanceTypeSelector
  , enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector
  , enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector
  , neighborsForString_maximumCount_distanceTypeSelector
  , neighborsForString_maximumCount_maximumDistance_distanceTypeSelector
  , vectorForStringSelector
  , getVector_forStringSelector
  , enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector
  , enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector
  , neighborsForVector_maximumCount_distanceTypeSelector
  , neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector
  , supportedRevisionsForLanguageSelector
  , currentRevisionForLanguageSelector
  , supportedSentenceEmbeddingRevisionsForLanguageSelector
  , currentSentenceEmbeddingRevisionForLanguageSelector
  , writeEmbeddingForDictionary_language_revision_toURL_errorSelector
  , dimensionSelector
  , vocabularySizeSelector
  , languageSelector
  , revisionSelector

  -- * Enum types
  , NLDistanceType(NLDistanceType)
  , pattern NLDistanceTypeCosine

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

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ wordEmbeddingForLanguage:@
wordEmbeddingForLanguage :: IsNSString language => language -> IO (Id NLEmbedding)
wordEmbeddingForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "wordEmbeddingForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @+ wordEmbeddingForLanguage:revision:@
wordEmbeddingForLanguage_revision :: IsNSString language => language -> CULong -> IO (Id NLEmbedding)
wordEmbeddingForLanguage_revision language revision =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "wordEmbeddingForLanguage:revision:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ()), argCULong (fromIntegral revision)] >>= retainedObject . castPtr

-- | @+ sentenceEmbeddingForLanguage:@
sentenceEmbeddingForLanguage :: IsNSString language => language -> IO (Id NLEmbedding)
sentenceEmbeddingForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "sentenceEmbeddingForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sentenceEmbeddingForLanguage:revision:@
sentenceEmbeddingForLanguage_revision :: IsNSString language => language -> CULong -> IO (Id NLEmbedding)
sentenceEmbeddingForLanguage_revision language revision =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "sentenceEmbeddingForLanguage:revision:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ()), argCULong (fromIntegral revision)] >>= retainedObject . castPtr

-- | @+ embeddingWithContentsOfURL:error:@
embeddingWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NLEmbedding)
embeddingWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "embeddingWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- containsString:@
containsString :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> IO Bool
containsString nlEmbedding  string =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nlEmbedding (mkSelector "containsString:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- distanceBetweenString:andString:distanceType:@
distanceBetweenString_andString_distanceType :: (IsNLEmbedding nlEmbedding, IsNSString firstString, IsNSString secondString) => nlEmbedding -> firstString -> secondString -> NLDistanceType -> IO CDouble
distanceBetweenString_andString_distanceType nlEmbedding  firstString secondString distanceType =
withObjCPtr firstString $ \raw_firstString ->
  withObjCPtr secondString $ \raw_secondString ->
      sendMsg nlEmbedding (mkSelector "distanceBetweenString:andString:distanceType:") retCDouble [argPtr (castPtr raw_firstString :: Ptr ()), argPtr (castPtr raw_secondString :: Ptr ()), argCLong (coerce distanceType)]

-- | @- enumerateNeighborsForString:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForString_maximumCount_distanceType_usingBlock nlEmbedding  string maxCount distanceType block =
withObjCPtr string $ \raw_string ->
    sendMsg nlEmbedding (mkSelector "enumerateNeighborsForString:maximumCount:distanceType:usingBlock:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argCULong (fromIntegral maxCount), argCLong (coerce distanceType), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> CDouble -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlock nlEmbedding  string maxCount maxDistance distanceType block =
withObjCPtr string $ \raw_string ->
    sendMsg nlEmbedding (mkSelector "enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argCULong (fromIntegral maxCount), argCDouble (fromIntegral maxDistance), argCLong (coerce distanceType), argPtr (castPtr block :: Ptr ())]

-- | @- neighborsForString:maximumCount:distanceType:@
neighborsForString_maximumCount_distanceType :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> NLDistanceType -> IO (Id NSArray)
neighborsForString_maximumCount_distanceType nlEmbedding  string maxCount distanceType =
withObjCPtr string $ \raw_string ->
    sendMsg nlEmbedding (mkSelector "neighborsForString:maximumCount:distanceType:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (fromIntegral maxCount), argCLong (coerce distanceType)] >>= retainedObject . castPtr

-- | @- neighborsForString:maximumCount:maximumDistance:distanceType:@
neighborsForString_maximumCount_maximumDistance_distanceType :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> CDouble -> NLDistanceType -> IO (Id NSArray)
neighborsForString_maximumCount_maximumDistance_distanceType nlEmbedding  string maxCount maxDistance distanceType =
withObjCPtr string $ \raw_string ->
    sendMsg nlEmbedding (mkSelector "neighborsForString:maximumCount:maximumDistance:distanceType:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (fromIntegral maxCount), argCDouble (fromIntegral maxDistance), argCLong (coerce distanceType)] >>= retainedObject . castPtr

-- | @- vectorForString:@
vectorForString :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> IO (Id NSArray)
vectorForString nlEmbedding  string =
withObjCPtr string $ \raw_string ->
    sendMsg nlEmbedding (mkSelector "vectorForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- getVector:forString:@
getVector_forString :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> Ptr CFloat -> string -> IO Bool
getVector_forString nlEmbedding  vector string =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nlEmbedding (mkSelector "getVector:forString:") retCULong [argPtr vector, argPtr (castPtr raw_string :: Ptr ())]

-- | @- enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForVector_maximumCount_distanceType_usingBlock nlEmbedding  vector maxCount distanceType block =
withObjCPtr vector $ \raw_vector ->
    sendMsg nlEmbedding (mkSelector "enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:") retVoid [argPtr (castPtr raw_vector :: Ptr ()), argCULong (fromIntegral maxCount), argCLong (coerce distanceType), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> CDouble -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlock nlEmbedding  vector maxCount maxDistance distanceType block =
withObjCPtr vector $ \raw_vector ->
    sendMsg nlEmbedding (mkSelector "enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:") retVoid [argPtr (castPtr raw_vector :: Ptr ()), argCULong (fromIntegral maxCount), argCDouble (fromIntegral maxDistance), argCLong (coerce distanceType), argPtr (castPtr block :: Ptr ())]

-- | @- neighborsForVector:maximumCount:distanceType:@
neighborsForVector_maximumCount_distanceType :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> NLDistanceType -> IO (Id NSArray)
neighborsForVector_maximumCount_distanceType nlEmbedding  vector maxCount distanceType =
withObjCPtr vector $ \raw_vector ->
    sendMsg nlEmbedding (mkSelector "neighborsForVector:maximumCount:distanceType:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ()), argCULong (fromIntegral maxCount), argCLong (coerce distanceType)] >>= retainedObject . castPtr

-- | @- neighborsForVector:maximumCount:maximumDistance:distanceType:@
neighborsForVector_maximumCount_maximumDistance_distanceType :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> CDouble -> NLDistanceType -> IO (Id NSArray)
neighborsForVector_maximumCount_maximumDistance_distanceType nlEmbedding  vector maxCount maxDistance distanceType =
withObjCPtr vector $ \raw_vector ->
    sendMsg nlEmbedding (mkSelector "neighborsForVector:maximumCount:maximumDistance:distanceType:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ()), argCULong (fromIntegral maxCount), argCDouble (fromIntegral maxDistance), argCLong (coerce distanceType)] >>= retainedObject . castPtr

-- | @+ supportedRevisionsForLanguage:@
supportedRevisionsForLanguage :: IsNSString language => language -> IO (Id NSIndexSet)
supportedRevisionsForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "supportedRevisionsForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @+ currentRevisionForLanguage:@
currentRevisionForLanguage :: IsNSString language => language -> IO CULong
currentRevisionForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "currentRevisionForLanguage:") retCULong [argPtr (castPtr raw_language :: Ptr ())]

-- | @+ supportedSentenceEmbeddingRevisionsForLanguage:@
supportedSentenceEmbeddingRevisionsForLanguage :: IsNSString language => language -> IO (Id NSIndexSet)
supportedSentenceEmbeddingRevisionsForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "supportedSentenceEmbeddingRevisionsForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @+ currentSentenceEmbeddingRevisionForLanguage:@
currentSentenceEmbeddingRevisionForLanguage :: IsNSString language => language -> IO CULong
currentSentenceEmbeddingRevisionForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "currentSentenceEmbeddingRevisionForLanguage:") retCULong [argPtr (castPtr raw_language :: Ptr ())]

-- | @+ writeEmbeddingForDictionary:language:revision:toURL:error:@
writeEmbeddingForDictionary_language_revision_toURL_error :: (IsNSDictionary dictionary, IsNSString language, IsNSURL url, IsNSError error_) => dictionary -> language -> CULong -> url -> error_ -> IO Bool
writeEmbeddingForDictionary_language_revision_toURL_error dictionary language revision url error_ =
  do
    cls' <- getRequiredClass "NLEmbedding"
    withObjCPtr dictionary $ \raw_dictionary ->
      withObjCPtr language $ \raw_language ->
        withObjCPtr url $ \raw_url ->
          withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "writeEmbeddingForDictionary:language:revision:toURL:error:") retCULong [argPtr (castPtr raw_dictionary :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argCULong (fromIntegral revision), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- dimension@
dimension :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO CULong
dimension nlEmbedding  =
  sendMsg nlEmbedding (mkSelector "dimension") retCULong []

-- | @- vocabularySize@
vocabularySize :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO CULong
vocabularySize nlEmbedding  =
  sendMsg nlEmbedding (mkSelector "vocabularySize") retCULong []

-- | @- language@
language :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO (Id NSString)
language nlEmbedding  =
  sendMsg nlEmbedding (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- revision@
revision :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO CULong
revision nlEmbedding  =
  sendMsg nlEmbedding (mkSelector "revision") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wordEmbeddingForLanguage:@
wordEmbeddingForLanguageSelector :: Selector
wordEmbeddingForLanguageSelector = mkSelector "wordEmbeddingForLanguage:"

-- | @Selector@ for @wordEmbeddingForLanguage:revision:@
wordEmbeddingForLanguage_revisionSelector :: Selector
wordEmbeddingForLanguage_revisionSelector = mkSelector "wordEmbeddingForLanguage:revision:"

-- | @Selector@ for @sentenceEmbeddingForLanguage:@
sentenceEmbeddingForLanguageSelector :: Selector
sentenceEmbeddingForLanguageSelector = mkSelector "sentenceEmbeddingForLanguage:"

-- | @Selector@ for @sentenceEmbeddingForLanguage:revision:@
sentenceEmbeddingForLanguage_revisionSelector :: Selector
sentenceEmbeddingForLanguage_revisionSelector = mkSelector "sentenceEmbeddingForLanguage:revision:"

-- | @Selector@ for @embeddingWithContentsOfURL:error:@
embeddingWithContentsOfURL_errorSelector :: Selector
embeddingWithContentsOfURL_errorSelector = mkSelector "embeddingWithContentsOfURL:error:"

-- | @Selector@ for @containsString:@
containsStringSelector :: Selector
containsStringSelector = mkSelector "containsString:"

-- | @Selector@ for @distanceBetweenString:andString:distanceType:@
distanceBetweenString_andString_distanceTypeSelector :: Selector
distanceBetweenString_andString_distanceTypeSelector = mkSelector "distanceBetweenString:andString:distanceType:"

-- | @Selector@ for @enumerateNeighborsForString:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector :: Selector
enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForString:maximumCount:distanceType:usingBlock:"

-- | @Selector@ for @enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector :: Selector
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:"

-- | @Selector@ for @neighborsForString:maximumCount:distanceType:@
neighborsForString_maximumCount_distanceTypeSelector :: Selector
neighborsForString_maximumCount_distanceTypeSelector = mkSelector "neighborsForString:maximumCount:distanceType:"

-- | @Selector@ for @neighborsForString:maximumCount:maximumDistance:distanceType:@
neighborsForString_maximumCount_maximumDistance_distanceTypeSelector :: Selector
neighborsForString_maximumCount_maximumDistance_distanceTypeSelector = mkSelector "neighborsForString:maximumCount:maximumDistance:distanceType:"

-- | @Selector@ for @vectorForString:@
vectorForStringSelector :: Selector
vectorForStringSelector = mkSelector "vectorForString:"

-- | @Selector@ for @getVector:forString:@
getVector_forStringSelector :: Selector
getVector_forStringSelector = mkSelector "getVector:forString:"

-- | @Selector@ for @enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector :: Selector
enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:"

-- | @Selector@ for @enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector :: Selector
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:"

-- | @Selector@ for @neighborsForVector:maximumCount:distanceType:@
neighborsForVector_maximumCount_distanceTypeSelector :: Selector
neighborsForVector_maximumCount_distanceTypeSelector = mkSelector "neighborsForVector:maximumCount:distanceType:"

-- | @Selector@ for @neighborsForVector:maximumCount:maximumDistance:distanceType:@
neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector :: Selector
neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector = mkSelector "neighborsForVector:maximumCount:maximumDistance:distanceType:"

-- | @Selector@ for @supportedRevisionsForLanguage:@
supportedRevisionsForLanguageSelector :: Selector
supportedRevisionsForLanguageSelector = mkSelector "supportedRevisionsForLanguage:"

-- | @Selector@ for @currentRevisionForLanguage:@
currentRevisionForLanguageSelector :: Selector
currentRevisionForLanguageSelector = mkSelector "currentRevisionForLanguage:"

-- | @Selector@ for @supportedSentenceEmbeddingRevisionsForLanguage:@
supportedSentenceEmbeddingRevisionsForLanguageSelector :: Selector
supportedSentenceEmbeddingRevisionsForLanguageSelector = mkSelector "supportedSentenceEmbeddingRevisionsForLanguage:"

-- | @Selector@ for @currentSentenceEmbeddingRevisionForLanguage:@
currentSentenceEmbeddingRevisionForLanguageSelector :: Selector
currentSentenceEmbeddingRevisionForLanguageSelector = mkSelector "currentSentenceEmbeddingRevisionForLanguage:"

-- | @Selector@ for @writeEmbeddingForDictionary:language:revision:toURL:error:@
writeEmbeddingForDictionary_language_revision_toURL_errorSelector :: Selector
writeEmbeddingForDictionary_language_revision_toURL_errorSelector = mkSelector "writeEmbeddingForDictionary:language:revision:toURL:error:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @vocabularySize@
vocabularySizeSelector :: Selector
vocabularySizeSelector = mkSelector "vocabularySize"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

