{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , containsStringSelector
  , currentRevisionForLanguageSelector
  , currentSentenceEmbeddingRevisionForLanguageSelector
  , dimensionSelector
  , distanceBetweenString_andString_distanceTypeSelector
  , embeddingWithContentsOfURL_errorSelector
  , enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector
  , enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector
  , enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector
  , enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector
  , getVector_forStringSelector
  , languageSelector
  , neighborsForString_maximumCount_distanceTypeSelector
  , neighborsForString_maximumCount_maximumDistance_distanceTypeSelector
  , neighborsForVector_maximumCount_distanceTypeSelector
  , neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector
  , revisionSelector
  , sentenceEmbeddingForLanguageSelector
  , sentenceEmbeddingForLanguage_revisionSelector
  , supportedRevisionsForLanguageSelector
  , supportedSentenceEmbeddingRevisionsForLanguageSelector
  , vectorForStringSelector
  , vocabularySizeSelector
  , wordEmbeddingForLanguageSelector
  , wordEmbeddingForLanguage_revisionSelector
  , writeEmbeddingForDictionary_language_revision_toURL_errorSelector

  -- * Enum types
  , NLDistanceType(NLDistanceType)
  , pattern NLDistanceTypeCosine

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' wordEmbeddingForLanguageSelector (toNSString language)

-- | @+ wordEmbeddingForLanguage:revision:@
wordEmbeddingForLanguage_revision :: IsNSString language => language -> CULong -> IO (Id NLEmbedding)
wordEmbeddingForLanguage_revision language revision =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' wordEmbeddingForLanguage_revisionSelector (toNSString language) revision

-- | @+ sentenceEmbeddingForLanguage:@
sentenceEmbeddingForLanguage :: IsNSString language => language -> IO (Id NLEmbedding)
sentenceEmbeddingForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' sentenceEmbeddingForLanguageSelector (toNSString language)

-- | @+ sentenceEmbeddingForLanguage:revision:@
sentenceEmbeddingForLanguage_revision :: IsNSString language => language -> CULong -> IO (Id NLEmbedding)
sentenceEmbeddingForLanguage_revision language revision =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' sentenceEmbeddingForLanguage_revisionSelector (toNSString language) revision

-- | @+ embeddingWithContentsOfURL:error:@
embeddingWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NLEmbedding)
embeddingWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' embeddingWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- containsString:@
containsString :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> IO Bool
containsString nlEmbedding string =
  sendMessage nlEmbedding containsStringSelector (toNSString string)

-- | @- distanceBetweenString:andString:distanceType:@
distanceBetweenString_andString_distanceType :: (IsNLEmbedding nlEmbedding, IsNSString firstString, IsNSString secondString) => nlEmbedding -> firstString -> secondString -> NLDistanceType -> IO CDouble
distanceBetweenString_andString_distanceType nlEmbedding firstString secondString distanceType =
  sendMessage nlEmbedding distanceBetweenString_andString_distanceTypeSelector (toNSString firstString) (toNSString secondString) distanceType

-- | @- enumerateNeighborsForString:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForString_maximumCount_distanceType_usingBlock nlEmbedding string maxCount distanceType block =
  sendMessage nlEmbedding enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector (toNSString string) maxCount distanceType block

-- | @- enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> CDouble -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlock nlEmbedding string maxCount maxDistance distanceType block =
  sendMessage nlEmbedding enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector (toNSString string) maxCount maxDistance distanceType block

-- | @- neighborsForString:maximumCount:distanceType:@
neighborsForString_maximumCount_distanceType :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> NLDistanceType -> IO (Id NSArray)
neighborsForString_maximumCount_distanceType nlEmbedding string maxCount distanceType =
  sendMessage nlEmbedding neighborsForString_maximumCount_distanceTypeSelector (toNSString string) maxCount distanceType

-- | @- neighborsForString:maximumCount:maximumDistance:distanceType:@
neighborsForString_maximumCount_maximumDistance_distanceType :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> CULong -> CDouble -> NLDistanceType -> IO (Id NSArray)
neighborsForString_maximumCount_maximumDistance_distanceType nlEmbedding string maxCount maxDistance distanceType =
  sendMessage nlEmbedding neighborsForString_maximumCount_maximumDistance_distanceTypeSelector (toNSString string) maxCount maxDistance distanceType

-- | @- vectorForString:@
vectorForString :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> string -> IO (Id NSArray)
vectorForString nlEmbedding string =
  sendMessage nlEmbedding vectorForStringSelector (toNSString string)

-- | @- getVector:forString:@
getVector_forString :: (IsNLEmbedding nlEmbedding, IsNSString string) => nlEmbedding -> Ptr CFloat -> string -> IO Bool
getVector_forString nlEmbedding vector string =
  sendMessage nlEmbedding getVector_forStringSelector vector (toNSString string)

-- | @- enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForVector_maximumCount_distanceType_usingBlock nlEmbedding vector maxCount distanceType block =
  sendMessage nlEmbedding enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector (toNSArray vector) maxCount distanceType block

-- | @- enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlock :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> CDouble -> NLDistanceType -> Ptr () -> IO ()
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlock nlEmbedding vector maxCount maxDistance distanceType block =
  sendMessage nlEmbedding enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector (toNSArray vector) maxCount maxDistance distanceType block

-- | @- neighborsForVector:maximumCount:distanceType:@
neighborsForVector_maximumCount_distanceType :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> NLDistanceType -> IO (Id NSArray)
neighborsForVector_maximumCount_distanceType nlEmbedding vector maxCount distanceType =
  sendMessage nlEmbedding neighborsForVector_maximumCount_distanceTypeSelector (toNSArray vector) maxCount distanceType

-- | @- neighborsForVector:maximumCount:maximumDistance:distanceType:@
neighborsForVector_maximumCount_maximumDistance_distanceType :: (IsNLEmbedding nlEmbedding, IsNSArray vector) => nlEmbedding -> vector -> CULong -> CDouble -> NLDistanceType -> IO (Id NSArray)
neighborsForVector_maximumCount_maximumDistance_distanceType nlEmbedding vector maxCount maxDistance distanceType =
  sendMessage nlEmbedding neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector (toNSArray vector) maxCount maxDistance distanceType

-- | @+ supportedRevisionsForLanguage:@
supportedRevisionsForLanguage :: IsNSString language => language -> IO (Id NSIndexSet)
supportedRevisionsForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' supportedRevisionsForLanguageSelector (toNSString language)

-- | @+ currentRevisionForLanguage:@
currentRevisionForLanguage :: IsNSString language => language -> IO CULong
currentRevisionForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' currentRevisionForLanguageSelector (toNSString language)

-- | @+ supportedSentenceEmbeddingRevisionsForLanguage:@
supportedSentenceEmbeddingRevisionsForLanguage :: IsNSString language => language -> IO (Id NSIndexSet)
supportedSentenceEmbeddingRevisionsForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' supportedSentenceEmbeddingRevisionsForLanguageSelector (toNSString language)

-- | @+ currentSentenceEmbeddingRevisionForLanguage:@
currentSentenceEmbeddingRevisionForLanguage :: IsNSString language => language -> IO CULong
currentSentenceEmbeddingRevisionForLanguage language =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' currentSentenceEmbeddingRevisionForLanguageSelector (toNSString language)

-- | @+ writeEmbeddingForDictionary:language:revision:toURL:error:@
writeEmbeddingForDictionary_language_revision_toURL_error :: (IsNSDictionary dictionary, IsNSString language, IsNSURL url, IsNSError error_) => dictionary -> language -> CULong -> url -> error_ -> IO Bool
writeEmbeddingForDictionary_language_revision_toURL_error dictionary language revision url error_ =
  do
    cls' <- getRequiredClass "NLEmbedding"
    sendClassMessage cls' writeEmbeddingForDictionary_language_revision_toURL_errorSelector (toNSDictionary dictionary) (toNSString language) revision (toNSURL url) (toNSError error_)

-- | @- dimension@
dimension :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO CULong
dimension nlEmbedding =
  sendMessage nlEmbedding dimensionSelector

-- | @- vocabularySize@
vocabularySize :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO CULong
vocabularySize nlEmbedding =
  sendMessage nlEmbedding vocabularySizeSelector

-- | @- language@
language :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO (Id NSString)
language nlEmbedding =
  sendMessage nlEmbedding languageSelector

-- | @- revision@
revision :: IsNLEmbedding nlEmbedding => nlEmbedding -> IO CULong
revision nlEmbedding =
  sendMessage nlEmbedding revisionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wordEmbeddingForLanguage:@
wordEmbeddingForLanguageSelector :: Selector '[Id NSString] (Id NLEmbedding)
wordEmbeddingForLanguageSelector = mkSelector "wordEmbeddingForLanguage:"

-- | @Selector@ for @wordEmbeddingForLanguage:revision:@
wordEmbeddingForLanguage_revisionSelector :: Selector '[Id NSString, CULong] (Id NLEmbedding)
wordEmbeddingForLanguage_revisionSelector = mkSelector "wordEmbeddingForLanguage:revision:"

-- | @Selector@ for @sentenceEmbeddingForLanguage:@
sentenceEmbeddingForLanguageSelector :: Selector '[Id NSString] (Id NLEmbedding)
sentenceEmbeddingForLanguageSelector = mkSelector "sentenceEmbeddingForLanguage:"

-- | @Selector@ for @sentenceEmbeddingForLanguage:revision:@
sentenceEmbeddingForLanguage_revisionSelector :: Selector '[Id NSString, CULong] (Id NLEmbedding)
sentenceEmbeddingForLanguage_revisionSelector = mkSelector "sentenceEmbeddingForLanguage:revision:"

-- | @Selector@ for @embeddingWithContentsOfURL:error:@
embeddingWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NLEmbedding)
embeddingWithContentsOfURL_errorSelector = mkSelector "embeddingWithContentsOfURL:error:"

-- | @Selector@ for @containsString:@
containsStringSelector :: Selector '[Id NSString] Bool
containsStringSelector = mkSelector "containsString:"

-- | @Selector@ for @distanceBetweenString:andString:distanceType:@
distanceBetweenString_andString_distanceTypeSelector :: Selector '[Id NSString, Id NSString, NLDistanceType] CDouble
distanceBetweenString_andString_distanceTypeSelector = mkSelector "distanceBetweenString:andString:distanceType:"

-- | @Selector@ for @enumerateNeighborsForString:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector :: Selector '[Id NSString, CULong, NLDistanceType, Ptr ()] ()
enumerateNeighborsForString_maximumCount_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForString:maximumCount:distanceType:usingBlock:"

-- | @Selector@ for @enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector :: Selector '[Id NSString, CULong, CDouble, NLDistanceType, Ptr ()] ()
enumerateNeighborsForString_maximumCount_maximumDistance_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForString:maximumCount:maximumDistance:distanceType:usingBlock:"

-- | @Selector@ for @neighborsForString:maximumCount:distanceType:@
neighborsForString_maximumCount_distanceTypeSelector :: Selector '[Id NSString, CULong, NLDistanceType] (Id NSArray)
neighborsForString_maximumCount_distanceTypeSelector = mkSelector "neighborsForString:maximumCount:distanceType:"

-- | @Selector@ for @neighborsForString:maximumCount:maximumDistance:distanceType:@
neighborsForString_maximumCount_maximumDistance_distanceTypeSelector :: Selector '[Id NSString, CULong, CDouble, NLDistanceType] (Id NSArray)
neighborsForString_maximumCount_maximumDistance_distanceTypeSelector = mkSelector "neighborsForString:maximumCount:maximumDistance:distanceType:"

-- | @Selector@ for @vectorForString:@
vectorForStringSelector :: Selector '[Id NSString] (Id NSArray)
vectorForStringSelector = mkSelector "vectorForString:"

-- | @Selector@ for @getVector:forString:@
getVector_forStringSelector :: Selector '[Ptr CFloat, Id NSString] Bool
getVector_forStringSelector = mkSelector "getVector:forString:"

-- | @Selector@ for @enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector :: Selector '[Id NSArray, CULong, NLDistanceType, Ptr ()] ()
enumerateNeighborsForVector_maximumCount_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForVector:maximumCount:distanceType:usingBlock:"

-- | @Selector@ for @enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:@
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector :: Selector '[Id NSArray, CULong, CDouble, NLDistanceType, Ptr ()] ()
enumerateNeighborsForVector_maximumCount_maximumDistance_distanceType_usingBlockSelector = mkSelector "enumerateNeighborsForVector:maximumCount:maximumDistance:distanceType:usingBlock:"

-- | @Selector@ for @neighborsForVector:maximumCount:distanceType:@
neighborsForVector_maximumCount_distanceTypeSelector :: Selector '[Id NSArray, CULong, NLDistanceType] (Id NSArray)
neighborsForVector_maximumCount_distanceTypeSelector = mkSelector "neighborsForVector:maximumCount:distanceType:"

-- | @Selector@ for @neighborsForVector:maximumCount:maximumDistance:distanceType:@
neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector :: Selector '[Id NSArray, CULong, CDouble, NLDistanceType] (Id NSArray)
neighborsForVector_maximumCount_maximumDistance_distanceTypeSelector = mkSelector "neighborsForVector:maximumCount:maximumDistance:distanceType:"

-- | @Selector@ for @supportedRevisionsForLanguage:@
supportedRevisionsForLanguageSelector :: Selector '[Id NSString] (Id NSIndexSet)
supportedRevisionsForLanguageSelector = mkSelector "supportedRevisionsForLanguage:"

-- | @Selector@ for @currentRevisionForLanguage:@
currentRevisionForLanguageSelector :: Selector '[Id NSString] CULong
currentRevisionForLanguageSelector = mkSelector "currentRevisionForLanguage:"

-- | @Selector@ for @supportedSentenceEmbeddingRevisionsForLanguage:@
supportedSentenceEmbeddingRevisionsForLanguageSelector :: Selector '[Id NSString] (Id NSIndexSet)
supportedSentenceEmbeddingRevisionsForLanguageSelector = mkSelector "supportedSentenceEmbeddingRevisionsForLanguage:"

-- | @Selector@ for @currentSentenceEmbeddingRevisionForLanguage:@
currentSentenceEmbeddingRevisionForLanguageSelector :: Selector '[Id NSString] CULong
currentSentenceEmbeddingRevisionForLanguageSelector = mkSelector "currentSentenceEmbeddingRevisionForLanguage:"

-- | @Selector@ for @writeEmbeddingForDictionary:language:revision:toURL:error:@
writeEmbeddingForDictionary_language_revision_toURL_errorSelector :: Selector '[Id NSDictionary, Id NSString, CULong, Id NSURL, Id NSError] Bool
writeEmbeddingForDictionary_language_revision_toURL_errorSelector = mkSelector "writeEmbeddingForDictionary:language:revision:toURL:error:"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @vocabularySize@
vocabularySizeSelector :: Selector '[] CULong
vocabularySizeSelector = mkSelector "vocabularySize"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] CULong
revisionSelector = mkSelector "revision"

