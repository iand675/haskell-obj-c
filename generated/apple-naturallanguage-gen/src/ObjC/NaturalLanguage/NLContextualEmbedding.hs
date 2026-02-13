{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLContextualEmbedding@.
module ObjC.NaturalLanguage.NLContextualEmbedding
  ( NLContextualEmbedding
  , IsNLContextualEmbedding(..)
  , init_
  , contextualEmbeddingWithModelIdentifier
  , contextualEmbeddingsForValues
  , contextualEmbeddingWithLanguage
  , contextualEmbeddingWithScript
  , loadWithError
  , unload
  , embeddingResultForString_language_error
  , requestEmbeddingAssetsWithCompletionHandler
  , modelIdentifier
  , languages
  , scripts
  , revision
  , dimension
  , maximumSequenceLength
  , hasAvailableAssets
  , contextualEmbeddingWithLanguageSelector
  , contextualEmbeddingWithModelIdentifierSelector
  , contextualEmbeddingWithScriptSelector
  , contextualEmbeddingsForValuesSelector
  , dimensionSelector
  , embeddingResultForString_language_errorSelector
  , hasAvailableAssetsSelector
  , initSelector
  , languagesSelector
  , loadWithErrorSelector
  , maximumSequenceLengthSelector
  , modelIdentifierSelector
  , requestEmbeddingAssetsWithCompletionHandlerSelector
  , revisionSelector
  , scriptsSelector
  , unloadSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NLContextualEmbedding)
init_ nlContextualEmbedding =
  sendOwnedMessage nlContextualEmbedding initSelector

-- | @+ contextualEmbeddingWithModelIdentifier:@
contextualEmbeddingWithModelIdentifier :: IsNSString modelIdentifier => modelIdentifier -> IO (Id NLContextualEmbedding)
contextualEmbeddingWithModelIdentifier modelIdentifier =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    sendClassMessage cls' contextualEmbeddingWithModelIdentifierSelector (toNSString modelIdentifier)

-- | @+ contextualEmbeddingsForValues:@
contextualEmbeddingsForValues :: IsNSDictionary valuesDictionary => valuesDictionary -> IO (Id NSArray)
contextualEmbeddingsForValues valuesDictionary =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    sendClassMessage cls' contextualEmbeddingsForValuesSelector (toNSDictionary valuesDictionary)

-- | @+ contextualEmbeddingWithLanguage:@
contextualEmbeddingWithLanguage :: IsNSString language => language -> IO (Id NLContextualEmbedding)
contextualEmbeddingWithLanguage language =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    sendClassMessage cls' contextualEmbeddingWithLanguageSelector (toNSString language)

-- | @+ contextualEmbeddingWithScript:@
contextualEmbeddingWithScript :: IsNSString script => script -> IO (Id NLContextualEmbedding)
contextualEmbeddingWithScript script =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    sendClassMessage cls' contextualEmbeddingWithScriptSelector (toNSString script)

-- | @- loadWithError:@
loadWithError :: (IsNLContextualEmbedding nlContextualEmbedding, IsNSError error_) => nlContextualEmbedding -> error_ -> IO Bool
loadWithError nlContextualEmbedding error_ =
  sendMessage nlContextualEmbedding loadWithErrorSelector (toNSError error_)

-- | @- unload@
unload :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO ()
unload nlContextualEmbedding =
  sendMessage nlContextualEmbedding unloadSelector

-- | @- embeddingResultForString:language:error:@
embeddingResultForString_language_error :: (IsNLContextualEmbedding nlContextualEmbedding, IsNSString string, IsNSString language, IsNSError error_) => nlContextualEmbedding -> string -> language -> error_ -> IO (Id NLContextualEmbeddingResult)
embeddingResultForString_language_error nlContextualEmbedding string language error_ =
  sendMessage nlContextualEmbedding embeddingResultForString_language_errorSelector (toNSString string) (toNSString language) (toNSError error_)

-- | @- requestEmbeddingAssetsWithCompletionHandler:@
requestEmbeddingAssetsWithCompletionHandler :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> Ptr () -> IO ()
requestEmbeddingAssetsWithCompletionHandler nlContextualEmbedding completionHandler =
  sendMessage nlContextualEmbedding requestEmbeddingAssetsWithCompletionHandlerSelector completionHandler

-- | @- modelIdentifier@
modelIdentifier :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NSString)
modelIdentifier nlContextualEmbedding =
  sendMessage nlContextualEmbedding modelIdentifierSelector

-- | @- languages@
languages :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NSArray)
languages nlContextualEmbedding =
  sendMessage nlContextualEmbedding languagesSelector

-- | @- scripts@
scripts :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NSArray)
scripts nlContextualEmbedding =
  sendMessage nlContextualEmbedding scriptsSelector

-- | @- revision@
revision :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO CULong
revision nlContextualEmbedding =
  sendMessage nlContextualEmbedding revisionSelector

-- | @- dimension@
dimension :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO CULong
dimension nlContextualEmbedding =
  sendMessage nlContextualEmbedding dimensionSelector

-- | @- maximumSequenceLength@
maximumSequenceLength :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO CULong
maximumSequenceLength nlContextualEmbedding =
  sendMessage nlContextualEmbedding maximumSequenceLengthSelector

-- | @- hasAvailableAssets@
hasAvailableAssets :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO Bool
hasAvailableAssets nlContextualEmbedding =
  sendMessage nlContextualEmbedding hasAvailableAssetsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NLContextualEmbedding)
initSelector = mkSelector "init"

-- | @Selector@ for @contextualEmbeddingWithModelIdentifier:@
contextualEmbeddingWithModelIdentifierSelector :: Selector '[Id NSString] (Id NLContextualEmbedding)
contextualEmbeddingWithModelIdentifierSelector = mkSelector "contextualEmbeddingWithModelIdentifier:"

-- | @Selector@ for @contextualEmbeddingsForValues:@
contextualEmbeddingsForValuesSelector :: Selector '[Id NSDictionary] (Id NSArray)
contextualEmbeddingsForValuesSelector = mkSelector "contextualEmbeddingsForValues:"

-- | @Selector@ for @contextualEmbeddingWithLanguage:@
contextualEmbeddingWithLanguageSelector :: Selector '[Id NSString] (Id NLContextualEmbedding)
contextualEmbeddingWithLanguageSelector = mkSelector "contextualEmbeddingWithLanguage:"

-- | @Selector@ for @contextualEmbeddingWithScript:@
contextualEmbeddingWithScriptSelector :: Selector '[Id NSString] (Id NLContextualEmbedding)
contextualEmbeddingWithScriptSelector = mkSelector "contextualEmbeddingWithScript:"

-- | @Selector@ for @loadWithError:@
loadWithErrorSelector :: Selector '[Id NSError] Bool
loadWithErrorSelector = mkSelector "loadWithError:"

-- | @Selector@ for @unload@
unloadSelector :: Selector '[] ()
unloadSelector = mkSelector "unload"

-- | @Selector@ for @embeddingResultForString:language:error:@
embeddingResultForString_language_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] (Id NLContextualEmbeddingResult)
embeddingResultForString_language_errorSelector = mkSelector "embeddingResultForString:language:error:"

-- | @Selector@ for @requestEmbeddingAssetsWithCompletionHandler:@
requestEmbeddingAssetsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestEmbeddingAssetsWithCompletionHandlerSelector = mkSelector "requestEmbeddingAssetsWithCompletionHandler:"

-- | @Selector@ for @modelIdentifier@
modelIdentifierSelector :: Selector '[] (Id NSString)
modelIdentifierSelector = mkSelector "modelIdentifier"

-- | @Selector@ for @languages@
languagesSelector :: Selector '[] (Id NSArray)
languagesSelector = mkSelector "languages"

-- | @Selector@ for @scripts@
scriptsSelector :: Selector '[] (Id NSArray)
scriptsSelector = mkSelector "scripts"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] CULong
revisionSelector = mkSelector "revision"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CULong
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @maximumSequenceLength@
maximumSequenceLengthSelector :: Selector '[] CULong
maximumSequenceLengthSelector = mkSelector "maximumSequenceLength"

-- | @Selector@ for @hasAvailableAssets@
hasAvailableAssetsSelector :: Selector '[] Bool
hasAvailableAssetsSelector = mkSelector "hasAvailableAssets"

