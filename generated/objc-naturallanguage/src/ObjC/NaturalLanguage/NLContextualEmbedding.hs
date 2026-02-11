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
  , initSelector
  , contextualEmbeddingWithModelIdentifierSelector
  , contextualEmbeddingsForValuesSelector
  , contextualEmbeddingWithLanguageSelector
  , contextualEmbeddingWithScriptSelector
  , loadWithErrorSelector
  , unloadSelector
  , embeddingResultForString_language_errorSelector
  , requestEmbeddingAssetsWithCompletionHandlerSelector
  , modelIdentifierSelector
  , languagesSelector
  , scriptsSelector
  , revisionSelector
  , dimensionSelector
  , maximumSequenceLengthSelector
  , hasAvailableAssetsSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NLContextualEmbedding)
init_ nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ contextualEmbeddingWithModelIdentifier:@
contextualEmbeddingWithModelIdentifier :: IsNSString modelIdentifier => modelIdentifier -> IO (Id NLContextualEmbedding)
contextualEmbeddingWithModelIdentifier modelIdentifier =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    withObjCPtr modelIdentifier $ \raw_modelIdentifier ->
      sendClassMsg cls' (mkSelector "contextualEmbeddingWithModelIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_modelIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextualEmbeddingsForValues:@
contextualEmbeddingsForValues :: IsNSDictionary valuesDictionary => valuesDictionary -> IO (Id NSArray)
contextualEmbeddingsForValues valuesDictionary =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    withObjCPtr valuesDictionary $ \raw_valuesDictionary ->
      sendClassMsg cls' (mkSelector "contextualEmbeddingsForValues:") (retPtr retVoid) [argPtr (castPtr raw_valuesDictionary :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextualEmbeddingWithLanguage:@
contextualEmbeddingWithLanguage :: IsNSString language => language -> IO (Id NLContextualEmbedding)
contextualEmbeddingWithLanguage language =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "contextualEmbeddingWithLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextualEmbeddingWithScript:@
contextualEmbeddingWithScript :: IsNSString script => script -> IO (Id NLContextualEmbedding)
contextualEmbeddingWithScript script =
  do
    cls' <- getRequiredClass "NLContextualEmbedding"
    withObjCPtr script $ \raw_script ->
      sendClassMsg cls' (mkSelector "contextualEmbeddingWithScript:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ())] >>= retainedObject . castPtr

-- | @- loadWithError:@
loadWithError :: (IsNLContextualEmbedding nlContextualEmbedding, IsNSError error_) => nlContextualEmbedding -> error_ -> IO Bool
loadWithError nlContextualEmbedding  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nlContextualEmbedding (mkSelector "loadWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- unload@
unload :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO ()
unload nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "unload") retVoid []

-- | @- embeddingResultForString:language:error:@
embeddingResultForString_language_error :: (IsNLContextualEmbedding nlContextualEmbedding, IsNSString string, IsNSString language, IsNSError error_) => nlContextualEmbedding -> string -> language -> error_ -> IO (Id NLContextualEmbeddingResult)
embeddingResultForString_language_error nlContextualEmbedding  string language error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr language $ \raw_language ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nlContextualEmbedding (mkSelector "embeddingResultForString:language:error:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- requestEmbeddingAssetsWithCompletionHandler:@
requestEmbeddingAssetsWithCompletionHandler :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> Ptr () -> IO ()
requestEmbeddingAssetsWithCompletionHandler nlContextualEmbedding  completionHandler =
  sendMsg nlContextualEmbedding (mkSelector "requestEmbeddingAssetsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- modelIdentifier@
modelIdentifier :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NSString)
modelIdentifier nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "modelIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- languages@
languages :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NSArray)
languages nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "languages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scripts@
scripts :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO (Id NSArray)
scripts nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "scripts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- revision@
revision :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO CULong
revision nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "revision") retCULong []

-- | @- dimension@
dimension :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO CULong
dimension nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "dimension") retCULong []

-- | @- maximumSequenceLength@
maximumSequenceLength :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO CULong
maximumSequenceLength nlContextualEmbedding  =
  sendMsg nlContextualEmbedding (mkSelector "maximumSequenceLength") retCULong []

-- | @- hasAvailableAssets@
hasAvailableAssets :: IsNLContextualEmbedding nlContextualEmbedding => nlContextualEmbedding -> IO Bool
hasAvailableAssets nlContextualEmbedding  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nlContextualEmbedding (mkSelector "hasAvailableAssets") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @contextualEmbeddingWithModelIdentifier:@
contextualEmbeddingWithModelIdentifierSelector :: Selector
contextualEmbeddingWithModelIdentifierSelector = mkSelector "contextualEmbeddingWithModelIdentifier:"

-- | @Selector@ for @contextualEmbeddingsForValues:@
contextualEmbeddingsForValuesSelector :: Selector
contextualEmbeddingsForValuesSelector = mkSelector "contextualEmbeddingsForValues:"

-- | @Selector@ for @contextualEmbeddingWithLanguage:@
contextualEmbeddingWithLanguageSelector :: Selector
contextualEmbeddingWithLanguageSelector = mkSelector "contextualEmbeddingWithLanguage:"

-- | @Selector@ for @contextualEmbeddingWithScript:@
contextualEmbeddingWithScriptSelector :: Selector
contextualEmbeddingWithScriptSelector = mkSelector "contextualEmbeddingWithScript:"

-- | @Selector@ for @loadWithError:@
loadWithErrorSelector :: Selector
loadWithErrorSelector = mkSelector "loadWithError:"

-- | @Selector@ for @unload@
unloadSelector :: Selector
unloadSelector = mkSelector "unload"

-- | @Selector@ for @embeddingResultForString:language:error:@
embeddingResultForString_language_errorSelector :: Selector
embeddingResultForString_language_errorSelector = mkSelector "embeddingResultForString:language:error:"

-- | @Selector@ for @requestEmbeddingAssetsWithCompletionHandler:@
requestEmbeddingAssetsWithCompletionHandlerSelector :: Selector
requestEmbeddingAssetsWithCompletionHandlerSelector = mkSelector "requestEmbeddingAssetsWithCompletionHandler:"

-- | @Selector@ for @modelIdentifier@
modelIdentifierSelector :: Selector
modelIdentifierSelector = mkSelector "modelIdentifier"

-- | @Selector@ for @languages@
languagesSelector :: Selector
languagesSelector = mkSelector "languages"

-- | @Selector@ for @scripts@
scriptsSelector :: Selector
scriptsSelector = mkSelector "scripts"

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

-- | @Selector@ for @maximumSequenceLength@
maximumSequenceLengthSelector :: Selector
maximumSequenceLengthSelector = mkSelector "maximumSequenceLength"

-- | @Selector@ for @hasAvailableAssets@
hasAvailableAssetsSelector :: Selector
hasAvailableAssetsSelector = mkSelector "hasAvailableAssets"

