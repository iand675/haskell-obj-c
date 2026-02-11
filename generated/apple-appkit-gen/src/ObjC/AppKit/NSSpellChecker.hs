{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSpellChecker@.
module ObjC.AppKit.NSSpellChecker
  ( NSSpellChecker
  , IsNSSpellChecker(..)
  , uniqueSpellDocumentTag
  , checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount
  , checkSpellingOfString_startingAt
  , countWordsInString_language
  , checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details
  , checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCount
  , menuForResult_string_options_atLocation_inView
  , userQuotesArrayForLanguage
  , updateSpellingPanelWithMisspelledWord
  , updateSpellingPanelWithGrammarString_detail
  , updatePanels
  , ignoreWord_inSpellDocumentWithTag
  , ignoredWordsInSpellDocumentWithTag
  , setIgnoredWords_inSpellDocumentWithTag
  , guessesForWordRange_inString_language_inSpellDocumentWithTag
  , correctionForWordRange_inString_language_inSpellDocumentWithTag
  , completionsForPartialWordRange_inString_language_inSpellDocumentWithTag
  , languageForWordRange_inString_orthography
  , closeSpellDocumentWithTag
  , recordResponse_toCorrection_forWord_language_inSpellDocumentWithTag
  , showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandler
  , dismissCorrectionIndicatorForView
  , showInlinePredictionForCandidates_client
  , preventsAutocorrectionBeforeString_language
  , deletesAutospaceBetweenString_andString_language
  , setWordFieldStringValue
  , learnWord
  , hasLearnedWord
  , unlearnWord
  , language
  , setLanguage
  , guessesForWord
  , forgetWord
  , sharedSpellChecker
  , sharedSpellCheckerExists
  , userReplacementsDictionary
  , spellingPanel
  , accessoryView
  , setAccessoryView
  , substitutionsPanel
  , substitutionsPanelAccessoryViewController
  , setSubstitutionsPanelAccessoryViewController
  , availableLanguages
  , userPreferredLanguages
  , automaticallyIdentifiesLanguages
  , setAutomaticallyIdentifiesLanguages
  , automaticTextReplacementEnabled
  , automaticSpellingCorrectionEnabled
  , automaticQuoteSubstitutionEnabled
  , automaticDashSubstitutionEnabled
  , automaticCapitalizationEnabled
  , automaticPeriodSubstitutionEnabled
  , automaticTextCompletionEnabled
  , automaticInlinePredictionEnabled
  , uniqueSpellDocumentTagSelector
  , checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector
  , checkSpellingOfString_startingAtSelector
  , countWordsInString_languageSelector
  , checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector
  , checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector
  , menuForResult_string_options_atLocation_inViewSelector
  , userQuotesArrayForLanguageSelector
  , updateSpellingPanelWithMisspelledWordSelector
  , updateSpellingPanelWithGrammarString_detailSelector
  , updatePanelsSelector
  , ignoreWord_inSpellDocumentWithTagSelector
  , ignoredWordsInSpellDocumentWithTagSelector
  , setIgnoredWords_inSpellDocumentWithTagSelector
  , guessesForWordRange_inString_language_inSpellDocumentWithTagSelector
  , correctionForWordRange_inString_language_inSpellDocumentWithTagSelector
  , completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector
  , languageForWordRange_inString_orthographySelector
  , closeSpellDocumentWithTagSelector
  , recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector
  , showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector
  , dismissCorrectionIndicatorForViewSelector
  , showInlinePredictionForCandidates_clientSelector
  , preventsAutocorrectionBeforeString_languageSelector
  , deletesAutospaceBetweenString_andString_languageSelector
  , setWordFieldStringValueSelector
  , learnWordSelector
  , hasLearnedWordSelector
  , unlearnWordSelector
  , languageSelector
  , setLanguageSelector
  , guessesForWordSelector
  , forgetWordSelector
  , sharedSpellCheckerSelector
  , sharedSpellCheckerExistsSelector
  , userReplacementsDictionarySelector
  , spellingPanelSelector
  , accessoryViewSelector
  , setAccessoryViewSelector
  , substitutionsPanelSelector
  , substitutionsPanelAccessoryViewControllerSelector
  , setSubstitutionsPanelAccessoryViewControllerSelector
  , availableLanguagesSelector
  , userPreferredLanguagesSelector
  , automaticallyIdentifiesLanguagesSelector
  , setAutomaticallyIdentifiesLanguagesSelector
  , automaticTextReplacementEnabledSelector
  , automaticSpellingCorrectionEnabledSelector
  , automaticQuoteSubstitutionEnabledSelector
  , automaticDashSubstitutionEnabledSelector
  , automaticCapitalizationEnabledSelector
  , automaticPeriodSubstitutionEnabledSelector
  , automaticTextCompletionEnabledSelector
  , automaticInlinePredictionEnabledSelector

  -- * Enum types
  , NSCorrectionIndicatorType(NSCorrectionIndicatorType)
  , pattern NSCorrectionIndicatorTypeDefault
  , pattern NSCorrectionIndicatorTypeReversion
  , pattern NSCorrectionIndicatorTypeGuesses
  , NSCorrectionResponse(NSCorrectionResponse)
  , pattern NSCorrectionResponseNone
  , pattern NSCorrectionResponseAccepted
  , pattern NSCorrectionResponseRejected
  , pattern NSCorrectionResponseIgnored
  , pattern NSCorrectionResponseEdited
  , pattern NSCorrectionResponseReverted

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ uniqueSpellDocumentTag@
uniqueSpellDocumentTag :: IO CLong
uniqueSpellDocumentTag  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMsg cls' (mkSelector "uniqueSpellDocumentTag") retCLong []

-- | @- checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:@
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck, IsNSString language) => nsSpellChecker -> stringToCheck -> CLong -> language -> Bool -> CLong -> Ptr CLong -> IO NSRange
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount nsSpellChecker  stringToCheck startingOffset language wrapFlag tag wordCount =
  withObjCPtr stringToCheck $ \raw_stringToCheck ->
    withObjCPtr language $ \raw_language ->
        sendMsgStret nsSpellChecker (mkSelector "checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:") retNSRange [argPtr (castPtr raw_stringToCheck :: Ptr ()), argCLong startingOffset, argPtr (castPtr raw_language :: Ptr ()), argCULong (if wrapFlag then 1 else 0), argCLong tag, argPtr wordCount]

-- | @- checkSpellingOfString:startingAt:@
checkSpellingOfString_startingAt :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck) => nsSpellChecker -> stringToCheck -> CLong -> IO NSRange
checkSpellingOfString_startingAt nsSpellChecker  stringToCheck startingOffset =
  withObjCPtr stringToCheck $ \raw_stringToCheck ->
      sendMsgStret nsSpellChecker (mkSelector "checkSpellingOfString:startingAt:") retNSRange [argPtr (castPtr raw_stringToCheck :: Ptr ()), argCLong startingOffset]

-- | @- countWordsInString:language:@
countWordsInString_language :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCount, IsNSString language) => nsSpellChecker -> stringToCount -> language -> IO CLong
countWordsInString_language nsSpellChecker  stringToCount language =
  withObjCPtr stringToCount $ \raw_stringToCount ->
    withObjCPtr language $ \raw_language ->
        sendMsg nsSpellChecker (mkSelector "countWordsInString:language:") retCLong [argPtr (castPtr raw_stringToCount :: Ptr ()), argPtr (castPtr raw_language :: Ptr ())]

-- | @- checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:@
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck, IsNSString language, IsNSArray details) => nsSpellChecker -> stringToCheck -> CLong -> language -> Bool -> CLong -> details -> IO NSRange
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details nsSpellChecker  stringToCheck startingOffset language wrapFlag tag details =
  withObjCPtr stringToCheck $ \raw_stringToCheck ->
    withObjCPtr language $ \raw_language ->
      withObjCPtr details $ \raw_details ->
          sendMsgStret nsSpellChecker (mkSelector "checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:") retNSRange [argPtr (castPtr raw_stringToCheck :: Ptr ()), argCLong startingOffset, argPtr (castPtr raw_language :: Ptr ()), argCULong (if wrapFlag then 1 else 0), argCLong tag, argPtr (castPtr raw_details :: Ptr ())]

-- | @- checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:@
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCount :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck, IsNSDictionary options, IsNSOrthography orthography) => nsSpellChecker -> stringToCheck -> NSRange -> CULong -> options -> CLong -> orthography -> Ptr CLong -> IO (Id NSArray)
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCount nsSpellChecker  stringToCheck range checkingTypes options tag orthography wordCount =
  withObjCPtr stringToCheck $ \raw_stringToCheck ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr orthography $ \raw_orthography ->
          sendMsg nsSpellChecker (mkSelector "checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:") (retPtr retVoid) [argPtr (castPtr raw_stringToCheck :: Ptr ()), argNSRange range, argCULong checkingTypes, argPtr (castPtr raw_options :: Ptr ()), argCLong tag, argPtr (castPtr raw_orthography :: Ptr ()), argPtr wordCount] >>= retainedObject . castPtr

-- | @- menuForResult:string:options:atLocation:inView:@
menuForResult_string_options_atLocation_inView :: (IsNSSpellChecker nsSpellChecker, IsNSTextCheckingResult result, IsNSString checkedString, IsNSDictionary options, IsNSView view) => nsSpellChecker -> result -> checkedString -> options -> NSPoint -> view -> IO (Id NSMenu)
menuForResult_string_options_atLocation_inView nsSpellChecker  result checkedString options location view =
  withObjCPtr result $ \raw_result ->
    withObjCPtr checkedString $ \raw_checkedString ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr view $ \raw_view ->
            sendMsg nsSpellChecker (mkSelector "menuForResult:string:options:atLocation:inView:") (retPtr retVoid) [argPtr (castPtr raw_result :: Ptr ()), argPtr (castPtr raw_checkedString :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argNSPoint location, argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- userQuotesArrayForLanguage:@
userQuotesArrayForLanguage :: (IsNSSpellChecker nsSpellChecker, IsNSString language) => nsSpellChecker -> language -> IO (Id NSArray)
userQuotesArrayForLanguage nsSpellChecker  language =
  withObjCPtr language $ \raw_language ->
      sendMsg nsSpellChecker (mkSelector "userQuotesArrayForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- updateSpellingPanelWithMisspelledWord:@
updateSpellingPanelWithMisspelledWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
updateSpellingPanelWithMisspelledWord nsSpellChecker  word =
  withObjCPtr word $ \raw_word ->
      sendMsg nsSpellChecker (mkSelector "updateSpellingPanelWithMisspelledWord:") retVoid [argPtr (castPtr raw_word :: Ptr ())]

-- | @- updateSpellingPanelWithGrammarString:detail:@
updateSpellingPanelWithGrammarString_detail :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSDictionary detail) => nsSpellChecker -> string -> detail -> IO ()
updateSpellingPanelWithGrammarString_detail nsSpellChecker  string detail =
  withObjCPtr string $ \raw_string ->
    withObjCPtr detail $ \raw_detail ->
        sendMsg nsSpellChecker (mkSelector "updateSpellingPanelWithGrammarString:detail:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_detail :: Ptr ())]

-- | @- updatePanels@
updatePanels :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO ()
updatePanels nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "updatePanels") retVoid []

-- | @- ignoreWord:inSpellDocumentWithTag:@
ignoreWord_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString wordToIgnore) => nsSpellChecker -> wordToIgnore -> CLong -> IO ()
ignoreWord_inSpellDocumentWithTag nsSpellChecker  wordToIgnore tag =
  withObjCPtr wordToIgnore $ \raw_wordToIgnore ->
      sendMsg nsSpellChecker (mkSelector "ignoreWord:inSpellDocumentWithTag:") retVoid [argPtr (castPtr raw_wordToIgnore :: Ptr ()), argCLong tag]

-- | @- ignoredWordsInSpellDocumentWithTag:@
ignoredWordsInSpellDocumentWithTag :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> CLong -> IO (Id NSArray)
ignoredWordsInSpellDocumentWithTag nsSpellChecker  tag =
    sendMsg nsSpellChecker (mkSelector "ignoredWordsInSpellDocumentWithTag:") (retPtr retVoid) [argCLong tag] >>= retainedObject . castPtr

-- | @- setIgnoredWords:inSpellDocumentWithTag:@
setIgnoredWords_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSArray words_) => nsSpellChecker -> words_ -> CLong -> IO ()
setIgnoredWords_inSpellDocumentWithTag nsSpellChecker  words_ tag =
  withObjCPtr words_ $ \raw_words_ ->
      sendMsg nsSpellChecker (mkSelector "setIgnoredWords:inSpellDocumentWithTag:") retVoid [argPtr (castPtr raw_words_ :: Ptr ()), argCLong tag]

-- | @- guessesForWordRange:inString:language:inSpellDocumentWithTag:@
guessesForWordRange_inString_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> NSRange -> string -> language -> CLong -> IO (Id NSArray)
guessesForWordRange_inString_language_inSpellDocumentWithTag nsSpellChecker  range string language tag =
  withObjCPtr string $ \raw_string ->
    withObjCPtr language $ \raw_language ->
        sendMsg nsSpellChecker (mkSelector "guessesForWordRange:inString:language:inSpellDocumentWithTag:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argCLong tag] >>= retainedObject . castPtr

-- | @- correctionForWordRange:inString:language:inSpellDocumentWithTag:@
correctionForWordRange_inString_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> NSRange -> string -> language -> CLong -> IO (Id NSString)
correctionForWordRange_inString_language_inSpellDocumentWithTag nsSpellChecker  range string language tag =
  withObjCPtr string $ \raw_string ->
    withObjCPtr language $ \raw_language ->
        sendMsg nsSpellChecker (mkSelector "correctionForWordRange:inString:language:inSpellDocumentWithTag:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argCLong tag] >>= retainedObject . castPtr

-- | @- completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:@
completionsForPartialWordRange_inString_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> NSRange -> string -> language -> CLong -> IO (Id NSArray)
completionsForPartialWordRange_inString_language_inSpellDocumentWithTag nsSpellChecker  range string language tag =
  withObjCPtr string $ \raw_string ->
    withObjCPtr language $ \raw_language ->
        sendMsg nsSpellChecker (mkSelector "completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argCLong tag] >>= retainedObject . castPtr

-- | @- languageForWordRange:inString:orthography:@
languageForWordRange_inString_orthography :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSOrthography orthography) => nsSpellChecker -> NSRange -> string -> orthography -> IO (Id NSString)
languageForWordRange_inString_orthography nsSpellChecker  range string orthography =
  withObjCPtr string $ \raw_string ->
    withObjCPtr orthography $ \raw_orthography ->
        sendMsg nsSpellChecker (mkSelector "languageForWordRange:inString:orthography:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_orthography :: Ptr ())] >>= retainedObject . castPtr

-- | @- closeSpellDocumentWithTag:@
closeSpellDocumentWithTag :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> CLong -> IO ()
closeSpellDocumentWithTag nsSpellChecker  tag =
    sendMsg nsSpellChecker (mkSelector "closeSpellDocumentWithTag:") retVoid [argCLong tag]

-- | @- recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:@
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString correction, IsNSString word, IsNSString language) => nsSpellChecker -> NSCorrectionResponse -> correction -> word -> language -> CLong -> IO ()
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTag nsSpellChecker  response correction word language tag =
  withObjCPtr correction $ \raw_correction ->
    withObjCPtr word $ \raw_word ->
      withObjCPtr language $ \raw_language ->
          sendMsg nsSpellChecker (mkSelector "recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:") retVoid [argCLong (coerce response), argPtr (castPtr raw_correction :: Ptr ()), argPtr (castPtr raw_word :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argCLong tag]

-- | @- showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:@
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandler :: (IsNSSpellChecker nsSpellChecker, IsNSString primaryString, IsNSArray alternativeStrings, IsNSView view) => nsSpellChecker -> NSCorrectionIndicatorType -> primaryString -> alternativeStrings -> NSRect -> view -> Ptr () -> IO ()
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandler nsSpellChecker  type_ primaryString alternativeStrings rectOfTypedString view completionBlock =
  withObjCPtr primaryString $ \raw_primaryString ->
    withObjCPtr alternativeStrings $ \raw_alternativeStrings ->
      withObjCPtr view $ \raw_view ->
          sendMsg nsSpellChecker (mkSelector "showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:") retVoid [argCLong (coerce type_), argPtr (castPtr raw_primaryString :: Ptr ()), argPtr (castPtr raw_alternativeStrings :: Ptr ()), argNSRect rectOfTypedString, argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr completionBlock :: Ptr ())]

-- | @- dismissCorrectionIndicatorForView:@
dismissCorrectionIndicatorForView :: (IsNSSpellChecker nsSpellChecker, IsNSView view) => nsSpellChecker -> view -> IO ()
dismissCorrectionIndicatorForView nsSpellChecker  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsSpellChecker (mkSelector "dismissCorrectionIndicatorForView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- showInlinePredictionForCandidates:client:@
showInlinePredictionForCandidates_client :: (IsNSSpellChecker nsSpellChecker, IsNSArray candidates) => nsSpellChecker -> candidates -> RawId -> IO ()
showInlinePredictionForCandidates_client nsSpellChecker  candidates client =
  withObjCPtr candidates $ \raw_candidates ->
      sendMsg nsSpellChecker (mkSelector "showInlinePredictionForCandidates:client:") retVoid [argPtr (castPtr raw_candidates :: Ptr ()), argPtr (castPtr (unRawId client) :: Ptr ())]

-- | @- preventsAutocorrectionBeforeString:language:@
preventsAutocorrectionBeforeString_language :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> string -> language -> IO Bool
preventsAutocorrectionBeforeString_language nsSpellChecker  string language =
  withObjCPtr string $ \raw_string ->
    withObjCPtr language $ \raw_language ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellChecker (mkSelector "preventsAutocorrectionBeforeString:language:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_language :: Ptr ())]

-- | @- deletesAutospaceBetweenString:andString:language:@
deletesAutospaceBetweenString_andString_language :: (IsNSSpellChecker nsSpellChecker, IsNSString precedingString, IsNSString followingString, IsNSString language) => nsSpellChecker -> precedingString -> followingString -> language -> IO Bool
deletesAutospaceBetweenString_andString_language nsSpellChecker  precedingString followingString language =
  withObjCPtr precedingString $ \raw_precedingString ->
    withObjCPtr followingString $ \raw_followingString ->
      withObjCPtr language $ \raw_language ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellChecker (mkSelector "deletesAutospaceBetweenString:andString:language:") retCULong [argPtr (castPtr raw_precedingString :: Ptr ()), argPtr (castPtr raw_followingString :: Ptr ()), argPtr (castPtr raw_language :: Ptr ())]

-- | @- setWordFieldStringValue:@
setWordFieldStringValue :: (IsNSSpellChecker nsSpellChecker, IsNSString string) => nsSpellChecker -> string -> IO ()
setWordFieldStringValue nsSpellChecker  string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsSpellChecker (mkSelector "setWordFieldStringValue:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- learnWord:@
learnWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
learnWord nsSpellChecker  word =
  withObjCPtr word $ \raw_word ->
      sendMsg nsSpellChecker (mkSelector "learnWord:") retVoid [argPtr (castPtr raw_word :: Ptr ())]

-- | @- hasLearnedWord:@
hasLearnedWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO Bool
hasLearnedWord nsSpellChecker  word =
  withObjCPtr word $ \raw_word ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellChecker (mkSelector "hasLearnedWord:") retCULong [argPtr (castPtr raw_word :: Ptr ())]

-- | @- unlearnWord:@
unlearnWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
unlearnWord nsSpellChecker  word =
  withObjCPtr word $ \raw_word ->
      sendMsg nsSpellChecker (mkSelector "unlearnWord:") retVoid [argPtr (castPtr raw_word :: Ptr ())]

-- | @- language@
language :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSString)
language nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguage:@
setLanguage :: (IsNSSpellChecker nsSpellChecker, IsNSString language) => nsSpellChecker -> language -> IO Bool
setLanguage nsSpellChecker  language =
  withObjCPtr language $ \raw_language ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellChecker (mkSelector "setLanguage:") retCULong [argPtr (castPtr raw_language :: Ptr ())]

-- | @- guessesForWord:@
guessesForWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO (Id NSArray)
guessesForWord nsSpellChecker  word =
  withObjCPtr word $ \raw_word ->
      sendMsg nsSpellChecker (mkSelector "guessesForWord:") (retPtr retVoid) [argPtr (castPtr raw_word :: Ptr ())] >>= retainedObject . castPtr

-- | @- forgetWord:@
forgetWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
forgetWord nsSpellChecker  word =
  withObjCPtr word $ \raw_word ->
      sendMsg nsSpellChecker (mkSelector "forgetWord:") retVoid [argPtr (castPtr raw_word :: Ptr ())]

-- | @+ sharedSpellChecker@
sharedSpellChecker :: IO (Id NSSpellChecker)
sharedSpellChecker  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMsg cls' (mkSelector "sharedSpellChecker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sharedSpellCheckerExists@
sharedSpellCheckerExists :: IO Bool
sharedSpellCheckerExists  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "sharedSpellCheckerExists") retCULong []

-- | @- userReplacementsDictionary@
userReplacementsDictionary :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSDictionary)
userReplacementsDictionary nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "userReplacementsDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- spellingPanel@
spellingPanel :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSPanel)
spellingPanel nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "spellingPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accessoryView@
accessoryView :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSView)
accessoryView nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSSpellChecker nsSpellChecker, IsNSView value) => nsSpellChecker -> value -> IO ()
setAccessoryView nsSpellChecker  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSpellChecker (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- substitutionsPanel@
substitutionsPanel :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSPanel)
substitutionsPanel nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "substitutionsPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- substitutionsPanelAccessoryViewController@
substitutionsPanelAccessoryViewController :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSViewController)
substitutionsPanelAccessoryViewController nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "substitutionsPanelAccessoryViewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubstitutionsPanelAccessoryViewController:@
setSubstitutionsPanelAccessoryViewController :: (IsNSSpellChecker nsSpellChecker, IsNSViewController value) => nsSpellChecker -> value -> IO ()
setSubstitutionsPanelAccessoryViewController nsSpellChecker  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSpellChecker (mkSelector "setSubstitutionsPanelAccessoryViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- availableLanguages@
availableLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSArray)
availableLanguages nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "availableLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userPreferredLanguages@
userPreferredLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSArray)
userPreferredLanguages nsSpellChecker  =
    sendMsg nsSpellChecker (mkSelector "userPreferredLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- automaticallyIdentifiesLanguages@
automaticallyIdentifiesLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO Bool
automaticallyIdentifiesLanguages nsSpellChecker  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellChecker (mkSelector "automaticallyIdentifiesLanguages") retCULong []

-- | @- setAutomaticallyIdentifiesLanguages:@
setAutomaticallyIdentifiesLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> Bool -> IO ()
setAutomaticallyIdentifiesLanguages nsSpellChecker  value =
    sendMsg nsSpellChecker (mkSelector "setAutomaticallyIdentifiesLanguages:") retVoid [argCULong (if value then 1 else 0)]

-- | @+ automaticTextReplacementEnabled@
automaticTextReplacementEnabled :: IO Bool
automaticTextReplacementEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticTextReplacementEnabled") retCULong []

-- | @+ automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabled :: IO Bool
automaticSpellingCorrectionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticSpellingCorrectionEnabled") retCULong []

-- | @+ automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabled :: IO Bool
automaticQuoteSubstitutionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticQuoteSubstitutionEnabled") retCULong []

-- | @+ automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabled :: IO Bool
automaticDashSubstitutionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticDashSubstitutionEnabled") retCULong []

-- | @+ automaticCapitalizationEnabled@
automaticCapitalizationEnabled :: IO Bool
automaticCapitalizationEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticCapitalizationEnabled") retCULong []

-- | @+ automaticPeriodSubstitutionEnabled@
automaticPeriodSubstitutionEnabled :: IO Bool
automaticPeriodSubstitutionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticPeriodSubstitutionEnabled") retCULong []

-- | @+ automaticTextCompletionEnabled@
automaticTextCompletionEnabled :: IO Bool
automaticTextCompletionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticTextCompletionEnabled") retCULong []

-- | @+ automaticInlinePredictionEnabled@
automaticInlinePredictionEnabled :: IO Bool
automaticInlinePredictionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticInlinePredictionEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueSpellDocumentTag@
uniqueSpellDocumentTagSelector :: Selector
uniqueSpellDocumentTagSelector = mkSelector "uniqueSpellDocumentTag"

-- | @Selector@ for @checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:@
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector :: Selector
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector = mkSelector "checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:"

-- | @Selector@ for @checkSpellingOfString:startingAt:@
checkSpellingOfString_startingAtSelector :: Selector
checkSpellingOfString_startingAtSelector = mkSelector "checkSpellingOfString:startingAt:"

-- | @Selector@ for @countWordsInString:language:@
countWordsInString_languageSelector :: Selector
countWordsInString_languageSelector = mkSelector "countWordsInString:language:"

-- | @Selector@ for @checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:@
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector :: Selector
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector = mkSelector "checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:"

-- | @Selector@ for @checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:@
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector :: Selector
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector = mkSelector "checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:"

-- | @Selector@ for @menuForResult:string:options:atLocation:inView:@
menuForResult_string_options_atLocation_inViewSelector :: Selector
menuForResult_string_options_atLocation_inViewSelector = mkSelector "menuForResult:string:options:atLocation:inView:"

-- | @Selector@ for @userQuotesArrayForLanguage:@
userQuotesArrayForLanguageSelector :: Selector
userQuotesArrayForLanguageSelector = mkSelector "userQuotesArrayForLanguage:"

-- | @Selector@ for @updateSpellingPanelWithMisspelledWord:@
updateSpellingPanelWithMisspelledWordSelector :: Selector
updateSpellingPanelWithMisspelledWordSelector = mkSelector "updateSpellingPanelWithMisspelledWord:"

-- | @Selector@ for @updateSpellingPanelWithGrammarString:detail:@
updateSpellingPanelWithGrammarString_detailSelector :: Selector
updateSpellingPanelWithGrammarString_detailSelector = mkSelector "updateSpellingPanelWithGrammarString:detail:"

-- | @Selector@ for @updatePanels@
updatePanelsSelector :: Selector
updatePanelsSelector = mkSelector "updatePanels"

-- | @Selector@ for @ignoreWord:inSpellDocumentWithTag:@
ignoreWord_inSpellDocumentWithTagSelector :: Selector
ignoreWord_inSpellDocumentWithTagSelector = mkSelector "ignoreWord:inSpellDocumentWithTag:"

-- | @Selector@ for @ignoredWordsInSpellDocumentWithTag:@
ignoredWordsInSpellDocumentWithTagSelector :: Selector
ignoredWordsInSpellDocumentWithTagSelector = mkSelector "ignoredWordsInSpellDocumentWithTag:"

-- | @Selector@ for @setIgnoredWords:inSpellDocumentWithTag:@
setIgnoredWords_inSpellDocumentWithTagSelector :: Selector
setIgnoredWords_inSpellDocumentWithTagSelector = mkSelector "setIgnoredWords:inSpellDocumentWithTag:"

-- | @Selector@ for @guessesForWordRange:inString:language:inSpellDocumentWithTag:@
guessesForWordRange_inString_language_inSpellDocumentWithTagSelector :: Selector
guessesForWordRange_inString_language_inSpellDocumentWithTagSelector = mkSelector "guessesForWordRange:inString:language:inSpellDocumentWithTag:"

-- | @Selector@ for @correctionForWordRange:inString:language:inSpellDocumentWithTag:@
correctionForWordRange_inString_language_inSpellDocumentWithTagSelector :: Selector
correctionForWordRange_inString_language_inSpellDocumentWithTagSelector = mkSelector "correctionForWordRange:inString:language:inSpellDocumentWithTag:"

-- | @Selector@ for @completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:@
completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector :: Selector
completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector = mkSelector "completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:"

-- | @Selector@ for @languageForWordRange:inString:orthography:@
languageForWordRange_inString_orthographySelector :: Selector
languageForWordRange_inString_orthographySelector = mkSelector "languageForWordRange:inString:orthography:"

-- | @Selector@ for @closeSpellDocumentWithTag:@
closeSpellDocumentWithTagSelector :: Selector
closeSpellDocumentWithTagSelector = mkSelector "closeSpellDocumentWithTag:"

-- | @Selector@ for @recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:@
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector :: Selector
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector = mkSelector "recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:"

-- | @Selector@ for @showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:@
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector :: Selector
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector = mkSelector "showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:"

-- | @Selector@ for @dismissCorrectionIndicatorForView:@
dismissCorrectionIndicatorForViewSelector :: Selector
dismissCorrectionIndicatorForViewSelector = mkSelector "dismissCorrectionIndicatorForView:"

-- | @Selector@ for @showInlinePredictionForCandidates:client:@
showInlinePredictionForCandidates_clientSelector :: Selector
showInlinePredictionForCandidates_clientSelector = mkSelector "showInlinePredictionForCandidates:client:"

-- | @Selector@ for @preventsAutocorrectionBeforeString:language:@
preventsAutocorrectionBeforeString_languageSelector :: Selector
preventsAutocorrectionBeforeString_languageSelector = mkSelector "preventsAutocorrectionBeforeString:language:"

-- | @Selector@ for @deletesAutospaceBetweenString:andString:language:@
deletesAutospaceBetweenString_andString_languageSelector :: Selector
deletesAutospaceBetweenString_andString_languageSelector = mkSelector "deletesAutospaceBetweenString:andString:language:"

-- | @Selector@ for @setWordFieldStringValue:@
setWordFieldStringValueSelector :: Selector
setWordFieldStringValueSelector = mkSelector "setWordFieldStringValue:"

-- | @Selector@ for @learnWord:@
learnWordSelector :: Selector
learnWordSelector = mkSelector "learnWord:"

-- | @Selector@ for @hasLearnedWord:@
hasLearnedWordSelector :: Selector
hasLearnedWordSelector = mkSelector "hasLearnedWord:"

-- | @Selector@ for @unlearnWord:@
unlearnWordSelector :: Selector
unlearnWordSelector = mkSelector "unlearnWord:"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @guessesForWord:@
guessesForWordSelector :: Selector
guessesForWordSelector = mkSelector "guessesForWord:"

-- | @Selector@ for @forgetWord:@
forgetWordSelector :: Selector
forgetWordSelector = mkSelector "forgetWord:"

-- | @Selector@ for @sharedSpellChecker@
sharedSpellCheckerSelector :: Selector
sharedSpellCheckerSelector = mkSelector "sharedSpellChecker"

-- | @Selector@ for @sharedSpellCheckerExists@
sharedSpellCheckerExistsSelector :: Selector
sharedSpellCheckerExistsSelector = mkSelector "sharedSpellCheckerExists"

-- | @Selector@ for @userReplacementsDictionary@
userReplacementsDictionarySelector :: Selector
userReplacementsDictionarySelector = mkSelector "userReplacementsDictionary"

-- | @Selector@ for @spellingPanel@
spellingPanelSelector :: Selector
spellingPanelSelector = mkSelector "spellingPanel"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @substitutionsPanel@
substitutionsPanelSelector :: Selector
substitutionsPanelSelector = mkSelector "substitutionsPanel"

-- | @Selector@ for @substitutionsPanelAccessoryViewController@
substitutionsPanelAccessoryViewControllerSelector :: Selector
substitutionsPanelAccessoryViewControllerSelector = mkSelector "substitutionsPanelAccessoryViewController"

-- | @Selector@ for @setSubstitutionsPanelAccessoryViewController:@
setSubstitutionsPanelAccessoryViewControllerSelector :: Selector
setSubstitutionsPanelAccessoryViewControllerSelector = mkSelector "setSubstitutionsPanelAccessoryViewController:"

-- | @Selector@ for @availableLanguages@
availableLanguagesSelector :: Selector
availableLanguagesSelector = mkSelector "availableLanguages"

-- | @Selector@ for @userPreferredLanguages@
userPreferredLanguagesSelector :: Selector
userPreferredLanguagesSelector = mkSelector "userPreferredLanguages"

-- | @Selector@ for @automaticallyIdentifiesLanguages@
automaticallyIdentifiesLanguagesSelector :: Selector
automaticallyIdentifiesLanguagesSelector = mkSelector "automaticallyIdentifiesLanguages"

-- | @Selector@ for @setAutomaticallyIdentifiesLanguages:@
setAutomaticallyIdentifiesLanguagesSelector :: Selector
setAutomaticallyIdentifiesLanguagesSelector = mkSelector "setAutomaticallyIdentifiesLanguages:"

-- | @Selector@ for @automaticTextReplacementEnabled@
automaticTextReplacementEnabledSelector :: Selector
automaticTextReplacementEnabledSelector = mkSelector "automaticTextReplacementEnabled"

-- | @Selector@ for @automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabledSelector :: Selector
automaticSpellingCorrectionEnabledSelector = mkSelector "automaticSpellingCorrectionEnabled"

-- | @Selector@ for @automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabledSelector :: Selector
automaticQuoteSubstitutionEnabledSelector = mkSelector "automaticQuoteSubstitutionEnabled"

-- | @Selector@ for @automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabledSelector :: Selector
automaticDashSubstitutionEnabledSelector = mkSelector "automaticDashSubstitutionEnabled"

-- | @Selector@ for @automaticCapitalizationEnabled@
automaticCapitalizationEnabledSelector :: Selector
automaticCapitalizationEnabledSelector = mkSelector "automaticCapitalizationEnabled"

-- | @Selector@ for @automaticPeriodSubstitutionEnabled@
automaticPeriodSubstitutionEnabledSelector :: Selector
automaticPeriodSubstitutionEnabledSelector = mkSelector "automaticPeriodSubstitutionEnabled"

-- | @Selector@ for @automaticTextCompletionEnabled@
automaticTextCompletionEnabledSelector :: Selector
automaticTextCompletionEnabledSelector = mkSelector "automaticTextCompletionEnabled"

-- | @Selector@ for @automaticInlinePredictionEnabled@
automaticInlinePredictionEnabledSelector :: Selector
automaticInlinePredictionEnabledSelector = mkSelector "automaticInlinePredictionEnabled"

