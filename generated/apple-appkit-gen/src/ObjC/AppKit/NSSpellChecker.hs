{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , accessoryViewSelector
  , automaticCapitalizationEnabledSelector
  , automaticDashSubstitutionEnabledSelector
  , automaticInlinePredictionEnabledSelector
  , automaticPeriodSubstitutionEnabledSelector
  , automaticQuoteSubstitutionEnabledSelector
  , automaticSpellingCorrectionEnabledSelector
  , automaticTextCompletionEnabledSelector
  , automaticTextReplacementEnabledSelector
  , automaticallyIdentifiesLanguagesSelector
  , availableLanguagesSelector
  , checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector
  , checkSpellingOfString_startingAtSelector
  , checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector
  , checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector
  , closeSpellDocumentWithTagSelector
  , completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector
  , correctionForWordRange_inString_language_inSpellDocumentWithTagSelector
  , countWordsInString_languageSelector
  , deletesAutospaceBetweenString_andString_languageSelector
  , dismissCorrectionIndicatorForViewSelector
  , forgetWordSelector
  , guessesForWordRange_inString_language_inSpellDocumentWithTagSelector
  , guessesForWordSelector
  , hasLearnedWordSelector
  , ignoreWord_inSpellDocumentWithTagSelector
  , ignoredWordsInSpellDocumentWithTagSelector
  , languageForWordRange_inString_orthographySelector
  , languageSelector
  , learnWordSelector
  , menuForResult_string_options_atLocation_inViewSelector
  , preventsAutocorrectionBeforeString_languageSelector
  , recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector
  , setAccessoryViewSelector
  , setAutomaticallyIdentifiesLanguagesSelector
  , setIgnoredWords_inSpellDocumentWithTagSelector
  , setLanguageSelector
  , setSubstitutionsPanelAccessoryViewControllerSelector
  , setWordFieldStringValueSelector
  , sharedSpellCheckerExistsSelector
  , sharedSpellCheckerSelector
  , showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector
  , showInlinePredictionForCandidates_clientSelector
  , spellingPanelSelector
  , substitutionsPanelAccessoryViewControllerSelector
  , substitutionsPanelSelector
  , uniqueSpellDocumentTagSelector
  , unlearnWordSelector
  , updatePanelsSelector
  , updateSpellingPanelWithGrammarString_detailSelector
  , updateSpellingPanelWithMisspelledWordSelector
  , userPreferredLanguagesSelector
  , userQuotesArrayForLanguageSelector
  , userReplacementsDictionarySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' uniqueSpellDocumentTagSelector

-- | @- checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:@
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck, IsNSString language) => nsSpellChecker -> stringToCheck -> CLong -> language -> Bool -> CLong -> Ptr CLong -> IO NSRange
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount nsSpellChecker stringToCheck startingOffset language wrapFlag tag wordCount =
  sendMessage nsSpellChecker checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector (toNSString stringToCheck) startingOffset (toNSString language) wrapFlag tag wordCount

-- | @- checkSpellingOfString:startingAt:@
checkSpellingOfString_startingAt :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck) => nsSpellChecker -> stringToCheck -> CLong -> IO NSRange
checkSpellingOfString_startingAt nsSpellChecker stringToCheck startingOffset =
  sendMessage nsSpellChecker checkSpellingOfString_startingAtSelector (toNSString stringToCheck) startingOffset

-- | @- countWordsInString:language:@
countWordsInString_language :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCount, IsNSString language) => nsSpellChecker -> stringToCount -> language -> IO CLong
countWordsInString_language nsSpellChecker stringToCount language =
  sendMessage nsSpellChecker countWordsInString_languageSelector (toNSString stringToCount) (toNSString language)

-- | @- checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:@
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck, IsNSString language, IsNSArray details) => nsSpellChecker -> stringToCheck -> CLong -> language -> Bool -> CLong -> details -> IO NSRange
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details nsSpellChecker stringToCheck startingOffset language wrapFlag tag details =
  sendMessage nsSpellChecker checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector (toNSString stringToCheck) startingOffset (toNSString language) wrapFlag tag (toNSArray details)

-- | @- checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:@
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCount :: (IsNSSpellChecker nsSpellChecker, IsNSString stringToCheck, IsNSDictionary options, IsNSOrthography orthography) => nsSpellChecker -> stringToCheck -> NSRange -> CULong -> options -> CLong -> orthography -> Ptr CLong -> IO (Id NSArray)
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCount nsSpellChecker stringToCheck range checkingTypes options tag orthography wordCount =
  sendMessage nsSpellChecker checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector (toNSString stringToCheck) range checkingTypes (toNSDictionary options) tag (toNSOrthography orthography) wordCount

-- | @- menuForResult:string:options:atLocation:inView:@
menuForResult_string_options_atLocation_inView :: (IsNSSpellChecker nsSpellChecker, IsNSTextCheckingResult result, IsNSString checkedString, IsNSDictionary options, IsNSView view) => nsSpellChecker -> result -> checkedString -> options -> NSPoint -> view -> IO (Id NSMenu)
menuForResult_string_options_atLocation_inView nsSpellChecker result checkedString options location view =
  sendMessage nsSpellChecker menuForResult_string_options_atLocation_inViewSelector (toNSTextCheckingResult result) (toNSString checkedString) (toNSDictionary options) location (toNSView view)

-- | @- userQuotesArrayForLanguage:@
userQuotesArrayForLanguage :: (IsNSSpellChecker nsSpellChecker, IsNSString language) => nsSpellChecker -> language -> IO (Id NSArray)
userQuotesArrayForLanguage nsSpellChecker language =
  sendMessage nsSpellChecker userQuotesArrayForLanguageSelector (toNSString language)

-- | @- updateSpellingPanelWithMisspelledWord:@
updateSpellingPanelWithMisspelledWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
updateSpellingPanelWithMisspelledWord nsSpellChecker word =
  sendMessage nsSpellChecker updateSpellingPanelWithMisspelledWordSelector (toNSString word)

-- | @- updateSpellingPanelWithGrammarString:detail:@
updateSpellingPanelWithGrammarString_detail :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSDictionary detail) => nsSpellChecker -> string -> detail -> IO ()
updateSpellingPanelWithGrammarString_detail nsSpellChecker string detail =
  sendMessage nsSpellChecker updateSpellingPanelWithGrammarString_detailSelector (toNSString string) (toNSDictionary detail)

-- | @- updatePanels@
updatePanels :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO ()
updatePanels nsSpellChecker =
  sendMessage nsSpellChecker updatePanelsSelector

-- | @- ignoreWord:inSpellDocumentWithTag:@
ignoreWord_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString wordToIgnore) => nsSpellChecker -> wordToIgnore -> CLong -> IO ()
ignoreWord_inSpellDocumentWithTag nsSpellChecker wordToIgnore tag =
  sendMessage nsSpellChecker ignoreWord_inSpellDocumentWithTagSelector (toNSString wordToIgnore) tag

-- | @- ignoredWordsInSpellDocumentWithTag:@
ignoredWordsInSpellDocumentWithTag :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> CLong -> IO (Id NSArray)
ignoredWordsInSpellDocumentWithTag nsSpellChecker tag =
  sendMessage nsSpellChecker ignoredWordsInSpellDocumentWithTagSelector tag

-- | @- setIgnoredWords:inSpellDocumentWithTag:@
setIgnoredWords_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSArray words_) => nsSpellChecker -> words_ -> CLong -> IO ()
setIgnoredWords_inSpellDocumentWithTag nsSpellChecker words_ tag =
  sendMessage nsSpellChecker setIgnoredWords_inSpellDocumentWithTagSelector (toNSArray words_) tag

-- | @- guessesForWordRange:inString:language:inSpellDocumentWithTag:@
guessesForWordRange_inString_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> NSRange -> string -> language -> CLong -> IO (Id NSArray)
guessesForWordRange_inString_language_inSpellDocumentWithTag nsSpellChecker range string language tag =
  sendMessage nsSpellChecker guessesForWordRange_inString_language_inSpellDocumentWithTagSelector range (toNSString string) (toNSString language) tag

-- | @- correctionForWordRange:inString:language:inSpellDocumentWithTag:@
correctionForWordRange_inString_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> NSRange -> string -> language -> CLong -> IO (Id NSString)
correctionForWordRange_inString_language_inSpellDocumentWithTag nsSpellChecker range string language tag =
  sendMessage nsSpellChecker correctionForWordRange_inString_language_inSpellDocumentWithTagSelector range (toNSString string) (toNSString language) tag

-- | @- completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:@
completionsForPartialWordRange_inString_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> NSRange -> string -> language -> CLong -> IO (Id NSArray)
completionsForPartialWordRange_inString_language_inSpellDocumentWithTag nsSpellChecker range string language tag =
  sendMessage nsSpellChecker completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector range (toNSString string) (toNSString language) tag

-- | @- languageForWordRange:inString:orthography:@
languageForWordRange_inString_orthography :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSOrthography orthography) => nsSpellChecker -> NSRange -> string -> orthography -> IO (Id NSString)
languageForWordRange_inString_orthography nsSpellChecker range string orthography =
  sendMessage nsSpellChecker languageForWordRange_inString_orthographySelector range (toNSString string) (toNSOrthography orthography)

-- | @- closeSpellDocumentWithTag:@
closeSpellDocumentWithTag :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> CLong -> IO ()
closeSpellDocumentWithTag nsSpellChecker tag =
  sendMessage nsSpellChecker closeSpellDocumentWithTagSelector tag

-- | @- recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:@
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTag :: (IsNSSpellChecker nsSpellChecker, IsNSString correction, IsNSString word, IsNSString language) => nsSpellChecker -> NSCorrectionResponse -> correction -> word -> language -> CLong -> IO ()
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTag nsSpellChecker response correction word language tag =
  sendMessage nsSpellChecker recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector response (toNSString correction) (toNSString word) (toNSString language) tag

-- | @- showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:@
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandler :: (IsNSSpellChecker nsSpellChecker, IsNSString primaryString, IsNSArray alternativeStrings, IsNSView view) => nsSpellChecker -> NSCorrectionIndicatorType -> primaryString -> alternativeStrings -> NSRect -> view -> Ptr () -> IO ()
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandler nsSpellChecker type_ primaryString alternativeStrings rectOfTypedString view completionBlock =
  sendMessage nsSpellChecker showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector type_ (toNSString primaryString) (toNSArray alternativeStrings) rectOfTypedString (toNSView view) completionBlock

-- | @- dismissCorrectionIndicatorForView:@
dismissCorrectionIndicatorForView :: (IsNSSpellChecker nsSpellChecker, IsNSView view) => nsSpellChecker -> view -> IO ()
dismissCorrectionIndicatorForView nsSpellChecker view =
  sendMessage nsSpellChecker dismissCorrectionIndicatorForViewSelector (toNSView view)

-- | @- showInlinePredictionForCandidates:client:@
showInlinePredictionForCandidates_client :: (IsNSSpellChecker nsSpellChecker, IsNSArray candidates) => nsSpellChecker -> candidates -> RawId -> IO ()
showInlinePredictionForCandidates_client nsSpellChecker candidates client =
  sendMessage nsSpellChecker showInlinePredictionForCandidates_clientSelector (toNSArray candidates) client

-- | @- preventsAutocorrectionBeforeString:language:@
preventsAutocorrectionBeforeString_language :: (IsNSSpellChecker nsSpellChecker, IsNSString string, IsNSString language) => nsSpellChecker -> string -> language -> IO Bool
preventsAutocorrectionBeforeString_language nsSpellChecker string language =
  sendMessage nsSpellChecker preventsAutocorrectionBeforeString_languageSelector (toNSString string) (toNSString language)

-- | @- deletesAutospaceBetweenString:andString:language:@
deletesAutospaceBetweenString_andString_language :: (IsNSSpellChecker nsSpellChecker, IsNSString precedingString, IsNSString followingString, IsNSString language) => nsSpellChecker -> precedingString -> followingString -> language -> IO Bool
deletesAutospaceBetweenString_andString_language nsSpellChecker precedingString followingString language =
  sendMessage nsSpellChecker deletesAutospaceBetweenString_andString_languageSelector (toNSString precedingString) (toNSString followingString) (toNSString language)

-- | @- setWordFieldStringValue:@
setWordFieldStringValue :: (IsNSSpellChecker nsSpellChecker, IsNSString string) => nsSpellChecker -> string -> IO ()
setWordFieldStringValue nsSpellChecker string =
  sendMessage nsSpellChecker setWordFieldStringValueSelector (toNSString string)

-- | @- learnWord:@
learnWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
learnWord nsSpellChecker word =
  sendMessage nsSpellChecker learnWordSelector (toNSString word)

-- | @- hasLearnedWord:@
hasLearnedWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO Bool
hasLearnedWord nsSpellChecker word =
  sendMessage nsSpellChecker hasLearnedWordSelector (toNSString word)

-- | @- unlearnWord:@
unlearnWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
unlearnWord nsSpellChecker word =
  sendMessage nsSpellChecker unlearnWordSelector (toNSString word)

-- | @- language@
language :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSString)
language nsSpellChecker =
  sendMessage nsSpellChecker languageSelector

-- | @- setLanguage:@
setLanguage :: (IsNSSpellChecker nsSpellChecker, IsNSString language) => nsSpellChecker -> language -> IO Bool
setLanguage nsSpellChecker language =
  sendMessage nsSpellChecker setLanguageSelector (toNSString language)

-- | @- guessesForWord:@
guessesForWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO (Id NSArray)
guessesForWord nsSpellChecker word =
  sendMessage nsSpellChecker guessesForWordSelector (toNSString word)

-- | @- forgetWord:@
forgetWord :: (IsNSSpellChecker nsSpellChecker, IsNSString word) => nsSpellChecker -> word -> IO ()
forgetWord nsSpellChecker word =
  sendMessage nsSpellChecker forgetWordSelector (toNSString word)

-- | @+ sharedSpellChecker@
sharedSpellChecker :: IO (Id NSSpellChecker)
sharedSpellChecker  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' sharedSpellCheckerSelector

-- | @+ sharedSpellCheckerExists@
sharedSpellCheckerExists :: IO Bool
sharedSpellCheckerExists  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' sharedSpellCheckerExistsSelector

-- | @- userReplacementsDictionary@
userReplacementsDictionary :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSDictionary)
userReplacementsDictionary nsSpellChecker =
  sendMessage nsSpellChecker userReplacementsDictionarySelector

-- | @- spellingPanel@
spellingPanel :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSPanel)
spellingPanel nsSpellChecker =
  sendMessage nsSpellChecker spellingPanelSelector

-- | @- accessoryView@
accessoryView :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSView)
accessoryView nsSpellChecker =
  sendMessage nsSpellChecker accessoryViewSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSSpellChecker nsSpellChecker, IsNSView value) => nsSpellChecker -> value -> IO ()
setAccessoryView nsSpellChecker value =
  sendMessage nsSpellChecker setAccessoryViewSelector (toNSView value)

-- | @- substitutionsPanel@
substitutionsPanel :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSPanel)
substitutionsPanel nsSpellChecker =
  sendMessage nsSpellChecker substitutionsPanelSelector

-- | @- substitutionsPanelAccessoryViewController@
substitutionsPanelAccessoryViewController :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSViewController)
substitutionsPanelAccessoryViewController nsSpellChecker =
  sendMessage nsSpellChecker substitutionsPanelAccessoryViewControllerSelector

-- | @- setSubstitutionsPanelAccessoryViewController:@
setSubstitutionsPanelAccessoryViewController :: (IsNSSpellChecker nsSpellChecker, IsNSViewController value) => nsSpellChecker -> value -> IO ()
setSubstitutionsPanelAccessoryViewController nsSpellChecker value =
  sendMessage nsSpellChecker setSubstitutionsPanelAccessoryViewControllerSelector (toNSViewController value)

-- | @- availableLanguages@
availableLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSArray)
availableLanguages nsSpellChecker =
  sendMessage nsSpellChecker availableLanguagesSelector

-- | @- userPreferredLanguages@
userPreferredLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO (Id NSArray)
userPreferredLanguages nsSpellChecker =
  sendMessage nsSpellChecker userPreferredLanguagesSelector

-- | @- automaticallyIdentifiesLanguages@
automaticallyIdentifiesLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> IO Bool
automaticallyIdentifiesLanguages nsSpellChecker =
  sendMessage nsSpellChecker automaticallyIdentifiesLanguagesSelector

-- | @- setAutomaticallyIdentifiesLanguages:@
setAutomaticallyIdentifiesLanguages :: IsNSSpellChecker nsSpellChecker => nsSpellChecker -> Bool -> IO ()
setAutomaticallyIdentifiesLanguages nsSpellChecker value =
  sendMessage nsSpellChecker setAutomaticallyIdentifiesLanguagesSelector value

-- | @+ automaticTextReplacementEnabled@
automaticTextReplacementEnabled :: IO Bool
automaticTextReplacementEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticTextReplacementEnabledSelector

-- | @+ automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabled :: IO Bool
automaticSpellingCorrectionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticSpellingCorrectionEnabledSelector

-- | @+ automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabled :: IO Bool
automaticQuoteSubstitutionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticQuoteSubstitutionEnabledSelector

-- | @+ automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabled :: IO Bool
automaticDashSubstitutionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticDashSubstitutionEnabledSelector

-- | @+ automaticCapitalizationEnabled@
automaticCapitalizationEnabled :: IO Bool
automaticCapitalizationEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticCapitalizationEnabledSelector

-- | @+ automaticPeriodSubstitutionEnabled@
automaticPeriodSubstitutionEnabled :: IO Bool
automaticPeriodSubstitutionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticPeriodSubstitutionEnabledSelector

-- | @+ automaticTextCompletionEnabled@
automaticTextCompletionEnabled :: IO Bool
automaticTextCompletionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticTextCompletionEnabledSelector

-- | @+ automaticInlinePredictionEnabled@
automaticInlinePredictionEnabled :: IO Bool
automaticInlinePredictionEnabled  =
  do
    cls' <- getRequiredClass "NSSpellChecker"
    sendClassMessage cls' automaticInlinePredictionEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueSpellDocumentTag@
uniqueSpellDocumentTagSelector :: Selector '[] CLong
uniqueSpellDocumentTagSelector = mkSelector "uniqueSpellDocumentTag"

-- | @Selector@ for @checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:@
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector :: Selector '[Id NSString, CLong, Id NSString, Bool, CLong, Ptr CLong] NSRange
checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCountSelector = mkSelector "checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:"

-- | @Selector@ for @checkSpellingOfString:startingAt:@
checkSpellingOfString_startingAtSelector :: Selector '[Id NSString, CLong] NSRange
checkSpellingOfString_startingAtSelector = mkSelector "checkSpellingOfString:startingAt:"

-- | @Selector@ for @countWordsInString:language:@
countWordsInString_languageSelector :: Selector '[Id NSString, Id NSString] CLong
countWordsInString_languageSelector = mkSelector "countWordsInString:language:"

-- | @Selector@ for @checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:@
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector :: Selector '[Id NSString, CLong, Id NSString, Bool, CLong, Id NSArray] NSRange
checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_detailsSelector = mkSelector "checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:"

-- | @Selector@ for @checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:@
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector :: Selector '[Id NSString, NSRange, CULong, Id NSDictionary, CLong, Id NSOrthography, Ptr CLong] (Id NSArray)
checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCountSelector = mkSelector "checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:"

-- | @Selector@ for @menuForResult:string:options:atLocation:inView:@
menuForResult_string_options_atLocation_inViewSelector :: Selector '[Id NSTextCheckingResult, Id NSString, Id NSDictionary, NSPoint, Id NSView] (Id NSMenu)
menuForResult_string_options_atLocation_inViewSelector = mkSelector "menuForResult:string:options:atLocation:inView:"

-- | @Selector@ for @userQuotesArrayForLanguage:@
userQuotesArrayForLanguageSelector :: Selector '[Id NSString] (Id NSArray)
userQuotesArrayForLanguageSelector = mkSelector "userQuotesArrayForLanguage:"

-- | @Selector@ for @updateSpellingPanelWithMisspelledWord:@
updateSpellingPanelWithMisspelledWordSelector :: Selector '[Id NSString] ()
updateSpellingPanelWithMisspelledWordSelector = mkSelector "updateSpellingPanelWithMisspelledWord:"

-- | @Selector@ for @updateSpellingPanelWithGrammarString:detail:@
updateSpellingPanelWithGrammarString_detailSelector :: Selector '[Id NSString, Id NSDictionary] ()
updateSpellingPanelWithGrammarString_detailSelector = mkSelector "updateSpellingPanelWithGrammarString:detail:"

-- | @Selector@ for @updatePanels@
updatePanelsSelector :: Selector '[] ()
updatePanelsSelector = mkSelector "updatePanels"

-- | @Selector@ for @ignoreWord:inSpellDocumentWithTag:@
ignoreWord_inSpellDocumentWithTagSelector :: Selector '[Id NSString, CLong] ()
ignoreWord_inSpellDocumentWithTagSelector = mkSelector "ignoreWord:inSpellDocumentWithTag:"

-- | @Selector@ for @ignoredWordsInSpellDocumentWithTag:@
ignoredWordsInSpellDocumentWithTagSelector :: Selector '[CLong] (Id NSArray)
ignoredWordsInSpellDocumentWithTagSelector = mkSelector "ignoredWordsInSpellDocumentWithTag:"

-- | @Selector@ for @setIgnoredWords:inSpellDocumentWithTag:@
setIgnoredWords_inSpellDocumentWithTagSelector :: Selector '[Id NSArray, CLong] ()
setIgnoredWords_inSpellDocumentWithTagSelector = mkSelector "setIgnoredWords:inSpellDocumentWithTag:"

-- | @Selector@ for @guessesForWordRange:inString:language:inSpellDocumentWithTag:@
guessesForWordRange_inString_language_inSpellDocumentWithTagSelector :: Selector '[NSRange, Id NSString, Id NSString, CLong] (Id NSArray)
guessesForWordRange_inString_language_inSpellDocumentWithTagSelector = mkSelector "guessesForWordRange:inString:language:inSpellDocumentWithTag:"

-- | @Selector@ for @correctionForWordRange:inString:language:inSpellDocumentWithTag:@
correctionForWordRange_inString_language_inSpellDocumentWithTagSelector :: Selector '[NSRange, Id NSString, Id NSString, CLong] (Id NSString)
correctionForWordRange_inString_language_inSpellDocumentWithTagSelector = mkSelector "correctionForWordRange:inString:language:inSpellDocumentWithTag:"

-- | @Selector@ for @completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:@
completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector :: Selector '[NSRange, Id NSString, Id NSString, CLong] (Id NSArray)
completionsForPartialWordRange_inString_language_inSpellDocumentWithTagSelector = mkSelector "completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:"

-- | @Selector@ for @languageForWordRange:inString:orthography:@
languageForWordRange_inString_orthographySelector :: Selector '[NSRange, Id NSString, Id NSOrthography] (Id NSString)
languageForWordRange_inString_orthographySelector = mkSelector "languageForWordRange:inString:orthography:"

-- | @Selector@ for @closeSpellDocumentWithTag:@
closeSpellDocumentWithTagSelector :: Selector '[CLong] ()
closeSpellDocumentWithTagSelector = mkSelector "closeSpellDocumentWithTag:"

-- | @Selector@ for @recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:@
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector :: Selector '[NSCorrectionResponse, Id NSString, Id NSString, Id NSString, CLong] ()
recordResponse_toCorrection_forWord_language_inSpellDocumentWithTagSelector = mkSelector "recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:"

-- | @Selector@ for @showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:@
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector :: Selector '[NSCorrectionIndicatorType, Id NSString, Id NSArray, NSRect, Id NSView, Ptr ()] ()
showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandlerSelector = mkSelector "showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:"

-- | @Selector@ for @dismissCorrectionIndicatorForView:@
dismissCorrectionIndicatorForViewSelector :: Selector '[Id NSView] ()
dismissCorrectionIndicatorForViewSelector = mkSelector "dismissCorrectionIndicatorForView:"

-- | @Selector@ for @showInlinePredictionForCandidates:client:@
showInlinePredictionForCandidates_clientSelector :: Selector '[Id NSArray, RawId] ()
showInlinePredictionForCandidates_clientSelector = mkSelector "showInlinePredictionForCandidates:client:"

-- | @Selector@ for @preventsAutocorrectionBeforeString:language:@
preventsAutocorrectionBeforeString_languageSelector :: Selector '[Id NSString, Id NSString] Bool
preventsAutocorrectionBeforeString_languageSelector = mkSelector "preventsAutocorrectionBeforeString:language:"

-- | @Selector@ for @deletesAutospaceBetweenString:andString:language:@
deletesAutospaceBetweenString_andString_languageSelector :: Selector '[Id NSString, Id NSString, Id NSString] Bool
deletesAutospaceBetweenString_andString_languageSelector = mkSelector "deletesAutospaceBetweenString:andString:language:"

-- | @Selector@ for @setWordFieldStringValue:@
setWordFieldStringValueSelector :: Selector '[Id NSString] ()
setWordFieldStringValueSelector = mkSelector "setWordFieldStringValue:"

-- | @Selector@ for @learnWord:@
learnWordSelector :: Selector '[Id NSString] ()
learnWordSelector = mkSelector "learnWord:"

-- | @Selector@ for @hasLearnedWord:@
hasLearnedWordSelector :: Selector '[Id NSString] Bool
hasLearnedWordSelector = mkSelector "hasLearnedWord:"

-- | @Selector@ for @unlearnWord:@
unlearnWordSelector :: Selector '[Id NSString] ()
unlearnWordSelector = mkSelector "unlearnWord:"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector '[Id NSString] Bool
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @guessesForWord:@
guessesForWordSelector :: Selector '[Id NSString] (Id NSArray)
guessesForWordSelector = mkSelector "guessesForWord:"

-- | @Selector@ for @forgetWord:@
forgetWordSelector :: Selector '[Id NSString] ()
forgetWordSelector = mkSelector "forgetWord:"

-- | @Selector@ for @sharedSpellChecker@
sharedSpellCheckerSelector :: Selector '[] (Id NSSpellChecker)
sharedSpellCheckerSelector = mkSelector "sharedSpellChecker"

-- | @Selector@ for @sharedSpellCheckerExists@
sharedSpellCheckerExistsSelector :: Selector '[] Bool
sharedSpellCheckerExistsSelector = mkSelector "sharedSpellCheckerExists"

-- | @Selector@ for @userReplacementsDictionary@
userReplacementsDictionarySelector :: Selector '[] (Id NSDictionary)
userReplacementsDictionarySelector = mkSelector "userReplacementsDictionary"

-- | @Selector@ for @spellingPanel@
spellingPanelSelector :: Selector '[] (Id NSPanel)
spellingPanelSelector = mkSelector "spellingPanel"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @substitutionsPanel@
substitutionsPanelSelector :: Selector '[] (Id NSPanel)
substitutionsPanelSelector = mkSelector "substitutionsPanel"

-- | @Selector@ for @substitutionsPanelAccessoryViewController@
substitutionsPanelAccessoryViewControllerSelector :: Selector '[] (Id NSViewController)
substitutionsPanelAccessoryViewControllerSelector = mkSelector "substitutionsPanelAccessoryViewController"

-- | @Selector@ for @setSubstitutionsPanelAccessoryViewController:@
setSubstitutionsPanelAccessoryViewControllerSelector :: Selector '[Id NSViewController] ()
setSubstitutionsPanelAccessoryViewControllerSelector = mkSelector "setSubstitutionsPanelAccessoryViewController:"

-- | @Selector@ for @availableLanguages@
availableLanguagesSelector :: Selector '[] (Id NSArray)
availableLanguagesSelector = mkSelector "availableLanguages"

-- | @Selector@ for @userPreferredLanguages@
userPreferredLanguagesSelector :: Selector '[] (Id NSArray)
userPreferredLanguagesSelector = mkSelector "userPreferredLanguages"

-- | @Selector@ for @automaticallyIdentifiesLanguages@
automaticallyIdentifiesLanguagesSelector :: Selector '[] Bool
automaticallyIdentifiesLanguagesSelector = mkSelector "automaticallyIdentifiesLanguages"

-- | @Selector@ for @setAutomaticallyIdentifiesLanguages:@
setAutomaticallyIdentifiesLanguagesSelector :: Selector '[Bool] ()
setAutomaticallyIdentifiesLanguagesSelector = mkSelector "setAutomaticallyIdentifiesLanguages:"

-- | @Selector@ for @automaticTextReplacementEnabled@
automaticTextReplacementEnabledSelector :: Selector '[] Bool
automaticTextReplacementEnabledSelector = mkSelector "automaticTextReplacementEnabled"

-- | @Selector@ for @automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabledSelector :: Selector '[] Bool
automaticSpellingCorrectionEnabledSelector = mkSelector "automaticSpellingCorrectionEnabled"

-- | @Selector@ for @automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabledSelector :: Selector '[] Bool
automaticQuoteSubstitutionEnabledSelector = mkSelector "automaticQuoteSubstitutionEnabled"

-- | @Selector@ for @automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabledSelector :: Selector '[] Bool
automaticDashSubstitutionEnabledSelector = mkSelector "automaticDashSubstitutionEnabled"

-- | @Selector@ for @automaticCapitalizationEnabled@
automaticCapitalizationEnabledSelector :: Selector '[] Bool
automaticCapitalizationEnabledSelector = mkSelector "automaticCapitalizationEnabled"

-- | @Selector@ for @automaticPeriodSubstitutionEnabled@
automaticPeriodSubstitutionEnabledSelector :: Selector '[] Bool
automaticPeriodSubstitutionEnabledSelector = mkSelector "automaticPeriodSubstitutionEnabled"

-- | @Selector@ for @automaticTextCompletionEnabled@
automaticTextCompletionEnabledSelector :: Selector '[] Bool
automaticTextCompletionEnabledSelector = mkSelector "automaticTextCompletionEnabled"

-- | @Selector@ for @automaticInlinePredictionEnabled@
automaticInlinePredictionEnabledSelector :: Selector '[] Bool
automaticInlinePredictionEnabledSelector = mkSelector "automaticInlinePredictionEnabled"

