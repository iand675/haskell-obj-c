{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.Foundation.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , initWithActivityType
  , init_
  , addUserInfoEntriesFromDictionary
  , becomeCurrent
  , resignCurrent
  , invalidate
  , getContinuationStreamsWithCompletionHandler
  , deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandler
  , deleteAllSavedUserActivitiesWithCompletionHandler
  , activityType
  , title
  , setTitle
  , userInfo
  , setUserInfo
  , requiredUserInfoKeys
  , setRequiredUserInfoKeys
  , needsSave
  , setNeedsSave
  , webpageURL
  , setWebpageURL
  , referrerURL
  , setReferrerURL
  , expirationDate
  , setExpirationDate
  , keywords
  , setKeywords
  , supportsContinuationStreams
  , setSupportsContinuationStreams
  , delegate
  , setDelegate
  , targetContentIdentifier
  , setTargetContentIdentifier
  , eligibleForHandoff
  , setEligibleForHandoff
  , eligibleForSearch
  , setEligibleForSearch
  , eligibleForPublicIndexing
  , setEligibleForPublicIndexing
  , eligibleForPrediction
  , setEligibleForPrediction
  , persistentIdentifier
  , setPersistentIdentifier
  , activityTypeSelector
  , addUserInfoEntriesFromDictionarySelector
  , becomeCurrentSelector
  , delegateSelector
  , deleteAllSavedUserActivitiesWithCompletionHandlerSelector
  , deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector
  , eligibleForHandoffSelector
  , eligibleForPredictionSelector
  , eligibleForPublicIndexingSelector
  , eligibleForSearchSelector
  , expirationDateSelector
  , getContinuationStreamsWithCompletionHandlerSelector
  , initSelector
  , initWithActivityTypeSelector
  , invalidateSelector
  , keywordsSelector
  , needsSaveSelector
  , persistentIdentifierSelector
  , referrerURLSelector
  , requiredUserInfoKeysSelector
  , resignCurrentSelector
  , setDelegateSelector
  , setEligibleForHandoffSelector
  , setEligibleForPredictionSelector
  , setEligibleForPublicIndexingSelector
  , setEligibleForSearchSelector
  , setExpirationDateSelector
  , setKeywordsSelector
  , setNeedsSaveSelector
  , setPersistentIdentifierSelector
  , setReferrerURLSelector
  , setRequiredUserInfoKeysSelector
  , setSupportsContinuationStreamsSelector
  , setTargetContentIdentifierSelector
  , setTitleSelector
  , setUserInfoSelector
  , setWebpageURLSelector
  , supportsContinuationStreamsSelector
  , targetContentIdentifierSelector
  , titleSelector
  , userInfoSelector
  , webpageURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithActivityType:@
initWithActivityType :: (IsNSUserActivity nsUserActivity, IsNSString activityType) => nsUserActivity -> activityType -> IO (Id NSUserActivity)
initWithActivityType nsUserActivity activityType =
  sendOwnedMessage nsUserActivity initWithActivityTypeSelector (toNSString activityType)

-- | @- init@
init_ :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSUserActivity)
init_ nsUserActivity =
  sendOwnedMessage nsUserActivity initSelector

-- | @- addUserInfoEntriesFromDictionary:@
addUserInfoEntriesFromDictionary :: (IsNSUserActivity nsUserActivity, IsNSDictionary otherDictionary) => nsUserActivity -> otherDictionary -> IO ()
addUserInfoEntriesFromDictionary nsUserActivity otherDictionary =
  sendMessage nsUserActivity addUserInfoEntriesFromDictionarySelector (toNSDictionary otherDictionary)

-- | @- becomeCurrent@
becomeCurrent :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO ()
becomeCurrent nsUserActivity =
  sendMessage nsUserActivity becomeCurrentSelector

-- | @- resignCurrent@
resignCurrent :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO ()
resignCurrent nsUserActivity =
  sendMessage nsUserActivity resignCurrentSelector

-- | @- invalidate@
invalidate :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO ()
invalidate nsUserActivity =
  sendMessage nsUserActivity invalidateSelector

-- | @- getContinuationStreamsWithCompletionHandler:@
getContinuationStreamsWithCompletionHandler :: IsNSUserActivity nsUserActivity => nsUserActivity -> Ptr () -> IO ()
getContinuationStreamsWithCompletionHandler nsUserActivity completionHandler =
  sendMessage nsUserActivity getContinuationStreamsWithCompletionHandlerSelector completionHandler

-- | @+ deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:@
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandler :: IsNSArray persistentIdentifiers => persistentIdentifiers -> Ptr () -> IO ()
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandler persistentIdentifiers handler =
  do
    cls' <- getRequiredClass "NSUserActivity"
    sendClassMessage cls' deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector (toNSArray persistentIdentifiers) handler

-- | @+ deleteAllSavedUserActivitiesWithCompletionHandler:@
deleteAllSavedUserActivitiesWithCompletionHandler :: Ptr () -> IO ()
deleteAllSavedUserActivitiesWithCompletionHandler handler =
  do
    cls' <- getRequiredClass "NSUserActivity"
    sendClassMessage cls' deleteAllSavedUserActivitiesWithCompletionHandlerSelector handler

-- | @- activityType@
activityType :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
activityType nsUserActivity =
  sendMessage nsUserActivity activityTypeSelector

-- | @- title@
title :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
title nsUserActivity =
  sendMessage nsUserActivity titleSelector

-- | @- setTitle:@
setTitle :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setTitle nsUserActivity value =
  sendMessage nsUserActivity setTitleSelector (toNSString value)

-- | @- userInfo@
userInfo :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSDictionary)
userInfo nsUserActivity =
  sendMessage nsUserActivity userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSUserActivity nsUserActivity, IsNSDictionary value) => nsUserActivity -> value -> IO ()
setUserInfo nsUserActivity value =
  sendMessage nsUserActivity setUserInfoSelector (toNSDictionary value)

-- | @- requiredUserInfoKeys@
requiredUserInfoKeys :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSSet)
requiredUserInfoKeys nsUserActivity =
  sendMessage nsUserActivity requiredUserInfoKeysSelector

-- | @- setRequiredUserInfoKeys:@
setRequiredUserInfoKeys :: (IsNSUserActivity nsUserActivity, IsNSSet value) => nsUserActivity -> value -> IO ()
setRequiredUserInfoKeys nsUserActivity value =
  sendMessage nsUserActivity setRequiredUserInfoKeysSelector (toNSSet value)

-- | @- needsSave@
needsSave :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
needsSave nsUserActivity =
  sendMessage nsUserActivity needsSaveSelector

-- | @- setNeedsSave:@
setNeedsSave :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setNeedsSave nsUserActivity value =
  sendMessage nsUserActivity setNeedsSaveSelector value

-- | @- webpageURL@
webpageURL :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSURL)
webpageURL nsUserActivity =
  sendMessage nsUserActivity webpageURLSelector

-- | @- setWebpageURL:@
setWebpageURL :: (IsNSUserActivity nsUserActivity, IsNSURL value) => nsUserActivity -> value -> IO ()
setWebpageURL nsUserActivity value =
  sendMessage nsUserActivity setWebpageURLSelector (toNSURL value)

-- | @- referrerURL@
referrerURL :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSURL)
referrerURL nsUserActivity =
  sendMessage nsUserActivity referrerURLSelector

-- | @- setReferrerURL:@
setReferrerURL :: (IsNSUserActivity nsUserActivity, IsNSURL value) => nsUserActivity -> value -> IO ()
setReferrerURL nsUserActivity value =
  sendMessage nsUserActivity setReferrerURLSelector (toNSURL value)

-- | @- expirationDate@
expirationDate :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSDate)
expirationDate nsUserActivity =
  sendMessage nsUserActivity expirationDateSelector

-- | @- setExpirationDate:@
setExpirationDate :: (IsNSUserActivity nsUserActivity, IsNSDate value) => nsUserActivity -> value -> IO ()
setExpirationDate nsUserActivity value =
  sendMessage nsUserActivity setExpirationDateSelector (toNSDate value)

-- | @- keywords@
keywords :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSSet)
keywords nsUserActivity =
  sendMessage nsUserActivity keywordsSelector

-- | @- setKeywords:@
setKeywords :: (IsNSUserActivity nsUserActivity, IsNSSet value) => nsUserActivity -> value -> IO ()
setKeywords nsUserActivity value =
  sendMessage nsUserActivity setKeywordsSelector (toNSSet value)

-- | @- supportsContinuationStreams@
supportsContinuationStreams :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
supportsContinuationStreams nsUserActivity =
  sendMessage nsUserActivity supportsContinuationStreamsSelector

-- | @- setSupportsContinuationStreams:@
setSupportsContinuationStreams :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setSupportsContinuationStreams nsUserActivity value =
  sendMessage nsUserActivity setSupportsContinuationStreamsSelector value

-- | @- delegate@
delegate :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO RawId
delegate nsUserActivity =
  sendMessage nsUserActivity delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSUserActivity nsUserActivity => nsUserActivity -> RawId -> IO ()
setDelegate nsUserActivity value =
  sendMessage nsUserActivity setDelegateSelector value

-- | @- targetContentIdentifier@
targetContentIdentifier :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
targetContentIdentifier nsUserActivity =
  sendMessage nsUserActivity targetContentIdentifierSelector

-- | @- setTargetContentIdentifier:@
setTargetContentIdentifier :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setTargetContentIdentifier nsUserActivity value =
  sendMessage nsUserActivity setTargetContentIdentifierSelector (toNSString value)

-- | @- eligibleForHandoff@
eligibleForHandoff :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForHandoff nsUserActivity =
  sendMessage nsUserActivity eligibleForHandoffSelector

-- | @- setEligibleForHandoff:@
setEligibleForHandoff :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForHandoff nsUserActivity value =
  sendMessage nsUserActivity setEligibleForHandoffSelector value

-- | @- eligibleForSearch@
eligibleForSearch :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForSearch nsUserActivity =
  sendMessage nsUserActivity eligibleForSearchSelector

-- | @- setEligibleForSearch:@
setEligibleForSearch :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForSearch nsUserActivity value =
  sendMessage nsUserActivity setEligibleForSearchSelector value

-- | @- eligibleForPublicIndexing@
eligibleForPublicIndexing :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForPublicIndexing nsUserActivity =
  sendMessage nsUserActivity eligibleForPublicIndexingSelector

-- | @- setEligibleForPublicIndexing:@
setEligibleForPublicIndexing :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForPublicIndexing nsUserActivity value =
  sendMessage nsUserActivity setEligibleForPublicIndexingSelector value

-- | @- eligibleForPrediction@
eligibleForPrediction :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForPrediction nsUserActivity =
  sendMessage nsUserActivity eligibleForPredictionSelector

-- | @- setEligibleForPrediction:@
setEligibleForPrediction :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForPrediction nsUserActivity value =
  sendMessage nsUserActivity setEligibleForPredictionSelector value

-- | @- persistentIdentifier@
persistentIdentifier :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
persistentIdentifier nsUserActivity =
  sendMessage nsUserActivity persistentIdentifierSelector

-- | @- setPersistentIdentifier:@
setPersistentIdentifier :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setPersistentIdentifier nsUserActivity value =
  sendMessage nsUserActivity setPersistentIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithActivityType:@
initWithActivityTypeSelector :: Selector '[Id NSString] (Id NSUserActivity)
initWithActivityTypeSelector = mkSelector "initWithActivityType:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSUserActivity)
initSelector = mkSelector "init"

-- | @Selector@ for @addUserInfoEntriesFromDictionary:@
addUserInfoEntriesFromDictionarySelector :: Selector '[Id NSDictionary] ()
addUserInfoEntriesFromDictionarySelector = mkSelector "addUserInfoEntriesFromDictionary:"

-- | @Selector@ for @becomeCurrent@
becomeCurrentSelector :: Selector '[] ()
becomeCurrentSelector = mkSelector "becomeCurrent"

-- | @Selector@ for @resignCurrent@
resignCurrentSelector :: Selector '[] ()
resignCurrentSelector = mkSelector "resignCurrent"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @getContinuationStreamsWithCompletionHandler:@
getContinuationStreamsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getContinuationStreamsWithCompletionHandlerSelector = mkSelector "getContinuationStreamsWithCompletionHandler:"

-- | @Selector@ for @deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:@
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector = mkSelector "deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:"

-- | @Selector@ for @deleteAllSavedUserActivitiesWithCompletionHandler:@
deleteAllSavedUserActivitiesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
deleteAllSavedUserActivitiesWithCompletionHandlerSelector = mkSelector "deleteAllSavedUserActivitiesWithCompletionHandler:"

-- | @Selector@ for @activityType@
activityTypeSelector :: Selector '[] (Id NSString)
activityTypeSelector = mkSelector "activityType"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @requiredUserInfoKeys@
requiredUserInfoKeysSelector :: Selector '[] (Id NSSet)
requiredUserInfoKeysSelector = mkSelector "requiredUserInfoKeys"

-- | @Selector@ for @setRequiredUserInfoKeys:@
setRequiredUserInfoKeysSelector :: Selector '[Id NSSet] ()
setRequiredUserInfoKeysSelector = mkSelector "setRequiredUserInfoKeys:"

-- | @Selector@ for @needsSave@
needsSaveSelector :: Selector '[] Bool
needsSaveSelector = mkSelector "needsSave"

-- | @Selector@ for @setNeedsSave:@
setNeedsSaveSelector :: Selector '[Bool] ()
setNeedsSaveSelector = mkSelector "setNeedsSave:"

-- | @Selector@ for @webpageURL@
webpageURLSelector :: Selector '[] (Id NSURL)
webpageURLSelector = mkSelector "webpageURL"

-- | @Selector@ for @setWebpageURL:@
setWebpageURLSelector :: Selector '[Id NSURL] ()
setWebpageURLSelector = mkSelector "setWebpageURL:"

-- | @Selector@ for @referrerURL@
referrerURLSelector :: Selector '[] (Id NSURL)
referrerURLSelector = mkSelector "referrerURL"

-- | @Selector@ for @setReferrerURL:@
setReferrerURLSelector :: Selector '[Id NSURL] ()
setReferrerURLSelector = mkSelector "setReferrerURL:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector '[Id NSDate] ()
setExpirationDateSelector = mkSelector "setExpirationDate:"

-- | @Selector@ for @keywords@
keywordsSelector :: Selector '[] (Id NSSet)
keywordsSelector = mkSelector "keywords"

-- | @Selector@ for @setKeywords:@
setKeywordsSelector :: Selector '[Id NSSet] ()
setKeywordsSelector = mkSelector "setKeywords:"

-- | @Selector@ for @supportsContinuationStreams@
supportsContinuationStreamsSelector :: Selector '[] Bool
supportsContinuationStreamsSelector = mkSelector "supportsContinuationStreams"

-- | @Selector@ for @setSupportsContinuationStreams:@
setSupportsContinuationStreamsSelector :: Selector '[Bool] ()
setSupportsContinuationStreamsSelector = mkSelector "setSupportsContinuationStreams:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @targetContentIdentifier@
targetContentIdentifierSelector :: Selector '[] (Id NSString)
targetContentIdentifierSelector = mkSelector "targetContentIdentifier"

-- | @Selector@ for @setTargetContentIdentifier:@
setTargetContentIdentifierSelector :: Selector '[Id NSString] ()
setTargetContentIdentifierSelector = mkSelector "setTargetContentIdentifier:"

-- | @Selector@ for @eligibleForHandoff@
eligibleForHandoffSelector :: Selector '[] Bool
eligibleForHandoffSelector = mkSelector "eligibleForHandoff"

-- | @Selector@ for @setEligibleForHandoff:@
setEligibleForHandoffSelector :: Selector '[Bool] ()
setEligibleForHandoffSelector = mkSelector "setEligibleForHandoff:"

-- | @Selector@ for @eligibleForSearch@
eligibleForSearchSelector :: Selector '[] Bool
eligibleForSearchSelector = mkSelector "eligibleForSearch"

-- | @Selector@ for @setEligibleForSearch:@
setEligibleForSearchSelector :: Selector '[Bool] ()
setEligibleForSearchSelector = mkSelector "setEligibleForSearch:"

-- | @Selector@ for @eligibleForPublicIndexing@
eligibleForPublicIndexingSelector :: Selector '[] Bool
eligibleForPublicIndexingSelector = mkSelector "eligibleForPublicIndexing"

-- | @Selector@ for @setEligibleForPublicIndexing:@
setEligibleForPublicIndexingSelector :: Selector '[Bool] ()
setEligibleForPublicIndexingSelector = mkSelector "setEligibleForPublicIndexing:"

-- | @Selector@ for @eligibleForPrediction@
eligibleForPredictionSelector :: Selector '[] Bool
eligibleForPredictionSelector = mkSelector "eligibleForPrediction"

-- | @Selector@ for @setEligibleForPrediction:@
setEligibleForPredictionSelector :: Selector '[Bool] ()
setEligibleForPredictionSelector = mkSelector "setEligibleForPrediction:"

-- | @Selector@ for @persistentIdentifier@
persistentIdentifierSelector :: Selector '[] (Id NSString)
persistentIdentifierSelector = mkSelector "persistentIdentifier"

-- | @Selector@ for @setPersistentIdentifier:@
setPersistentIdentifierSelector :: Selector '[Id NSString] ()
setPersistentIdentifierSelector = mkSelector "setPersistentIdentifier:"

