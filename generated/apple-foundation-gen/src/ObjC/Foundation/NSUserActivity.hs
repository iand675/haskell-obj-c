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
  , initWithActivityTypeSelector
  , initSelector
  , addUserInfoEntriesFromDictionarySelector
  , becomeCurrentSelector
  , resignCurrentSelector
  , invalidateSelector
  , getContinuationStreamsWithCompletionHandlerSelector
  , deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector
  , deleteAllSavedUserActivitiesWithCompletionHandlerSelector
  , activityTypeSelector
  , titleSelector
  , setTitleSelector
  , userInfoSelector
  , setUserInfoSelector
  , requiredUserInfoKeysSelector
  , setRequiredUserInfoKeysSelector
  , needsSaveSelector
  , setNeedsSaveSelector
  , webpageURLSelector
  , setWebpageURLSelector
  , referrerURLSelector
  , setReferrerURLSelector
  , expirationDateSelector
  , setExpirationDateSelector
  , keywordsSelector
  , setKeywordsSelector
  , supportsContinuationStreamsSelector
  , setSupportsContinuationStreamsSelector
  , delegateSelector
  , setDelegateSelector
  , targetContentIdentifierSelector
  , setTargetContentIdentifierSelector
  , eligibleForHandoffSelector
  , setEligibleForHandoffSelector
  , eligibleForSearchSelector
  , setEligibleForSearchSelector
  , eligibleForPublicIndexingSelector
  , setEligibleForPublicIndexingSelector
  , eligibleForPredictionSelector
  , setEligibleForPredictionSelector
  , persistentIdentifierSelector
  , setPersistentIdentifierSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- initWithActivityType:@
initWithActivityType :: (IsNSUserActivity nsUserActivity, IsNSString activityType) => nsUserActivity -> activityType -> IO (Id NSUserActivity)
initWithActivityType nsUserActivity  activityType =
  withObjCPtr activityType $ \raw_activityType ->
      sendMsg nsUserActivity (mkSelector "initWithActivityType:") (retPtr retVoid) [argPtr (castPtr raw_activityType :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSUserActivity)
init_ nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- addUserInfoEntriesFromDictionary:@
addUserInfoEntriesFromDictionary :: (IsNSUserActivity nsUserActivity, IsNSDictionary otherDictionary) => nsUserActivity -> otherDictionary -> IO ()
addUserInfoEntriesFromDictionary nsUserActivity  otherDictionary =
  withObjCPtr otherDictionary $ \raw_otherDictionary ->
      sendMsg nsUserActivity (mkSelector "addUserInfoEntriesFromDictionary:") retVoid [argPtr (castPtr raw_otherDictionary :: Ptr ())]

-- | @- becomeCurrent@
becomeCurrent :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO ()
becomeCurrent nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "becomeCurrent") retVoid []

-- | @- resignCurrent@
resignCurrent :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO ()
resignCurrent nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "resignCurrent") retVoid []

-- | @- invalidate@
invalidate :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO ()
invalidate nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "invalidate") retVoid []

-- | @- getContinuationStreamsWithCompletionHandler:@
getContinuationStreamsWithCompletionHandler :: IsNSUserActivity nsUserActivity => nsUserActivity -> Ptr () -> IO ()
getContinuationStreamsWithCompletionHandler nsUserActivity  completionHandler =
    sendMsg nsUserActivity (mkSelector "getContinuationStreamsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:@
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandler :: IsNSArray persistentIdentifiers => persistentIdentifiers -> Ptr () -> IO ()
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandler persistentIdentifiers handler =
  do
    cls' <- getRequiredClass "NSUserActivity"
    withObjCPtr persistentIdentifiers $ \raw_persistentIdentifiers ->
      sendClassMsg cls' (mkSelector "deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:") retVoid [argPtr (castPtr raw_persistentIdentifiers :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @+ deleteAllSavedUserActivitiesWithCompletionHandler:@
deleteAllSavedUserActivitiesWithCompletionHandler :: Ptr () -> IO ()
deleteAllSavedUserActivitiesWithCompletionHandler handler =
  do
    cls' <- getRequiredClass "NSUserActivity"
    sendClassMsg cls' (mkSelector "deleteAllSavedUserActivitiesWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- activityType@
activityType :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
activityType nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "activityType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
title nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setTitle nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSDictionary)
userInfo nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSUserActivity nsUserActivity, IsNSDictionary value) => nsUserActivity -> value -> IO ()
setUserInfo nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredUserInfoKeys@
requiredUserInfoKeys :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSSet)
requiredUserInfoKeys nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "requiredUserInfoKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiredUserInfoKeys:@
setRequiredUserInfoKeys :: (IsNSUserActivity nsUserActivity, IsNSSet value) => nsUserActivity -> value -> IO ()
setRequiredUserInfoKeys nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setRequiredUserInfoKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- needsSave@
needsSave :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
needsSave nsUserActivity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "needsSave") retCULong []

-- | @- setNeedsSave:@
setNeedsSave :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setNeedsSave nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setNeedsSave:") retVoid [argCULong (if value then 1 else 0)]

-- | @- webpageURL@
webpageURL :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSURL)
webpageURL nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "webpageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebpageURL:@
setWebpageURL :: (IsNSUserActivity nsUserActivity, IsNSURL value) => nsUserActivity -> value -> IO ()
setWebpageURL nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setWebpageURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referrerURL@
referrerURL :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSURL)
referrerURL nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "referrerURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferrerURL:@
setReferrerURL :: (IsNSUserActivity nsUserActivity, IsNSURL value) => nsUserActivity -> value -> IO ()
setReferrerURL nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setReferrerURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expirationDate@
expirationDate :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSDate)
expirationDate nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpirationDate:@
setExpirationDate :: (IsNSUserActivity nsUserActivity, IsNSDate value) => nsUserActivity -> value -> IO ()
setExpirationDate nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setExpirationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keywords@
keywords :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSSet)
keywords nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "keywords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeywords:@
setKeywords :: (IsNSUserActivity nsUserActivity, IsNSSet value) => nsUserActivity -> value -> IO ()
setKeywords nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setKeywords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportsContinuationStreams@
supportsContinuationStreams :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
supportsContinuationStreams nsUserActivity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "supportsContinuationStreams") retCULong []

-- | @- setSupportsContinuationStreams:@
setSupportsContinuationStreams :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setSupportsContinuationStreams nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setSupportsContinuationStreams:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delegate@
delegate :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO RawId
delegate nsUserActivity  =
    fmap (RawId . castPtr) $ sendMsg nsUserActivity (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSUserActivity nsUserActivity => nsUserActivity -> RawId -> IO ()
setDelegate nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- targetContentIdentifier@
targetContentIdentifier :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
targetContentIdentifier nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "targetContentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetContentIdentifier:@
setTargetContentIdentifier :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setTargetContentIdentifier nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setTargetContentIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- eligibleForHandoff@
eligibleForHandoff :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForHandoff nsUserActivity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "eligibleForHandoff") retCULong []

-- | @- setEligibleForHandoff:@
setEligibleForHandoff :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForHandoff nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setEligibleForHandoff:") retVoid [argCULong (if value then 1 else 0)]

-- | @- eligibleForSearch@
eligibleForSearch :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForSearch nsUserActivity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "eligibleForSearch") retCULong []

-- | @- setEligibleForSearch:@
setEligibleForSearch :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForSearch nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setEligibleForSearch:") retVoid [argCULong (if value then 1 else 0)]

-- | @- eligibleForPublicIndexing@
eligibleForPublicIndexing :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForPublicIndexing nsUserActivity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "eligibleForPublicIndexing") retCULong []

-- | @- setEligibleForPublicIndexing:@
setEligibleForPublicIndexing :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForPublicIndexing nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setEligibleForPublicIndexing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- eligibleForPrediction@
eligibleForPrediction :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO Bool
eligibleForPrediction nsUserActivity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserActivity (mkSelector "eligibleForPrediction") retCULong []

-- | @- setEligibleForPrediction:@
setEligibleForPrediction :: IsNSUserActivity nsUserActivity => nsUserActivity -> Bool -> IO ()
setEligibleForPrediction nsUserActivity  value =
    sendMsg nsUserActivity (mkSelector "setEligibleForPrediction:") retVoid [argCULong (if value then 1 else 0)]

-- | @- persistentIdentifier@
persistentIdentifier :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
persistentIdentifier nsUserActivity  =
    sendMsg nsUserActivity (mkSelector "persistentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPersistentIdentifier:@
setPersistentIdentifier :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setPersistentIdentifier nsUserActivity  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserActivity (mkSelector "setPersistentIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithActivityType:@
initWithActivityTypeSelector :: Selector
initWithActivityTypeSelector = mkSelector "initWithActivityType:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @addUserInfoEntriesFromDictionary:@
addUserInfoEntriesFromDictionarySelector :: Selector
addUserInfoEntriesFromDictionarySelector = mkSelector "addUserInfoEntriesFromDictionary:"

-- | @Selector@ for @becomeCurrent@
becomeCurrentSelector :: Selector
becomeCurrentSelector = mkSelector "becomeCurrent"

-- | @Selector@ for @resignCurrent@
resignCurrentSelector :: Selector
resignCurrentSelector = mkSelector "resignCurrent"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @getContinuationStreamsWithCompletionHandler:@
getContinuationStreamsWithCompletionHandlerSelector :: Selector
getContinuationStreamsWithCompletionHandlerSelector = mkSelector "getContinuationStreamsWithCompletionHandler:"

-- | @Selector@ for @deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:@
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector :: Selector
deleteSavedUserActivitiesWithPersistentIdentifiers_completionHandlerSelector = mkSelector "deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:"

-- | @Selector@ for @deleteAllSavedUserActivitiesWithCompletionHandler:@
deleteAllSavedUserActivitiesWithCompletionHandlerSelector :: Selector
deleteAllSavedUserActivitiesWithCompletionHandlerSelector = mkSelector "deleteAllSavedUserActivitiesWithCompletionHandler:"

-- | @Selector@ for @activityType@
activityTypeSelector :: Selector
activityTypeSelector = mkSelector "activityType"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @requiredUserInfoKeys@
requiredUserInfoKeysSelector :: Selector
requiredUserInfoKeysSelector = mkSelector "requiredUserInfoKeys"

-- | @Selector@ for @setRequiredUserInfoKeys:@
setRequiredUserInfoKeysSelector :: Selector
setRequiredUserInfoKeysSelector = mkSelector "setRequiredUserInfoKeys:"

-- | @Selector@ for @needsSave@
needsSaveSelector :: Selector
needsSaveSelector = mkSelector "needsSave"

-- | @Selector@ for @setNeedsSave:@
setNeedsSaveSelector :: Selector
setNeedsSaveSelector = mkSelector "setNeedsSave:"

-- | @Selector@ for @webpageURL@
webpageURLSelector :: Selector
webpageURLSelector = mkSelector "webpageURL"

-- | @Selector@ for @setWebpageURL:@
setWebpageURLSelector :: Selector
setWebpageURLSelector = mkSelector "setWebpageURL:"

-- | @Selector@ for @referrerURL@
referrerURLSelector :: Selector
referrerURLSelector = mkSelector "referrerURL"

-- | @Selector@ for @setReferrerURL:@
setReferrerURLSelector :: Selector
setReferrerURLSelector = mkSelector "setReferrerURL:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector
setExpirationDateSelector = mkSelector "setExpirationDate:"

-- | @Selector@ for @keywords@
keywordsSelector :: Selector
keywordsSelector = mkSelector "keywords"

-- | @Selector@ for @setKeywords:@
setKeywordsSelector :: Selector
setKeywordsSelector = mkSelector "setKeywords:"

-- | @Selector@ for @supportsContinuationStreams@
supportsContinuationStreamsSelector :: Selector
supportsContinuationStreamsSelector = mkSelector "supportsContinuationStreams"

-- | @Selector@ for @setSupportsContinuationStreams:@
setSupportsContinuationStreamsSelector :: Selector
setSupportsContinuationStreamsSelector = mkSelector "setSupportsContinuationStreams:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @targetContentIdentifier@
targetContentIdentifierSelector :: Selector
targetContentIdentifierSelector = mkSelector "targetContentIdentifier"

-- | @Selector@ for @setTargetContentIdentifier:@
setTargetContentIdentifierSelector :: Selector
setTargetContentIdentifierSelector = mkSelector "setTargetContentIdentifier:"

-- | @Selector@ for @eligibleForHandoff@
eligibleForHandoffSelector :: Selector
eligibleForHandoffSelector = mkSelector "eligibleForHandoff"

-- | @Selector@ for @setEligibleForHandoff:@
setEligibleForHandoffSelector :: Selector
setEligibleForHandoffSelector = mkSelector "setEligibleForHandoff:"

-- | @Selector@ for @eligibleForSearch@
eligibleForSearchSelector :: Selector
eligibleForSearchSelector = mkSelector "eligibleForSearch"

-- | @Selector@ for @setEligibleForSearch:@
setEligibleForSearchSelector :: Selector
setEligibleForSearchSelector = mkSelector "setEligibleForSearch:"

-- | @Selector@ for @eligibleForPublicIndexing@
eligibleForPublicIndexingSelector :: Selector
eligibleForPublicIndexingSelector = mkSelector "eligibleForPublicIndexing"

-- | @Selector@ for @setEligibleForPublicIndexing:@
setEligibleForPublicIndexingSelector :: Selector
setEligibleForPublicIndexingSelector = mkSelector "setEligibleForPublicIndexing:"

-- | @Selector@ for @eligibleForPrediction@
eligibleForPredictionSelector :: Selector
eligibleForPredictionSelector = mkSelector "eligibleForPrediction"

-- | @Selector@ for @setEligibleForPrediction:@
setEligibleForPredictionSelector :: Selector
setEligibleForPredictionSelector = mkSelector "setEligibleForPrediction:"

-- | @Selector@ for @persistentIdentifier@
persistentIdentifierSelector :: Selector
persistentIdentifierSelector = mkSelector "persistentIdentifier"

-- | @Selector@ for @setPersistentIdentifier:@
setPersistentIdentifierSelector :: Selector
setPersistentIdentifierSelector = mkSelector "setPersistentIdentifier:"

