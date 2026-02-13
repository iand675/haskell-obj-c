{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartPhotoPlaybackIntentResponse@.
module ObjC.Intents.INStartPhotoPlaybackIntentResponse
  ( INStartPhotoPlaybackIntentResponse
  , IsINStartPhotoPlaybackIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , searchResultsCount
  , setSearchResultsCount
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , searchResultsCountSelector
  , setSearchResultsCountSelector

  -- * Enum types
  , INStartPhotoPlaybackIntentResponseCode(INStartPhotoPlaybackIntentResponseCode)
  , pattern INStartPhotoPlaybackIntentResponseCodeUnspecified
  , pattern INStartPhotoPlaybackIntentResponseCodeReady
  , pattern INStartPhotoPlaybackIntentResponseCodeContinueInApp
  , pattern INStartPhotoPlaybackIntentResponseCodeFailure
  , pattern INStartPhotoPlaybackIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartPhotoPlaybackIntentResponseCodeFailureAppConfigurationRequired

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse => inStartPhotoPlaybackIntentResponse -> IO RawId
init_ inStartPhotoPlaybackIntentResponse =
  sendOwnedMessage inStartPhotoPlaybackIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse, IsNSUserActivity userActivity) => inStartPhotoPlaybackIntentResponse -> INStartPhotoPlaybackIntentResponseCode -> userActivity -> IO (Id INStartPhotoPlaybackIntentResponse)
initWithCode_userActivity inStartPhotoPlaybackIntentResponse code userActivity =
  sendOwnedMessage inStartPhotoPlaybackIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse => inStartPhotoPlaybackIntentResponse -> IO INStartPhotoPlaybackIntentResponseCode
code inStartPhotoPlaybackIntentResponse =
  sendMessage inStartPhotoPlaybackIntentResponse codeSelector

-- | @- searchResultsCount@
searchResultsCount :: IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse => inStartPhotoPlaybackIntentResponse -> IO (Id NSNumber)
searchResultsCount inStartPhotoPlaybackIntentResponse =
  sendMessage inStartPhotoPlaybackIntentResponse searchResultsCountSelector

-- | @- setSearchResultsCount:@
setSearchResultsCount :: (IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse, IsNSNumber value) => inStartPhotoPlaybackIntentResponse -> value -> IO ()
setSearchResultsCount inStartPhotoPlaybackIntentResponse value =
  sendMessage inStartPhotoPlaybackIntentResponse setSearchResultsCountSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INStartPhotoPlaybackIntentResponseCode, Id NSUserActivity] (Id INStartPhotoPlaybackIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INStartPhotoPlaybackIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @searchResultsCount@
searchResultsCountSelector :: Selector '[] (Id NSNumber)
searchResultsCountSelector = mkSelector "searchResultsCount"

-- | @Selector@ for @setSearchResultsCount:@
setSearchResultsCountSelector :: Selector '[Id NSNumber] ()
setSearchResultsCountSelector = mkSelector "setSearchResultsCount:"

