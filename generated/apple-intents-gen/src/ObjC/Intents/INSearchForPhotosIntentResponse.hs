{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForPhotosIntentResponse@.
module ObjC.Intents.INSearchForPhotosIntentResponse
  ( INSearchForPhotosIntentResponse
  , IsINSearchForPhotosIntentResponse(..)
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
  , INSearchForPhotosIntentResponseCode(INSearchForPhotosIntentResponseCode)
  , pattern INSearchForPhotosIntentResponseCodeUnspecified
  , pattern INSearchForPhotosIntentResponseCodeReady
  , pattern INSearchForPhotosIntentResponseCodeContinueInApp
  , pattern INSearchForPhotosIntentResponseCodeFailure
  , pattern INSearchForPhotosIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchForPhotosIntentResponseCodeFailureAppConfigurationRequired

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
init_ :: IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse => inSearchForPhotosIntentResponse -> IO RawId
init_ inSearchForPhotosIntentResponse =
  sendOwnedMessage inSearchForPhotosIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse, IsNSUserActivity userActivity) => inSearchForPhotosIntentResponse -> INSearchForPhotosIntentResponseCode -> userActivity -> IO (Id INSearchForPhotosIntentResponse)
initWithCode_userActivity inSearchForPhotosIntentResponse code userActivity =
  sendOwnedMessage inSearchForPhotosIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse => inSearchForPhotosIntentResponse -> IO INSearchForPhotosIntentResponseCode
code inSearchForPhotosIntentResponse =
  sendMessage inSearchForPhotosIntentResponse codeSelector

-- | @- searchResultsCount@
searchResultsCount :: IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse => inSearchForPhotosIntentResponse -> IO (Id NSNumber)
searchResultsCount inSearchForPhotosIntentResponse =
  sendMessage inSearchForPhotosIntentResponse searchResultsCountSelector

-- | @- setSearchResultsCount:@
setSearchResultsCount :: (IsINSearchForPhotosIntentResponse inSearchForPhotosIntentResponse, IsNSNumber value) => inSearchForPhotosIntentResponse -> value -> IO ()
setSearchResultsCount inSearchForPhotosIntentResponse value =
  sendMessage inSearchForPhotosIntentResponse setSearchResultsCountSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchForPhotosIntentResponseCode, Id NSUserActivity] (Id INSearchForPhotosIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchForPhotosIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @searchResultsCount@
searchResultsCountSelector :: Selector '[] (Id NSNumber)
searchResultsCountSelector = mkSelector "searchResultsCount"

-- | @Selector@ for @setSearchResultsCount:@
setSearchResultsCountSelector :: Selector '[Id NSNumber] ()
setSearchResultsCountSelector = mkSelector "setSearchResultsCount:"

