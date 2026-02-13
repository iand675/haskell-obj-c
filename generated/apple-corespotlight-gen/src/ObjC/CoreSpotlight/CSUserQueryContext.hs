{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSUserQueryContext@.
module ObjC.CoreSpotlight.CSUserQueryContext
  ( CSUserQueryContext
  , IsCSUserQueryContext(..)
  , userQueryContext
  , userQueryContextWithCurrentSuggestion
  , enableRankedResults
  , setEnableRankedResults
  , disableSemanticSearch
  , setDisableSemanticSearch
  , maxResultCount
  , setMaxResultCount
  , maxSuggestionCount
  , setMaxSuggestionCount
  , maxRankedResultCount
  , setMaxRankedResultCount
  , disableSemanticSearchSelector
  , enableRankedResultsSelector
  , maxRankedResultCountSelector
  , maxResultCountSelector
  , maxSuggestionCountSelector
  , setDisableSemanticSearchSelector
  , setEnableRankedResultsSelector
  , setMaxRankedResultCountSelector
  , setMaxResultCountSelector
  , setMaxSuggestionCountSelector
  , userQueryContextSelector
  , userQueryContextWithCurrentSuggestionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ userQueryContext@
userQueryContext :: IO (Id CSUserQueryContext)
userQueryContext  =
  do
    cls' <- getRequiredClass "CSUserQueryContext"
    sendClassMessage cls' userQueryContextSelector

-- | @+ userQueryContextWithCurrentSuggestion:@
userQueryContextWithCurrentSuggestion :: IsCSSuggestion currentSuggestion => currentSuggestion -> IO (Id CSUserQueryContext)
userQueryContextWithCurrentSuggestion currentSuggestion =
  do
    cls' <- getRequiredClass "CSUserQueryContext"
    sendClassMessage cls' userQueryContextWithCurrentSuggestionSelector (toCSSuggestion currentSuggestion)

-- | @- enableRankedResults@
enableRankedResults :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO Bool
enableRankedResults csUserQueryContext =
  sendMessage csUserQueryContext enableRankedResultsSelector

-- | @- setEnableRankedResults:@
setEnableRankedResults :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> Bool -> IO ()
setEnableRankedResults csUserQueryContext value =
  sendMessage csUserQueryContext setEnableRankedResultsSelector value

-- | @- disableSemanticSearch@
disableSemanticSearch :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO Bool
disableSemanticSearch csUserQueryContext =
  sendMessage csUserQueryContext disableSemanticSearchSelector

-- | @- setDisableSemanticSearch:@
setDisableSemanticSearch :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> Bool -> IO ()
setDisableSemanticSearch csUserQueryContext value =
  sendMessage csUserQueryContext setDisableSemanticSearchSelector value

-- | @- maxResultCount@
maxResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO CLong
maxResultCount csUserQueryContext =
  sendMessage csUserQueryContext maxResultCountSelector

-- | @- setMaxResultCount:@
setMaxResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> CLong -> IO ()
setMaxResultCount csUserQueryContext value =
  sendMessage csUserQueryContext setMaxResultCountSelector value

-- | @- maxSuggestionCount@
maxSuggestionCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO CLong
maxSuggestionCount csUserQueryContext =
  sendMessage csUserQueryContext maxSuggestionCountSelector

-- | @- setMaxSuggestionCount:@
setMaxSuggestionCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> CLong -> IO ()
setMaxSuggestionCount csUserQueryContext value =
  sendMessage csUserQueryContext setMaxSuggestionCountSelector value

-- | @- maxRankedResultCount@
maxRankedResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO CLong
maxRankedResultCount csUserQueryContext =
  sendMessage csUserQueryContext maxRankedResultCountSelector

-- | @- setMaxRankedResultCount:@
setMaxRankedResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> CLong -> IO ()
setMaxRankedResultCount csUserQueryContext value =
  sendMessage csUserQueryContext setMaxRankedResultCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userQueryContext@
userQueryContextSelector :: Selector '[] (Id CSUserQueryContext)
userQueryContextSelector = mkSelector "userQueryContext"

-- | @Selector@ for @userQueryContextWithCurrentSuggestion:@
userQueryContextWithCurrentSuggestionSelector :: Selector '[Id CSSuggestion] (Id CSUserQueryContext)
userQueryContextWithCurrentSuggestionSelector = mkSelector "userQueryContextWithCurrentSuggestion:"

-- | @Selector@ for @enableRankedResults@
enableRankedResultsSelector :: Selector '[] Bool
enableRankedResultsSelector = mkSelector "enableRankedResults"

-- | @Selector@ for @setEnableRankedResults:@
setEnableRankedResultsSelector :: Selector '[Bool] ()
setEnableRankedResultsSelector = mkSelector "setEnableRankedResults:"

-- | @Selector@ for @disableSemanticSearch@
disableSemanticSearchSelector :: Selector '[] Bool
disableSemanticSearchSelector = mkSelector "disableSemanticSearch"

-- | @Selector@ for @setDisableSemanticSearch:@
setDisableSemanticSearchSelector :: Selector '[Bool] ()
setDisableSemanticSearchSelector = mkSelector "setDisableSemanticSearch:"

-- | @Selector@ for @maxResultCount@
maxResultCountSelector :: Selector '[] CLong
maxResultCountSelector = mkSelector "maxResultCount"

-- | @Selector@ for @setMaxResultCount:@
setMaxResultCountSelector :: Selector '[CLong] ()
setMaxResultCountSelector = mkSelector "setMaxResultCount:"

-- | @Selector@ for @maxSuggestionCount@
maxSuggestionCountSelector :: Selector '[] CLong
maxSuggestionCountSelector = mkSelector "maxSuggestionCount"

-- | @Selector@ for @setMaxSuggestionCount:@
setMaxSuggestionCountSelector :: Selector '[CLong] ()
setMaxSuggestionCountSelector = mkSelector "setMaxSuggestionCount:"

-- | @Selector@ for @maxRankedResultCount@
maxRankedResultCountSelector :: Selector '[] CLong
maxRankedResultCountSelector = mkSelector "maxRankedResultCount"

-- | @Selector@ for @setMaxRankedResultCount:@
setMaxRankedResultCountSelector :: Selector '[CLong] ()
setMaxRankedResultCountSelector = mkSelector "setMaxRankedResultCount:"

