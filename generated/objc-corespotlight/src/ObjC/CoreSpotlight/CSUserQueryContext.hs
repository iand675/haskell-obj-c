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
  , userQueryContextSelector
  , userQueryContextWithCurrentSuggestionSelector
  , enableRankedResultsSelector
  , setEnableRankedResultsSelector
  , disableSemanticSearchSelector
  , setDisableSemanticSearchSelector
  , maxResultCountSelector
  , setMaxResultCountSelector
  , maxSuggestionCountSelector
  , setMaxSuggestionCountSelector
  , maxRankedResultCountSelector
  , setMaxRankedResultCountSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ userQueryContext@
userQueryContext :: IO (Id CSUserQueryContext)
userQueryContext  =
  do
    cls' <- getRequiredClass "CSUserQueryContext"
    sendClassMsg cls' (mkSelector "userQueryContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ userQueryContextWithCurrentSuggestion:@
userQueryContextWithCurrentSuggestion :: IsCSSuggestion currentSuggestion => currentSuggestion -> IO (Id CSUserQueryContext)
userQueryContextWithCurrentSuggestion currentSuggestion =
  do
    cls' <- getRequiredClass "CSUserQueryContext"
    withObjCPtr currentSuggestion $ \raw_currentSuggestion ->
      sendClassMsg cls' (mkSelector "userQueryContextWithCurrentSuggestion:") (retPtr retVoid) [argPtr (castPtr raw_currentSuggestion :: Ptr ())] >>= retainedObject . castPtr

-- | @- enableRankedResults@
enableRankedResults :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO Bool
enableRankedResults csUserQueryContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csUserQueryContext (mkSelector "enableRankedResults") retCULong []

-- | @- setEnableRankedResults:@
setEnableRankedResults :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> Bool -> IO ()
setEnableRankedResults csUserQueryContext  value =
  sendMsg csUserQueryContext (mkSelector "setEnableRankedResults:") retVoid [argCULong (if value then 1 else 0)]

-- | @- disableSemanticSearch@
disableSemanticSearch :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO Bool
disableSemanticSearch csUserQueryContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csUserQueryContext (mkSelector "disableSemanticSearch") retCULong []

-- | @- setDisableSemanticSearch:@
setDisableSemanticSearch :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> Bool -> IO ()
setDisableSemanticSearch csUserQueryContext  value =
  sendMsg csUserQueryContext (mkSelector "setDisableSemanticSearch:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maxResultCount@
maxResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO CLong
maxResultCount csUserQueryContext  =
  sendMsg csUserQueryContext (mkSelector "maxResultCount") retCLong []

-- | @- setMaxResultCount:@
setMaxResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> CLong -> IO ()
setMaxResultCount csUserQueryContext  value =
  sendMsg csUserQueryContext (mkSelector "setMaxResultCount:") retVoid [argCLong (fromIntegral value)]

-- | @- maxSuggestionCount@
maxSuggestionCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO CLong
maxSuggestionCount csUserQueryContext  =
  sendMsg csUserQueryContext (mkSelector "maxSuggestionCount") retCLong []

-- | @- setMaxSuggestionCount:@
setMaxSuggestionCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> CLong -> IO ()
setMaxSuggestionCount csUserQueryContext  value =
  sendMsg csUserQueryContext (mkSelector "setMaxSuggestionCount:") retVoid [argCLong (fromIntegral value)]

-- | @- maxRankedResultCount@
maxRankedResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> IO CLong
maxRankedResultCount csUserQueryContext  =
  sendMsg csUserQueryContext (mkSelector "maxRankedResultCount") retCLong []

-- | @- setMaxRankedResultCount:@
setMaxRankedResultCount :: IsCSUserQueryContext csUserQueryContext => csUserQueryContext -> CLong -> IO ()
setMaxRankedResultCount csUserQueryContext  value =
  sendMsg csUserQueryContext (mkSelector "setMaxRankedResultCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userQueryContext@
userQueryContextSelector :: Selector
userQueryContextSelector = mkSelector "userQueryContext"

-- | @Selector@ for @userQueryContextWithCurrentSuggestion:@
userQueryContextWithCurrentSuggestionSelector :: Selector
userQueryContextWithCurrentSuggestionSelector = mkSelector "userQueryContextWithCurrentSuggestion:"

-- | @Selector@ for @enableRankedResults@
enableRankedResultsSelector :: Selector
enableRankedResultsSelector = mkSelector "enableRankedResults"

-- | @Selector@ for @setEnableRankedResults:@
setEnableRankedResultsSelector :: Selector
setEnableRankedResultsSelector = mkSelector "setEnableRankedResults:"

-- | @Selector@ for @disableSemanticSearch@
disableSemanticSearchSelector :: Selector
disableSemanticSearchSelector = mkSelector "disableSemanticSearch"

-- | @Selector@ for @setDisableSemanticSearch:@
setDisableSemanticSearchSelector :: Selector
setDisableSemanticSearchSelector = mkSelector "setDisableSemanticSearch:"

-- | @Selector@ for @maxResultCount@
maxResultCountSelector :: Selector
maxResultCountSelector = mkSelector "maxResultCount"

-- | @Selector@ for @setMaxResultCount:@
setMaxResultCountSelector :: Selector
setMaxResultCountSelector = mkSelector "setMaxResultCount:"

-- | @Selector@ for @maxSuggestionCount@
maxSuggestionCountSelector :: Selector
maxSuggestionCountSelector = mkSelector "maxSuggestionCount"

-- | @Selector@ for @setMaxSuggestionCount:@
setMaxSuggestionCountSelector :: Selector
setMaxSuggestionCountSelector = mkSelector "setMaxSuggestionCount:"

-- | @Selector@ for @maxRankedResultCount@
maxRankedResultCountSelector :: Selector
maxRankedResultCountSelector = mkSelector "maxRankedResultCount"

-- | @Selector@ for @setMaxRankedResultCount:@
setMaxRankedResultCountSelector :: Selector
setMaxRankedResultCountSelector = mkSelector "setMaxRankedResultCount:"

