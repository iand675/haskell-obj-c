{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSUserQuery@.
module ObjC.CoreSpotlight.CSUserQuery
  ( CSUserQuery
  , IsCSUserQuery(..)
  , prepare
  , prepareProtectionClasses
  , initWithUserQueryString_userQueryContext
  , userEngagedWithItem_visibleItems_userInteractionType
  , userEngagedWithSuggestion_visibleSuggestions_userInteractionType
  , start
  , cancel
  , foundSuggestionCount
  , cancelSelector
  , foundSuggestionCountSelector
  , initWithUserQueryString_userQueryContextSelector
  , prepareProtectionClassesSelector
  , prepareSelector
  , startSelector
  , userEngagedWithItem_visibleItems_userInteractionTypeSelector
  , userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector

  -- * Enum types
  , CSUserInteraction(CSUserInteraction)
  , pattern CSUserInteractionSelect
  , pattern CSUserInteractionDefault
  , pattern CSUserInteractionFocus

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ prepare@
prepare :: IO ()
prepare  =
  do
    cls' <- getRequiredClass "CSUserQuery"
    sendClassMessage cls' prepareSelector

-- | @+ prepareProtectionClasses:@
prepareProtectionClasses :: IsNSArray protectionClasses => protectionClasses -> IO ()
prepareProtectionClasses protectionClasses =
  do
    cls' <- getRequiredClass "CSUserQuery"
    sendClassMessage cls' prepareProtectionClassesSelector (toNSArray protectionClasses)

-- | @- initWithUserQueryString:userQueryContext:@
initWithUserQueryString_userQueryContext :: (IsCSUserQuery csUserQuery, IsNSString userQueryString, IsCSUserQueryContext userQueryContext) => csUserQuery -> userQueryString -> userQueryContext -> IO (Id CSUserQuery)
initWithUserQueryString_userQueryContext csUserQuery userQueryString userQueryContext =
  sendOwnedMessage csUserQuery initWithUserQueryString_userQueryContextSelector (toNSString userQueryString) (toCSUserQueryContext userQueryContext)

-- | @- userEngagedWithItem:visibleItems:userInteractionType:@
userEngagedWithItem_visibleItems_userInteractionType :: (IsCSUserQuery csUserQuery, IsCSSearchableItem item, IsNSArray visibleItems) => csUserQuery -> item -> visibleItems -> CSUserInteraction -> IO ()
userEngagedWithItem_visibleItems_userInteractionType csUserQuery item visibleItems userInteractionType =
  sendMessage csUserQuery userEngagedWithItem_visibleItems_userInteractionTypeSelector (toCSSearchableItem item) (toNSArray visibleItems) userInteractionType

-- | @- userEngagedWithSuggestion:visibleSuggestions:userInteractionType:@
userEngagedWithSuggestion_visibleSuggestions_userInteractionType :: (IsCSUserQuery csUserQuery, IsCSSuggestion suggestion, IsNSArray visibleSuggestions) => csUserQuery -> suggestion -> visibleSuggestions -> CSUserInteraction -> IO ()
userEngagedWithSuggestion_visibleSuggestions_userInteractionType csUserQuery suggestion visibleSuggestions userInteractionType =
  sendMessage csUserQuery userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector (toCSSuggestion suggestion) (toNSArray visibleSuggestions) userInteractionType

-- | @- start@
start :: IsCSUserQuery csUserQuery => csUserQuery -> IO ()
start csUserQuery =
  sendMessage csUserQuery startSelector

-- | @- cancel@
cancel :: IsCSUserQuery csUserQuery => csUserQuery -> IO ()
cancel csUserQuery =
  sendMessage csUserQuery cancelSelector

-- | @- foundSuggestionCount@
foundSuggestionCount :: IsCSUserQuery csUserQuery => csUserQuery -> IO CLong
foundSuggestionCount csUserQuery =
  sendMessage csUserQuery foundSuggestionCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepare@
prepareSelector :: Selector '[] ()
prepareSelector = mkSelector "prepare"

-- | @Selector@ for @prepareProtectionClasses:@
prepareProtectionClassesSelector :: Selector '[Id NSArray] ()
prepareProtectionClassesSelector = mkSelector "prepareProtectionClasses:"

-- | @Selector@ for @initWithUserQueryString:userQueryContext:@
initWithUserQueryString_userQueryContextSelector :: Selector '[Id NSString, Id CSUserQueryContext] (Id CSUserQuery)
initWithUserQueryString_userQueryContextSelector = mkSelector "initWithUserQueryString:userQueryContext:"

-- | @Selector@ for @userEngagedWithItem:visibleItems:userInteractionType:@
userEngagedWithItem_visibleItems_userInteractionTypeSelector :: Selector '[Id CSSearchableItem, Id NSArray, CSUserInteraction] ()
userEngagedWithItem_visibleItems_userInteractionTypeSelector = mkSelector "userEngagedWithItem:visibleItems:userInteractionType:"

-- | @Selector@ for @userEngagedWithSuggestion:visibleSuggestions:userInteractionType:@
userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector :: Selector '[Id CSSuggestion, Id NSArray, CSUserInteraction] ()
userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector = mkSelector "userEngagedWithSuggestion:visibleSuggestions:userInteractionType:"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @foundSuggestionCount@
foundSuggestionCountSelector :: Selector '[] CLong
foundSuggestionCountSelector = mkSelector "foundSuggestionCount"

