{-# LANGUAGE PatternSynonyms #-}
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
  , prepareSelector
  , prepareProtectionClassesSelector
  , initWithUserQueryString_userQueryContextSelector
  , userEngagedWithItem_visibleItems_userInteractionTypeSelector
  , userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector
  , startSelector
  , cancelSelector
  , foundSuggestionCountSelector

  -- * Enum types
  , CSUserInteraction(CSUserInteraction)
  , pattern CSUserInteractionSelect
  , pattern CSUserInteractionDefault
  , pattern CSUserInteractionFocus

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
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ prepare@
prepare :: IO ()
prepare  =
  do
    cls' <- getRequiredClass "CSUserQuery"
    sendClassMsg cls' (mkSelector "prepare") retVoid []

-- | @+ prepareProtectionClasses:@
prepareProtectionClasses :: IsNSArray protectionClasses => protectionClasses -> IO ()
prepareProtectionClasses protectionClasses =
  do
    cls' <- getRequiredClass "CSUserQuery"
    withObjCPtr protectionClasses $ \raw_protectionClasses ->
      sendClassMsg cls' (mkSelector "prepareProtectionClasses:") retVoid [argPtr (castPtr raw_protectionClasses :: Ptr ())]

-- | @- initWithUserQueryString:userQueryContext:@
initWithUserQueryString_userQueryContext :: (IsCSUserQuery csUserQuery, IsNSString userQueryString, IsCSUserQueryContext userQueryContext) => csUserQuery -> userQueryString -> userQueryContext -> IO (Id CSUserQuery)
initWithUserQueryString_userQueryContext csUserQuery  userQueryString userQueryContext =
withObjCPtr userQueryString $ \raw_userQueryString ->
  withObjCPtr userQueryContext $ \raw_userQueryContext ->
      sendMsg csUserQuery (mkSelector "initWithUserQueryString:userQueryContext:") (retPtr retVoid) [argPtr (castPtr raw_userQueryString :: Ptr ()), argPtr (castPtr raw_userQueryContext :: Ptr ())] >>= ownedObject . castPtr

-- | @- userEngagedWithItem:visibleItems:userInteractionType:@
userEngagedWithItem_visibleItems_userInteractionType :: (IsCSUserQuery csUserQuery, IsCSSearchableItem item, IsNSArray visibleItems) => csUserQuery -> item -> visibleItems -> CSUserInteraction -> IO ()
userEngagedWithItem_visibleItems_userInteractionType csUserQuery  item visibleItems userInteractionType =
withObjCPtr item $ \raw_item ->
  withObjCPtr visibleItems $ \raw_visibleItems ->
      sendMsg csUserQuery (mkSelector "userEngagedWithItem:visibleItems:userInteractionType:") retVoid [argPtr (castPtr raw_item :: Ptr ()), argPtr (castPtr raw_visibleItems :: Ptr ()), argCLong (coerce userInteractionType)]

-- | @- userEngagedWithSuggestion:visibleSuggestions:userInteractionType:@
userEngagedWithSuggestion_visibleSuggestions_userInteractionType :: (IsCSUserQuery csUserQuery, IsCSSuggestion suggestion, IsNSArray visibleSuggestions) => csUserQuery -> suggestion -> visibleSuggestions -> CSUserInteraction -> IO ()
userEngagedWithSuggestion_visibleSuggestions_userInteractionType csUserQuery  suggestion visibleSuggestions userInteractionType =
withObjCPtr suggestion $ \raw_suggestion ->
  withObjCPtr visibleSuggestions $ \raw_visibleSuggestions ->
      sendMsg csUserQuery (mkSelector "userEngagedWithSuggestion:visibleSuggestions:userInteractionType:") retVoid [argPtr (castPtr raw_suggestion :: Ptr ()), argPtr (castPtr raw_visibleSuggestions :: Ptr ()), argCLong (coerce userInteractionType)]

-- | @- start@
start :: IsCSUserQuery csUserQuery => csUserQuery -> IO ()
start csUserQuery  =
  sendMsg csUserQuery (mkSelector "start") retVoid []

-- | @- cancel@
cancel :: IsCSUserQuery csUserQuery => csUserQuery -> IO ()
cancel csUserQuery  =
  sendMsg csUserQuery (mkSelector "cancel") retVoid []

-- | @- foundSuggestionCount@
foundSuggestionCount :: IsCSUserQuery csUserQuery => csUserQuery -> IO CLong
foundSuggestionCount csUserQuery  =
  sendMsg csUserQuery (mkSelector "foundSuggestionCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepare@
prepareSelector :: Selector
prepareSelector = mkSelector "prepare"

-- | @Selector@ for @prepareProtectionClasses:@
prepareProtectionClassesSelector :: Selector
prepareProtectionClassesSelector = mkSelector "prepareProtectionClasses:"

-- | @Selector@ for @initWithUserQueryString:userQueryContext:@
initWithUserQueryString_userQueryContextSelector :: Selector
initWithUserQueryString_userQueryContextSelector = mkSelector "initWithUserQueryString:userQueryContext:"

-- | @Selector@ for @userEngagedWithItem:visibleItems:userInteractionType:@
userEngagedWithItem_visibleItems_userInteractionTypeSelector :: Selector
userEngagedWithItem_visibleItems_userInteractionTypeSelector = mkSelector "userEngagedWithItem:visibleItems:userInteractionType:"

-- | @Selector@ for @userEngagedWithSuggestion:visibleSuggestions:userInteractionType:@
userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector :: Selector
userEngagedWithSuggestion_visibleSuggestions_userInteractionTypeSelector = mkSelector "userEngagedWithSuggestion:visibleSuggestions:userInteractionType:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @foundSuggestionCount@
foundSuggestionCountSelector :: Selector
foundSuggestionCountSelector = mkSelector "foundSuggestionCount"

