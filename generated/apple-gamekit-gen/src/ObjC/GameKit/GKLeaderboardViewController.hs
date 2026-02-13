{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | View controller that provides the standard user interface for leaderboards.  Present modally from the top view controller.
--
-- Generated bindings for @GKLeaderboardViewController@.
module ObjC.GameKit.GKLeaderboardViewController
  ( GKLeaderboardViewController
  , IsGKLeaderboardViewController(..)
  , timeScope
  , setTimeScope
  , category
  , setCategory
  , leaderboardDelegate
  , setLeaderboardDelegate
  , categorySelector
  , leaderboardDelegateSelector
  , setCategorySelector
  , setLeaderboardDelegateSelector
  , setTimeScopeSelector
  , timeScopeSelector

  -- * Enum types
  , GKLeaderboardTimeScope(GKLeaderboardTimeScope)
  , pattern GKLeaderboardTimeScopeToday
  , pattern GKLeaderboardTimeScopeWeek
  , pattern GKLeaderboardTimeScopeAllTime

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- timeScope@
timeScope :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> IO GKLeaderboardTimeScope
timeScope gkLeaderboardViewController =
  sendMessage gkLeaderboardViewController timeScopeSelector

-- | @- setTimeScope:@
setTimeScope :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> GKLeaderboardTimeScope -> IO ()
setTimeScope gkLeaderboardViewController value =
  sendMessage gkLeaderboardViewController setTimeScopeSelector value

-- | @- category@
category :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> IO (Id NSString)
category gkLeaderboardViewController =
  sendMessage gkLeaderboardViewController categorySelector

-- | @- setCategory:@
setCategory :: (IsGKLeaderboardViewController gkLeaderboardViewController, IsNSString value) => gkLeaderboardViewController -> value -> IO ()
setCategory gkLeaderboardViewController value =
  sendMessage gkLeaderboardViewController setCategorySelector (toNSString value)

-- | @- leaderboardDelegate@
leaderboardDelegate :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> IO RawId
leaderboardDelegate gkLeaderboardViewController =
  sendMessage gkLeaderboardViewController leaderboardDelegateSelector

-- | @- setLeaderboardDelegate:@
setLeaderboardDelegate :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> RawId -> IO ()
setLeaderboardDelegate gkLeaderboardViewController value =
  sendMessage gkLeaderboardViewController setLeaderboardDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timeScope@
timeScopeSelector :: Selector '[] GKLeaderboardTimeScope
timeScopeSelector = mkSelector "timeScope"

-- | @Selector@ for @setTimeScope:@
setTimeScopeSelector :: Selector '[GKLeaderboardTimeScope] ()
setTimeScopeSelector = mkSelector "setTimeScope:"

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector '[Id NSString] ()
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @leaderboardDelegate@
leaderboardDelegateSelector :: Selector '[] RawId
leaderboardDelegateSelector = mkSelector "leaderboardDelegate"

-- | @Selector@ for @setLeaderboardDelegate:@
setLeaderboardDelegateSelector :: Selector '[RawId] ()
setLeaderboardDelegateSelector = mkSelector "setLeaderboardDelegate:"

