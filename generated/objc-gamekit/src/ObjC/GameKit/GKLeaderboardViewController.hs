{-# LANGUAGE PatternSynonyms #-}
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
  , timeScopeSelector
  , setTimeScopeSelector
  , categorySelector
  , setCategorySelector

  -- * Enum types
  , GKLeaderboardTimeScope(GKLeaderboardTimeScope)
  , pattern GKLeaderboardTimeScopeToday
  , pattern GKLeaderboardTimeScopeWeek
  , pattern GKLeaderboardTimeScopeAllTime

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

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- timeScope@
timeScope :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> IO GKLeaderboardTimeScope
timeScope gkLeaderboardViewController  =
  fmap (coerce :: CLong -> GKLeaderboardTimeScope) $ sendMsg gkLeaderboardViewController (mkSelector "timeScope") retCLong []

-- | @- setTimeScope:@
setTimeScope :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> GKLeaderboardTimeScope -> IO ()
setTimeScope gkLeaderboardViewController  value =
  sendMsg gkLeaderboardViewController (mkSelector "setTimeScope:") retVoid [argCLong (coerce value)]

-- | @- category@
category :: IsGKLeaderboardViewController gkLeaderboardViewController => gkLeaderboardViewController -> IO (Id NSString)
category gkLeaderboardViewController  =
  sendMsg gkLeaderboardViewController (mkSelector "category") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCategory:@
setCategory :: (IsGKLeaderboardViewController gkLeaderboardViewController, IsNSString value) => gkLeaderboardViewController -> value -> IO ()
setCategory gkLeaderboardViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkLeaderboardViewController (mkSelector "setCategory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timeScope@
timeScopeSelector :: Selector
timeScopeSelector = mkSelector "timeScope"

-- | @Selector@ for @setTimeScope:@
setTimeScopeSelector :: Selector
setTimeScopeSelector = mkSelector "setTimeScope:"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector
setCategorySelector = mkSelector "setCategory:"

