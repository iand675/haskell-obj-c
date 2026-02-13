{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | View controller that provides the standard user interface for achievements. Present modally from the top view controller.
--
-- Generated bindings for @GKAchievementViewController@.
module ObjC.GameKit.GKAchievementViewController
  ( GKAchievementViewController
  , IsGKAchievementViewController(..)
  , achievementDelegate
  , setAchievementDelegate
  , achievementDelegateSelector
  , setAchievementDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- achievementDelegate@
achievementDelegate :: IsGKAchievementViewController gkAchievementViewController => gkAchievementViewController -> IO RawId
achievementDelegate gkAchievementViewController =
  sendMessage gkAchievementViewController achievementDelegateSelector

-- | @- setAchievementDelegate:@
setAchievementDelegate :: IsGKAchievementViewController gkAchievementViewController => gkAchievementViewController -> RawId -> IO ()
setAchievementDelegate gkAchievementViewController value =
  sendMessage gkAchievementViewController setAchievementDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @achievementDelegate@
achievementDelegateSelector :: Selector '[] RawId
achievementDelegateSelector = mkSelector "achievementDelegate"

-- | @Selector@ for @setAchievementDelegate:@
setAchievementDelegateSelector :: Selector '[RawId] ()
setAchievementDelegateSelector = mkSelector "setAchievementDelegate:"

