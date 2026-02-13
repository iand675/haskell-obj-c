{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKAchievementChallenge@.
module ObjC.GameKit.GKAchievementChallenge
  ( GKAchievementChallenge
  , IsGKAchievementChallenge(..)
  , achievement
  , achievementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The achievement to achieve to satisfy this challenge
--
-- ObjC selector: @- achievement@
achievement :: IsGKAchievementChallenge gkAchievementChallenge => gkAchievementChallenge -> IO (Id GKAchievement)
achievement gkAchievementChallenge =
  sendMessage gkAchievementChallenge achievementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @achievement@
achievementSelector :: Selector '[] (Id GKAchievement)
achievementSelector = mkSelector "achievement"

