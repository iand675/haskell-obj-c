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
import ObjC.Foundation.Internal.Classes

-- | The achievement to achieve to satisfy this challenge
--
-- ObjC selector: @- achievement@
achievement :: IsGKAchievementChallenge gkAchievementChallenge => gkAchievementChallenge -> IO (Id GKAchievement)
achievement gkAchievementChallenge  =
  sendMsg gkAchievementChallenge (mkSelector "achievement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @achievement@
achievementSelector :: Selector
achievementSelector = mkSelector "achievement"

