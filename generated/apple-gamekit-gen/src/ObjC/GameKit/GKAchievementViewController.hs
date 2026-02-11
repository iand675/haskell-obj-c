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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- achievementDelegate@
achievementDelegate :: IsGKAchievementViewController gkAchievementViewController => gkAchievementViewController -> IO RawId
achievementDelegate gkAchievementViewController  =
    fmap (RawId . castPtr) $ sendMsg gkAchievementViewController (mkSelector "achievementDelegate") (retPtr retVoid) []

-- | @- setAchievementDelegate:@
setAchievementDelegate :: IsGKAchievementViewController gkAchievementViewController => gkAchievementViewController -> RawId -> IO ()
setAchievementDelegate gkAchievementViewController  value =
    sendMsg gkAchievementViewController (mkSelector "setAchievementDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @achievementDelegate@
achievementDelegateSelector :: Selector
achievementDelegateSelector = mkSelector "achievementDelegate"

-- | @Selector@ for @setAchievementDelegate:@
setAchievementDelegateSelector :: Selector
setAchievementDelegateSelector = mkSelector "setAchievementDelegate:"

