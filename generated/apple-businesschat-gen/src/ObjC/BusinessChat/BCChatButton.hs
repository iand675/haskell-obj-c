{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BCChatButton@.
module ObjC.BusinessChat.BCChatButton
  ( BCChatButton
  , IsBCChatButton(..)
  , initWithStyle
  , initWithCoder
  , initWithCoderSelector
  , initWithStyleSelector

  -- * Enum types
  , BCChatButtonStyle(BCChatButtonStyle)
  , pattern BCChatButtonStyleLight
  , pattern BCChatButtonStyleDark

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BusinessChat.Internal.Classes
import ObjC.BusinessChat.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates and returns a BCChatButton configured for a given style.
--
-- @style@ — The visual style of the button.
--
-- Returns: BCChatButton instance.
--
-- ObjC selector: @- initWithStyle:@
initWithStyle :: IsBCChatButton bcChatButton => bcChatButton -> BCChatButtonStyle -> IO (Id BCChatButton)
initWithStyle bcChatButton style =
  sendOwnedMessage bcChatButton initWithStyleSelector style

-- | @- initWithCoder:@
initWithCoder :: (IsBCChatButton bcChatButton, IsNSCoder coder) => bcChatButton -> coder -> IO (Id BCChatButton)
initWithCoder bcChatButton coder =
  sendOwnedMessage bcChatButton initWithCoderSelector (toNSCoder coder)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStyle:@
initWithStyleSelector :: Selector '[BCChatButtonStyle] (Id BCChatButton)
initWithStyleSelector = mkSelector "initWithStyle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id BCChatButton)
initWithCoderSelector = mkSelector "initWithCoder:"

