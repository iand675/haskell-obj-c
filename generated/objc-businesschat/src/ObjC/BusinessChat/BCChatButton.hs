{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BCChatButton@.
module ObjC.BusinessChat.BCChatButton
  ( BCChatButton
  , IsBCChatButton(..)
  , initWithStyle
  , initWithCoder
  , initWithStyleSelector
  , initWithCoderSelector

  -- * Enum types
  , BCChatButtonStyle(BCChatButtonStyle)
  , pattern BCChatButtonStyleLight
  , pattern BCChatButtonStyleDark

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
initWithStyle bcChatButton  style =
  sendMsg bcChatButton (mkSelector "initWithStyle:") (retPtr retVoid) [argCLong (coerce style)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsBCChatButton bcChatButton, IsNSCoder coder) => bcChatButton -> coder -> IO (Id BCChatButton)
initWithCoder bcChatButton  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg bcChatButton (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStyle:@
initWithStyleSelector :: Selector
initWithStyleSelector = mkSelector "initWithStyle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

