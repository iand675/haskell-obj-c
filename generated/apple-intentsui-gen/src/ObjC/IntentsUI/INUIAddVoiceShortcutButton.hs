{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUIAddVoiceShortcutButton@.
module ObjC.IntentsUI.INUIAddVoiceShortcutButton
  ( INUIAddVoiceShortcutButton
  , IsINUIAddVoiceShortcutButton(..)
  , initWithStyle
  , init_
  , style
  , setStyle
  , delegate
  , setDelegate
  , shortcut
  , setShortcut
  , cornerRadius
  , setCornerRadius
  , cornerRadiusSelector
  , delegateSelector
  , initSelector
  , initWithStyleSelector
  , setCornerRadiusSelector
  , setDelegateSelector
  , setShortcutSelector
  , setStyleSelector
  , shortcutSelector
  , styleSelector

  -- * Enum types
  , INUIAddVoiceShortcutButtonStyle(INUIAddVoiceShortcutButtonStyle)
  , pattern INUIAddVoiceShortcutButtonStyleWhite
  , pattern INUIAddVoiceShortcutButtonStyleWhiteOutline
  , pattern INUIAddVoiceShortcutButtonStyleBlack
  , pattern INUIAddVoiceShortcutButtonStyleBlackOutline
  , pattern INUIAddVoiceShortcutButtonStyleAutomatic
  , pattern INUIAddVoiceShortcutButtonStyleAutomaticOutline

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IntentsUI.Internal.Classes
import ObjC.IntentsUI.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- | @- initWithStyle:@
initWithStyle :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> INUIAddVoiceShortcutButtonStyle -> IO (Id INUIAddVoiceShortcutButton)
initWithStyle inuiAddVoiceShortcutButton style =
  sendOwnedMessage inuiAddVoiceShortcutButton initWithStyleSelector style

-- | @- init@
init_ :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO (Id INUIAddVoiceShortcutButton)
init_ inuiAddVoiceShortcutButton =
  sendOwnedMessage inuiAddVoiceShortcutButton initSelector

-- | @- style@
style :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO INUIAddVoiceShortcutButtonStyle
style inuiAddVoiceShortcutButton =
  sendMessage inuiAddVoiceShortcutButton styleSelector

-- | @- setStyle:@
setStyle :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> INUIAddVoiceShortcutButtonStyle -> IO ()
setStyle inuiAddVoiceShortcutButton value =
  sendMessage inuiAddVoiceShortcutButton setStyleSelector value

-- | @- delegate@
delegate :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO RawId
delegate inuiAddVoiceShortcutButton =
  sendMessage inuiAddVoiceShortcutButton delegateSelector

-- | @- setDelegate:@
setDelegate :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> RawId -> IO ()
setDelegate inuiAddVoiceShortcutButton value =
  sendMessage inuiAddVoiceShortcutButton setDelegateSelector value

-- | @- shortcut@
shortcut :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO (Id INShortcut)
shortcut inuiAddVoiceShortcutButton =
  sendMessage inuiAddVoiceShortcutButton shortcutSelector

-- | @- setShortcut:@
setShortcut :: (IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton, IsINShortcut value) => inuiAddVoiceShortcutButton -> value -> IO ()
setShortcut inuiAddVoiceShortcutButton value =
  sendMessage inuiAddVoiceShortcutButton setShortcutSelector (toINShortcut value)

-- | A custom corner radius for the @INUIAddVoiceShortcutButton.@
--
-- If the provided corner radius is greater than half of the button’s height, it will be capped at half of the button’s height.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO CDouble
cornerRadius inuiAddVoiceShortcutButton =
  sendMessage inuiAddVoiceShortcutButton cornerRadiusSelector

-- | A custom corner radius for the @INUIAddVoiceShortcutButton.@
--
-- If the provided corner radius is greater than half of the button’s height, it will be capped at half of the button’s height.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> CDouble -> IO ()
setCornerRadius inuiAddVoiceShortcutButton value =
  sendMessage inuiAddVoiceShortcutButton setCornerRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStyle:@
initWithStyleSelector :: Selector '[INUIAddVoiceShortcutButtonStyle] (Id INUIAddVoiceShortcutButton)
initWithStyleSelector = mkSelector "initWithStyle:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INUIAddVoiceShortcutButton)
initSelector = mkSelector "init"

-- | @Selector@ for @style@
styleSelector :: Selector '[] INUIAddVoiceShortcutButtonStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[INUIAddVoiceShortcutButtonStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @shortcut@
shortcutSelector :: Selector '[] (Id INShortcut)
shortcutSelector = mkSelector "shortcut"

-- | @Selector@ for @setShortcut:@
setShortcutSelector :: Selector '[Id INShortcut] ()
setShortcutSelector = mkSelector "setShortcut:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

