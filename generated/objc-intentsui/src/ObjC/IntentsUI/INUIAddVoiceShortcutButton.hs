{-# LANGUAGE PatternSynonyms #-}
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
  , shortcut
  , setShortcut
  , cornerRadius
  , setCornerRadius
  , initWithStyleSelector
  , initSelector
  , styleSelector
  , setStyleSelector
  , shortcutSelector
  , setShortcutSelector
  , cornerRadiusSelector
  , setCornerRadiusSelector

  -- * Enum types
  , INUIAddVoiceShortcutButtonStyle(INUIAddVoiceShortcutButtonStyle)
  , pattern INUIAddVoiceShortcutButtonStyleWhite
  , pattern INUIAddVoiceShortcutButtonStyleWhiteOutline
  , pattern INUIAddVoiceShortcutButtonStyleBlack
  , pattern INUIAddVoiceShortcutButtonStyleBlackOutline
  , pattern INUIAddVoiceShortcutButtonStyleAutomatic
  , pattern INUIAddVoiceShortcutButtonStyleAutomaticOutline

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

import ObjC.IntentsUI.Internal.Classes
import ObjC.IntentsUI.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- | @- initWithStyle:@
initWithStyle :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> INUIAddVoiceShortcutButtonStyle -> IO (Id INUIAddVoiceShortcutButton)
initWithStyle inuiAddVoiceShortcutButton  style =
  sendMsg inuiAddVoiceShortcutButton (mkSelector "initWithStyle:") (retPtr retVoid) [argCULong (coerce style)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO (Id INUIAddVoiceShortcutButton)
init_ inuiAddVoiceShortcutButton  =
  sendMsg inuiAddVoiceShortcutButton (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- style@
style :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO INUIAddVoiceShortcutButtonStyle
style inuiAddVoiceShortcutButton  =
  fmap (coerce :: CULong -> INUIAddVoiceShortcutButtonStyle) $ sendMsg inuiAddVoiceShortcutButton (mkSelector "style") retCULong []

-- | @- setStyle:@
setStyle :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> INUIAddVoiceShortcutButtonStyle -> IO ()
setStyle inuiAddVoiceShortcutButton  value =
  sendMsg inuiAddVoiceShortcutButton (mkSelector "setStyle:") retVoid [argCULong (coerce value)]

-- | @- shortcut@
shortcut :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO (Id INShortcut)
shortcut inuiAddVoiceShortcutButton  =
  sendMsg inuiAddVoiceShortcutButton (mkSelector "shortcut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShortcut:@
setShortcut :: (IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton, IsINShortcut value) => inuiAddVoiceShortcutButton -> value -> IO ()
setShortcut inuiAddVoiceShortcutButton  value =
withObjCPtr value $ \raw_value ->
    sendMsg inuiAddVoiceShortcutButton (mkSelector "setShortcut:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A custom corner radius for the @INUIAddVoiceShortcutButton.@
--
-- If the provided corner radius is greater than half of the button’s height, it will be capped at half of the button’s height.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> IO CDouble
cornerRadius inuiAddVoiceShortcutButton  =
  sendMsg inuiAddVoiceShortcutButton (mkSelector "cornerRadius") retCDouble []

-- | A custom corner radius for the @INUIAddVoiceShortcutButton.@
--
-- If the provided corner radius is greater than half of the button’s height, it will be capped at half of the button’s height.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsINUIAddVoiceShortcutButton inuiAddVoiceShortcutButton => inuiAddVoiceShortcutButton -> CDouble -> IO ()
setCornerRadius inuiAddVoiceShortcutButton  value =
  sendMsg inuiAddVoiceShortcutButton (mkSelector "setCornerRadius:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStyle:@
initWithStyleSelector :: Selector
initWithStyleSelector = mkSelector "initWithStyle:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @shortcut@
shortcutSelector :: Selector
shortcutSelector = mkSelector "shortcut"

-- | @Selector@ for @setShortcut:@
setShortcutSelector :: Selector
setShortcutSelector = mkSelector "setShortcut:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

