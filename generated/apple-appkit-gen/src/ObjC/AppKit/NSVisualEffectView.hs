{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSVisualEffectView@.
module ObjC.AppKit.NSVisualEffectView
  ( NSVisualEffectView
  , IsNSVisualEffectView(..)
  , viewDidMoveToWindow
  , viewWillMoveToWindow
  , material
  , setMaterial
  , interiorBackgroundStyle
  , blendingMode
  , setBlendingMode
  , state
  , setState
  , maskImage
  , setMaskImage
  , emphasized
  , setEmphasized
  , blendingModeSelector
  , emphasizedSelector
  , interiorBackgroundStyleSelector
  , maskImageSelector
  , materialSelector
  , setBlendingModeSelector
  , setEmphasizedSelector
  , setMaskImageSelector
  , setMaterialSelector
  , setStateSelector
  , stateSelector
  , viewDidMoveToWindowSelector
  , viewWillMoveToWindowSelector

  -- * Enum types
  , NSBackgroundStyle(NSBackgroundStyle)
  , pattern NSBackgroundStyleNormal
  , pattern NSBackgroundStyleEmphasized
  , pattern NSBackgroundStyleRaised
  , pattern NSBackgroundStyleLowered
  , NSVisualEffectBlendingMode(NSVisualEffectBlendingMode)
  , pattern NSVisualEffectBlendingModeBehindWindow
  , pattern NSVisualEffectBlendingModeWithinWindow
  , NSVisualEffectMaterial(NSVisualEffectMaterial)
  , pattern NSVisualEffectMaterialTitlebar
  , pattern NSVisualEffectMaterialSelection
  , pattern NSVisualEffectMaterialMenu
  , pattern NSVisualEffectMaterialPopover
  , pattern NSVisualEffectMaterialSidebar
  , pattern NSVisualEffectMaterialHeaderView
  , pattern NSVisualEffectMaterialSheet
  , pattern NSVisualEffectMaterialWindowBackground
  , pattern NSVisualEffectMaterialHUDWindow
  , pattern NSVisualEffectMaterialFullScreenUI
  , pattern NSVisualEffectMaterialToolTip
  , pattern NSVisualEffectMaterialContentBackground
  , pattern NSVisualEffectMaterialUnderWindowBackground
  , pattern NSVisualEffectMaterialUnderPageBackground
  , pattern NSVisualEffectMaterialAppearanceBased
  , pattern NSVisualEffectMaterialLight
  , pattern NSVisualEffectMaterialDark
  , pattern NSVisualEffectMaterialMediumLight
  , pattern NSVisualEffectMaterialUltraDark
  , NSVisualEffectState(NSVisualEffectState)
  , pattern NSVisualEffectStateFollowsWindowActiveState
  , pattern NSVisualEffectStateActive
  , pattern NSVisualEffectStateInactive

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- viewDidMoveToWindow@
viewDidMoveToWindow :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO ()
viewDidMoveToWindow nsVisualEffectView =
  sendMessage nsVisualEffectView viewDidMoveToWindowSelector

-- | @- viewWillMoveToWindow:@
viewWillMoveToWindow :: (IsNSVisualEffectView nsVisualEffectView, IsNSWindow newWindow) => nsVisualEffectView -> newWindow -> IO ()
viewWillMoveToWindow nsVisualEffectView newWindow =
  sendMessage nsVisualEffectView viewWillMoveToWindowSelector (toNSWindow newWindow)

-- | A value indicating which material is shown by the NSVisualEffectView.  See the comments on NSVisualEffectMaterial.  Defaults to NSVisualEffectMaterialAppearanceBased.  You should instead specify an appropriate semantic material value.  See the comments on NSVisualEffectMaterial.
--
-- ObjC selector: @- material@
material :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSVisualEffectMaterial
material nsVisualEffectView =
  sendMessage nsVisualEffectView materialSelector

-- | A value indicating which material is shown by the NSVisualEffectView.  See the comments on NSVisualEffectMaterial.  Defaults to NSVisualEffectMaterialAppearanceBased.  You should instead specify an appropriate semantic material value.  See the comments on NSVisualEffectMaterial.
--
-- ObjC selector: @- setMaterial:@
setMaterial :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> NSVisualEffectMaterial -> IO ()
setMaterial nsVisualEffectView value =
  sendMessage nsVisualEffectView setMaterialSelector value

-- | An NSBackgroundStyle value that most closely matches the look of the material shown by the NSVisualEffectView.
--
-- ObjC selector: @- interiorBackgroundStyle@
interiorBackgroundStyle :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSBackgroundStyle
interiorBackgroundStyle nsVisualEffectView =
  sendMessage nsVisualEffectView interiorBackgroundStyleSelector

-- | A value controlling how the NSVisualEffectView generates its material.  See the comments on NSVisualEffectBlendingMode.  Not all materials support both blending modes, so NSVisualEffectView may fall back to a more appropriate blending mode as needed.  Defaults to NSVisualEffectBlendingModeBehindWindow.
--
-- ObjC selector: @- blendingMode@
blendingMode :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSVisualEffectBlendingMode
blendingMode nsVisualEffectView =
  sendMessage nsVisualEffectView blendingModeSelector

-- | A value controlling how the NSVisualEffectView generates its material.  See the comments on NSVisualEffectBlendingMode.  Not all materials support both blending modes, so NSVisualEffectView may fall back to a more appropriate blending mode as needed.  Defaults to NSVisualEffectBlendingModeBehindWindow.
--
-- ObjC selector: @- setBlendingMode:@
setBlendingMode :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> NSVisualEffectBlendingMode -> IO ()
setBlendingMode nsVisualEffectView value =
  sendMessage nsVisualEffectView setBlendingModeSelector value

-- | A value controlling when the NSVisualEffectView takes on the active look.  See the comments on NSVisualEffectState.  Defaults to NSVisualEffectStateFollowsWindowActiveState.
--
-- ObjC selector: @- state@
state :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSVisualEffectState
state nsVisualEffectView =
  sendMessage nsVisualEffectView stateSelector

-- | A value controlling when the NSVisualEffectView takes on the active look.  See the comments on NSVisualEffectState.  Defaults to NSVisualEffectStateFollowsWindowActiveState.
--
-- ObjC selector: @- setState:@
setState :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> NSVisualEffectState -> IO ()
setState nsVisualEffectView value =
  sendMessage nsVisualEffectView setStateSelector value

-- | An image whose alpha channel is used to mask the material generated by the NSVisualEffectView.  (It does not also mask subviews.)  Defaults to nil.  It is best to set this to the smallest mask image possible and properly set the image's capInsets property to stretch it.  Setting the maskImage on an NSVisualEffectView that is the contentView of a window will correctly influence the window's shadow.
--
-- ObjC selector: @- maskImage@
maskImage :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO (Id NSImage)
maskImage nsVisualEffectView =
  sendMessage nsVisualEffectView maskImageSelector

-- | An image whose alpha channel is used to mask the material generated by the NSVisualEffectView.  (It does not also mask subviews.)  Defaults to nil.  It is best to set this to the smallest mask image possible and properly set the image's capInsets property to stretch it.  Setting the maskImage on an NSVisualEffectView that is the contentView of a window will correctly influence the window's shadow.
--
-- ObjC selector: @- setMaskImage:@
setMaskImage :: (IsNSVisualEffectView nsVisualEffectView, IsNSImage value) => nsVisualEffectView -> value -> IO ()
setMaskImage nsVisualEffectView value =
  sendMessage nsVisualEffectView setMaskImageSelector (toNSImage value)

-- | When YES, the material takes on the emphasized look.  Defaults to NO.  Some, but not all, materials change their look when emphasized.  This is used to indicate that an associated view has firstResponder status.
--
-- ObjC selector: @- emphasized@
emphasized :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO Bool
emphasized nsVisualEffectView =
  sendMessage nsVisualEffectView emphasizedSelector

-- | When YES, the material takes on the emphasized look.  Defaults to NO.  Some, but not all, materials change their look when emphasized.  This is used to indicate that an associated view has firstResponder status.
--
-- ObjC selector: @- setEmphasized:@
setEmphasized :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> Bool -> IO ()
setEmphasized nsVisualEffectView value =
  sendMessage nsVisualEffectView setEmphasizedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewDidMoveToWindow@
viewDidMoveToWindowSelector :: Selector '[] ()
viewDidMoveToWindowSelector = mkSelector "viewDidMoveToWindow"

-- | @Selector@ for @viewWillMoveToWindow:@
viewWillMoveToWindowSelector :: Selector '[Id NSWindow] ()
viewWillMoveToWindowSelector = mkSelector "viewWillMoveToWindow:"

-- | @Selector@ for @material@
materialSelector :: Selector '[] NSVisualEffectMaterial
materialSelector = mkSelector "material"

-- | @Selector@ for @setMaterial:@
setMaterialSelector :: Selector '[NSVisualEffectMaterial] ()
setMaterialSelector = mkSelector "setMaterial:"

-- | @Selector@ for @interiorBackgroundStyle@
interiorBackgroundStyleSelector :: Selector '[] NSBackgroundStyle
interiorBackgroundStyleSelector = mkSelector "interiorBackgroundStyle"

-- | @Selector@ for @blendingMode@
blendingModeSelector :: Selector '[] NSVisualEffectBlendingMode
blendingModeSelector = mkSelector "blendingMode"

-- | @Selector@ for @setBlendingMode:@
setBlendingModeSelector :: Selector '[NSVisualEffectBlendingMode] ()
setBlendingModeSelector = mkSelector "setBlendingMode:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] NSVisualEffectState
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[NSVisualEffectState] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @maskImage@
maskImageSelector :: Selector '[] (Id NSImage)
maskImageSelector = mkSelector "maskImage"

-- | @Selector@ for @setMaskImage:@
setMaskImageSelector :: Selector '[Id NSImage] ()
setMaskImageSelector = mkSelector "setMaskImage:"

-- | @Selector@ for @emphasized@
emphasizedSelector :: Selector '[] Bool
emphasizedSelector = mkSelector "emphasized"

-- | @Selector@ for @setEmphasized:@
setEmphasizedSelector :: Selector '[Bool] ()
setEmphasizedSelector = mkSelector "setEmphasized:"

