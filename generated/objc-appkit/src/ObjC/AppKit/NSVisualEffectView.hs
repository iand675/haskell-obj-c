{-# LANGUAGE PatternSynonyms #-}
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
  , viewDidMoveToWindowSelector
  , viewWillMoveToWindowSelector
  , materialSelector
  , setMaterialSelector
  , interiorBackgroundStyleSelector
  , blendingModeSelector
  , setBlendingModeSelector
  , stateSelector
  , setStateSelector
  , maskImageSelector
  , setMaskImageSelector
  , emphasizedSelector
  , setEmphasizedSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- viewDidMoveToWindow@
viewDidMoveToWindow :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO ()
viewDidMoveToWindow nsVisualEffectView  =
  sendMsg nsVisualEffectView (mkSelector "viewDidMoveToWindow") retVoid []

-- | @- viewWillMoveToWindow:@
viewWillMoveToWindow :: (IsNSVisualEffectView nsVisualEffectView, IsNSWindow newWindow) => nsVisualEffectView -> newWindow -> IO ()
viewWillMoveToWindow nsVisualEffectView  newWindow =
withObjCPtr newWindow $ \raw_newWindow ->
    sendMsg nsVisualEffectView (mkSelector "viewWillMoveToWindow:") retVoid [argPtr (castPtr raw_newWindow :: Ptr ())]

-- | A value indicating which material is shown by the NSVisualEffectView.  See the comments on NSVisualEffectMaterial.  Defaults to NSVisualEffectMaterialAppearanceBased.  You should instead specify an appropriate semantic material value.  See the comments on NSVisualEffectMaterial.
--
-- ObjC selector: @- material@
material :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSVisualEffectMaterial
material nsVisualEffectView  =
  fmap (coerce :: CLong -> NSVisualEffectMaterial) $ sendMsg nsVisualEffectView (mkSelector "material") retCLong []

-- | A value indicating which material is shown by the NSVisualEffectView.  See the comments on NSVisualEffectMaterial.  Defaults to NSVisualEffectMaterialAppearanceBased.  You should instead specify an appropriate semantic material value.  See the comments on NSVisualEffectMaterial.
--
-- ObjC selector: @- setMaterial:@
setMaterial :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> NSVisualEffectMaterial -> IO ()
setMaterial nsVisualEffectView  value =
  sendMsg nsVisualEffectView (mkSelector "setMaterial:") retVoid [argCLong (coerce value)]

-- | An NSBackgroundStyle value that most closely matches the look of the material shown by the NSVisualEffectView.
--
-- ObjC selector: @- interiorBackgroundStyle@
interiorBackgroundStyle :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSBackgroundStyle
interiorBackgroundStyle nsVisualEffectView  =
  fmap (coerce :: CLong -> NSBackgroundStyle) $ sendMsg nsVisualEffectView (mkSelector "interiorBackgroundStyle") retCLong []

-- | A value controlling how the NSVisualEffectView generates its material.  See the comments on NSVisualEffectBlendingMode.  Not all materials support both blending modes, so NSVisualEffectView may fall back to a more appropriate blending mode as needed.  Defaults to NSVisualEffectBlendingModeBehindWindow.
--
-- ObjC selector: @- blendingMode@
blendingMode :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSVisualEffectBlendingMode
blendingMode nsVisualEffectView  =
  fmap (coerce :: CLong -> NSVisualEffectBlendingMode) $ sendMsg nsVisualEffectView (mkSelector "blendingMode") retCLong []

-- | A value controlling how the NSVisualEffectView generates its material.  See the comments on NSVisualEffectBlendingMode.  Not all materials support both blending modes, so NSVisualEffectView may fall back to a more appropriate blending mode as needed.  Defaults to NSVisualEffectBlendingModeBehindWindow.
--
-- ObjC selector: @- setBlendingMode:@
setBlendingMode :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> NSVisualEffectBlendingMode -> IO ()
setBlendingMode nsVisualEffectView  value =
  sendMsg nsVisualEffectView (mkSelector "setBlendingMode:") retVoid [argCLong (coerce value)]

-- | A value controlling when the NSVisualEffectView takes on the active look.  See the comments on NSVisualEffectState.  Defaults to NSVisualEffectStateFollowsWindowActiveState.
--
-- ObjC selector: @- state@
state :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO NSVisualEffectState
state nsVisualEffectView  =
  fmap (coerce :: CLong -> NSVisualEffectState) $ sendMsg nsVisualEffectView (mkSelector "state") retCLong []

-- | A value controlling when the NSVisualEffectView takes on the active look.  See the comments on NSVisualEffectState.  Defaults to NSVisualEffectStateFollowsWindowActiveState.
--
-- ObjC selector: @- setState:@
setState :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> NSVisualEffectState -> IO ()
setState nsVisualEffectView  value =
  sendMsg nsVisualEffectView (mkSelector "setState:") retVoid [argCLong (coerce value)]

-- | An image whose alpha channel is used to mask the material generated by the NSVisualEffectView.  (It does not also mask subviews.)  Defaults to nil.  It is best to set this to the smallest mask image possible and properly set the image's capInsets property to stretch it.  Setting the maskImage on an NSVisualEffectView that is the contentView of a window will correctly influence the window's shadow.
--
-- ObjC selector: @- maskImage@
maskImage :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO (Id NSImage)
maskImage nsVisualEffectView  =
  sendMsg nsVisualEffectView (mkSelector "maskImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An image whose alpha channel is used to mask the material generated by the NSVisualEffectView.  (It does not also mask subviews.)  Defaults to nil.  It is best to set this to the smallest mask image possible and properly set the image's capInsets property to stretch it.  Setting the maskImage on an NSVisualEffectView that is the contentView of a window will correctly influence the window's shadow.
--
-- ObjC selector: @- setMaskImage:@
setMaskImage :: (IsNSVisualEffectView nsVisualEffectView, IsNSImage value) => nsVisualEffectView -> value -> IO ()
setMaskImage nsVisualEffectView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsVisualEffectView (mkSelector "setMaskImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | When YES, the material takes on the emphasized look.  Defaults to NO.  Some, but not all, materials change their look when emphasized.  This is used to indicate that an associated view has firstResponder status.
--
-- ObjC selector: @- emphasized@
emphasized :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> IO Bool
emphasized nsVisualEffectView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsVisualEffectView (mkSelector "emphasized") retCULong []

-- | When YES, the material takes on the emphasized look.  Defaults to NO.  Some, but not all, materials change their look when emphasized.  This is used to indicate that an associated view has firstResponder status.
--
-- ObjC selector: @- setEmphasized:@
setEmphasized :: IsNSVisualEffectView nsVisualEffectView => nsVisualEffectView -> Bool -> IO ()
setEmphasized nsVisualEffectView  value =
  sendMsg nsVisualEffectView (mkSelector "setEmphasized:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewDidMoveToWindow@
viewDidMoveToWindowSelector :: Selector
viewDidMoveToWindowSelector = mkSelector "viewDidMoveToWindow"

-- | @Selector@ for @viewWillMoveToWindow:@
viewWillMoveToWindowSelector :: Selector
viewWillMoveToWindowSelector = mkSelector "viewWillMoveToWindow:"

-- | @Selector@ for @material@
materialSelector :: Selector
materialSelector = mkSelector "material"

-- | @Selector@ for @setMaterial:@
setMaterialSelector :: Selector
setMaterialSelector = mkSelector "setMaterial:"

-- | @Selector@ for @interiorBackgroundStyle@
interiorBackgroundStyleSelector :: Selector
interiorBackgroundStyleSelector = mkSelector "interiorBackgroundStyle"

-- | @Selector@ for @blendingMode@
blendingModeSelector :: Selector
blendingModeSelector = mkSelector "blendingMode"

-- | @Selector@ for @setBlendingMode:@
setBlendingModeSelector :: Selector
setBlendingModeSelector = mkSelector "setBlendingMode:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @maskImage@
maskImageSelector :: Selector
maskImageSelector = mkSelector "maskImage"

-- | @Selector@ for @setMaskImage:@
setMaskImageSelector :: Selector
setMaskImageSelector = mkSelector "setMaskImage:"

-- | @Selector@ for @emphasized@
emphasizedSelector :: Selector
emphasizedSelector = mkSelector "emphasized"

-- | @Selector@ for @setEmphasized:@
setEmphasizedSelector :: Selector
setEmphasizedSelector = mkSelector "setEmphasized:"

