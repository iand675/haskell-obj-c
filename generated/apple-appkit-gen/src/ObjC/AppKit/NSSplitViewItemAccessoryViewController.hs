{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSplitViewItemAccessoryViewController@.
module ObjC.AppKit.NSSplitViewItemAccessoryViewController
  ( NSSplitViewItemAccessoryViewController
  , IsNSSplitViewItemAccessoryViewController(..)
  , viewWillAppear
  , viewDidAppear
  , viewWillDisappear
  , viewDidDisappear
  , hidden
  , setHidden
  , automaticallyAppliesContentInsets
  , setAutomaticallyAppliesContentInsets
  , preferredScrollEdgeEffectStyle
  , setPreferredScrollEdgeEffectStyle
  , viewWillAppearSelector
  , viewDidAppearSelector
  , viewWillDisappearSelector
  , viewDidDisappearSelector
  , hiddenSelector
  , setHiddenSelector
  , automaticallyAppliesContentInsetsSelector
  , setAutomaticallyAppliesContentInsetsSelector
  , preferredScrollEdgeEffectStyleSelector
  , setPreferredScrollEdgeEffectStyleSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- viewWillAppear@
viewWillAppear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewWillAppear nsSplitViewItemAccessoryViewController  =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "viewWillAppear") retVoid []

-- | @- viewDidAppear@
viewDidAppear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewDidAppear nsSplitViewItemAccessoryViewController  =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "viewDidAppear") retVoid []

-- | @- viewWillDisappear@
viewWillDisappear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewWillDisappear nsSplitViewItemAccessoryViewController  =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "viewWillDisappear") retVoid []

-- | @- viewDidDisappear@
viewDidDisappear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewDidDisappear nsSplitViewItemAccessoryViewController  =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "viewDidDisappear") retVoid []

-- | When set, this property will collapse the accessory view to 0 height (animatable) but not remove it from the window. Set through the animator object to animate it.
--
-- ObjC selector: @- hidden@
hidden :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO Bool
hidden nsSplitViewItemAccessoryViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItemAccessoryViewController (mkSelector "hidden") retCULong []

-- | When set, this property will collapse the accessory view to 0 height (animatable) but not remove it from the window. Set through the animator object to animate it.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> Bool -> IO ()
setHidden nsSplitViewItemAccessoryViewController  value =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether or not standard content insets should be applied to the view. Defaults to YES.
--
-- ObjC selector: @- automaticallyAppliesContentInsets@
automaticallyAppliesContentInsets :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO Bool
automaticallyAppliesContentInsets nsSplitViewItemAccessoryViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitViewItemAccessoryViewController (mkSelector "automaticallyAppliesContentInsets") retCULong []

-- | Whether or not standard content insets should be applied to the view. Defaults to YES.
--
-- ObjC selector: @- setAutomaticallyAppliesContentInsets:@
setAutomaticallyAppliesContentInsets :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> Bool -> IO ()
setAutomaticallyAppliesContentInsets nsSplitViewItemAccessoryViewController  value =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "setAutomaticallyAppliesContentInsets:") retVoid [argCULong (if value then 1 else 0)]

-- | The split view item accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the interior edge of a titlebar accessory:
--
-- splitViewItemAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyle :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO (Id NSScrollEdgeEffectStyle)
preferredScrollEdgeEffectStyle nsSplitViewItemAccessoryViewController  =
    sendMsg nsSplitViewItemAccessoryViewController (mkSelector "preferredScrollEdgeEffectStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The split view item accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the interior edge of a titlebar accessory:
--
-- splitViewItemAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyle :: (IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController, IsNSScrollEdgeEffectStyle value) => nsSplitViewItemAccessoryViewController -> value -> IO ()
setPreferredScrollEdgeEffectStyle nsSplitViewItemAccessoryViewController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSplitViewItemAccessoryViewController (mkSelector "setPreferredScrollEdgeEffectStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewWillAppear@
viewWillAppearSelector :: Selector
viewWillAppearSelector = mkSelector "viewWillAppear"

-- | @Selector@ for @viewDidAppear@
viewDidAppearSelector :: Selector
viewDidAppearSelector = mkSelector "viewDidAppear"

-- | @Selector@ for @viewWillDisappear@
viewWillDisappearSelector :: Selector
viewWillDisappearSelector = mkSelector "viewWillDisappear"

-- | @Selector@ for @viewDidDisappear@
viewDidDisappearSelector :: Selector
viewDidDisappearSelector = mkSelector "viewDidDisappear"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @automaticallyAppliesContentInsets@
automaticallyAppliesContentInsetsSelector :: Selector
automaticallyAppliesContentInsetsSelector = mkSelector "automaticallyAppliesContentInsets"

-- | @Selector@ for @setAutomaticallyAppliesContentInsets:@
setAutomaticallyAppliesContentInsetsSelector :: Selector
setAutomaticallyAppliesContentInsetsSelector = mkSelector "setAutomaticallyAppliesContentInsets:"

-- | @Selector@ for @preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyleSelector :: Selector
preferredScrollEdgeEffectStyleSelector = mkSelector "preferredScrollEdgeEffectStyle"

-- | @Selector@ for @setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyleSelector :: Selector
setPreferredScrollEdgeEffectStyleSelector = mkSelector "setPreferredScrollEdgeEffectStyle:"

