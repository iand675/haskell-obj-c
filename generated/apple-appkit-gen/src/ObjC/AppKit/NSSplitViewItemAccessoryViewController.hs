{-# LANGUAGE DataKinds #-}
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
  , automaticallyAppliesContentInsetsSelector
  , hiddenSelector
  , preferredScrollEdgeEffectStyleSelector
  , setAutomaticallyAppliesContentInsetsSelector
  , setHiddenSelector
  , setPreferredScrollEdgeEffectStyleSelector
  , viewDidAppearSelector
  , viewDidDisappearSelector
  , viewWillAppearSelector
  , viewWillDisappearSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- viewWillAppear@
viewWillAppear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewWillAppear nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController viewWillAppearSelector

-- | @- viewDidAppear@
viewDidAppear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewDidAppear nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController viewDidAppearSelector

-- | @- viewWillDisappear@
viewWillDisappear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewWillDisappear nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController viewWillDisappearSelector

-- | @- viewDidDisappear@
viewDidDisappear :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO ()
viewDidDisappear nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController viewDidDisappearSelector

-- | When set, this property will collapse the accessory view to 0 height (animatable) but not remove it from the window. Set through the animator object to animate it.
--
-- ObjC selector: @- hidden@
hidden :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO Bool
hidden nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController hiddenSelector

-- | When set, this property will collapse the accessory view to 0 height (animatable) but not remove it from the window. Set through the animator object to animate it.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> Bool -> IO ()
setHidden nsSplitViewItemAccessoryViewController value =
  sendMessage nsSplitViewItemAccessoryViewController setHiddenSelector value

-- | Whether or not standard content insets should be applied to the view. Defaults to YES.
--
-- ObjC selector: @- automaticallyAppliesContentInsets@
automaticallyAppliesContentInsets :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO Bool
automaticallyAppliesContentInsets nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController automaticallyAppliesContentInsetsSelector

-- | Whether or not standard content insets should be applied to the view. Defaults to YES.
--
-- ObjC selector: @- setAutomaticallyAppliesContentInsets:@
setAutomaticallyAppliesContentInsets :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> Bool -> IO ()
setAutomaticallyAppliesContentInsets nsSplitViewItemAccessoryViewController value =
  sendMessage nsSplitViewItemAccessoryViewController setAutomaticallyAppliesContentInsetsSelector value

-- | The split view item accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the interior edge of a titlebar accessory:
--
-- splitViewItemAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyle :: IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController => nsSplitViewItemAccessoryViewController -> IO (Id NSScrollEdgeEffectStyle)
preferredScrollEdgeEffectStyle nsSplitViewItemAccessoryViewController =
  sendMessage nsSplitViewItemAccessoryViewController preferredScrollEdgeEffectStyleSelector

-- | The split view item accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the interior edge of a titlebar accessory:
--
-- splitViewItemAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyle :: (IsNSSplitViewItemAccessoryViewController nsSplitViewItemAccessoryViewController, IsNSScrollEdgeEffectStyle value) => nsSplitViewItemAccessoryViewController -> value -> IO ()
setPreferredScrollEdgeEffectStyle nsSplitViewItemAccessoryViewController value =
  sendMessage nsSplitViewItemAccessoryViewController setPreferredScrollEdgeEffectStyleSelector (toNSScrollEdgeEffectStyle value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewWillAppear@
viewWillAppearSelector :: Selector '[] ()
viewWillAppearSelector = mkSelector "viewWillAppear"

-- | @Selector@ for @viewDidAppear@
viewDidAppearSelector :: Selector '[] ()
viewDidAppearSelector = mkSelector "viewDidAppear"

-- | @Selector@ for @viewWillDisappear@
viewWillDisappearSelector :: Selector '[] ()
viewWillDisappearSelector = mkSelector "viewWillDisappear"

-- | @Selector@ for @viewDidDisappear@
viewDidDisappearSelector :: Selector '[] ()
viewDidDisappearSelector = mkSelector "viewDidDisappear"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @automaticallyAppliesContentInsets@
automaticallyAppliesContentInsetsSelector :: Selector '[] Bool
automaticallyAppliesContentInsetsSelector = mkSelector "automaticallyAppliesContentInsets"

-- | @Selector@ for @setAutomaticallyAppliesContentInsets:@
setAutomaticallyAppliesContentInsetsSelector :: Selector '[Bool] ()
setAutomaticallyAppliesContentInsetsSelector = mkSelector "setAutomaticallyAppliesContentInsets:"

-- | @Selector@ for @preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyleSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
preferredScrollEdgeEffectStyleSelector = mkSelector "preferredScrollEdgeEffectStyle"

-- | @Selector@ for @setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyleSelector :: Selector '[Id NSScrollEdgeEffectStyle] ()
setPreferredScrollEdgeEffectStyleSelector = mkSelector "setPreferredScrollEdgeEffectStyle:"

