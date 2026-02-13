{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTitlebarAccessoryViewController@.
module ObjC.AppKit.NSTitlebarAccessoryViewController
  ( NSTitlebarAccessoryViewController
  , IsNSTitlebarAccessoryViewController(..)
  , viewWillAppear
  , viewDidAppear
  , viewDidDisappear
  , layoutAttribute
  , setLayoutAttribute
  , fullScreenMinHeight
  , setFullScreenMinHeight
  , hidden
  , setHidden
  , automaticallyAdjustsSize
  , setAutomaticallyAdjustsSize
  , preferredScrollEdgeEffectStyle
  , setPreferredScrollEdgeEffectStyle
  , automaticallyAdjustsSizeSelector
  , fullScreenMinHeightSelector
  , hiddenSelector
  , layoutAttributeSelector
  , preferredScrollEdgeEffectStyleSelector
  , setAutomaticallyAdjustsSizeSelector
  , setFullScreenMinHeightSelector
  , setHiddenSelector
  , setLayoutAttributeSelector
  , setPreferredScrollEdgeEffectStyleSelector
  , viewDidAppearSelector
  , viewDidDisappearSelector
  , viewWillAppearSelector

  -- * Enum types
  , NSLayoutAttribute(NSLayoutAttribute)
  , pattern NSLayoutAttributeLeft
  , pattern NSLayoutAttributeRight
  , pattern NSLayoutAttributeTop
  , pattern NSLayoutAttributeBottom
  , pattern NSLayoutAttributeLeading
  , pattern NSLayoutAttributeTrailing
  , pattern NSLayoutAttributeWidth
  , pattern NSLayoutAttributeHeight
  , pattern NSLayoutAttributeCenterX
  , pattern NSLayoutAttributeCenterY
  , pattern NSLayoutAttributeLastBaseline
  , pattern NSLayoutAttributeBaseline
  , pattern NSLayoutAttributeFirstBaseline
  , pattern NSLayoutAttributeNotAnAttribute

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

-- | @- viewWillAppear@
viewWillAppear :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO ()
viewWillAppear nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController viewWillAppearSelector

-- | @- viewDidAppear@
viewDidAppear :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO ()
viewDidAppear nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController viewDidAppearSelector

-- | @- viewDidDisappear@
viewDidDisappear :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO ()
viewDidDisappear nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController viewDidDisappearSelector

-- | @- layoutAttribute@
layoutAttribute :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO NSLayoutAttribute
layoutAttribute nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController layoutAttributeSelector

-- | @- setLayoutAttribute:@
setLayoutAttribute :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> NSLayoutAttribute -> IO ()
setLayoutAttribute nsTitlebarAccessoryViewController value =
  sendMessage nsTitlebarAccessoryViewController setLayoutAttributeSelector value

-- | @- fullScreenMinHeight@
fullScreenMinHeight :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO CDouble
fullScreenMinHeight nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController fullScreenMinHeightSelector

-- | @- setFullScreenMinHeight:@
setFullScreenMinHeight :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> CDouble -> IO ()
setFullScreenMinHeight nsTitlebarAccessoryViewController value =
  sendMessage nsTitlebarAccessoryViewController setFullScreenMinHeightSelector value

-- | @- hidden@
hidden :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO Bool
hidden nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> Bool -> IO ()
setHidden nsTitlebarAccessoryViewController value =
  sendMessage nsTitlebarAccessoryViewController setHiddenSelector value

-- | @- automaticallyAdjustsSize@
automaticallyAdjustsSize :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO Bool
automaticallyAdjustsSize nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController automaticallyAdjustsSizeSelector

-- | @- setAutomaticallyAdjustsSize:@
setAutomaticallyAdjustsSize :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> Bool -> IO ()
setAutomaticallyAdjustsSize nsTitlebarAccessoryViewController value =
  sendMessage nsTitlebarAccessoryViewController setAutomaticallyAdjustsSizeSelector value

-- | The titlebar accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the bottom edge of a titlebar accessory:
--
-- titlebarAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyle :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO (Id NSScrollEdgeEffectStyle)
preferredScrollEdgeEffectStyle nsTitlebarAccessoryViewController =
  sendMessage nsTitlebarAccessoryViewController preferredScrollEdgeEffectStyleSelector

-- | The titlebar accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the bottom edge of a titlebar accessory:
--
-- titlebarAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyle :: (IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController, IsNSScrollEdgeEffectStyle value) => nsTitlebarAccessoryViewController -> value -> IO ()
setPreferredScrollEdgeEffectStyle nsTitlebarAccessoryViewController value =
  sendMessage nsTitlebarAccessoryViewController setPreferredScrollEdgeEffectStyleSelector (toNSScrollEdgeEffectStyle value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewWillAppear@
viewWillAppearSelector :: Selector '[] ()
viewWillAppearSelector = mkSelector "viewWillAppear"

-- | @Selector@ for @viewDidAppear@
viewDidAppearSelector :: Selector '[] ()
viewDidAppearSelector = mkSelector "viewDidAppear"

-- | @Selector@ for @viewDidDisappear@
viewDidDisappearSelector :: Selector '[] ()
viewDidDisappearSelector = mkSelector "viewDidDisappear"

-- | @Selector@ for @layoutAttribute@
layoutAttributeSelector :: Selector '[] NSLayoutAttribute
layoutAttributeSelector = mkSelector "layoutAttribute"

-- | @Selector@ for @setLayoutAttribute:@
setLayoutAttributeSelector :: Selector '[NSLayoutAttribute] ()
setLayoutAttributeSelector = mkSelector "setLayoutAttribute:"

-- | @Selector@ for @fullScreenMinHeight@
fullScreenMinHeightSelector :: Selector '[] CDouble
fullScreenMinHeightSelector = mkSelector "fullScreenMinHeight"

-- | @Selector@ for @setFullScreenMinHeight:@
setFullScreenMinHeightSelector :: Selector '[CDouble] ()
setFullScreenMinHeightSelector = mkSelector "setFullScreenMinHeight:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @automaticallyAdjustsSize@
automaticallyAdjustsSizeSelector :: Selector '[] Bool
automaticallyAdjustsSizeSelector = mkSelector "automaticallyAdjustsSize"

-- | @Selector@ for @setAutomaticallyAdjustsSize:@
setAutomaticallyAdjustsSizeSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsSizeSelector = mkSelector "setAutomaticallyAdjustsSize:"

-- | @Selector@ for @preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyleSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
preferredScrollEdgeEffectStyleSelector = mkSelector "preferredScrollEdgeEffectStyle"

-- | @Selector@ for @setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyleSelector :: Selector '[Id NSScrollEdgeEffectStyle] ()
setPreferredScrollEdgeEffectStyleSelector = mkSelector "setPreferredScrollEdgeEffectStyle:"

