{-# LANGUAGE PatternSynonyms #-}
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
  , viewWillAppearSelector
  , viewDidAppearSelector
  , viewDidDisappearSelector
  , layoutAttributeSelector
  , setLayoutAttributeSelector
  , fullScreenMinHeightSelector
  , setFullScreenMinHeightSelector
  , hiddenSelector
  , setHiddenSelector
  , automaticallyAdjustsSizeSelector
  , setAutomaticallyAdjustsSizeSelector
  , preferredScrollEdgeEffectStyleSelector
  , setPreferredScrollEdgeEffectStyleSelector

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

-- | @- viewWillAppear@
viewWillAppear :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO ()
viewWillAppear nsTitlebarAccessoryViewController  =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "viewWillAppear") retVoid []

-- | @- viewDidAppear@
viewDidAppear :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO ()
viewDidAppear nsTitlebarAccessoryViewController  =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "viewDidAppear") retVoid []

-- | @- viewDidDisappear@
viewDidDisappear :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO ()
viewDidDisappear nsTitlebarAccessoryViewController  =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "viewDidDisappear") retVoid []

-- | @- layoutAttribute@
layoutAttribute :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO NSLayoutAttribute
layoutAttribute nsTitlebarAccessoryViewController  =
    fmap (coerce :: CLong -> NSLayoutAttribute) $ sendMsg nsTitlebarAccessoryViewController (mkSelector "layoutAttribute") retCLong []

-- | @- setLayoutAttribute:@
setLayoutAttribute :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> NSLayoutAttribute -> IO ()
setLayoutAttribute nsTitlebarAccessoryViewController  value =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "setLayoutAttribute:") retVoid [argCLong (coerce value)]

-- | @- fullScreenMinHeight@
fullScreenMinHeight :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO CDouble
fullScreenMinHeight nsTitlebarAccessoryViewController  =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "fullScreenMinHeight") retCDouble []

-- | @- setFullScreenMinHeight:@
setFullScreenMinHeight :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> CDouble -> IO ()
setFullScreenMinHeight nsTitlebarAccessoryViewController  value =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "setFullScreenMinHeight:") retVoid [argCDouble value]

-- | @- hidden@
hidden :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO Bool
hidden nsTitlebarAccessoryViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTitlebarAccessoryViewController (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> Bool -> IO ()
setHidden nsTitlebarAccessoryViewController  value =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticallyAdjustsSize@
automaticallyAdjustsSize :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO Bool
automaticallyAdjustsSize nsTitlebarAccessoryViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTitlebarAccessoryViewController (mkSelector "automaticallyAdjustsSize") retCULong []

-- | @- setAutomaticallyAdjustsSize:@
setAutomaticallyAdjustsSize :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> Bool -> IO ()
setAutomaticallyAdjustsSize nsTitlebarAccessoryViewController  value =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "setAutomaticallyAdjustsSize:") retVoid [argCULong (if value then 1 else 0)]

-- | The titlebar accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the bottom edge of a titlebar accessory:
--
-- titlebarAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyle :: IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController => nsTitlebarAccessoryViewController -> IO (Id NSScrollEdgeEffectStyle)
preferredScrollEdgeEffectStyle nsTitlebarAccessoryViewController  =
    sendMsg nsTitlebarAccessoryViewController (mkSelector "preferredScrollEdgeEffectStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The titlebar accessory’s preferred effect for content scrolling behind it.
--
-- To allow for a soft edge on the bottom edge of a titlebar accessory:
--
-- titlebarAccessoryViewController.preferredScrollEdgeEffectStyle = NSScrollEdgeEffectStyle.softStyle;
--
-- ObjC selector: @- setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyle :: (IsNSTitlebarAccessoryViewController nsTitlebarAccessoryViewController, IsNSScrollEdgeEffectStyle value) => nsTitlebarAccessoryViewController -> value -> IO ()
setPreferredScrollEdgeEffectStyle nsTitlebarAccessoryViewController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTitlebarAccessoryViewController (mkSelector "setPreferredScrollEdgeEffectStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewWillAppear@
viewWillAppearSelector :: Selector
viewWillAppearSelector = mkSelector "viewWillAppear"

-- | @Selector@ for @viewDidAppear@
viewDidAppearSelector :: Selector
viewDidAppearSelector = mkSelector "viewDidAppear"

-- | @Selector@ for @viewDidDisappear@
viewDidDisappearSelector :: Selector
viewDidDisappearSelector = mkSelector "viewDidDisappear"

-- | @Selector@ for @layoutAttribute@
layoutAttributeSelector :: Selector
layoutAttributeSelector = mkSelector "layoutAttribute"

-- | @Selector@ for @setLayoutAttribute:@
setLayoutAttributeSelector :: Selector
setLayoutAttributeSelector = mkSelector "setLayoutAttribute:"

-- | @Selector@ for @fullScreenMinHeight@
fullScreenMinHeightSelector :: Selector
fullScreenMinHeightSelector = mkSelector "fullScreenMinHeight"

-- | @Selector@ for @setFullScreenMinHeight:@
setFullScreenMinHeightSelector :: Selector
setFullScreenMinHeightSelector = mkSelector "setFullScreenMinHeight:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @automaticallyAdjustsSize@
automaticallyAdjustsSizeSelector :: Selector
automaticallyAdjustsSizeSelector = mkSelector "automaticallyAdjustsSize"

-- | @Selector@ for @setAutomaticallyAdjustsSize:@
setAutomaticallyAdjustsSizeSelector :: Selector
setAutomaticallyAdjustsSizeSelector = mkSelector "setAutomaticallyAdjustsSize:"

-- | @Selector@ for @preferredScrollEdgeEffectStyle@
preferredScrollEdgeEffectStyleSelector :: Selector
preferredScrollEdgeEffectStyleSelector = mkSelector "preferredScrollEdgeEffectStyle"

-- | @Selector@ for @setPreferredScrollEdgeEffectStyle:@
setPreferredScrollEdgeEffectStyleSelector :: Selector
setPreferredScrollEdgeEffectStyleSelector = mkSelector "setPreferredScrollEdgeEffectStyle:"

