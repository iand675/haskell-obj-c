{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProgressIndicator@.
module ObjC.AppKit.NSProgressIndicator
  ( NSProgressIndicator
  , IsNSProgressIndicator(..)
  , incrementBy
  , startAnimation
  , stopAnimation
  , sizeToFit
  , animationDelay
  , setAnimationDelay
  , animate
  , indeterminate
  , setIndeterminate
  , controlSize
  , setControlSize
  , doubleValue
  , setDoubleValue
  , minValue
  , setMinValue
  , maxValue
  , setMaxValue
  , usesThreadedAnimation
  , setUsesThreadedAnimation
  , style
  , setStyle
  , displayedWhenStopped
  , setDisplayedWhenStopped
  , bezeled
  , setBezeled
  , controlTint
  , setControlTint
  , incrementBySelector
  , startAnimationSelector
  , stopAnimationSelector
  , sizeToFitSelector
  , animationDelaySelector
  , setAnimationDelaySelector
  , animateSelector
  , indeterminateSelector
  , setIndeterminateSelector
  , controlSizeSelector
  , setControlSizeSelector
  , doubleValueSelector
  , setDoubleValueSelector
  , minValueSelector
  , setMinValueSelector
  , maxValueSelector
  , setMaxValueSelector
  , usesThreadedAnimationSelector
  , setUsesThreadedAnimationSelector
  , styleSelector
  , setStyleSelector
  , displayedWhenStoppedSelector
  , setDisplayedWhenStoppedSelector
  , bezeledSelector
  , setBezeledSelector
  , controlTintSelector
  , setControlTintSelector

  -- * Enum types
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge
  , NSControlTint(NSControlTint)
  , pattern NSDefaultControlTint
  , pattern NSBlueControlTint
  , pattern NSGraphiteControlTint
  , pattern NSClearControlTint
  , NSProgressIndicatorStyle(NSProgressIndicatorStyle)
  , pattern NSProgressIndicatorStyleBar
  , pattern NSProgressIndicatorStyleSpinning

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

-- | @- incrementBy:@
incrementBy :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
incrementBy nsProgressIndicator  delta =
  sendMsg nsProgressIndicator (mkSelector "incrementBy:") retVoid [argCDouble (fromIntegral delta)]

-- | @- startAnimation:@
startAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> RawId -> IO ()
startAnimation nsProgressIndicator  sender =
  sendMsg nsProgressIndicator (mkSelector "startAnimation:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopAnimation:@
stopAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> RawId -> IO ()
stopAnimation nsProgressIndicator  sender =
  sendMsg nsProgressIndicator (mkSelector "stopAnimation:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- sizeToFit@
sizeToFit :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO ()
sizeToFit nsProgressIndicator  =
  sendMsg nsProgressIndicator (mkSelector "sizeToFit") retVoid []

-- | @- animationDelay@
animationDelay :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
animationDelay nsProgressIndicator  =
  sendMsg nsProgressIndicator (mkSelector "animationDelay") retCDouble []

-- | @- setAnimationDelay:@
setAnimationDelay :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setAnimationDelay nsProgressIndicator  delay =
  sendMsg nsProgressIndicator (mkSelector "setAnimationDelay:") retVoid [argCDouble (fromIntegral delay)]

-- | @- animate:@
animate :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> RawId -> IO ()
animate nsProgressIndicator  sender =
  sendMsg nsProgressIndicator (mkSelector "animate:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- indeterminate@
indeterminate :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
indeterminate nsProgressIndicator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgressIndicator (mkSelector "indeterminate") retCULong []

-- | @- setIndeterminate:@
setIndeterminate :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setIndeterminate nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setIndeterminate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- controlSize@
controlSize :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO NSControlSize
controlSize nsProgressIndicator  =
  fmap (coerce :: CULong -> NSControlSize) $ sendMsg nsProgressIndicator (mkSelector "controlSize") retCULong []

-- | @- setControlSize:@
setControlSize :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> NSControlSize -> IO ()
setControlSize nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setControlSize:") retVoid [argCULong (coerce value)]

-- | @- doubleValue@
doubleValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
doubleValue nsProgressIndicator  =
  sendMsg nsProgressIndicator (mkSelector "doubleValue") retCDouble []

-- | @- setDoubleValue:@
setDoubleValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setDoubleValue nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setDoubleValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- minValue@
minValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
minValue nsProgressIndicator  =
  sendMsg nsProgressIndicator (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setMinValue nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxValue@
maxValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
maxValue nsProgressIndicator  =
  sendMsg nsProgressIndicator (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setMaxValue nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- usesThreadedAnimation@
usesThreadedAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
usesThreadedAnimation nsProgressIndicator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgressIndicator (mkSelector "usesThreadedAnimation") retCULong []

-- | @- setUsesThreadedAnimation:@
setUsesThreadedAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setUsesThreadedAnimation nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setUsesThreadedAnimation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- style@
style :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO NSProgressIndicatorStyle
style nsProgressIndicator  =
  fmap (coerce :: CULong -> NSProgressIndicatorStyle) $ sendMsg nsProgressIndicator (mkSelector "style") retCULong []

-- | @- setStyle:@
setStyle :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> NSProgressIndicatorStyle -> IO ()
setStyle nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setStyle:") retVoid [argCULong (coerce value)]

-- | @- displayedWhenStopped@
displayedWhenStopped :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
displayedWhenStopped nsProgressIndicator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgressIndicator (mkSelector "displayedWhenStopped") retCULong []

-- | @- setDisplayedWhenStopped:@
setDisplayedWhenStopped :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setDisplayedWhenStopped nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setDisplayedWhenStopped:") retVoid [argCULong (if value then 1 else 0)]

-- | @- bezeled@
bezeled :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
bezeled nsProgressIndicator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgressIndicator (mkSelector "bezeled") retCULong []

-- | @- setBezeled:@
setBezeled :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setBezeled nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setBezeled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- controlTint@
controlTint :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO NSControlTint
controlTint nsProgressIndicator  =
  fmap (coerce :: CULong -> NSControlTint) $ sendMsg nsProgressIndicator (mkSelector "controlTint") retCULong []

-- | @- setControlTint:@
setControlTint :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> NSControlTint -> IO ()
setControlTint nsProgressIndicator  value =
  sendMsg nsProgressIndicator (mkSelector "setControlTint:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @incrementBy:@
incrementBySelector :: Selector
incrementBySelector = mkSelector "incrementBy:"

-- | @Selector@ for @startAnimation:@
startAnimationSelector :: Selector
startAnimationSelector = mkSelector "startAnimation:"

-- | @Selector@ for @stopAnimation:@
stopAnimationSelector :: Selector
stopAnimationSelector = mkSelector "stopAnimation:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @animationDelay@
animationDelaySelector :: Selector
animationDelaySelector = mkSelector "animationDelay"

-- | @Selector@ for @setAnimationDelay:@
setAnimationDelaySelector :: Selector
setAnimationDelaySelector = mkSelector "setAnimationDelay:"

-- | @Selector@ for @animate:@
animateSelector :: Selector
animateSelector = mkSelector "animate:"

-- | @Selector@ for @indeterminate@
indeterminateSelector :: Selector
indeterminateSelector = mkSelector "indeterminate"

-- | @Selector@ for @setIndeterminate:@
setIndeterminateSelector :: Selector
setIndeterminateSelector = mkSelector "setIndeterminate:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @setDoubleValue:@
setDoubleValueSelector :: Selector
setDoubleValueSelector = mkSelector "setDoubleValue:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @usesThreadedAnimation@
usesThreadedAnimationSelector :: Selector
usesThreadedAnimationSelector = mkSelector "usesThreadedAnimation"

-- | @Selector@ for @setUsesThreadedAnimation:@
setUsesThreadedAnimationSelector :: Selector
setUsesThreadedAnimationSelector = mkSelector "setUsesThreadedAnimation:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @displayedWhenStopped@
displayedWhenStoppedSelector :: Selector
displayedWhenStoppedSelector = mkSelector "displayedWhenStopped"

-- | @Selector@ for @setDisplayedWhenStopped:@
setDisplayedWhenStoppedSelector :: Selector
setDisplayedWhenStoppedSelector = mkSelector "setDisplayedWhenStopped:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector
setControlTintSelector = mkSelector "setControlTint:"

