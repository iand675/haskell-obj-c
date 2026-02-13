{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , observedProgress
  , setObservedProgress
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
  , animateSelector
  , animationDelaySelector
  , bezeledSelector
  , controlSizeSelector
  , controlTintSelector
  , displayedWhenStoppedSelector
  , doubleValueSelector
  , incrementBySelector
  , indeterminateSelector
  , maxValueSelector
  , minValueSelector
  , observedProgressSelector
  , setAnimationDelaySelector
  , setBezeledSelector
  , setControlSizeSelector
  , setControlTintSelector
  , setDisplayedWhenStoppedSelector
  , setDoubleValueSelector
  , setIndeterminateSelector
  , setMaxValueSelector
  , setMinValueSelector
  , setObservedProgressSelector
  , setStyleSelector
  , setUsesThreadedAnimationSelector
  , sizeToFitSelector
  , startAnimationSelector
  , stopAnimationSelector
  , styleSelector
  , usesThreadedAnimationSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- incrementBy:@
incrementBy :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
incrementBy nsProgressIndicator delta =
  sendMessage nsProgressIndicator incrementBySelector delta

-- | @- startAnimation:@
startAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> RawId -> IO ()
startAnimation nsProgressIndicator sender =
  sendMessage nsProgressIndicator startAnimationSelector sender

-- | @- stopAnimation:@
stopAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> RawId -> IO ()
stopAnimation nsProgressIndicator sender =
  sendMessage nsProgressIndicator stopAnimationSelector sender

-- | @- sizeToFit@
sizeToFit :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO ()
sizeToFit nsProgressIndicator =
  sendMessage nsProgressIndicator sizeToFitSelector

-- | @- animationDelay@
animationDelay :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
animationDelay nsProgressIndicator =
  sendMessage nsProgressIndicator animationDelaySelector

-- | @- setAnimationDelay:@
setAnimationDelay :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setAnimationDelay nsProgressIndicator delay =
  sendMessage nsProgressIndicator setAnimationDelaySelector delay

-- | @- animate:@
animate :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> RawId -> IO ()
animate nsProgressIndicator sender =
  sendMessage nsProgressIndicator animateSelector sender

-- | @- indeterminate@
indeterminate :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
indeterminate nsProgressIndicator =
  sendMessage nsProgressIndicator indeterminateSelector

-- | @- setIndeterminate:@
setIndeterminate :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setIndeterminate nsProgressIndicator value =
  sendMessage nsProgressIndicator setIndeterminateSelector value

-- | @- controlSize@
controlSize :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO NSControlSize
controlSize nsProgressIndicator =
  sendMessage nsProgressIndicator controlSizeSelector

-- | @- setControlSize:@
setControlSize :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> NSControlSize -> IO ()
setControlSize nsProgressIndicator value =
  sendMessage nsProgressIndicator setControlSizeSelector value

-- | @- doubleValue@
doubleValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
doubleValue nsProgressIndicator =
  sendMessage nsProgressIndicator doubleValueSelector

-- | @- setDoubleValue:@
setDoubleValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setDoubleValue nsProgressIndicator value =
  sendMessage nsProgressIndicator setDoubleValueSelector value

-- | @- minValue@
minValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
minValue nsProgressIndicator =
  sendMessage nsProgressIndicator minValueSelector

-- | @- setMinValue:@
setMinValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setMinValue nsProgressIndicator value =
  sendMessage nsProgressIndicator setMinValueSelector value

-- | @- maxValue@
maxValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO CDouble
maxValue nsProgressIndicator =
  sendMessage nsProgressIndicator maxValueSelector

-- | @- setMaxValue:@
setMaxValue :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> CDouble -> IO ()
setMaxValue nsProgressIndicator value =
  sendMessage nsProgressIndicator setMaxValueSelector value

-- | @- observedProgress@
observedProgress :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO (Id NSProgress)
observedProgress nsProgressIndicator =
  sendMessage nsProgressIndicator observedProgressSelector

-- | @- setObservedProgress:@
setObservedProgress :: (IsNSProgressIndicator nsProgressIndicator, IsNSProgress value) => nsProgressIndicator -> value -> IO ()
setObservedProgress nsProgressIndicator value =
  sendMessage nsProgressIndicator setObservedProgressSelector (toNSProgress value)

-- | @- usesThreadedAnimation@
usesThreadedAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
usesThreadedAnimation nsProgressIndicator =
  sendMessage nsProgressIndicator usesThreadedAnimationSelector

-- | @- setUsesThreadedAnimation:@
setUsesThreadedAnimation :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setUsesThreadedAnimation nsProgressIndicator value =
  sendMessage nsProgressIndicator setUsesThreadedAnimationSelector value

-- | @- style@
style :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO NSProgressIndicatorStyle
style nsProgressIndicator =
  sendMessage nsProgressIndicator styleSelector

-- | @- setStyle:@
setStyle :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> NSProgressIndicatorStyle -> IO ()
setStyle nsProgressIndicator value =
  sendMessage nsProgressIndicator setStyleSelector value

-- | @- displayedWhenStopped@
displayedWhenStopped :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
displayedWhenStopped nsProgressIndicator =
  sendMessage nsProgressIndicator displayedWhenStoppedSelector

-- | @- setDisplayedWhenStopped:@
setDisplayedWhenStopped :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setDisplayedWhenStopped nsProgressIndicator value =
  sendMessage nsProgressIndicator setDisplayedWhenStoppedSelector value

-- | @- bezeled@
bezeled :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO Bool
bezeled nsProgressIndicator =
  sendMessage nsProgressIndicator bezeledSelector

-- | @- setBezeled:@
setBezeled :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> Bool -> IO ()
setBezeled nsProgressIndicator value =
  sendMessage nsProgressIndicator setBezeledSelector value

-- | @- controlTint@
controlTint :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> IO NSControlTint
controlTint nsProgressIndicator =
  sendMessage nsProgressIndicator controlTintSelector

-- | @- setControlTint:@
setControlTint :: IsNSProgressIndicator nsProgressIndicator => nsProgressIndicator -> NSControlTint -> IO ()
setControlTint nsProgressIndicator value =
  sendMessage nsProgressIndicator setControlTintSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @incrementBy:@
incrementBySelector :: Selector '[CDouble] ()
incrementBySelector = mkSelector "incrementBy:"

-- | @Selector@ for @startAnimation:@
startAnimationSelector :: Selector '[RawId] ()
startAnimationSelector = mkSelector "startAnimation:"

-- | @Selector@ for @stopAnimation:@
stopAnimationSelector :: Selector '[RawId] ()
stopAnimationSelector = mkSelector "stopAnimation:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @animationDelay@
animationDelaySelector :: Selector '[] CDouble
animationDelaySelector = mkSelector "animationDelay"

-- | @Selector@ for @setAnimationDelay:@
setAnimationDelaySelector :: Selector '[CDouble] ()
setAnimationDelaySelector = mkSelector "setAnimationDelay:"

-- | @Selector@ for @animate:@
animateSelector :: Selector '[RawId] ()
animateSelector = mkSelector "animate:"

-- | @Selector@ for @indeterminate@
indeterminateSelector :: Selector '[] Bool
indeterminateSelector = mkSelector "indeterminate"

-- | @Selector@ for @setIndeterminate:@
setIndeterminateSelector :: Selector '[Bool] ()
setIndeterminateSelector = mkSelector "setIndeterminate:"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector '[] NSControlSize
controlSizeSelector = mkSelector "controlSize"

-- | @Selector@ for @setControlSize:@
setControlSizeSelector :: Selector '[NSControlSize] ()
setControlSizeSelector = mkSelector "setControlSize:"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @setDoubleValue:@
setDoubleValueSelector :: Selector '[CDouble] ()
setDoubleValueSelector = mkSelector "setDoubleValue:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CDouble
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector '[CDouble] ()
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CDouble
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector '[CDouble] ()
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @observedProgress@
observedProgressSelector :: Selector '[] (Id NSProgress)
observedProgressSelector = mkSelector "observedProgress"

-- | @Selector@ for @setObservedProgress:@
setObservedProgressSelector :: Selector '[Id NSProgress] ()
setObservedProgressSelector = mkSelector "setObservedProgress:"

-- | @Selector@ for @usesThreadedAnimation@
usesThreadedAnimationSelector :: Selector '[] Bool
usesThreadedAnimationSelector = mkSelector "usesThreadedAnimation"

-- | @Selector@ for @setUsesThreadedAnimation:@
setUsesThreadedAnimationSelector :: Selector '[Bool] ()
setUsesThreadedAnimationSelector = mkSelector "setUsesThreadedAnimation:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSProgressIndicatorStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[NSProgressIndicatorStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @displayedWhenStopped@
displayedWhenStoppedSelector :: Selector '[] Bool
displayedWhenStoppedSelector = mkSelector "displayedWhenStopped"

-- | @Selector@ for @setDisplayedWhenStopped:@
setDisplayedWhenStoppedSelector :: Selector '[Bool] ()
setDisplayedWhenStoppedSelector = mkSelector "setDisplayedWhenStopped:"

-- | @Selector@ for @bezeled@
bezeledSelector :: Selector '[] Bool
bezeledSelector = mkSelector "bezeled"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector '[Bool] ()
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @controlTint@
controlTintSelector :: Selector '[] NSControlTint
controlTintSelector = mkSelector "controlTint"

-- | @Selector@ for @setControlTint:@
setControlTintSelector :: Selector '[NSControlTint] ()
setControlTintSelector = mkSelector "setControlTint:"

