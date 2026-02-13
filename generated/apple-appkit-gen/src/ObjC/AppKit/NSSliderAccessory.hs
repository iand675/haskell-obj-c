{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSliderAccessory@.
module ObjC.AppKit.NSSliderAccessory
  ( NSSliderAccessory
  , IsNSSliderAccessory(..)
  , accessoryWithImage
  , behavior
  , setBehavior
  , enabled
  , setEnabled
  , accessoryWithImageSelector
  , behaviorSelector
  , enabledSelector
  , setBehaviorSelector
  , setEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ accessoryWithImage:@
accessoryWithImage :: IsNSImage image => image -> IO (Id NSSliderAccessory)
accessoryWithImage image =
  do
    cls' <- getRequiredClass "NSSliderAccessory"
    sendClassMessage cls' accessoryWithImageSelector (toNSImage image)

-- | The effect on interaction with the accessory. Defaults to @automaticBehavior@
--
-- ObjC selector: @- behavior@
behavior :: IsNSSliderAccessory nsSliderAccessory => nsSliderAccessory -> IO (Id NSSliderAccessoryBehavior)
behavior nsSliderAccessory =
  sendMessage nsSliderAccessory behaviorSelector

-- | The effect on interaction with the accessory. Defaults to @automaticBehavior@
--
-- ObjC selector: @- setBehavior:@
setBehavior :: (IsNSSliderAccessory nsSliderAccessory, IsNSSliderAccessoryBehavior value) => nsSliderAccessory -> value -> IO ()
setBehavior nsSliderAccessory value =
  sendMessage nsSliderAccessory setBehaviorSelector (toNSSliderAccessoryBehavior value)

-- | Whether or not the accessory is interactive and draws with an enabled appearance. Defaults to YES.
--
-- ObjC selector: @- enabled@
enabled :: IsNSSliderAccessory nsSliderAccessory => nsSliderAccessory -> IO Bool
enabled nsSliderAccessory =
  sendMessage nsSliderAccessory enabledSelector

-- | Whether or not the accessory is interactive and draws with an enabled appearance. Defaults to YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNSSliderAccessory nsSliderAccessory => nsSliderAccessory -> Bool -> IO ()
setEnabled nsSliderAccessory value =
  sendMessage nsSliderAccessory setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accessoryWithImage:@
accessoryWithImageSelector :: Selector '[Id NSImage] (Id NSSliderAccessory)
accessoryWithImageSelector = mkSelector "accessoryWithImage:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector '[] (Id NSSliderAccessoryBehavior)
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector '[Id NSSliderAccessoryBehavior] ()
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

