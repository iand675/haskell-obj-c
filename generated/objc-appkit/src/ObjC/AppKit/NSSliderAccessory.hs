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
  , setBehaviorSelector
  , enabledSelector
  , setEnabledSelector


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

-- | @+ accessoryWithImage:@
accessoryWithImage :: IsNSImage image => image -> IO (Id NSSliderAccessory)
accessoryWithImage image =
  do
    cls' <- getRequiredClass "NSSliderAccessory"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "accessoryWithImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | The effect on interaction with the accessory. Defaults to @automaticBehavior@
--
-- ObjC selector: @- behavior@
behavior :: IsNSSliderAccessory nsSliderAccessory => nsSliderAccessory -> IO (Id NSSliderAccessoryBehavior)
behavior nsSliderAccessory  =
  sendMsg nsSliderAccessory (mkSelector "behavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The effect on interaction with the accessory. Defaults to @automaticBehavior@
--
-- ObjC selector: @- setBehavior:@
setBehavior :: (IsNSSliderAccessory nsSliderAccessory, IsNSSliderAccessoryBehavior value) => nsSliderAccessory -> value -> IO ()
setBehavior nsSliderAccessory  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSliderAccessory (mkSelector "setBehavior:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether or not the accessory is interactive and draws with an enabled appearance. Defaults to YES.
--
-- ObjC selector: @- enabled@
enabled :: IsNSSliderAccessory nsSliderAccessory => nsSliderAccessory -> IO Bool
enabled nsSliderAccessory  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSliderAccessory (mkSelector "enabled") retCULong []

-- | Whether or not the accessory is interactive and draws with an enabled appearance. Defaults to YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNSSliderAccessory nsSliderAccessory => nsSliderAccessory -> Bool -> IO ()
setEnabled nsSliderAccessory  value =
  sendMsg nsSliderAccessory (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accessoryWithImage:@
accessoryWithImageSelector :: Selector
accessoryWithImageSelector = mkSelector "accessoryWithImage:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

