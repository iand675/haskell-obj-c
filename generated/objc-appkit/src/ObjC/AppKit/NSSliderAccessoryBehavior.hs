{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSliderAccessoryBehavior@.
module ObjC.AppKit.NSSliderAccessoryBehavior
  ( NSSliderAccessoryBehavior
  , IsNSSliderAccessoryBehavior(..)
  , behaviorWithTarget_action
  , behaviorWithHandler
  , handleAction
  , automaticBehavior
  , valueStepBehavior
  , valueResetBehavior
  , behaviorWithTarget_actionSelector
  , behaviorWithHandlerSelector
  , handleActionSelector
  , automaticBehaviorSelector
  , valueStepBehaviorSelector
  , valueResetBehaviorSelector


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

-- | The action is sent to the target on interaction. The optional first parameter is an NSSliderAccessory.
--
-- ObjC selector: @+ behaviorWithTarget:action:@
behaviorWithTarget_action :: RawId -> Selector -> IO (Id NSSliderAccessoryBehavior)
behaviorWithTarget_action target action =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMsg cls' (mkSelector "behaviorWithTarget:action:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | The handler block is invoked on interaction. This variant is not codable and will assert in @-encodeWithCoder:@.
--
-- ObjC selector: @+ behaviorWithHandler:@
behaviorWithHandler :: Ptr () -> IO (Id NSSliderAccessoryBehavior)
behaviorWithHandler handler =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMsg cls' (mkSelector "behaviorWithHandler:") (retPtr retVoid) [argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | Override point for custom subclasses to handle interaction.
--
-- ObjC selector: @- handleAction:@
handleAction :: (IsNSSliderAccessoryBehavior nsSliderAccessoryBehavior, IsNSSliderAccessory sender) => nsSliderAccessoryBehavior -> sender -> IO ()
handleAction nsSliderAccessoryBehavior  sender =
withObjCPtr sender $ \raw_sender ->
    sendMsg nsSliderAccessoryBehavior (mkSelector "handleAction:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | The behavior is automatically picked to be the system standard for the slider's current context, e.g. NSTouchBarItems have @.valueStep@ behavior.
--
-- ObjC selector: @+ automaticBehavior@
automaticBehavior :: IO (Id NSSliderAccessoryBehavior)
automaticBehavior  =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMsg cls' (mkSelector "automaticBehavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value of the slider moves towards the associated value for the accessory with by a delta of the slider's @altIncrementValue@.
--
-- ObjC selector: @+ valueStepBehavior@
valueStepBehavior :: IO (Id NSSliderAccessoryBehavior)
valueStepBehavior  =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMsg cls' (mkSelector "valueStepBehavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value of the slider is reset to the associated value for the accessory.
--
-- ObjC selector: @+ valueResetBehavior@
valueResetBehavior :: IO (Id NSSliderAccessoryBehavior)
valueResetBehavior  =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMsg cls' (mkSelector "valueResetBehavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @behaviorWithTarget:action:@
behaviorWithTarget_actionSelector :: Selector
behaviorWithTarget_actionSelector = mkSelector "behaviorWithTarget:action:"

-- | @Selector@ for @behaviorWithHandler:@
behaviorWithHandlerSelector :: Selector
behaviorWithHandlerSelector = mkSelector "behaviorWithHandler:"

-- | @Selector@ for @handleAction:@
handleActionSelector :: Selector
handleActionSelector = mkSelector "handleAction:"

-- | @Selector@ for @automaticBehavior@
automaticBehaviorSelector :: Selector
automaticBehaviorSelector = mkSelector "automaticBehavior"

-- | @Selector@ for @valueStepBehavior@
valueStepBehaviorSelector :: Selector
valueStepBehaviorSelector = mkSelector "valueStepBehavior"

-- | @Selector@ for @valueResetBehavior@
valueResetBehaviorSelector :: Selector
valueResetBehaviorSelector = mkSelector "valueResetBehavior"

