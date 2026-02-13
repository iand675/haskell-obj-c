{-# LANGUAGE DataKinds #-}
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
  , automaticBehaviorSelector
  , behaviorWithHandlerSelector
  , behaviorWithTarget_actionSelector
  , handleActionSelector
  , valueResetBehaviorSelector
  , valueStepBehaviorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The action is sent to the target on interaction. The optional first parameter is an NSSliderAccessory.
--
-- ObjC selector: @+ behaviorWithTarget:action:@
behaviorWithTarget_action :: RawId -> Sel -> IO (Id NSSliderAccessoryBehavior)
behaviorWithTarget_action target action =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMessage cls' behaviorWithTarget_actionSelector target action

-- | The handler block is invoked on interaction. This variant is not codable and will assert in @-encodeWithCoder:@.
--
-- ObjC selector: @+ behaviorWithHandler:@
behaviorWithHandler :: Ptr () -> IO (Id NSSliderAccessoryBehavior)
behaviorWithHandler handler =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMessage cls' behaviorWithHandlerSelector handler

-- | Override point for custom subclasses to handle interaction.
--
-- ObjC selector: @- handleAction:@
handleAction :: (IsNSSliderAccessoryBehavior nsSliderAccessoryBehavior, IsNSSliderAccessory sender) => nsSliderAccessoryBehavior -> sender -> IO ()
handleAction nsSliderAccessoryBehavior sender =
  sendMessage nsSliderAccessoryBehavior handleActionSelector (toNSSliderAccessory sender)

-- | The behavior is automatically picked to be the system standard for the slider's current context, e.g. NSTouchBarItems have @.valueStep@ behavior.
--
-- ObjC selector: @+ automaticBehavior@
automaticBehavior :: IO (Id NSSliderAccessoryBehavior)
automaticBehavior  =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMessage cls' automaticBehaviorSelector

-- | The value of the slider moves towards the associated value for the accessory with by a delta of the slider's @altIncrementValue@.
--
-- ObjC selector: @+ valueStepBehavior@
valueStepBehavior :: IO (Id NSSliderAccessoryBehavior)
valueStepBehavior  =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMessage cls' valueStepBehaviorSelector

-- | The value of the slider is reset to the associated value for the accessory.
--
-- ObjC selector: @+ valueResetBehavior@
valueResetBehavior :: IO (Id NSSliderAccessoryBehavior)
valueResetBehavior  =
  do
    cls' <- getRequiredClass "NSSliderAccessoryBehavior"
    sendClassMessage cls' valueResetBehaviorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @behaviorWithTarget:action:@
behaviorWithTarget_actionSelector :: Selector '[RawId, Sel] (Id NSSliderAccessoryBehavior)
behaviorWithTarget_actionSelector = mkSelector "behaviorWithTarget:action:"

-- | @Selector@ for @behaviorWithHandler:@
behaviorWithHandlerSelector :: Selector '[Ptr ()] (Id NSSliderAccessoryBehavior)
behaviorWithHandlerSelector = mkSelector "behaviorWithHandler:"

-- | @Selector@ for @handleAction:@
handleActionSelector :: Selector '[Id NSSliderAccessory] ()
handleActionSelector = mkSelector "handleAction:"

-- | @Selector@ for @automaticBehavior@
automaticBehaviorSelector :: Selector '[] (Id NSSliderAccessoryBehavior)
automaticBehaviorSelector = mkSelector "automaticBehavior"

-- | @Selector@ for @valueStepBehavior@
valueStepBehaviorSelector :: Selector '[] (Id NSSliderAccessoryBehavior)
valueStepBehaviorSelector = mkSelector "valueStepBehavior"

-- | @Selector@ for @valueResetBehavior@
valueResetBehaviorSelector :: Selector '[] (Id NSSliderAccessoryBehavior)
valueResetBehaviorSelector = mkSelector "valueResetBehavior"

