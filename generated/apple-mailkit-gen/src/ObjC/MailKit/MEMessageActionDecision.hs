{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MEMessageActionDecision@.
module ObjC.MailKit.MEMessageActionDecision
  ( MEMessageActionDecision
  , IsMEMessageActionDecision(..)
  , decisionApplyingAction
  , decisionApplyingActions
  , new
  , init_
  , invokeAgainWithBody
  , decisionApplyingActionSelector
  , decisionApplyingActionsSelector
  , initSelector
  , invokeAgainWithBodySelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ decisionApplyingAction:@
decisionApplyingAction :: IsMEMessageAction action => action -> IO (Id MEMessageActionDecision)
decisionApplyingAction action =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    sendClassMessage cls' decisionApplyingActionSelector (toMEMessageAction action)

-- | Creates an @MEMessageActionDecision@ with multiple actions. Conflicting actions will be ignored.
--
-- ObjC selector: @+ decisionApplyingActions:@
decisionApplyingActions :: IsNSArray actions => actions -> IO (Id MEMessageActionDecision)
decisionApplyingActions actions =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    sendClassMessage cls' decisionApplyingActionsSelector (toNSArray actions)

-- | @+ new@
new :: IO (Id MEMessageActionDecision)
new  =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEMessageActionDecision meMessageActionDecision => meMessageActionDecision -> IO (Id MEMessageActionDecision)
init_ meMessageActionDecision =
  sendOwnedMessage meMessageActionDecision initSelector

-- | @+ invokeAgainWithBody@
invokeAgainWithBody :: IO (Id MEMessageActionDecision)
invokeAgainWithBody  =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    sendClassMessage cls' invokeAgainWithBodySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decisionApplyingAction:@
decisionApplyingActionSelector :: Selector '[Id MEMessageAction] (Id MEMessageActionDecision)
decisionApplyingActionSelector = mkSelector "decisionApplyingAction:"

-- | @Selector@ for @decisionApplyingActions:@
decisionApplyingActionsSelector :: Selector '[Id NSArray] (Id MEMessageActionDecision)
decisionApplyingActionsSelector = mkSelector "decisionApplyingActions:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEMessageActionDecision)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEMessageActionDecision)
initSelector = mkSelector "init"

-- | @Selector@ for @invokeAgainWithBody@
invokeAgainWithBodySelector :: Selector '[] (Id MEMessageActionDecision)
invokeAgainWithBodySelector = mkSelector "invokeAgainWithBody"

