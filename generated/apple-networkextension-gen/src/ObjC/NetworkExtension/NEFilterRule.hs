{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterRule
--
-- The NEFilterRule class declares the programmatic interface of an object that defines a rule for matching network traffic and the action to take when the rule matches.
--
-- Generated bindings for @NEFilterRule@.
module ObjC.NetworkExtension.NEFilterRule
  ( NEFilterRule
  , IsNEFilterRule(..)
  , initWithNetworkRule_action
  , networkRule
  , action
  , actionSelector
  , initWithNetworkRule_actionSelector
  , networkRuleSelector

  -- * Enum types
  , NEFilterAction(NEFilterAction)
  , pattern NEFilterActionInvalid
  , pattern NEFilterActionAllow
  , pattern NEFilterActionDrop
  , pattern NEFilterActionRemediate
  , pattern NEFilterActionFilterData

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithNetworkRule:action:
--
-- Initialize a newly-allocated NEFilterRule object
--
-- @networkRule@ — A NENetworkRule object that defines the network traffic characteristics that this rule matches.
--
-- @action@ — The action to take when this rule matches.
--
-- ObjC selector: @- initWithNetworkRule:action:@
initWithNetworkRule_action :: (IsNEFilterRule neFilterRule, IsNENetworkRule networkRule) => neFilterRule -> networkRule -> NEFilterAction -> IO (Id NEFilterRule)
initWithNetworkRule_action neFilterRule networkRule action =
  sendOwnedMessage neFilterRule initWithNetworkRule_actionSelector (toNENetworkRule networkRule) action

-- | matchNetworkRule
--
-- The NENetworkRule that defines the network traffic characteristics that this rule matches.
--
-- ObjC selector: @- networkRule@
networkRule :: IsNEFilterRule neFilterRule => neFilterRule -> IO (Id NENetworkRule)
networkRule neFilterRule =
  sendMessage neFilterRule networkRuleSelector

-- | action
--
-- The action to take when this rule matches network traffic.
--
-- ObjC selector: @- action@
action :: IsNEFilterRule neFilterRule => neFilterRule -> IO NEFilterAction
action neFilterRule =
  sendMessage neFilterRule actionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNetworkRule:action:@
initWithNetworkRule_actionSelector :: Selector '[Id NENetworkRule, NEFilterAction] (Id NEFilterRule)
initWithNetworkRule_actionSelector = mkSelector "initWithNetworkRule:action:"

-- | @Selector@ for @networkRule@
networkRuleSelector :: Selector '[] (Id NENetworkRule)
networkRuleSelector = mkSelector "networkRule"

-- | @Selector@ for @action@
actionSelector :: Selector '[] NEFilterAction
actionSelector = mkSelector "action"

