{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterSettings
--
-- The NEFilterSettings class declares the programmatic interface for an object that contains filter settings.
--
-- NEFilterSettings is used by NEFilterDataProviders to communicate the desired settings for the filter to the framework. The framework takes care of applying the contained settings to the system.
--
-- Generated bindings for @NEFilterSettings@.
module ObjC.NetworkExtension.NEFilterSettings
  ( NEFilterSettings
  , IsNEFilterSettings(..)
  , initWithRules_defaultAction
  , rules
  , defaultAction
  , defaultActionSelector
  , initWithRules_defaultActionSelector
  , rulesSelector

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

-- | initWithRules:defaultAction:
--
-- Initialize a newly-allocated NEFilterSettings object with a set of filtering rules and a default filter action to takke if none    of the rules match.
--
-- @rules@ — An NSArray containing an ordered list of NEFilterRule objects. The maximum number of rules that this array can contain is 1000.
--
-- @defaultAction@ — The NEFilterAction to take for flows of network (non-loopback) data that do not match any of the specified rules. The default defaultAction is     NEFilterActionFilterData. If defaultAction is NEFilterActionAllow or NEFilterActionDrop, then the rules array must contain at least one NEFilterRule.     The default action for loopback traffic is NEFilterActionAllow and cannot be changed. To filter loopback traffic you must include rules in the rules array that specifically match loopback traffic     and have an action of NEFilterActionFilterData.
--
-- Returns: the newly-initialized NEFilterSettings object.
--
-- ObjC selector: @- initWithRules:defaultAction:@
initWithRules_defaultAction :: (IsNEFilterSettings neFilterSettings, IsNSArray rules) => neFilterSettings -> rules -> NEFilterAction -> IO (Id NEFilterSettings)
initWithRules_defaultAction neFilterSettings rules defaultAction =
  sendOwnedMessage neFilterSettings initWithRules_defaultActionSelector (toNSArray rules) defaultAction

-- | rules
--
-- An NSArray containing an ordered list of NEFilterRuleObjects. After the NEFilterSettings are applied to the system,     each network flow is matched against these rules in order, and the NEFilterAction of the first rule that matches is taken:         NEFilterActionAllow: Allow the flow of data to proceed on its journey through the networking stack without consulting this provider.         NEFilterActionDrop: Drop the flow without consulting this provider.         NEFilterActionFilterData: Call this provider's handleNewFlow: method with the flow.
--
-- ObjC selector: @- rules@
rules :: IsNEFilterSettings neFilterSettings => neFilterSettings -> IO (Id NSArray)
rules neFilterSettings =
  sendMessage neFilterSettings rulesSelector

-- | defaultAction
--
-- An NEFilterAction containing the default action to take for flows of network data that do not match any of the specified rules.
--
-- ObjC selector: @- defaultAction@
defaultAction :: IsNEFilterSettings neFilterSettings => neFilterSettings -> IO NEFilterAction
defaultAction neFilterSettings =
  sendMessage neFilterSettings defaultActionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRules:defaultAction:@
initWithRules_defaultActionSelector :: Selector '[Id NSArray, NEFilterAction] (Id NEFilterSettings)
initWithRules_defaultActionSelector = mkSelector "initWithRules:defaultAction:"

-- | @Selector@ for @rules@
rulesSelector :: Selector '[] (Id NSArray)
rulesSelector = mkSelector "rules"

-- | @Selector@ for @defaultAction@
defaultActionSelector :: Selector '[] NEFilterAction
defaultActionSelector = mkSelector "defaultAction"

