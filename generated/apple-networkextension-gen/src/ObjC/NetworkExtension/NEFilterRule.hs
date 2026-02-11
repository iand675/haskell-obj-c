{-# LANGUAGE PatternSynonyms #-}
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
  , initWithNetworkRule_actionSelector
  , networkRuleSelector
  , actionSelector

  -- * Enum types
  , NEFilterAction(NEFilterAction)
  , pattern NEFilterActionInvalid
  , pattern NEFilterActionAllow
  , pattern NEFilterActionDrop
  , pattern NEFilterActionRemediate
  , pattern NEFilterActionFilterData

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
initWithNetworkRule_action neFilterRule  networkRule action =
  withObjCPtr networkRule $ \raw_networkRule ->
      sendMsg neFilterRule (mkSelector "initWithNetworkRule:action:") (retPtr retVoid) [argPtr (castPtr raw_networkRule :: Ptr ()), argCLong (coerce action)] >>= ownedObject . castPtr

-- | matchNetworkRule
--
-- The NENetworkRule that defines the network traffic characteristics that this rule matches.
--
-- ObjC selector: @- networkRule@
networkRule :: IsNEFilterRule neFilterRule => neFilterRule -> IO (Id NENetworkRule)
networkRule neFilterRule  =
    sendMsg neFilterRule (mkSelector "networkRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | action
--
-- The action to take when this rule matches network traffic.
--
-- ObjC selector: @- action@
action :: IsNEFilterRule neFilterRule => neFilterRule -> IO NEFilterAction
action neFilterRule  =
    fmap (coerce :: CLong -> NEFilterAction) $ sendMsg neFilterRule (mkSelector "action") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNetworkRule:action:@
initWithNetworkRule_actionSelector :: Selector
initWithNetworkRule_actionSelector = mkSelector "initWithNetworkRule:action:"

-- | @Selector@ for @networkRule@
networkRuleSelector :: Selector
networkRuleSelector = mkSelector "networkRule"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

