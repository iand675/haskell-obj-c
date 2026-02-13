{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEOnDemandRuleEvaluateConnection
--
-- The NEOnDemandRuleEvaluateConnection class declares the programmatic interface for an object that defines an On Demand rule with the "Evaluate Connection" action.
--
-- When rules of this class match, the properties of the network connection being established are matched against a set of connection rules. The action of the matched rule (if any) is used to determine whether or not the VPN will be started.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEOnDemandRuleEvaluateConnection@.
module ObjC.NetworkExtension.NEOnDemandRuleEvaluateConnection
  ( NEOnDemandRuleEvaluateConnection
  , IsNEOnDemandRuleEvaluateConnection(..)
  , connectionRules
  , setConnectionRules
  , connectionRulesSelector
  , setConnectionRulesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | connectionRules
--
-- An array of NEEvaluateConnectionRule objects. Each NEEvaluateConnectionRule object is evaluated in order against the properties of the network connection being established.
--
-- ObjC selector: @- connectionRules@
connectionRules :: IsNEOnDemandRuleEvaluateConnection neOnDemandRuleEvaluateConnection => neOnDemandRuleEvaluateConnection -> IO (Id NSArray)
connectionRules neOnDemandRuleEvaluateConnection =
  sendMessage neOnDemandRuleEvaluateConnection connectionRulesSelector

-- | connectionRules
--
-- An array of NEEvaluateConnectionRule objects. Each NEEvaluateConnectionRule object is evaluated in order against the properties of the network connection being established.
--
-- ObjC selector: @- setConnectionRules:@
setConnectionRules :: (IsNEOnDemandRuleEvaluateConnection neOnDemandRuleEvaluateConnection, IsNSArray value) => neOnDemandRuleEvaluateConnection -> value -> IO ()
setConnectionRules neOnDemandRuleEvaluateConnection value =
  sendMessage neOnDemandRuleEvaluateConnection setConnectionRulesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionRules@
connectionRulesSelector :: Selector '[] (Id NSArray)
connectionRulesSelector = mkSelector "connectionRules"

-- | @Selector@ for @setConnectionRules:@
setConnectionRulesSelector :: Selector '[Id NSArray] ()
setConnectionRulesSelector = mkSelector "setConnectionRules:"

