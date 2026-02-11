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
import ObjC.Foundation.Internal.Classes

-- | connectionRules
--
-- An array of NEEvaluateConnectionRule objects. Each NEEvaluateConnectionRule object is evaluated in order against the properties of the network connection being established.
--
-- ObjC selector: @- connectionRules@
connectionRules :: IsNEOnDemandRuleEvaluateConnection neOnDemandRuleEvaluateConnection => neOnDemandRuleEvaluateConnection -> IO (Id NSArray)
connectionRules neOnDemandRuleEvaluateConnection  =
    sendMsg neOnDemandRuleEvaluateConnection (mkSelector "connectionRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | connectionRules
--
-- An array of NEEvaluateConnectionRule objects. Each NEEvaluateConnectionRule object is evaluated in order against the properties of the network connection being established.
--
-- ObjC selector: @- setConnectionRules:@
setConnectionRules :: (IsNEOnDemandRuleEvaluateConnection neOnDemandRuleEvaluateConnection, IsNSArray value) => neOnDemandRuleEvaluateConnection -> value -> IO ()
setConnectionRules neOnDemandRuleEvaluateConnection  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neOnDemandRuleEvaluateConnection (mkSelector "setConnectionRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionRules@
connectionRulesSelector :: Selector
connectionRulesSelector = mkSelector "connectionRules"

-- | @Selector@ for @setConnectionRules:@
setConnectionRulesSelector :: Selector
setConnectionRulesSelector = mkSelector "setConnectionRules:"

