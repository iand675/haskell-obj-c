{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEEvaluateConnectionRule
--
-- The NEEvaluateConnectionRule class declares the programmatic interface for an object that associates properties of network connections with an action.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEEvaluateConnectionRule@.
module ObjC.NetworkExtension.NEEvaluateConnectionRule
  ( NEEvaluateConnectionRule
  , IsNEEvaluateConnectionRule(..)
  , initWithMatchDomains_andAction
  , action
  , initWithMatchDomains_andActionSelector
  , actionSelector

  -- * Enum types
  , NEEvaluateConnectionRuleAction(NEEvaluateConnectionRuleAction)
  , pattern NEEvaluateConnectionRuleActionConnectIfNeeded
  , pattern NEEvaluateConnectionRuleActionNeverConnect

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

-- | initWithMatchDomains:andAction
--
-- Initialize an NEEvaluateConnectionRule instance with a list of destination host domains and an action
--
-- ObjC selector: @- initWithMatchDomains:andAction:@
initWithMatchDomains_andAction :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSArray domains) => neEvaluateConnectionRule -> domains -> NEEvaluateConnectionRuleAction -> IO (Id NEEvaluateConnectionRule)
initWithMatchDomains_andAction neEvaluateConnectionRule  domains action =
withObjCPtr domains $ \raw_domains ->
    sendMsg neEvaluateConnectionRule (mkSelector "initWithMatchDomains:andAction:") (retPtr retVoid) [argPtr (castPtr raw_domains :: Ptr ()), argCLong (coerce action)] >>= ownedObject . castPtr

-- | action
--
-- The action to take if the properties of the network connection being established match the rule.
--
-- ObjC selector: @- action@
action :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO NEEvaluateConnectionRuleAction
action neEvaluateConnectionRule  =
  fmap (coerce :: CLong -> NEEvaluateConnectionRuleAction) $ sendMsg neEvaluateConnectionRule (mkSelector "action") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchDomains:andAction:@
initWithMatchDomains_andActionSelector :: Selector
initWithMatchDomains_andActionSelector = mkSelector "initWithMatchDomains:andAction:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

