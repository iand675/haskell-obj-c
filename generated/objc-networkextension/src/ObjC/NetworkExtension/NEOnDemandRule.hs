{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEOnDemandRule
--
-- The NEOnDemandRule class declares the programmatic interface for an object that defines an On Demand rule.
--
-- NEOnDemandRule is an abstract base class from which other action-specific rule classes are derived.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEOnDemandRule@.
module ObjC.NetworkExtension.NEOnDemandRule
  ( NEOnDemandRule
  , IsNEOnDemandRule(..)
  , action
  , interfaceTypeMatch
  , setInterfaceTypeMatch
  , actionSelector
  , interfaceTypeMatchSelector
  , setInterfaceTypeMatchSelector

  -- * Enum types
  , NEOnDemandRuleAction(NEOnDemandRuleAction)
  , pattern NEOnDemandRuleActionConnect
  , pattern NEOnDemandRuleActionDisconnect
  , pattern NEOnDemandRuleActionEvaluateConnection
  , pattern NEOnDemandRuleActionIgnore
  , NEOnDemandRuleInterfaceType(NEOnDemandRuleInterfaceType)
  , pattern NEOnDemandRuleInterfaceTypeAny
  , pattern NEOnDemandRuleInterfaceTypeEthernet
  , pattern NEOnDemandRuleInterfaceTypeWiFi
  , pattern NEOnDemandRuleInterfaceTypeCellular

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

-- | action
--
-- The rule's action
--
-- ObjC selector: @- action@
action :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO NEOnDemandRuleAction
action neOnDemandRule  =
  fmap (coerce :: CLong -> NEOnDemandRuleAction) $ sendMsg neOnDemandRule (mkSelector "action") retCLong []

-- | interfaceTypeMatch
--
-- The type of interface that this rule matches. If the current primary network interface is of this type and all of the other conditions in the rule match, then the rule matches. If this property is 0 (the default), then the current primary interface type does not factor into the rule match.
--
-- ObjC selector: @- interfaceTypeMatch@
interfaceTypeMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO NEOnDemandRuleInterfaceType
interfaceTypeMatch neOnDemandRule  =
  fmap (coerce :: CLong -> NEOnDemandRuleInterfaceType) $ sendMsg neOnDemandRule (mkSelector "interfaceTypeMatch") retCLong []

-- | interfaceTypeMatch
--
-- The type of interface that this rule matches. If the current primary network interface is of this type and all of the other conditions in the rule match, then the rule matches. If this property is 0 (the default), then the current primary interface type does not factor into the rule match.
--
-- ObjC selector: @- setInterfaceTypeMatch:@
setInterfaceTypeMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> NEOnDemandRuleInterfaceType -> IO ()
setInterfaceTypeMatch neOnDemandRule  value =
  sendMsg neOnDemandRule (mkSelector "setInterfaceTypeMatch:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @interfaceTypeMatch@
interfaceTypeMatchSelector :: Selector
interfaceTypeMatchSelector = mkSelector "interfaceTypeMatch"

-- | @Selector@ for @setInterfaceTypeMatch:@
setInterfaceTypeMatchSelector :: Selector
setInterfaceTypeMatchSelector = mkSelector "setInterfaceTypeMatch:"

