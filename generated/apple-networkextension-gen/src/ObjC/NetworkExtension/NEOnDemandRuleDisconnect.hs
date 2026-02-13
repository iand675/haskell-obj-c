{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEOnDemandRuleDisconnect
--
-- The NEOnDemandRuleDisconnect class declares the programmatic interface for an object that defines an On Demand rule with the "Disconnect" action.
--
-- When rules of this class match, the VPN connection is not started, and the VPN connection is disconnected if it is not currently disconnected.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEOnDemandRuleDisconnect@.
module ObjC.NetworkExtension.NEOnDemandRuleDisconnect
  ( NEOnDemandRuleDisconnect
  , IsNEOnDemandRuleDisconnect(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

