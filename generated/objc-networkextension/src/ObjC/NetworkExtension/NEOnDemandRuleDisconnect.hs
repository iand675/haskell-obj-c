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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

