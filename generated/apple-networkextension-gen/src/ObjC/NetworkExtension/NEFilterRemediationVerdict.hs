{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterRemediationVerdict
--
-- The NEFilterRemediationVerdict class declares the programmatic interface of an object that is the verdict for a flow which has been blocked by the filter, but the user has made a request for remediation.
--
-- NEFilterRemediationVerdict is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterRemediationVerdict@.
module ObjC.NetworkExtension.NEFilterRemediationVerdict
  ( NEFilterRemediationVerdict
  , IsNEFilterRemediationVerdict(..)
  , allowVerdict
  , dropVerdict
  , needRulesVerdict
  , allowVerdictSelector
  , dropVerdictSelector
  , needRulesVerdictSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | allowVerdict
--
-- This class method returns a verdict indicating that the flow should be allowed.
--
-- Returns: The NEFilterRemediationVerdict object.
--
-- ObjC selector: @+ allowVerdict@
allowVerdict :: IO (Id NEFilterRemediationVerdict)
allowVerdict  =
  do
    cls' <- getRequiredClass "NEFilterRemediationVerdict"
    sendClassMessage cls' allowVerdictSelector

-- | dropVerdict
--
-- This class method returns a verdict indicating that the flow should be dropped.
--
-- Returns: The NEFilterRemediationVerdict object.
--
-- ObjC selector: @+ dropVerdict@
dropVerdict :: IO (Id NEFilterRemediationVerdict)
dropVerdict  =
  do
    cls' <- getRequiredClass "NEFilterRemediationVerdict"
    sendClassMessage cls' dropVerdictSelector

-- | needRulesVerdict
--
-- This class method returns a verdict indicating that control provider needs to be asked how to handle the remediation. The control provider can either drop or allow the flow, or update the rules and ask the data provider to decide on the data flow again.
--
-- Returns: The NEFilterRemediationVerdict object.
--
-- ObjC selector: @+ needRulesVerdict@
needRulesVerdict :: IO (Id NEFilterRemediationVerdict)
needRulesVerdict  =
  do
    cls' <- getRequiredClass "NEFilterRemediationVerdict"
    sendClassMessage cls' needRulesVerdictSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowVerdict@
allowVerdictSelector :: Selector '[] (Id NEFilterRemediationVerdict)
allowVerdictSelector = mkSelector "allowVerdict"

-- | @Selector@ for @dropVerdict@
dropVerdictSelector :: Selector '[] (Id NEFilterRemediationVerdict)
dropVerdictSelector = mkSelector "dropVerdict"

-- | @Selector@ for @needRulesVerdict@
needRulesVerdictSelector :: Selector '[] (Id NEFilterRemediationVerdict)
needRulesVerdictSelector = mkSelector "needRulesVerdict"

