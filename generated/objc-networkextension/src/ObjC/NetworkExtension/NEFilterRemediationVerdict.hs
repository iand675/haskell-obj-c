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
    sendClassMsg cls' (mkSelector "allowVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "dropVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "needRulesVerdict") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowVerdict@
allowVerdictSelector :: Selector
allowVerdictSelector = mkSelector "allowVerdict"

-- | @Selector@ for @dropVerdict@
dropVerdictSelector :: Selector
dropVerdictSelector = mkSelector "dropVerdict"

-- | @Selector@ for @needRulesVerdict@
needRulesVerdictSelector :: Selector
needRulesVerdictSelector = mkSelector "needRulesVerdict"

