{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterControlVerdict
--
-- The NEFilterControlVerdict declares the programmatic interface of an object that is the verdict for a new flow of network data by the control provider.
--
-- NEFilterControlVerdict is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterControlVerdict@.
module ObjC.NetworkExtension.NEFilterControlVerdict
  ( NEFilterControlVerdict
  , IsNEFilterControlVerdict(..)
  , allowVerdictWithUpdateRules
  , dropVerdictWithUpdateRules
  , updateRules
  , allowVerdictWithUpdateRulesSelector
  , dropVerdictWithUpdateRulesSelector
  , updateRulesSelector


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

-- | allowVerdictWithUpdateRules:
--
-- This class method returns a verdict indicating that the flow should be allowed to go through, and also tell the data provider whether to update its rules or not.
--
-- @updateRules@ — YES if the control provider has updated the rules and wants to communicate that to the data provider
--
-- Returns: The NEFilterControlVerdict object.
--
-- ObjC selector: @+ allowVerdictWithUpdateRules:@
allowVerdictWithUpdateRules :: Bool -> IO (Id NEFilterControlVerdict)
allowVerdictWithUpdateRules updateRules =
  do
    cls' <- getRequiredClass "NEFilterControlVerdict"
    sendClassMsg cls' (mkSelector "allowVerdictWithUpdateRules:") (retPtr retVoid) [argCULong (if updateRules then 1 else 0)] >>= retainedObject . castPtr

-- | dropVerdictWithUpdateRules:
--
-- This class method returns a verdict indicating that the flow should be dropped, and also tell the data provider whether to update its rules or not.
--
-- @updateRules@ — YES if the control provider has updated the rules and wants to communicate that to the data provider
--
-- Returns: The NEFilterControlVerdict object.
--
-- ObjC selector: @+ dropVerdictWithUpdateRules:@
dropVerdictWithUpdateRules :: Bool -> IO (Id NEFilterControlVerdict)
dropVerdictWithUpdateRules updateRules =
  do
    cls' <- getRequiredClass "NEFilterControlVerdict"
    sendClassMsg cls' (mkSelector "dropVerdictWithUpdateRules:") (retPtr retVoid) [argCULong (if updateRules then 1 else 0)] >>= retainedObject . castPtr

-- | updateRules
--
-- This class method returns a verdict indicating that the flow should be handled by the data provider, and the rules needed by the data provider have been set.
--
-- Returns: The NEFilterControlVerdict object.
--
-- ObjC selector: @+ updateRules@
updateRules :: IO (Id NEFilterControlVerdict)
updateRules  =
  do
    cls' <- getRequiredClass "NEFilterControlVerdict"
    sendClassMsg cls' (mkSelector "updateRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowVerdictWithUpdateRules:@
allowVerdictWithUpdateRulesSelector :: Selector
allowVerdictWithUpdateRulesSelector = mkSelector "allowVerdictWithUpdateRules:"

-- | @Selector@ for @dropVerdictWithUpdateRules:@
dropVerdictWithUpdateRulesSelector :: Selector
dropVerdictWithUpdateRulesSelector = mkSelector "dropVerdictWithUpdateRules:"

-- | @Selector@ for @updateRules@
updateRulesSelector :: Selector
updateRulesSelector = mkSelector "updateRules"

