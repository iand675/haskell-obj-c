{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterVerdict
--
-- The NEFilterVerdict class declares the programmatic interface for an object that is the verdict for a flow of network data.
--
-- NEFilterVerdict is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterVerdict@.
module ObjC.NetworkExtension.NEFilterVerdict
  ( NEFilterVerdict
  , IsNEFilterVerdict(..)
  , shouldReport
  , setShouldReport
  , shouldReportSelector
  , setShouldReportSelector


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

-- | shouldReport
--
-- Whether or not to send a report to the control provider's -[NEFilterProvider handleReport:] method when processing this verdict and when the flow is closed. Since the data provider does not need to wait for a response from the control provider before continuing to process the flow, this is a more efficient way to report a flow to the control provider than returning a "need rules" verdict. If the verdict originates in the control provider, this property has no effect. This property applies when the action taken upon a flow is allow, deny, remediate, or filterData (filterData for new flows only). Setting this flag on a verdict for a socket flow will also cause the data provider's -[NEFilterProvider handleReport:] method to be called when the flow is closed.
--
-- ObjC selector: @- shouldReport@
shouldReport :: IsNEFilterVerdict neFilterVerdict => neFilterVerdict -> IO Bool
shouldReport neFilterVerdict  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neFilterVerdict (mkSelector "shouldReport") retCULong []

-- | shouldReport
--
-- Whether or not to send a report to the control provider's -[NEFilterProvider handleReport:] method when processing this verdict and when the flow is closed. Since the data provider does not need to wait for a response from the control provider before continuing to process the flow, this is a more efficient way to report a flow to the control provider than returning a "need rules" verdict. If the verdict originates in the control provider, this property has no effect. This property applies when the action taken upon a flow is allow, deny, remediate, or filterData (filterData for new flows only). Setting this flag on a verdict for a socket flow will also cause the data provider's -[NEFilterProvider handleReport:] method to be called when the flow is closed.
--
-- ObjC selector: @- setShouldReport:@
setShouldReport :: IsNEFilterVerdict neFilterVerdict => neFilterVerdict -> Bool -> IO ()
setShouldReport neFilterVerdict  value =
  sendMsg neFilterVerdict (mkSelector "setShouldReport:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldReport@
shouldReportSelector :: Selector
shouldReportSelector = mkSelector "shouldReport"

-- | @Selector@ for @setShouldReport:@
setShouldReportSelector :: Selector
setShouldReportSelector = mkSelector "setShouldReport:"

