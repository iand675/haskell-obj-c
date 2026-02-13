{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissioningOperation@.
module ObjC.Matter.MTRCommissioningOperation
  ( MTRCommissioningOperation
  , IsMTRCommissioningOperation(..)
  , init_
  , new
  , initWithParameters_setupPayload_delegate_queue
  , startWithController
  , stop
  , matchedPayload
  , initSelector
  , initWithParameters_setupPayload_delegate_queueSelector
  , matchedPayloadSelector
  , newSelector
  , startWithControllerSelector
  , stopSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialized via initWithParameters:setupPayload:delegate:queue:
--
-- ObjC selector: @- init@
init_ :: IsMTRCommissioningOperation mtrCommissioningOperation => mtrCommissioningOperation -> IO (Id MTRCommissioningOperation)
init_ mtrCommissioningOperation =
  sendOwnedMessage mtrCommissioningOperation initSelector

-- | @+ new@
new :: IO (Id MTRCommissioningOperation)
new  =
  do
    cls' <- getRequiredClass "MTRCommissioningOperation"
    sendOwnedClassMessage cls' newSelector

-- | Prepare to commission a device with the given parameters and the given setup payload (QR code, manual pairing code, etc).  Returns nil if the payload is not valid.
--
-- The deviceAttestationDelegate property of MTRCommissioningParameters will be ignored. Device attestation notifications will be delivered to the MTRCommissioningDelegate instead.  The failSafeTimeout property of MTRCommissioningParameters will be respected.
--
-- The provided delegate will be notified about various things as commissioning proceeds.  The calls into the delegate will happen on the provided queue.
--
-- Modifying the parameters after this call will have no effect on the behavior of the MTRCommissioningOperation.
--
-- ObjC selector: @- initWithParameters:setupPayload:delegate:queue:@
initWithParameters_setupPayload_delegate_queue :: (IsMTRCommissioningOperation mtrCommissioningOperation, IsMTRCommissioningParameters parameters, IsNSString payload, IsNSObject queue) => mtrCommissioningOperation -> parameters -> payload -> RawId -> queue -> IO (Id MTRCommissioningOperation)
initWithParameters_setupPayload_delegate_queue mtrCommissioningOperation parameters payload delegate queue =
  sendOwnedMessage mtrCommissioningOperation initWithParameters_setupPayload_delegate_queueSelector (toMTRCommissioningParameters parameters) (toNSString payload) delegate (toNSObject queue)

-- | Start commissioning with the given controller (which identifies the fabric the commissionee should be commissioned into).  The delegate will be notified if there are any failures.
--
-- ObjC selector: @- startWithController:@
startWithController :: (IsMTRCommissioningOperation mtrCommissioningOperation, IsMTRDeviceController controller) => mtrCommissioningOperation -> controller -> IO ()
startWithController mtrCommissioningOperation controller =
  sendMessage mtrCommissioningOperation startWithControllerSelector (toMTRDeviceController controller)

-- | Stop commissioning.  This will typically result in commissioning:failedWithError: callbacks to delegates.
--
-- Returns YES if this commissioning was still in-progress and has now been stopped; returns NO if this commissioning wasn't in-progress.
--
-- Note that this can return NO while there are still pending async calls to delegate callbacks for the end of the commissioning.
--
-- ObjC selector: @- stop@
stop :: IsMTRCommissioningOperation mtrCommissioningOperation => mtrCommissioningOperation -> IO Bool
stop mtrCommissioningOperation =
  sendMessage mtrCommissioningOperation stopSelector

-- | If not nil, the payload (from possibly multiple payloads represented by the provided setupPayload) that represents the commissionee we successfully established PASE with.  This will only be non-nil after successful PASE establishment.
--
-- ObjC selector: @- matchedPayload@
matchedPayload :: IsMTRCommissioningOperation mtrCommissioningOperation => mtrCommissioningOperation -> IO (Id MTRSetupPayload)
matchedPayload mtrCommissioningOperation =
  sendMessage mtrCommissioningOperation matchedPayloadSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRCommissioningOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRCommissioningOperation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithParameters:setupPayload:delegate:queue:@
initWithParameters_setupPayload_delegate_queueSelector :: Selector '[Id MTRCommissioningParameters, Id NSString, RawId, Id NSObject] (Id MTRCommissioningOperation)
initWithParameters_setupPayload_delegate_queueSelector = mkSelector "initWithParameters:setupPayload:delegate:queue:"

-- | @Selector@ for @startWithController:@
startWithControllerSelector :: Selector '[Id MTRDeviceController] ()
startWithControllerSelector = mkSelector "startWithController:"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] Bool
stopSelector = mkSelector "stop"

-- | @Selector@ for @matchedPayload@
matchedPayloadSelector :: Selector '[] (Id MTRSetupPayload)
matchedPayloadSelector = mkSelector "matchedPayload"

