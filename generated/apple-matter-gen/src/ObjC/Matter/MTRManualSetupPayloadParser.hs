{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRManualSetupPayloadParser@.
module ObjC.Matter.MTRManualSetupPayloadParser
  ( MTRManualSetupPayloadParser
  , IsMTRManualSetupPayloadParser(..)
  , initWithDecimalStringRepresentation
  , populatePayload
  , initWithDecimalStringRepresentationSelector
  , populatePayloadSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDecimalStringRepresentation:@
initWithDecimalStringRepresentation :: (IsMTRManualSetupPayloadParser mtrManualSetupPayloadParser, IsNSString decimalStringRepresentation) => mtrManualSetupPayloadParser -> decimalStringRepresentation -> IO (Id MTRManualSetupPayloadParser)
initWithDecimalStringRepresentation mtrManualSetupPayloadParser decimalStringRepresentation =
  sendOwnedMessage mtrManualSetupPayloadParser initWithDecimalStringRepresentationSelector (toNSString decimalStringRepresentation)

-- | @- populatePayload:@
populatePayload :: (IsMTRManualSetupPayloadParser mtrManualSetupPayloadParser, IsNSError error_) => mtrManualSetupPayloadParser -> error_ -> IO (Id MTRSetupPayload)
populatePayload mtrManualSetupPayloadParser error_ =
  sendMessage mtrManualSetupPayloadParser populatePayloadSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDecimalStringRepresentation:@
initWithDecimalStringRepresentationSelector :: Selector '[Id NSString] (Id MTRManualSetupPayloadParser)
initWithDecimalStringRepresentationSelector = mkSelector "initWithDecimalStringRepresentation:"

-- | @Selector@ for @populatePayload:@
populatePayloadSelector :: Selector '[Id NSError] (Id MTRSetupPayload)
populatePayloadSelector = mkSelector "populatePayload:"

