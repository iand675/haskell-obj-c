{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRQRCodeSetupPayloadParser@.
module ObjC.Matter.MTRQRCodeSetupPayloadParser
  ( MTRQRCodeSetupPayloadParser
  , IsMTRQRCodeSetupPayloadParser(..)
  , initWithBase38Representation
  , populatePayload
  , initWithBase38RepresentationSelector
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

-- | @- initWithBase38Representation:@
initWithBase38Representation :: (IsMTRQRCodeSetupPayloadParser mtrqrCodeSetupPayloadParser, IsNSString base38Representation) => mtrqrCodeSetupPayloadParser -> base38Representation -> IO (Id MTRQRCodeSetupPayloadParser)
initWithBase38Representation mtrqrCodeSetupPayloadParser base38Representation =
  sendOwnedMessage mtrqrCodeSetupPayloadParser initWithBase38RepresentationSelector (toNSString base38Representation)

-- | @- populatePayload:@
populatePayload :: (IsMTRQRCodeSetupPayloadParser mtrqrCodeSetupPayloadParser, IsNSError error_) => mtrqrCodeSetupPayloadParser -> error_ -> IO (Id MTRSetupPayload)
populatePayload mtrqrCodeSetupPayloadParser error_ =
  sendMessage mtrqrCodeSetupPayloadParser populatePayloadSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBase38Representation:@
initWithBase38RepresentationSelector :: Selector '[Id NSString] (Id MTRQRCodeSetupPayloadParser)
initWithBase38RepresentationSelector = mkSelector "initWithBase38Representation:"

-- | @Selector@ for @populatePayload:@
populatePayloadSelector :: Selector '[Id NSError] (Id MTRSetupPayload)
populatePayloadSelector = mkSelector "populatePayload:"

