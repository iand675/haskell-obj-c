{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROnboardingPayloadParser@.
module ObjC.Matter.MTROnboardingPayloadParser
  ( MTROnboardingPayloadParser
  , IsMTROnboardingPayloadParser(..)
  , setupPayloadForOnboardingPayload_error
  , setupPayloadForOnboardingPayload_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ setupPayloadForOnboardingPayload:error:@
setupPayloadForOnboardingPayload_error :: (IsNSString onboardingPayload, IsNSError error_) => onboardingPayload -> error_ -> IO (Id MTRSetupPayload)
setupPayloadForOnboardingPayload_error onboardingPayload error_ =
  do
    cls' <- getRequiredClass "MTROnboardingPayloadParser"
    sendClassMessage cls' setupPayloadForOnboardingPayload_errorSelector (toNSString onboardingPayload) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setupPayloadForOnboardingPayload:error:@
setupPayloadForOnboardingPayload_errorSelector :: Selector '[Id NSString, Id NSError] (Id MTRSetupPayload)
setupPayloadForOnboardingPayload_errorSelector = mkSelector "setupPayloadForOnboardingPayload:error:"

