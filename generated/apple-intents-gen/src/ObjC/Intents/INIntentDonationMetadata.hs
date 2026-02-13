{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntentDonationMetadata@.
module ObjC.Intents.INIntentDonationMetadata
  ( INIntentDonationMetadata
  , IsINIntentDonationMetadata(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINIntentDonationMetadata inIntentDonationMetadata => inIntentDonationMetadata -> IO (Id INIntentDonationMetadata)
init_ inIntentDonationMetadata =
  sendOwnedMessage inIntentDonationMetadata initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INIntentDonationMetadata)
initSelector = mkSelector "init"

