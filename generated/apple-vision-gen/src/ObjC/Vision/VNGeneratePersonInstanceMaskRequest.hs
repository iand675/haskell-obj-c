{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that generates an instance mask of individual people found in the image.
--
-- Generated bindings for @VNGeneratePersonInstanceMaskRequest@.
module ObjC.Vision.VNGeneratePersonInstanceMaskRequest
  ( VNGeneratePersonInstanceMaskRequest
  , IsVNGeneratePersonInstanceMaskRequest(..)
  , results
  , resultsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | VNObservation results.
--
-- ObjC selector: @- results@
results :: IsVNGeneratePersonInstanceMaskRequest vnGeneratePersonInstanceMaskRequest => vnGeneratePersonInstanceMaskRequest -> IO (Id NSArray)
results vnGeneratePersonInstanceMaskRequest =
  sendMessage vnGeneratePersonInstanceMaskRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

