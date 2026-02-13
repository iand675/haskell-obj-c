{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that generates an instance mask of salient objects that can be separated from the background.
--
-- Generated bindings for @VNGenerateForegroundInstanceMaskRequest@.
module ObjC.Vision.VNGenerateForegroundInstanceMaskRequest
  ( VNGenerateForegroundInstanceMaskRequest
  , IsVNGenerateForegroundInstanceMaskRequest(..)
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
results :: IsVNGenerateForegroundInstanceMaskRequest vnGenerateForegroundInstanceMaskRequest => vnGenerateForegroundInstanceMaskRequest -> IO (Id NSArray)
results vnGenerateForegroundInstanceMaskRequest =
  sendMessage vnGenerateForegroundInstanceMaskRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

