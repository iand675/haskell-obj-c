{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An image registration request that will calculate a translational transformation for morphing a "floating" image onto an unchanging "reference" image.
--
-- The request is created with the targeted image acting as the floating image. Processing the request will calculate the affine transformations that morph the floating image onto the reference image.
--
-- Generated bindings for @VNTranslationalImageRegistrationRequest@.
module ObjC.Vision.VNTranslationalImageRegistrationRequest
  ( VNTranslationalImageRegistrationRequest
  , IsVNTranslationalImageRegistrationRequest(..)
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

-- | VNImageTranslationAlignmentObservation results.
--
-- ObjC selector: @- results@
results :: IsVNTranslationalImageRegistrationRequest vnTranslationalImageRegistrationRequest => vnTranslationalImageRegistrationRequest -> IO (Id NSArray)
results vnTranslationalImageRegistrationRequest =
  sendMessage vnTranslationalImageRegistrationRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

