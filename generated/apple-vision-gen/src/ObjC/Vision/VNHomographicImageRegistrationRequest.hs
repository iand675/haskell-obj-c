{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An image registration request that will calculate a homographic transformation for morphing a "floating" image onto an unchanging "reference" image.
--
-- The request is created with the targeted image acting as the floating image. Processing the request will calculate the matrix warp transform that morph the floating image onto the reference image.	            Note that the request will fail unless the pixel dimensions of the reference image do not exactly match the resolved region of interest of the floating image.
--
-- Generated bindings for @VNHomographicImageRegistrationRequest@.
module ObjC.Vision.VNHomographicImageRegistrationRequest
  ( VNHomographicImageRegistrationRequest
  , IsVNHomographicImageRegistrationRequest(..)
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

-- | VNImageHomographicAlignmentObservation results.
--
-- ObjC selector: @- results@
results :: IsVNHomographicImageRegistrationRequest vnHomographicImageRegistrationRequest => vnHomographicImageRegistrationRequest -> IO (Id NSArray)
results vnHomographicImageRegistrationRequest =
  sendMessage vnHomographicImageRegistrationRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

