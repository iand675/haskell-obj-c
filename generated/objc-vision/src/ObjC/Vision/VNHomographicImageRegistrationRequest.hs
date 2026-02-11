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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | VNImageHomographicAlignmentObservation results.
--
-- ObjC selector: @- results@
results :: IsVNHomographicImageRegistrationRequest vnHomographicImageRegistrationRequest => vnHomographicImageRegistrationRequest -> IO (Id NSArray)
results vnHomographicImageRegistrationRequest  =
  sendMsg vnHomographicImageRegistrationRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

