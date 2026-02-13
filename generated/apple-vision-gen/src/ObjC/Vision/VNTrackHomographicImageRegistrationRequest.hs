{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An image registration request that will produce a homographic transformation that can morph one image to another.
--
-- Because this is a stateful request, it must be performed on at least two images in order to produce an observation.
--
-- Generated bindings for @VNTrackHomographicImageRegistrationRequest@.
module ObjC.Vision.VNTrackHomographicImageRegistrationRequest
  ( VNTrackHomographicImageRegistrationRequest
  , IsVNTrackHomographicImageRegistrationRequest(..)
  , init_
  , initWithCompletionHandler
  , results
  , initSelector
  , initWithCompletionHandlerSelector
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

-- | Create a new request that can statefully track the homographic registration of two images.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero and a nil completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNTrackHomographicImageRegistrationRequest vnTrackHomographicImageRegistrationRequest => vnTrackHomographicImageRegistrationRequest -> IO (Id VNTrackHomographicImageRegistrationRequest)
init_ vnTrackHomographicImageRegistrationRequest =
  sendOwnedMessage vnTrackHomographicImageRegistrationRequest initSelector

-- | Create a new request that can statefully track the homographic registration of two images.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackHomographicImageRegistrationRequest vnTrackHomographicImageRegistrationRequest => vnTrackHomographicImageRegistrationRequest -> Ptr () -> IO (Id VNTrackHomographicImageRegistrationRequest)
initWithCompletionHandler vnTrackHomographicImageRegistrationRequest completionHandler =
  sendOwnedMessage vnTrackHomographicImageRegistrationRequest initWithCompletionHandlerSelector completionHandler

-- | VNImageHomographicAlignmentObservation results.
--
-- ObjC selector: @- results@
results :: IsVNTrackHomographicImageRegistrationRequest vnTrackHomographicImageRegistrationRequest => vnTrackHomographicImageRegistrationRequest -> IO (Id NSArray)
results vnTrackHomographicImageRegistrationRequest =
  sendMessage vnTrackHomographicImageRegistrationRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTrackHomographicImageRegistrationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTrackHomographicImageRegistrationRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

