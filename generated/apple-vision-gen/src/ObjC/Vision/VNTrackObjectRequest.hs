{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNTrackObjectRequest tracks an object in a sequence of images.
--
-- The VNTrackObjectRequest is a general purpose object tracker. This tracker is used when the tracked entity does not have a special tracker, like VNTrackRectangleRequest. The VNTrackObjectRequest is initialized with VNDetectedObjectObservation that contains bounding box for the object of interest. This tracker is processed using one of the [VNSequenceRequestHandler performRequests:...] methods.
--
-- Generated bindings for @VNTrackObjectRequest@.
module ObjC.Vision.VNTrackObjectRequest
  ( VNTrackObjectRequest
  , IsVNTrackObjectRequest(..)
  , initWithDetectedObjectObservation
  , initWithDetectedObjectObservation_completionHandler
  , init_
  , initWithCompletionHandler
  , initSelector
  , initWithCompletionHandlerSelector
  , initWithDetectedObjectObservationSelector
  , initWithDetectedObjectObservation_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new request with detected object observation.
--
-- @observation@ — Detected object observation with bounding box info.
--
-- ObjC selector: @- initWithDetectedObjectObservation:@
initWithDetectedObjectObservation :: (IsVNTrackObjectRequest vnTrackObjectRequest, IsVNDetectedObjectObservation observation) => vnTrackObjectRequest -> observation -> IO (Id VNTrackObjectRequest)
initWithDetectedObjectObservation vnTrackObjectRequest observation =
  sendOwnedMessage vnTrackObjectRequest initWithDetectedObjectObservationSelector (toVNDetectedObjectObservation observation)

-- | Create a new request with detected object observation.
--
-- @observation@ — Detected object observation with bounding box info.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithDetectedObjectObservation:completionHandler:@
initWithDetectedObjectObservation_completionHandler :: (IsVNTrackObjectRequest vnTrackObjectRequest, IsVNDetectedObjectObservation observation) => vnTrackObjectRequest -> observation -> Ptr () -> IO (Id VNTrackObjectRequest)
initWithDetectedObjectObservation_completionHandler vnTrackObjectRequest observation completionHandler =
  sendOwnedMessage vnTrackObjectRequest initWithDetectedObjectObservation_completionHandlerSelector (toVNDetectedObjectObservation observation) completionHandler

-- | @- init@
init_ :: IsVNTrackObjectRequest vnTrackObjectRequest => vnTrackObjectRequest -> IO (Id VNTrackObjectRequest)
init_ vnTrackObjectRequest =
  sendOwnedMessage vnTrackObjectRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackObjectRequest vnTrackObjectRequest => vnTrackObjectRequest -> Ptr () -> IO (Id VNTrackObjectRequest)
initWithCompletionHandler vnTrackObjectRequest completionHandler =
  sendOwnedMessage vnTrackObjectRequest initWithCompletionHandlerSelector completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDetectedObjectObservation:@
initWithDetectedObjectObservationSelector :: Selector '[Id VNDetectedObjectObservation] (Id VNTrackObjectRequest)
initWithDetectedObjectObservationSelector = mkSelector "initWithDetectedObjectObservation:"

-- | @Selector@ for @initWithDetectedObjectObservation:completionHandler:@
initWithDetectedObjectObservation_completionHandlerSelector :: Selector '[Id VNDetectedObjectObservation, Ptr ()] (Id VNTrackObjectRequest)
initWithDetectedObjectObservation_completionHandlerSelector = mkSelector "initWithDetectedObjectObservation:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTrackObjectRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTrackObjectRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

