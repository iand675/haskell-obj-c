{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNTrackRectangleRequest tracks a rectangle in a sequence of images.
--
-- The VNTrackRectangleRequest is a special tracker to track rectangular shape objects. The VNTrackRectangleRequest is initialized with a VNRectangleObservation object that contains a rectangle bounding box and four corners locations. VNRectangleObservation can be obtained by running rectangle detector  (VNDetectRectanglesRequest). The VNTrackRectangleRequest is processed using one of the [VNSequenceRequestHandler performRequests:...] methods.
--
-- Note: The rectangular object doesn't have to look like a rectangle when projected into the plane of the image of interest. For example, it may look like trapezoid.
--
-- Generated bindings for @VNTrackRectangleRequest@.
module ObjC.Vision.VNTrackRectangleRequest
  ( VNTrackRectangleRequest
  , IsVNTrackRectangleRequest(..)
  , initWithRectangleObservation
  , initWithRectangleObservation_completionHandler
  , init_
  , initWithCompletionHandler
  , initSelector
  , initWithCompletionHandlerSelector
  , initWithRectangleObservationSelector
  , initWithRectangleObservation_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new rectangle tracking request with rectangle observation.
--
-- @observation@ — Rectangle observation with bounding box and rectangle corners location info.
--
-- ObjC selector: @- initWithRectangleObservation:@
initWithRectangleObservation :: (IsVNTrackRectangleRequest vnTrackRectangleRequest, IsVNRectangleObservation observation) => vnTrackRectangleRequest -> observation -> IO (Id VNTrackRectangleRequest)
initWithRectangleObservation vnTrackRectangleRequest observation =
  sendOwnedMessage vnTrackRectangleRequest initWithRectangleObservationSelector (toVNRectangleObservation observation)

-- | Create a new rectangle tracking request with rectangle observation.
--
-- @observation@ — Rectangle observation with bounding box and rectangle corners location info.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithRectangleObservation:completionHandler:@
initWithRectangleObservation_completionHandler :: (IsVNTrackRectangleRequest vnTrackRectangleRequest, IsVNRectangleObservation observation) => vnTrackRectangleRequest -> observation -> Ptr () -> IO (Id VNTrackRectangleRequest)
initWithRectangleObservation_completionHandler vnTrackRectangleRequest observation completionHandler =
  sendOwnedMessage vnTrackRectangleRequest initWithRectangleObservation_completionHandlerSelector (toVNRectangleObservation observation) completionHandler

-- | @- init@
init_ :: IsVNTrackRectangleRequest vnTrackRectangleRequest => vnTrackRectangleRequest -> IO (Id VNTrackRectangleRequest)
init_ vnTrackRectangleRequest =
  sendOwnedMessage vnTrackRectangleRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackRectangleRequest vnTrackRectangleRequest => vnTrackRectangleRequest -> Ptr () -> IO (Id VNTrackRectangleRequest)
initWithCompletionHandler vnTrackRectangleRequest completionHandler =
  sendOwnedMessage vnTrackRectangleRequest initWithCompletionHandlerSelector completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRectangleObservation:@
initWithRectangleObservationSelector :: Selector '[Id VNRectangleObservation] (Id VNTrackRectangleRequest)
initWithRectangleObservationSelector = mkSelector "initWithRectangleObservation:"

-- | @Selector@ for @initWithRectangleObservation:completionHandler:@
initWithRectangleObservation_completionHandlerSelector :: Selector '[Id VNRectangleObservation, Ptr ()] (Id VNTrackRectangleRequest)
initWithRectangleObservation_completionHandlerSelector = mkSelector "initWithRectangleObservation:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTrackRectangleRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTrackRectangleRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

