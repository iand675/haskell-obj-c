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
  , initWithRectangleObservationSelector
  , initWithRectangleObservation_completionHandlerSelector
  , initSelector
  , initWithCompletionHandlerSelector


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

-- | Create a new rectangle tracking request with rectangle observation.
--
-- @observation@ — Rectangle observation with bounding box and rectangle corners location info.
--
-- ObjC selector: @- initWithRectangleObservation:@
initWithRectangleObservation :: (IsVNTrackRectangleRequest vnTrackRectangleRequest, IsVNRectangleObservation observation) => vnTrackRectangleRequest -> observation -> IO (Id VNTrackRectangleRequest)
initWithRectangleObservation vnTrackRectangleRequest  observation =
withObjCPtr observation $ \raw_observation ->
    sendMsg vnTrackRectangleRequest (mkSelector "initWithRectangleObservation:") (retPtr retVoid) [argPtr (castPtr raw_observation :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new rectangle tracking request with rectangle observation.
--
-- @observation@ — Rectangle observation with bounding box and rectangle corners location info.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithRectangleObservation:completionHandler:@
initWithRectangleObservation_completionHandler :: (IsVNTrackRectangleRequest vnTrackRectangleRequest, IsVNRectangleObservation observation) => vnTrackRectangleRequest -> observation -> Ptr () -> IO (Id VNTrackRectangleRequest)
initWithRectangleObservation_completionHandler vnTrackRectangleRequest  observation completionHandler =
withObjCPtr observation $ \raw_observation ->
    sendMsg vnTrackRectangleRequest (mkSelector "initWithRectangleObservation:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_observation :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNTrackRectangleRequest vnTrackRectangleRequest => vnTrackRectangleRequest -> IO (Id VNTrackRectangleRequest)
init_ vnTrackRectangleRequest  =
  sendMsg vnTrackRectangleRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackRectangleRequest vnTrackRectangleRequest => vnTrackRectangleRequest -> Ptr () -> IO (Id VNTrackRectangleRequest)
initWithCompletionHandler vnTrackRectangleRequest  completionHandler =
  sendMsg vnTrackRectangleRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRectangleObservation:@
initWithRectangleObservationSelector :: Selector
initWithRectangleObservationSelector = mkSelector "initWithRectangleObservation:"

-- | @Selector@ for @initWithRectangleObservation:completionHandler:@
initWithRectangleObservation_completionHandlerSelector :: Selector
initWithRectangleObservation_completionHandlerSelector = mkSelector "initWithRectangleObservation:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

