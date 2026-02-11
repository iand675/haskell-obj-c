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
  , initWithDetectedObjectObservationSelector
  , initWithDetectedObjectObservation_completionHandlerSelector
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

-- | Create a new request with detected object observation.
--
-- @observation@ — Detected object observation with bounding box info.
--
-- ObjC selector: @- initWithDetectedObjectObservation:@
initWithDetectedObjectObservation :: (IsVNTrackObjectRequest vnTrackObjectRequest, IsVNDetectedObjectObservation observation) => vnTrackObjectRequest -> observation -> IO (Id VNTrackObjectRequest)
initWithDetectedObjectObservation vnTrackObjectRequest  observation =
withObjCPtr observation $ \raw_observation ->
    sendMsg vnTrackObjectRequest (mkSelector "initWithDetectedObjectObservation:") (retPtr retVoid) [argPtr (castPtr raw_observation :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with detected object observation.
--
-- @observation@ — Detected object observation with bounding box info.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithDetectedObjectObservation:completionHandler:@
initWithDetectedObjectObservation_completionHandler :: (IsVNTrackObjectRequest vnTrackObjectRequest, IsVNDetectedObjectObservation observation) => vnTrackObjectRequest -> observation -> Ptr () -> IO (Id VNTrackObjectRequest)
initWithDetectedObjectObservation_completionHandler vnTrackObjectRequest  observation completionHandler =
withObjCPtr observation $ \raw_observation ->
    sendMsg vnTrackObjectRequest (mkSelector "initWithDetectedObjectObservation:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_observation :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNTrackObjectRequest vnTrackObjectRequest => vnTrackObjectRequest -> IO (Id VNTrackObjectRequest)
init_ vnTrackObjectRequest  =
  sendMsg vnTrackObjectRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackObjectRequest vnTrackObjectRequest => vnTrackObjectRequest -> Ptr () -> IO (Id VNTrackObjectRequest)
initWithCompletionHandler vnTrackObjectRequest  completionHandler =
  sendMsg vnTrackObjectRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDetectedObjectObservation:@
initWithDetectedObjectObservationSelector :: Selector
initWithDetectedObjectObservationSelector = mkSelector "initWithDetectedObjectObservation:"

-- | @Selector@ for @initWithDetectedObjectObservation:completionHandler:@
initWithDetectedObjectObservation_completionHandlerSelector :: Selector
initWithDetectedObjectObservation_completionHandlerSelector = mkSelector "initWithDetectedObjectObservation:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

