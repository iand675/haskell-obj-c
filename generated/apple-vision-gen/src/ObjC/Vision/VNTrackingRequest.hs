{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A base class for all tracking requests.
--
-- Since this class is not meant to be directly instantiated, no initializers are available.
--
-- Generated bindings for @VNTrackingRequest@.
module ObjC.Vision.VNTrackingRequest
  ( VNTrackingRequest
  , IsVNTrackingRequest(..)
  , supportedNumberOfTrackersAndReturnError
  , init_
  , initWithCompletionHandler
  , inputObservation
  , setInputObservation
  , trackingLevel
  , setTrackingLevel
  , lastFrame
  , setLastFrame
  , initSelector
  , initWithCompletionHandlerSelector
  , inputObservationSelector
  , lastFrameSelector
  , setInputObservationSelector
  , setLastFrameSelector
  , setTrackingLevelSelector
  , supportedNumberOfTrackersAndReturnErrorSelector
  , trackingLevelSelector

  -- * Enum types
  , VNRequestTrackingLevel(VNRequestTrackingLevel)
  , pattern VNRequestTrackingLevelAccurate
  , pattern VNRequestTrackingLevelFast

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | This class method returns a maximum number of allowed simultaneously executed trackers for [request revision x tracking level] combination
--
-- The total number of simultaneously running trackes is limited due to performance concerns. There is a limit for each combination of          [request revision x tracking level] and this method could be used to query that limit
--
-- @revision@ — The revision of a specific tracking request (an object of a subclass of VNTrackingRequest).
--
-- @trackingLevel@ — Tracking level of a specific tracking request (an object of a subclass of VNTrackingRequest).
--
-- @error@ — The address of a variable that will be populated with an error upon failure. If the caller does not need this information, NULL can be passed.
--
-- Returns: Maximum number of trackers for a given combination [request revision x tracking level], or 0 if such combination doesn't exist
--
-- ObjC selector: @- supportedNumberOfTrackersAndReturnError:@
supportedNumberOfTrackersAndReturnError :: (IsVNTrackingRequest vnTrackingRequest, IsNSError error_) => vnTrackingRequest -> error_ -> IO CULong
supportedNumberOfTrackersAndReturnError vnTrackingRequest error_ =
  sendMessage vnTrackingRequest supportedNumberOfTrackersAndReturnErrorSelector (toNSError error_)

-- | @- init@
init_ :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> IO (Id VNTrackingRequest)
init_ vnTrackingRequest =
  sendOwnedMessage vnTrackingRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> Ptr () -> IO (Id VNTrackingRequest)
initWithCompletionHandler vnTrackingRequest completionHandler =
  sendOwnedMessage vnTrackingRequest initWithCompletionHandlerSelector completionHandler

-- | property inputObservation
--
-- The observation object that defines a region to track. Providing an observation not returned from a tracker (e.g. user-defined, or from a detector) begins a new tracker for the sequence. Providing an observation that was returned from a tracker continues the use of that tracker, to track the region to the next frame. In general, unless documented in the request's documentation, the rectangle must be defined in normalized coordinates (both dimensions normalized to [0,1] with the origin at the lower-left corner).
--
-- ObjC selector: @- inputObservation@
inputObservation :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> IO (Id VNDetectedObjectObservation)
inputObservation vnTrackingRequest =
  sendMessage vnTrackingRequest inputObservationSelector

-- | property inputObservation
--
-- The observation object that defines a region to track. Providing an observation not returned from a tracker (e.g. user-defined, or from a detector) begins a new tracker for the sequence. Providing an observation that was returned from a tracker continues the use of that tracker, to track the region to the next frame. In general, unless documented in the request's documentation, the rectangle must be defined in normalized coordinates (both dimensions normalized to [0,1] with the origin at the lower-left corner).
--
-- ObjC selector: @- setInputObservation:@
setInputObservation :: (IsVNTrackingRequest vnTrackingRequest, IsVNDetectedObjectObservation value) => vnTrackingRequest -> value -> IO ()
setInputObservation vnTrackingRequest value =
  sendMessage vnTrackingRequest setInputObservationSelector (toVNDetectedObjectObservation value)

-- | property trackingLevel
--
-- Tracking level allows tuning tracking algorithm to prefer speed (VNRequestTrackingLevelFast) vs. tracking object location accuracy (VNRequestTrackingLevelAccurate). This property has no effect on general purpose object tracker (VNTrackObjectRequest) revision 2 (VNTrackObjectRequestRevision2)
--
-- ObjC selector: @- trackingLevel@
trackingLevel :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> IO VNRequestTrackingLevel
trackingLevel vnTrackingRequest =
  sendMessage vnTrackingRequest trackingLevelSelector

-- | property trackingLevel
--
-- Tracking level allows tuning tracking algorithm to prefer speed (VNRequestTrackingLevelFast) vs. tracking object location accuracy (VNRequestTrackingLevelAccurate). This property has no effect on general purpose object tracker (VNTrackObjectRequest) revision 2 (VNTrackObjectRequestRevision2)
--
-- ObjC selector: @- setTrackingLevel:@
setTrackingLevel :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> VNRequestTrackingLevel -> IO ()
setTrackingLevel vnTrackingRequest value =
  sendMessage vnTrackingRequest setTrackingLevelSelector value

-- | property lastFrame
--
-- This property allows marking the last frame for tracking using current tracker. If set to YES, the results for this frame will be processed and returned and the current tracker will be released to the pool of available trackers
--
-- ObjC selector: @- lastFrame@
lastFrame :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> IO Bool
lastFrame vnTrackingRequest =
  sendMessage vnTrackingRequest lastFrameSelector

-- | property lastFrame
--
-- This property allows marking the last frame for tracking using current tracker. If set to YES, the results for this frame will be processed and returned and the current tracker will be released to the pool of available trackers
--
-- ObjC selector: @- setLastFrame:@
setLastFrame :: IsVNTrackingRequest vnTrackingRequest => vnTrackingRequest -> Bool -> IO ()
setLastFrame vnTrackingRequest value =
  sendMessage vnTrackingRequest setLastFrameSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedNumberOfTrackersAndReturnError:@
supportedNumberOfTrackersAndReturnErrorSelector :: Selector '[Id NSError] CULong
supportedNumberOfTrackersAndReturnErrorSelector = mkSelector "supportedNumberOfTrackersAndReturnError:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTrackingRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTrackingRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @inputObservation@
inputObservationSelector :: Selector '[] (Id VNDetectedObjectObservation)
inputObservationSelector = mkSelector "inputObservation"

-- | @Selector@ for @setInputObservation:@
setInputObservationSelector :: Selector '[Id VNDetectedObjectObservation] ()
setInputObservationSelector = mkSelector "setInputObservation:"

-- | @Selector@ for @trackingLevel@
trackingLevelSelector :: Selector '[] VNRequestTrackingLevel
trackingLevelSelector = mkSelector "trackingLevel"

-- | @Selector@ for @setTrackingLevel:@
setTrackingLevelSelector :: Selector '[VNRequestTrackingLevel] ()
setTrackingLevelSelector = mkSelector "setTrackingLevel:"

-- | @Selector@ for @lastFrame@
lastFrameSelector :: Selector '[] Bool
lastFrameSelector = mkSelector "lastFrame"

-- | @Selector@ for @setLastFrame:@
setLastFrameSelector :: Selector '[Bool] ()
setLastFrameSelector = mkSelector "setLastFrame:"

