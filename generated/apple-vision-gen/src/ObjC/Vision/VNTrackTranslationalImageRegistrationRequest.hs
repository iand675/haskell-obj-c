{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An image registration request that will produce a translational transformation which will morph one image to another.
--
-- Because this is a stateful request, it must be performed on at least two images in order to produce an observation.
--
-- Generated bindings for @VNTrackTranslationalImageRegistrationRequest@.
module ObjC.Vision.VNTrackTranslationalImageRegistrationRequest
  ( VNTrackTranslationalImageRegistrationRequest
  , IsVNTrackTranslationalImageRegistrationRequest(..)
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

-- | Create a new request that can statefully track the translational registration of two images.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero and a nil completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNTrackTranslationalImageRegistrationRequest vnTrackTranslationalImageRegistrationRequest => vnTrackTranslationalImageRegistrationRequest -> IO (Id VNTrackTranslationalImageRegistrationRequest)
init_ vnTrackTranslationalImageRegistrationRequest =
  sendOwnedMessage vnTrackTranslationalImageRegistrationRequest initSelector

-- | Create a new request that can statefully track the translational registration of two images.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackTranslationalImageRegistrationRequest vnTrackTranslationalImageRegistrationRequest => vnTrackTranslationalImageRegistrationRequest -> Ptr () -> IO (Id VNTrackTranslationalImageRegistrationRequest)
initWithCompletionHandler vnTrackTranslationalImageRegistrationRequest completionHandler =
  sendOwnedMessage vnTrackTranslationalImageRegistrationRequest initWithCompletionHandlerSelector completionHandler

-- | VNImageTranslationAlignmentObservation results.
--
-- ObjC selector: @- results@
results :: IsVNTrackTranslationalImageRegistrationRequest vnTrackTranslationalImageRegistrationRequest => vnTrackTranslationalImageRegistrationRequest -> IO (Id NSArray)
results vnTrackTranslationalImageRegistrationRequest =
  sendMessage vnTrackTranslationalImageRegistrationRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTrackTranslationalImageRegistrationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTrackTranslationalImageRegistrationRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

