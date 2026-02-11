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

-- | Create a new request that can statefully track the translational registration of two images.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero and a nil completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNTrackTranslationalImageRegistrationRequest vnTrackTranslationalImageRegistrationRequest => vnTrackTranslationalImageRegistrationRequest -> IO (Id VNTrackTranslationalImageRegistrationRequest)
init_ vnTrackTranslationalImageRegistrationRequest  =
  sendMsg vnTrackTranslationalImageRegistrationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Create a new request that can statefully track the translational registration of two images.
--
-- This is a convenience initializer for a frame analysis spacing of kCMTimeZero.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTrackTranslationalImageRegistrationRequest vnTrackTranslationalImageRegistrationRequest => vnTrackTranslationalImageRegistrationRequest -> Ptr () -> IO (Id VNTrackTranslationalImageRegistrationRequest)
initWithCompletionHandler vnTrackTranslationalImageRegistrationRequest  completionHandler =
  sendMsg vnTrackTranslationalImageRegistrationRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | VNImageTranslationAlignmentObservation results.
--
-- ObjC selector: @- results@
results :: IsVNTrackTranslationalImageRegistrationRequest vnTrackTranslationalImageRegistrationRequest => vnTrackTranslationalImageRegistrationRequest -> IO (Id NSArray)
results vnTrackTranslationalImageRegistrationRequest  =
  sendMsg vnTrackTranslationalImageRegistrationRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

