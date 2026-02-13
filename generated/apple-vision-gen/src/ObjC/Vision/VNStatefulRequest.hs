{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that builds evidence over time by being reused on multiple images.
--
-- The request requires the use of CMSampleBuffers with timestamps as input; otherwise, a VNErrorTimeStampNotFound error will be returned. VNStatefulRequest is used as a base class of other requests, so no objects of this class should be created directly.
--
-- Generated bindings for @VNStatefulRequest@.
module ObjC.Vision.VNStatefulRequest
  ( VNStatefulRequest
  , IsVNStatefulRequest(..)
  , new
  , init_
  , initWithCompletionHandler
  , minimumLatencyFrameCount
  , initSelector
  , initWithCompletionHandlerSelector
  , minimumLatencyFrameCountSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNStatefulRequest)
new  =
  do
    cls' <- getRequiredClass "VNStatefulRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNStatefulRequest vnStatefulRequest => vnStatefulRequest -> IO (Id VNStatefulRequest)
init_ vnStatefulRequest =
  sendOwnedMessage vnStatefulRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNStatefulRequest vnStatefulRequest => vnStatefulRequest -> Ptr () -> IO (Id VNStatefulRequest)
initWithCompletionHandler vnStatefulRequest completionHandler =
  sendOwnedMessage vnStatefulRequest initWithCompletionHandlerSelector completionHandler

-- | The minimum number of frames that the request has to process on before reporting back any observation. This information is provided by the request once initialized with its required paramters.
--
-- Video based request often need a minimum number of frames before they can report back any observation. An example would be that a movement detection requires at least 5 frames to be detected. The minimumLatencyFrameCount for that request would report 5 and only after 5 frames have been processed an observation would be returned in the results. This latency is indicative of how responsive a request is in respect to the incoming data.
--
-- ObjC selector: @- minimumLatencyFrameCount@
minimumLatencyFrameCount :: IsVNStatefulRequest vnStatefulRequest => vnStatefulRequest -> IO CLong
minimumLatencyFrameCount vnStatefulRequest =
  sendMessage vnStatefulRequest minimumLatencyFrameCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNStatefulRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNStatefulRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNStatefulRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @minimumLatencyFrameCount@
minimumLatencyFrameCountSelector :: Selector '[] CLong
minimumLatencyFrameCountSelector = mkSelector "minimumLatencyFrameCount"

