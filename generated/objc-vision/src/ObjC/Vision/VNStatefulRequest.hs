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
  , newSelector
  , initSelector
  , initWithCompletionHandlerSelector
  , minimumLatencyFrameCountSelector


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

-- | @+ new@
new :: IO (Id VNStatefulRequest)
new  =
  do
    cls' <- getRequiredClass "VNStatefulRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNStatefulRequest vnStatefulRequest => vnStatefulRequest -> IO (Id VNStatefulRequest)
init_ vnStatefulRequest  =
  sendMsg vnStatefulRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNStatefulRequest vnStatefulRequest => vnStatefulRequest -> Ptr () -> IO (Id VNStatefulRequest)
initWithCompletionHandler vnStatefulRequest  completionHandler =
  sendMsg vnStatefulRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | The minimum number of frames that the request has to process on before reporting back any observation. This information is provided by the request once initialized with its required paramters.
--
-- Video based request often need a minimum number of frames before they can report back any observation. An example would be that a movement detection requires at least 5 frames to be detected. The minimumLatencyFrameCount for that request would report 5 and only after 5 frames have been processed an observation would be returned in the results. This latency is indicative of how responsive a request is in respect to the incoming data.
--
-- ObjC selector: @- minimumLatencyFrameCount@
minimumLatencyFrameCount :: IsVNStatefulRequest vnStatefulRequest => vnStatefulRequest -> IO CLong
minimumLatencyFrameCount vnStatefulRequest  =
  sendMsg vnStatefulRequest (mkSelector "minimumLatencyFrameCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @minimumLatencyFrameCount@
minimumLatencyFrameCountSelector :: Selector
minimumLatencyFrameCountSelector = mkSelector "minimumLatencyFrameCount"

