{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Movie-wide information required by the rendering session.
--
-- Generated bindings for @CNRenderingSessionAttributes@.
module ObjC.Cinematic.CNRenderingSessionAttributes
  ( CNRenderingSessionAttributes
  , IsCNRenderingSessionAttributes(..)
  , loadFromAsset_completionHandler
  , init_
  , new
  , renderingVersion
  , initSelector
  , loadFromAsset_completionHandlerSelector
  , newSelector
  , renderingVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load rendering session attributes from an asset asynchronously.
--
-- ObjC selector: @+ loadFromAsset:completionHandler:@
loadFromAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
loadFromAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNRenderingSessionAttributes"
    sendClassMessage cls' loadFromAsset_completionHandlerSelector (toAVAsset asset) completionHandler

-- | @- init@
init_ :: IsCNRenderingSessionAttributes cnRenderingSessionAttributes => cnRenderingSessionAttributes -> IO (Id CNRenderingSessionAttributes)
init_ cnRenderingSessionAttributes =
  sendOwnedMessage cnRenderingSessionAttributes initSelector

-- | @+ new@
new :: IO (Id CNRenderingSessionAttributes)
new  =
  do
    cls' <- getRequiredClass "CNRenderingSessionAttributes"
    sendOwnedClassMessage cls' newSelector

-- | Rendering version used to render the original.
--
-- ObjC selector: @- renderingVersion@
renderingVersion :: IsCNRenderingSessionAttributes cnRenderingSessionAttributes => cnRenderingSessionAttributes -> IO CLong
renderingVersion cnRenderingSessionAttributes =
  sendMessage cnRenderingSessionAttributes renderingVersionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromAsset:completionHandler:@
loadFromAsset_completionHandlerSelector :: Selector '[Id AVAsset, Ptr ()] ()
loadFromAsset_completionHandlerSelector = mkSelector "loadFromAsset:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNRenderingSessionAttributes)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNRenderingSessionAttributes)
newSelector = mkSelector "new"

-- | @Selector@ for @renderingVersion@
renderingVersionSelector :: Selector '[] CLong
renderingVersionSelector = mkSelector "renderingVersion"

