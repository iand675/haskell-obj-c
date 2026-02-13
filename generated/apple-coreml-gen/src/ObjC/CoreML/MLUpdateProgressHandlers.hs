{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Allows applications to register for progress and completion handlers.
--
-- Generated bindings for @MLUpdateProgressHandlers@.
module ObjC.CoreML.MLUpdateProgressHandlers
  ( MLUpdateProgressHandlers
  , IsMLUpdateProgressHandlers(..)
  , initForEvents_progressHandler_completionHandler
  , init_
  , new
  , initForEvents_progressHandler_completionHandlerSelector
  , initSelector
  , newSelector

  -- * Enum types
  , MLUpdateProgressEvent(MLUpdateProgressEvent)
  , pattern MLUpdateProgressEventTrainingBegin
  , pattern MLUpdateProgressEventEpochEnd
  , pattern MLUpdateProgressEventMiniBatchEnd

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initForEvents:progressHandler:completionHandler:@
initForEvents_progressHandler_completionHandler :: IsMLUpdateProgressHandlers mlUpdateProgressHandlers => mlUpdateProgressHandlers -> MLUpdateProgressEvent -> Ptr () -> Ptr () -> IO (Id MLUpdateProgressHandlers)
initForEvents_progressHandler_completionHandler mlUpdateProgressHandlers interestedEvents progressHandler completionHandler =
  sendOwnedMessage mlUpdateProgressHandlers initForEvents_progressHandler_completionHandlerSelector interestedEvents progressHandler completionHandler

-- | @- init@
init_ :: IsMLUpdateProgressHandlers mlUpdateProgressHandlers => mlUpdateProgressHandlers -> IO (Id MLUpdateProgressHandlers)
init_ mlUpdateProgressHandlers =
  sendOwnedMessage mlUpdateProgressHandlers initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLUpdateProgressHandlers"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForEvents:progressHandler:completionHandler:@
initForEvents_progressHandler_completionHandlerSelector :: Selector '[MLUpdateProgressEvent, Ptr (), Ptr ()] (Id MLUpdateProgressHandlers)
initForEvents_progressHandler_completionHandlerSelector = mkSelector "initForEvents:progressHandler:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLUpdateProgressHandlers)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

