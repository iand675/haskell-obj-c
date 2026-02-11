{-# LANGUAGE PatternSynonyms #-}
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

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initForEvents:progressHandler:completionHandler:@
initForEvents_progressHandler_completionHandler :: IsMLUpdateProgressHandlers mlUpdateProgressHandlers => mlUpdateProgressHandlers -> MLUpdateProgressEvent -> Ptr () -> Ptr () -> IO (Id MLUpdateProgressHandlers)
initForEvents_progressHandler_completionHandler mlUpdateProgressHandlers  interestedEvents progressHandler completionHandler =
  sendMsg mlUpdateProgressHandlers (mkSelector "initForEvents:progressHandler:completionHandler:") (retPtr retVoid) [argCLong (coerce interestedEvents), argPtr (castPtr progressHandler :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLUpdateProgressHandlers mlUpdateProgressHandlers => mlUpdateProgressHandlers -> IO (Id MLUpdateProgressHandlers)
init_ mlUpdateProgressHandlers  =
  sendMsg mlUpdateProgressHandlers (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLUpdateProgressHandlers"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initForEvents:progressHandler:completionHandler:@
initForEvents_progressHandler_completionHandlerSelector :: Selector
initForEvents_progressHandler_completionHandlerSelector = mkSelector "initForEvents:progressHandler:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

