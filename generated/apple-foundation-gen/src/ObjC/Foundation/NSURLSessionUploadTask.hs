{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionUploadTask@.
module ObjC.Foundation.NSURLSessionUploadTask
  ( NSURLSessionUploadTask
  , IsNSURLSessionUploadTask(..)
  , init_
  , new
  , cancelByProducingResumeData
  , cancelByProducingResumeDataSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSURLSessionUploadTask nsurlSessionUploadTask => nsurlSessionUploadTask -> IO (Id NSURLSessionUploadTask)
init_ nsurlSessionUploadTask =
  sendOwnedMessage nsurlSessionUploadTask initSelector

-- | @+ new@
new :: IO (Id NSURLSessionUploadTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionUploadTask"
    sendOwnedClassMessage cls' newSelector

-- | Cancels an upload and calls the completion handler with resume data for later use. resumeData will be nil if the server does not support the latest resumable uploads Internet-Draft from the HTTP Working Group, found at https://datatracker.ietf.org/doc/draft-ietf-httpbis-resumable-upload/
--
-- - Parameter completionHandler: The completion handler to call when the upload has been successfully canceled.
--
-- ObjC selector: @- cancelByProducingResumeData:@
cancelByProducingResumeData :: IsNSURLSessionUploadTask nsurlSessionUploadTask => nsurlSessionUploadTask -> Ptr () -> IO ()
cancelByProducingResumeData nsurlSessionUploadTask completionHandler =
  sendMessage nsurlSessionUploadTask cancelByProducingResumeDataSelector completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionUploadTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionUploadTask)
newSelector = mkSelector "new"

-- | @Selector@ for @cancelByProducingResumeData:@
cancelByProducingResumeDataSelector :: Selector '[Ptr ()] ()
cancelByProducingResumeDataSelector = mkSelector "cancelByProducingResumeData:"

