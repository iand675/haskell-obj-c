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
  , initSelector
  , newSelector
  , cancelByProducingResumeDataSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSURLSessionUploadTask nsurlSessionUploadTask => nsurlSessionUploadTask -> IO (Id NSURLSessionUploadTask)
init_ nsurlSessionUploadTask  =
  sendMsg nsurlSessionUploadTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionUploadTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionUploadTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Cancels an upload and calls the completion handler with resume data for later use. resumeData will be nil if the server does not support the latest resumable uploads Internet-Draft from the HTTP Working Group, found at https://datatracker.ietf.org/doc/draft-ietf-httpbis-resumable-upload/
--
-- - Parameter completionHandler: The completion handler to call when the upload has been successfully canceled.
--
-- ObjC selector: @- cancelByProducingResumeData:@
cancelByProducingResumeData :: IsNSURLSessionUploadTask nsurlSessionUploadTask => nsurlSessionUploadTask -> Ptr () -> IO ()
cancelByProducingResumeData nsurlSessionUploadTask  completionHandler =
  sendMsg nsurlSessionUploadTask (mkSelector "cancelByProducingResumeData:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @cancelByProducingResumeData:@
cancelByProducingResumeDataSelector :: Selector
cancelByProducingResumeDataSelector = mkSelector "cancelByProducingResumeData:"

