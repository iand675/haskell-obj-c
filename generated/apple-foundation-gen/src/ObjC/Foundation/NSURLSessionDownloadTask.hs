{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionDownloadTask@.
module ObjC.Foundation.NSURLSessionDownloadTask
  ( NSURLSessionDownloadTask
  , IsNSURLSessionDownloadTask(..)
  , cancelByProducingResumeData
  , init_
  , new
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

-- | @- cancelByProducingResumeData:@
cancelByProducingResumeData :: IsNSURLSessionDownloadTask nsurlSessionDownloadTask => nsurlSessionDownloadTask -> Ptr () -> IO ()
cancelByProducingResumeData nsurlSessionDownloadTask completionHandler =
  sendMessage nsurlSessionDownloadTask cancelByProducingResumeDataSelector completionHandler

-- | @- init@
init_ :: IsNSURLSessionDownloadTask nsurlSessionDownloadTask => nsurlSessionDownloadTask -> IO (Id NSURLSessionDownloadTask)
init_ nsurlSessionDownloadTask =
  sendOwnedMessage nsurlSessionDownloadTask initSelector

-- | @+ new@
new :: IO (Id NSURLSessionDownloadTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionDownloadTask"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancelByProducingResumeData:@
cancelByProducingResumeDataSelector :: Selector '[Ptr ()] ()
cancelByProducingResumeDataSelector = mkSelector "cancelByProducingResumeData:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionDownloadTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionDownloadTask)
newSelector = mkSelector "new"

