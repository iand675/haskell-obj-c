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

-- | @- cancelByProducingResumeData:@
cancelByProducingResumeData :: IsNSURLSessionDownloadTask nsurlSessionDownloadTask => nsurlSessionDownloadTask -> Ptr () -> IO ()
cancelByProducingResumeData nsurlSessionDownloadTask  completionHandler =
  sendMsg nsurlSessionDownloadTask (mkSelector "cancelByProducingResumeData:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- init@
init_ :: IsNSURLSessionDownloadTask nsurlSessionDownloadTask => nsurlSessionDownloadTask -> IO (Id NSURLSessionDownloadTask)
init_ nsurlSessionDownloadTask  =
  sendMsg nsurlSessionDownloadTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionDownloadTask)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionDownloadTask"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancelByProducingResumeData:@
cancelByProducingResumeDataSelector :: Selector
cancelByProducingResumeDataSelector = mkSelector "cancelByProducingResumeData:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

