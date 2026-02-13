{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetResourceLoadingRequestor
--
-- AVAssetResourceLoadingRequestor represents the originator of loading request
--
-- Information about the originator of a loading request, in order to decide whether or how to fulfill the request.
--
-- Generated bindings for @AVAssetResourceLoadingRequestor@.
module ObjC.AVFoundation.AVAssetResourceLoadingRequestor
  ( AVAssetResourceLoadingRequestor
  , IsAVAssetResourceLoadingRequestor(..)
  , init_
  , new
  , providesExpiredSessionReports
  , initSelector
  , newSelector
  , providesExpiredSessionReportsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetResourceLoadingRequestor avAssetResourceLoadingRequestor => avAssetResourceLoadingRequestor -> IO (Id AVAssetResourceLoadingRequestor)
init_ avAssetResourceLoadingRequestor =
  sendOwnedMessage avAssetResourceLoadingRequestor initSelector

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingRequestor)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingRequestor"
    sendOwnedClassMessage cls' newSelector

-- | providesExpiredSessionReports
--
-- Whether the requestor provides expired session reports (see AVContentKeySession)
--
-- ObjC selector: @- providesExpiredSessionReports@
providesExpiredSessionReports :: IsAVAssetResourceLoadingRequestor avAssetResourceLoadingRequestor => avAssetResourceLoadingRequestor -> IO Bool
providesExpiredSessionReports avAssetResourceLoadingRequestor =
  sendMessage avAssetResourceLoadingRequestor providesExpiredSessionReportsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetResourceLoadingRequestor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetResourceLoadingRequestor)
newSelector = mkSelector "new"

-- | @Selector@ for @providesExpiredSessionReports@
providesExpiredSessionReportsSelector :: Selector '[] Bool
providesExpiredSessionReportsSelector = mkSelector "providesExpiredSessionReports"

