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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetResourceLoadingRequestor avAssetResourceLoadingRequestor => avAssetResourceLoadingRequestor -> IO (Id AVAssetResourceLoadingRequestor)
init_ avAssetResourceLoadingRequestor  =
  sendMsg avAssetResourceLoadingRequestor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingRequestor)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingRequestor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | providesExpiredSessionReports
--
-- Whether the requestor provides expired session reports (see AVContentKeySession)
--
-- ObjC selector: @- providesExpiredSessionReports@
providesExpiredSessionReports :: IsAVAssetResourceLoadingRequestor avAssetResourceLoadingRequestor => avAssetResourceLoadingRequestor -> IO Bool
providesExpiredSessionReports avAssetResourceLoadingRequestor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoadingRequestor (mkSelector "providesExpiredSessionReports") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @providesExpiredSessionReports@
providesExpiredSessionReportsSelector :: Selector
providesExpiredSessionReportsSelector = mkSelector "providesExpiredSessionReports"

