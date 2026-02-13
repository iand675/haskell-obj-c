{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataItemFilter
--
-- Filters selected information from a metadata item.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetadataItemFilter@.
module ObjC.AVFoundation.AVMetadataItemFilter
  ( AVMetadataItemFilter
  , IsAVMetadataItemFilter(..)
  , metadataItemFilterForSharing
  , metadataItemFilterForSharingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ metadataItemFilterForSharing@
metadataItemFilterForSharing :: IO (Id AVMetadataItemFilter)
metadataItemFilterForSharing  =
  do
    cls' <- getRequiredClass "AVMetadataItemFilter"
    sendClassMessage cls' metadataItemFilterForSharingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataItemFilterForSharing@
metadataItemFilterForSharingSelector :: Selector '[] (Id AVMetadataItemFilter)
metadataItemFilterForSharingSelector = mkSelector "metadataItemFilterForSharing"

