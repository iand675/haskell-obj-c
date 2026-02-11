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

-- | @+ metadataItemFilterForSharing@
metadataItemFilterForSharing :: IO (Id AVMetadataItemFilter)
metadataItemFilterForSharing  =
  do
    cls' <- getRequiredClass "AVMetadataItemFilter"
    sendClassMsg cls' (mkSelector "metadataItemFilterForSharing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataItemFilterForSharing@
metadataItemFilterForSharingSelector :: Selector
metadataItemFilterForSharingSelector = mkSelector "metadataItemFilterForSharing"

