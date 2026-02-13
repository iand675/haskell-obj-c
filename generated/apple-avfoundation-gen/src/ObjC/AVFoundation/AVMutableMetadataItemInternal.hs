{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableMetadataItem
--
-- AVMutableMetadataItem provides support for building collections of metadata to be written    				to asset files via AVAssetExportSession, AVAssetWriter or AVAssetWriterInput.
--
-- Can be initialized from an existing AVMetadataItem or with a one or more of the basic properties					of a metadata item: a key, a keySpace, a locale, and a value.
--
-- Generated bindings for @AVMutableMetadataItemInternal@.
module ObjC.AVFoundation.AVMutableMetadataItemInternal
  ( AVMutableMetadataItemInternal
  , IsAVMutableMetadataItemInternal(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

