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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

