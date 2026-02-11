{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataItemFilter
--
-- AVMetadataItemFilter is a tool used to filter AVMetadataItems.
--
-- Instances of AVMetadataItemFilter are used to filter AVMetadataItems.  They are opaque, unmodifiable objects, created via AVMetadataItemFilter class methods.
--
-- Generated bindings for @AVMetadataItemFilterInternal@.
module ObjC.AVFoundation.AVMetadataItemFilterInternal
  ( AVMetadataItemFilterInternal
  , IsAVMetadataItemFilterInternal(..)


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

