{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete metadata object subclass representing a dog head.
--
-- ``AVMetadataDogHeadObject`` is a concrete subclass of ``AVMetadataObject`` representing a dog head.
--
-- Generated bindings for @AVMetadataDogHeadObject@.
module ObjC.AVFoundation.AVMetadataDogHeadObject
  ( AVMetadataDogHeadObject
  , IsAVMetadataDogHeadObject(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

