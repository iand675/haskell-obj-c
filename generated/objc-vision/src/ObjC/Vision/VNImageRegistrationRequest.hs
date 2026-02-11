{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will calculate a transformation for morphing a "floating" image onto an unchanging "reference" image.
--
-- The request is created with the targeted image acting as the floating image. Processing the request will calculate the transformations that morph the floating image onto the reference image.
--
-- Generated bindings for @VNImageRegistrationRequest@.
module ObjC.Vision.VNImageRegistrationRequest
  ( VNImageRegistrationRequest
  , IsVNImageRegistrationRequest(..)


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

