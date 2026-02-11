{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNPoint3D
--
-- VNPoint3D represents a single, immutable, three-dimensional point in an image.
--
-- It should be noted that VNPoint3D is not intended as an overall replacement of simd float4x4, but is used by observations that need to present points which may contain additional metadata.
--
-- Generated bindings for @VNPoint3D@.
module ObjC.Vision.VNPoint3D
  ( VNPoint3D
  , IsVNPoint3D(..)
  , init_
  , initSelector


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

-- | @- init@
init_ :: IsVNPoint3D vnPoint3D => vnPoint3D -> IO (Id VNPoint3D)
init_ vnPoint3D  =
  sendMsg vnPoint3D (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

