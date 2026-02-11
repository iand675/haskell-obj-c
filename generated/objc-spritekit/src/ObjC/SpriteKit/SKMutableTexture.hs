{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKMutableTexture@.
module ObjC.SpriteKit.SKMutableTexture
  ( SKMutableTexture
  , IsSKMutableTexture(..)
  , modifyPixelDataWithBlock
  , modifyPixelDataWithBlockSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Modify the created mutable texture.
--
-- ObjC selector: @- modifyPixelDataWithBlock:@
modifyPixelDataWithBlock :: IsSKMutableTexture skMutableTexture => skMutableTexture -> Ptr () -> IO ()
modifyPixelDataWithBlock skMutableTexture  block =
  sendMsg skMutableTexture (mkSelector "modifyPixelDataWithBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modifyPixelDataWithBlock:@
modifyPixelDataWithBlockSelector :: Selector
modifyPixelDataWithBlockSelector = mkSelector "modifyPixelDataWithBlock:"

