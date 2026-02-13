{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Modify the created mutable texture.
--
-- ObjC selector: @- modifyPixelDataWithBlock:@
modifyPixelDataWithBlock :: IsSKMutableTexture skMutableTexture => skMutableTexture -> Ptr () -> IO ()
modifyPixelDataWithBlock skMutableTexture block =
  sendMessage skMutableTexture modifyPixelDataWithBlockSelector block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modifyPixelDataWithBlock:@
modifyPixelDataWithBlockSelector :: Selector '[Ptr ()] ()
modifyPixelDataWithBlockSelector = mkSelector "modifyPixelDataWithBlock:"

