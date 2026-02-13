{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLImageSize@.
module ObjC.CoreML.MLImageSize
  ( MLImageSize
  , IsMLImageSize(..)
  , pixelsWide
  , pixelsHigh
  , pixelsHighSelector
  , pixelsWideSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pixelsWide@
pixelsWide :: IsMLImageSize mlImageSize => mlImageSize -> IO CLong
pixelsWide mlImageSize =
  sendMessage mlImageSize pixelsWideSelector

-- | @- pixelsHigh@
pixelsHigh :: IsMLImageSize mlImageSize => mlImageSize -> IO CLong
pixelsHigh mlImageSize =
  sendMessage mlImageSize pixelsHighSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector '[] CLong
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector '[] CLong
pixelsHighSelector = mkSelector "pixelsHigh"

