{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLImageSize@.
module ObjC.CoreML.MLImageSize
  ( MLImageSize
  , IsMLImageSize(..)
  , pixelsWide
  , pixelsHigh
  , pixelsWideSelector
  , pixelsHighSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pixelsWide@
pixelsWide :: IsMLImageSize mlImageSize => mlImageSize -> IO CLong
pixelsWide mlImageSize  =
  sendMsg mlImageSize (mkSelector "pixelsWide") retCLong []

-- | @- pixelsHigh@
pixelsHigh :: IsMLImageSize mlImageSize => mlImageSize -> IO CLong
pixelsHigh mlImageSize  =
  sendMsg mlImageSize (mkSelector "pixelsHigh") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector
pixelsHighSelector = mkSelector "pixelsHigh"

