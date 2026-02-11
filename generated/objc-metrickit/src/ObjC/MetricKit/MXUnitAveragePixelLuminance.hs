{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXUnitAveragePixelLuminance
--
-- An NSUnit subclass representing the linear space Display APL.
--
-- Generated bindings for @MXUnitAveragePixelLuminance@.
module ObjC.MetricKit.MXUnitAveragePixelLuminance
  ( MXUnitAveragePixelLuminance
  , IsMXUnitAveragePixelLuminance(..)
  , apl
  , aplSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ apl@
apl :: IO (Id MXUnitAveragePixelLuminance)
apl  =
  do
    cls' <- getRequiredClass "MXUnitAveragePixelLuminance"
    sendClassMsg cls' (mkSelector "apl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @apl@
aplSelector :: Selector
aplSelector = mkSelector "apl"

