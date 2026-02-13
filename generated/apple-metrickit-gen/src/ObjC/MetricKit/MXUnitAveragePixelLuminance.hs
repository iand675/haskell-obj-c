{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ apl@
apl :: IO (Id MXUnitAveragePixelLuminance)
apl  =
  do
    cls' <- getRequiredClass "MXUnitAveragePixelLuminance"
    sendClassMessage cls' aplSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @apl@
aplSelector :: Selector '[] (Id MXUnitAveragePixelLuminance)
aplSelector = mkSelector "apl"

