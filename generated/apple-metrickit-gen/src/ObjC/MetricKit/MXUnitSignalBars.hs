{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXUnitSignalBars
--
-- An NSUnit subclass representing the number of signal bars for signal strength.
--
-- Generated bindings for @MXUnitSignalBars@.
module ObjC.MetricKit.MXUnitSignalBars
  ( MXUnitSignalBars
  , IsMXUnitSignalBars(..)
  , bars
  , barsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ bars@
bars :: IO (Id MXUnitSignalBars)
bars  =
  do
    cls' <- getRequiredClass "MXUnitSignalBars"
    sendClassMessage cls' barsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bars@
barsSelector :: Selector '[] (Id MXUnitSignalBars)
barsSelector = mkSelector "bars"

