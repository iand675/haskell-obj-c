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

-- | @+ bars@
bars :: IO (Id MXUnitSignalBars)
bars  =
  do
    cls' <- getRequiredClass "MXUnitSignalBars"
    sendClassMsg cls' (mkSelector "bars") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bars@
barsSelector :: Selector
barsSelector = mkSelector "bars"

