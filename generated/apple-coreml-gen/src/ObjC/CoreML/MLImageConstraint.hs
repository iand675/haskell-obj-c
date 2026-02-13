{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLImageConstraint
--
-- Constraint on image properties.
--
-- Generated bindings for @MLImageConstraint@.
module ObjC.CoreML.MLImageConstraint
  ( MLImageConstraint
  , IsMLImageConstraint(..)
  , init_
  , pixelsHigh
  , pixelsWide
  , pixelFormatType
  , sizeConstraint
  , initSelector
  , pixelFormatTypeSelector
  , pixelsHighSelector
  , pixelsWideSelector
  , sizeConstraintSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO (Id MLImageConstraint)
init_ mlImageConstraint =
  sendOwnedMessage mlImageConstraint initSelector

-- | The required or default height of the image
--
-- ObjC selector: @- pixelsHigh@
pixelsHigh :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO CLong
pixelsHigh mlImageConstraint =
  sendMessage mlImageConstraint pixelsHighSelector

-- | The required or default width of the image
--
-- ObjC selector: @- pixelsWide@
pixelsWide :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO CLong
pixelsWide mlImageConstraint =
  sendMessage mlImageConstraint pixelsWideSelector

-- | The accepted kCVPixelFormatType for the image.
--
-- ObjC selector: @- pixelFormatType@
pixelFormatType :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO CUInt
pixelFormatType mlImageConstraint =
  sendMessage mlImageConstraint pixelFormatTypeSelector

-- | Detailed image size constraint
--
-- ObjC selector: @- sizeConstraint@
sizeConstraint :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO (Id MLImageSizeConstraint)
sizeConstraint mlImageConstraint =
  sendMessage mlImageConstraint sizeConstraintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLImageConstraint)
initSelector = mkSelector "init"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector '[] CLong
pixelsHighSelector = mkSelector "pixelsHigh"

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector '[] CLong
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @pixelFormatType@
pixelFormatTypeSelector :: Selector '[] CUInt
pixelFormatTypeSelector = mkSelector "pixelFormatType"

-- | @Selector@ for @sizeConstraint@
sizeConstraintSelector :: Selector '[] (Id MLImageSizeConstraint)
sizeConstraintSelector = mkSelector "sizeConstraint"

