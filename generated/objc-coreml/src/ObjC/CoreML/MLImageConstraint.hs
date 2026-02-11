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
  , pixelsHighSelector
  , pixelsWideSelector
  , pixelFormatTypeSelector
  , sizeConstraintSelector


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

-- | @- init@
init_ :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO (Id MLImageConstraint)
init_ mlImageConstraint  =
  sendMsg mlImageConstraint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The required or default height of the image
--
-- ObjC selector: @- pixelsHigh@
pixelsHigh :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO CLong
pixelsHigh mlImageConstraint  =
  sendMsg mlImageConstraint (mkSelector "pixelsHigh") retCLong []

-- | The required or default width of the image
--
-- ObjC selector: @- pixelsWide@
pixelsWide :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO CLong
pixelsWide mlImageConstraint  =
  sendMsg mlImageConstraint (mkSelector "pixelsWide") retCLong []

-- | The accepted kCVPixelFormatType for the image.
--
-- ObjC selector: @- pixelFormatType@
pixelFormatType :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO CUInt
pixelFormatType mlImageConstraint  =
  sendMsg mlImageConstraint (mkSelector "pixelFormatType") retCUInt []

-- | Detailed image size constraint
--
-- ObjC selector: @- sizeConstraint@
sizeConstraint :: IsMLImageConstraint mlImageConstraint => mlImageConstraint -> IO (Id MLImageSizeConstraint)
sizeConstraint mlImageConstraint  =
  sendMsg mlImageConstraint (mkSelector "sizeConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector
pixelsHighSelector = mkSelector "pixelsHigh"

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @pixelFormatType@
pixelFormatTypeSelector :: Selector
pixelFormatTypeSelector = mkSelector "pixelFormatType"

-- | @Selector@ for @sizeConstraint@
sizeConstraintSelector :: Selector
sizeConstraintSelector = mkSelector "sizeConstraint"

