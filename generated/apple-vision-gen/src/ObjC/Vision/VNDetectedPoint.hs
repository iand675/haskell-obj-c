{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNDetectedPoint
--
-- VNDetectedPoint is a VNPoint with a confidence value.
--
-- It should be noted that VNDetectedPoint is not intended as an overall replacement of CGPoint, NSPoint or vec2, but is used by observations that detect points of interest.
--
-- Generated bindings for @VNDetectedPoint@.
module ObjC.Vision.VNDetectedPoint
  ( VNDetectedPoint
  , IsVNDetectedPoint(..)
  , new
  , init_
  , initWithX_y
  , confidence
  , confidenceSelector
  , initSelector
  , initWithX_ySelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNDetectedPoint)
new  =
  do
    cls' <- getRequiredClass "VNDetectedPoint"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNDetectedPoint vnDetectedPoint => vnDetectedPoint -> IO (Id VNDetectedPoint)
init_ vnDetectedPoint =
  sendOwnedMessage vnDetectedPoint initSelector

-- | @- initWithX:y:@
initWithX_y :: IsVNDetectedPoint vnDetectedPoint => vnDetectedPoint -> CDouble -> CDouble -> IO (Id VNDetectedPoint)
initWithX_y vnDetectedPoint x y =
  sendOwnedMessage vnDetectedPoint initWithX_ySelector x y

-- | The confidence in the accuracy of the location, in the range of [0.0, 1.0].
--
-- ObjC selector: @- confidence@
confidence :: IsVNDetectedPoint vnDetectedPoint => vnDetectedPoint -> IO CFloat
confidence vnDetectedPoint =
  sendMessage vnDetectedPoint confidenceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNDetectedPoint)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNDetectedPoint)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithX:y:@
initWithX_ySelector :: Selector '[CDouble, CDouble] (Id VNDetectedPoint)
initWithX_ySelector = mkSelector "initWithX:y:"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CFloat
confidenceSelector = mkSelector "confidence"

