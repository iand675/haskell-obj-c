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
  , newSelector
  , initSelector
  , initWithX_ySelector
  , confidenceSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNDetectedPoint)
new  =
  do
    cls' <- getRequiredClass "VNDetectedPoint"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNDetectedPoint vnDetectedPoint => vnDetectedPoint -> IO (Id VNDetectedPoint)
init_ vnDetectedPoint  =
  sendMsg vnDetectedPoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithX:y:@
initWithX_y :: IsVNDetectedPoint vnDetectedPoint => vnDetectedPoint -> CDouble -> CDouble -> IO (Id VNDetectedPoint)
initWithX_y vnDetectedPoint  x y =
  sendMsg vnDetectedPoint (mkSelector "initWithX:y:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y)] >>= ownedObject . castPtr

-- | The confidence in the accuracy of the location, in the range of [0.0, 1.0].
--
-- ObjC selector: @- confidence@
confidence :: IsVNDetectedPoint vnDetectedPoint => vnDetectedPoint -> IO CFloat
confidence vnDetectedPoint  =
  sendMsg vnDetectedPoint (mkSelector "confidence") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithX:y:@
initWithX_ySelector :: Selector
initWithX_ySelector = mkSelector "initWithX:y:"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

