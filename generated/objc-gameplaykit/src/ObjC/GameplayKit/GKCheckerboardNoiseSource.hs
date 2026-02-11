{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Produces noise in a checkerboard pattern.
--
-- Generated bindings for @GKCheckerboardNoiseSource@.
module ObjC.GameplayKit.GKCheckerboardNoiseSource
  ( GKCheckerboardNoiseSource
  , IsGKCheckerboardNoiseSource(..)
  , checkerboardNoiseWithSquareSize
  , initWithSquareSize
  , squareSize
  , setSquareSize
  , checkerboardNoiseWithSquareSizeSelector
  , initWithSquareSizeSelector
  , squareSizeSelector
  , setSquareSizeSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ checkerboardNoiseWithSquareSize:@
checkerboardNoiseWithSquareSize :: CDouble -> IO (Id GKCheckerboardNoiseSource)
checkerboardNoiseWithSquareSize squareSize =
  do
    cls' <- getRequiredClass "GKCheckerboardNoiseSource"
    sendClassMsg cls' (mkSelector "checkerboardNoiseWithSquareSize:") (retPtr retVoid) [argCDouble (fromIntegral squareSize)] >>= retainedObject . castPtr

-- | @- initWithSquareSize:@
initWithSquareSize :: IsGKCheckerboardNoiseSource gkCheckerboardNoiseSource => gkCheckerboardNoiseSource -> CDouble -> IO (Id GKCheckerboardNoiseSource)
initWithSquareSize gkCheckerboardNoiseSource  squareSize =
  sendMsg gkCheckerboardNoiseSource (mkSelector "initWithSquareSize:") (retPtr retVoid) [argCDouble (fromIntegral squareSize)] >>= ownedObject . castPtr

-- | @- squareSize@
squareSize :: IsGKCheckerboardNoiseSource gkCheckerboardNoiseSource => gkCheckerboardNoiseSource -> IO CDouble
squareSize gkCheckerboardNoiseSource  =
  sendMsg gkCheckerboardNoiseSource (mkSelector "squareSize") retCDouble []

-- | @- setSquareSize:@
setSquareSize :: IsGKCheckerboardNoiseSource gkCheckerboardNoiseSource => gkCheckerboardNoiseSource -> CDouble -> IO ()
setSquareSize gkCheckerboardNoiseSource  value =
  sendMsg gkCheckerboardNoiseSource (mkSelector "setSquareSize:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkerboardNoiseWithSquareSize:@
checkerboardNoiseWithSquareSizeSelector :: Selector
checkerboardNoiseWithSquareSizeSelector = mkSelector "checkerboardNoiseWithSquareSize:"

-- | @Selector@ for @initWithSquareSize:@
initWithSquareSizeSelector :: Selector
initWithSquareSizeSelector = mkSelector "initWithSquareSize:"

-- | @Selector@ for @squareSize@
squareSizeSelector :: Selector
squareSizeSelector = mkSelector "squareSize"

-- | @Selector@ for @setSquareSize:@
setSquareSizeSelector :: Selector
setSquareSizeSelector = mkSelector "setSquareSize:"

