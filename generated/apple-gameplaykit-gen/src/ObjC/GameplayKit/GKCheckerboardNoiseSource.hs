{-# LANGUAGE DataKinds #-}
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
  , setSquareSizeSelector
  , squareSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ checkerboardNoiseWithSquareSize:@
checkerboardNoiseWithSquareSize :: CDouble -> IO (Id GKCheckerboardNoiseSource)
checkerboardNoiseWithSquareSize squareSize =
  do
    cls' <- getRequiredClass "GKCheckerboardNoiseSource"
    sendClassMessage cls' checkerboardNoiseWithSquareSizeSelector squareSize

-- | @- initWithSquareSize:@
initWithSquareSize :: IsGKCheckerboardNoiseSource gkCheckerboardNoiseSource => gkCheckerboardNoiseSource -> CDouble -> IO (Id GKCheckerboardNoiseSource)
initWithSquareSize gkCheckerboardNoiseSource squareSize =
  sendOwnedMessage gkCheckerboardNoiseSource initWithSquareSizeSelector squareSize

-- | @- squareSize@
squareSize :: IsGKCheckerboardNoiseSource gkCheckerboardNoiseSource => gkCheckerboardNoiseSource -> IO CDouble
squareSize gkCheckerboardNoiseSource =
  sendMessage gkCheckerboardNoiseSource squareSizeSelector

-- | @- setSquareSize:@
setSquareSize :: IsGKCheckerboardNoiseSource gkCheckerboardNoiseSource => gkCheckerboardNoiseSource -> CDouble -> IO ()
setSquareSize gkCheckerboardNoiseSource value =
  sendMessage gkCheckerboardNoiseSource setSquareSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkerboardNoiseWithSquareSize:@
checkerboardNoiseWithSquareSizeSelector :: Selector '[CDouble] (Id GKCheckerboardNoiseSource)
checkerboardNoiseWithSquareSizeSelector = mkSelector "checkerboardNoiseWithSquareSize:"

-- | @Selector@ for @initWithSquareSize:@
initWithSquareSizeSelector :: Selector '[CDouble] (Id GKCheckerboardNoiseSource)
initWithSquareSizeSelector = mkSelector "initWithSquareSize:"

-- | @Selector@ for @squareSize@
squareSizeSelector :: Selector '[] CDouble
squareSizeSelector = mkSelector "squareSize"

-- | @Selector@ for @setSquareSize:@
setSquareSizeSelector :: Selector '[CDouble] ()
setSquareSizeSelector = mkSelector "setSquareSize:"

