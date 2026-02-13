{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitFrequency@.
module ObjC.Foundation.NSUnitFrequency
  ( NSUnitFrequency
  , IsNSUnitFrequency(..)
  , terahertz
  , gigahertz
  , megahertz
  , kilohertz
  , hertz
  , millihertz
  , microhertz
  , nanohertz
  , framesPerSecond
  , framesPerSecondSelector
  , gigahertzSelector
  , hertzSelector
  , kilohertzSelector
  , megahertzSelector
  , microhertzSelector
  , millihertzSelector
  , nanohertzSelector
  , terahertzSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ terahertz@
terahertz :: IO (Id NSUnitFrequency)
terahertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' terahertzSelector

-- | @+ gigahertz@
gigahertz :: IO (Id NSUnitFrequency)
gigahertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' gigahertzSelector

-- | @+ megahertz@
megahertz :: IO (Id NSUnitFrequency)
megahertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' megahertzSelector

-- | @+ kilohertz@
kilohertz :: IO (Id NSUnitFrequency)
kilohertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' kilohertzSelector

-- | @+ hertz@
hertz :: IO (Id NSUnitFrequency)
hertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' hertzSelector

-- | @+ millihertz@
millihertz :: IO (Id NSUnitFrequency)
millihertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' millihertzSelector

-- | @+ microhertz@
microhertz :: IO (Id NSUnitFrequency)
microhertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' microhertzSelector

-- | @+ nanohertz@
nanohertz :: IO (Id NSUnitFrequency)
nanohertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' nanohertzSelector

-- | @+ framesPerSecond@
framesPerSecond :: IO (Id NSUnitFrequency)
framesPerSecond  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMessage cls' framesPerSecondSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @terahertz@
terahertzSelector :: Selector '[] (Id NSUnitFrequency)
terahertzSelector = mkSelector "terahertz"

-- | @Selector@ for @gigahertz@
gigahertzSelector :: Selector '[] (Id NSUnitFrequency)
gigahertzSelector = mkSelector "gigahertz"

-- | @Selector@ for @megahertz@
megahertzSelector :: Selector '[] (Id NSUnitFrequency)
megahertzSelector = mkSelector "megahertz"

-- | @Selector@ for @kilohertz@
kilohertzSelector :: Selector '[] (Id NSUnitFrequency)
kilohertzSelector = mkSelector "kilohertz"

-- | @Selector@ for @hertz@
hertzSelector :: Selector '[] (Id NSUnitFrequency)
hertzSelector = mkSelector "hertz"

-- | @Selector@ for @millihertz@
millihertzSelector :: Selector '[] (Id NSUnitFrequency)
millihertzSelector = mkSelector "millihertz"

-- | @Selector@ for @microhertz@
microhertzSelector :: Selector '[] (Id NSUnitFrequency)
microhertzSelector = mkSelector "microhertz"

-- | @Selector@ for @nanohertz@
nanohertzSelector :: Selector '[] (Id NSUnitFrequency)
nanohertzSelector = mkSelector "nanohertz"

-- | @Selector@ for @framesPerSecond@
framesPerSecondSelector :: Selector '[] (Id NSUnitFrequency)
framesPerSecondSelector = mkSelector "framesPerSecond"

