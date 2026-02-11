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
  , terahertzSelector
  , gigahertzSelector
  , megahertzSelector
  , kilohertzSelector
  , hertzSelector
  , millihertzSelector
  , microhertzSelector
  , nanohertzSelector
  , framesPerSecondSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ terahertz@
terahertz :: IO (Id NSUnitFrequency)
terahertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "terahertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gigahertz@
gigahertz :: IO (Id NSUnitFrequency)
gigahertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "gigahertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ megahertz@
megahertz :: IO (Id NSUnitFrequency)
megahertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "megahertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilohertz@
kilohertz :: IO (Id NSUnitFrequency)
kilohertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "kilohertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hertz@
hertz :: IO (Id NSUnitFrequency)
hertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "hertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ millihertz@
millihertz :: IO (Id NSUnitFrequency)
millihertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "millihertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microhertz@
microhertz :: IO (Id NSUnitFrequency)
microhertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "microhertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nanohertz@
nanohertz :: IO (Id NSUnitFrequency)
nanohertz  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "nanohertz") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ framesPerSecond@
framesPerSecond :: IO (Id NSUnitFrequency)
framesPerSecond  =
  do
    cls' <- getRequiredClass "NSUnitFrequency"
    sendClassMsg cls' (mkSelector "framesPerSecond") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @terahertz@
terahertzSelector :: Selector
terahertzSelector = mkSelector "terahertz"

-- | @Selector@ for @gigahertz@
gigahertzSelector :: Selector
gigahertzSelector = mkSelector "gigahertz"

-- | @Selector@ for @megahertz@
megahertzSelector :: Selector
megahertzSelector = mkSelector "megahertz"

-- | @Selector@ for @kilohertz@
kilohertzSelector :: Selector
kilohertzSelector = mkSelector "kilohertz"

-- | @Selector@ for @hertz@
hertzSelector :: Selector
hertzSelector = mkSelector "hertz"

-- | @Selector@ for @millihertz@
millihertzSelector :: Selector
millihertzSelector = mkSelector "millihertz"

-- | @Selector@ for @microhertz@
microhertzSelector :: Selector
microhertzSelector = mkSelector "microhertz"

-- | @Selector@ for @nanohertz@
nanohertzSelector :: Selector
nanohertzSelector = mkSelector "nanohertz"

-- | @Selector@ for @framesPerSecond@
framesPerSecondSelector :: Selector
framesPerSecondSelector = mkSelector "framesPerSecond"

