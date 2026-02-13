{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Produces 3D cylindrical noise with an infinite number of cylinders-within-cyliners of constantly-increasing radius.
--
-- Generated bindings for @GKCylindersNoiseSource@.
module ObjC.GameplayKit.GKCylindersNoiseSource
  ( GKCylindersNoiseSource
  , IsGKCylindersNoiseSource(..)
  , cylindersNoiseWithFrequency
  , initWithFrequency
  , frequency
  , setFrequency
  , cylindersNoiseWithFrequencySelector
  , frequencySelector
  , initWithFrequencySelector
  , setFrequencySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ cylindersNoiseWithFrequency:@
cylindersNoiseWithFrequency :: CDouble -> IO (Id GKCylindersNoiseSource)
cylindersNoiseWithFrequency frequency =
  do
    cls' <- getRequiredClass "GKCylindersNoiseSource"
    sendClassMessage cls' cylindersNoiseWithFrequencySelector frequency

-- | @- initWithFrequency:@
initWithFrequency :: IsGKCylindersNoiseSource gkCylindersNoiseSource => gkCylindersNoiseSource -> CDouble -> IO (Id GKCylindersNoiseSource)
initWithFrequency gkCylindersNoiseSource frequency =
  sendOwnedMessage gkCylindersNoiseSource initWithFrequencySelector frequency

-- | @- frequency@
frequency :: IsGKCylindersNoiseSource gkCylindersNoiseSource => gkCylindersNoiseSource -> IO CDouble
frequency gkCylindersNoiseSource =
  sendMessage gkCylindersNoiseSource frequencySelector

-- | @- setFrequency:@
setFrequency :: IsGKCylindersNoiseSource gkCylindersNoiseSource => gkCylindersNoiseSource -> CDouble -> IO ()
setFrequency gkCylindersNoiseSource value =
  sendMessage gkCylindersNoiseSource setFrequencySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cylindersNoiseWithFrequency:@
cylindersNoiseWithFrequencySelector :: Selector '[CDouble] (Id GKCylindersNoiseSource)
cylindersNoiseWithFrequencySelector = mkSelector "cylindersNoiseWithFrequency:"

-- | @Selector@ for @initWithFrequency:@
initWithFrequencySelector :: Selector '[CDouble] (Id GKCylindersNoiseSource)
initWithFrequencySelector = mkSelector "initWithFrequency:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

