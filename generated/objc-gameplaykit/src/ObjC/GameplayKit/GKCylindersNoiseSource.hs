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
  , initWithFrequencySelector
  , frequencySelector
  , setFrequencySelector


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

-- | @+ cylindersNoiseWithFrequency:@
cylindersNoiseWithFrequency :: CDouble -> IO (Id GKCylindersNoiseSource)
cylindersNoiseWithFrequency frequency =
  do
    cls' <- getRequiredClass "GKCylindersNoiseSource"
    sendClassMsg cls' (mkSelector "cylindersNoiseWithFrequency:") (retPtr retVoid) [argCDouble (fromIntegral frequency)] >>= retainedObject . castPtr

-- | @- initWithFrequency:@
initWithFrequency :: IsGKCylindersNoiseSource gkCylindersNoiseSource => gkCylindersNoiseSource -> CDouble -> IO (Id GKCylindersNoiseSource)
initWithFrequency gkCylindersNoiseSource  frequency =
  sendMsg gkCylindersNoiseSource (mkSelector "initWithFrequency:") (retPtr retVoid) [argCDouble (fromIntegral frequency)] >>= ownedObject . castPtr

-- | @- frequency@
frequency :: IsGKCylindersNoiseSource gkCylindersNoiseSource => gkCylindersNoiseSource -> IO CDouble
frequency gkCylindersNoiseSource  =
  sendMsg gkCylindersNoiseSource (mkSelector "frequency") retCDouble []

-- | @- setFrequency:@
setFrequency :: IsGKCylindersNoiseSource gkCylindersNoiseSource => gkCylindersNoiseSource -> CDouble -> IO ()
setFrequency gkCylindersNoiseSource  value =
  sendMsg gkCylindersNoiseSource (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cylindersNoiseWithFrequency:@
cylindersNoiseWithFrequencySelector :: Selector
cylindersNoiseWithFrequencySelector = mkSelector "cylindersNoiseWithFrequency:"

-- | @Selector@ for @initWithFrequency:@
initWithFrequencySelector :: Selector
initWithFrequencySelector = mkSelector "initWithFrequency:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

