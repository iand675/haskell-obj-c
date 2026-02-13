{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEConeDirectivityModelParameters
--
-- Cone directivity model parameters.
--
-- Generated bindings for @PHASEConeDirectivityModelParameters@.
module ObjC.PHASE.PHASEConeDirectivityModelParameters
  ( PHASEConeDirectivityModelParameters
  , IsPHASEConeDirectivityModelParameters(..)
  , initWithSubbandParameters
  , subbandParameters
  , initWithSubbandParametersSelector
  , subbandParametersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSubbandParameters:@
initWithSubbandParameters :: (IsPHASEConeDirectivityModelParameters phaseConeDirectivityModelParameters, IsNSArray subbandParameters) => phaseConeDirectivityModelParameters -> subbandParameters -> IO (Id PHASEConeDirectivityModelParameters)
initWithSubbandParameters phaseConeDirectivityModelParameters subbandParameters =
  sendOwnedMessage phaseConeDirectivityModelParameters initWithSubbandParametersSelector (toNSArray subbandParameters)

-- | subbandParameters
--
-- An array of subband parameters.
--
-- ObjC selector: @- subbandParameters@
subbandParameters :: IsPHASEConeDirectivityModelParameters phaseConeDirectivityModelParameters => phaseConeDirectivityModelParameters -> IO (Id NSArray)
subbandParameters phaseConeDirectivityModelParameters =
  sendMessage phaseConeDirectivityModelParameters subbandParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSubbandParameters:@
initWithSubbandParametersSelector :: Selector '[Id NSArray] (Id PHASEConeDirectivityModelParameters)
initWithSubbandParametersSelector = mkSelector "initWithSubbandParameters:"

-- | @Selector@ for @subbandParameters@
subbandParametersSelector :: Selector '[] (Id NSArray)
subbandParametersSelector = mkSelector "subbandParameters"

