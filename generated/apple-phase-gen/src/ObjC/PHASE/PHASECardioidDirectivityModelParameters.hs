{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASECardioidDirectivityModelParameters
--
-- Cardioid directivity model parameters.
--
-- Generated bindings for @PHASECardioidDirectivityModelParameters@.
module ObjC.PHASE.PHASECardioidDirectivityModelParameters
  ( PHASECardioidDirectivityModelParameters
  , IsPHASECardioidDirectivityModelParameters(..)
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
initWithSubbandParameters :: (IsPHASECardioidDirectivityModelParameters phaseCardioidDirectivityModelParameters, IsNSArray subbandParameters) => phaseCardioidDirectivityModelParameters -> subbandParameters -> IO (Id PHASECardioidDirectivityModelParameters)
initWithSubbandParameters phaseCardioidDirectivityModelParameters subbandParameters =
  sendOwnedMessage phaseCardioidDirectivityModelParameters initWithSubbandParametersSelector (toNSArray subbandParameters)

-- | subbandParameters
--
-- An array of subband parameters.
--
-- ObjC selector: @- subbandParameters@
subbandParameters :: IsPHASECardioidDirectivityModelParameters phaseCardioidDirectivityModelParameters => phaseCardioidDirectivityModelParameters -> IO (Id NSArray)
subbandParameters phaseCardioidDirectivityModelParameters =
  sendMessage phaseCardioidDirectivityModelParameters subbandParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSubbandParameters:@
initWithSubbandParametersSelector :: Selector '[Id NSArray] (Id PHASECardioidDirectivityModelParameters)
initWithSubbandParametersSelector = mkSelector "initWithSubbandParameters:"

-- | @Selector@ for @subbandParameters@
subbandParametersSelector :: Selector '[] (Id NSArray)
subbandParametersSelector = mkSelector "subbandParameters"

