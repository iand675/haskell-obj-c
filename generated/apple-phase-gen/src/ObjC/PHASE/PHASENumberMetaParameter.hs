{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASENumberMetaParameter
--
-- An object that represents an active numeric metaparameter in the system
--
-- Generated bindings for @PHASENumberMetaParameter@.
module ObjC.PHASE.PHASENumberMetaParameter
  ( PHASENumberMetaParameter
  , IsPHASENumberMetaParameter(..)
  , init_
  , new
  , fadeToValue_duration
  , minimum_
  , maximum_
  , fadeToValue_durationSelector
  , initSelector
  , maximumSelector
  , minimumSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> IO (Id PHASENumberMetaParameter)
init_ phaseNumberMetaParameter =
  sendOwnedMessage phaseNumberMetaParameter initSelector

-- | @+ new@
new :: IO (Id PHASENumberMetaParameter)
new  =
  do
    cls' <- getRequiredClass "PHASENumberMetaParameter"
    sendOwnedClassMessage cls' newSelector

-- | fadeToValue
--
-- Fades to a new value over an interval of time
--
-- @value@ — The new destination value to fade to
--
-- @duration@ — The length of time it takes to arrive at the destination value
--
-- ObjC selector: @- fadeToValue:duration:@
fadeToValue_duration :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> CDouble -> CDouble -> IO ()
fadeToValue_duration phaseNumberMetaParameter value duration =
  sendMessage phaseNumberMetaParameter fadeToValue_durationSelector value duration

-- | minimum
--
-- The minimum value this metaparameter can be set to
--
-- ObjC selector: @- minimum@
minimum_ :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> IO CDouble
minimum_ phaseNumberMetaParameter =
  sendMessage phaseNumberMetaParameter minimumSelector

-- | maximum
--
-- The maximum value this metaparameter can be set to
--
-- ObjC selector: @- maximum@
maximum_ :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> IO CDouble
maximum_ phaseNumberMetaParameter =
  sendMessage phaseNumberMetaParameter maximumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASENumberMetaParameter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASENumberMetaParameter)
newSelector = mkSelector "new"

-- | @Selector@ for @fadeToValue:duration:@
fadeToValue_durationSelector :: Selector '[CDouble, CDouble] ()
fadeToValue_durationSelector = mkSelector "fadeToValue:duration:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector '[] CDouble
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @maximum@
maximumSelector :: Selector '[] CDouble
maximumSelector = mkSelector "maximum"

