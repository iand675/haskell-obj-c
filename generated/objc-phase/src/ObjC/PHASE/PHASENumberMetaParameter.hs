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
  , initSelector
  , newSelector
  , fadeToValue_durationSelector
  , minimumSelector
  , maximumSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> IO (Id PHASENumberMetaParameter)
init_ phaseNumberMetaParameter  =
  sendMsg phaseNumberMetaParameter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASENumberMetaParameter)
new  =
  do
    cls' <- getRequiredClass "PHASENumberMetaParameter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
fadeToValue_duration phaseNumberMetaParameter  value duration =
  sendMsg phaseNumberMetaParameter (mkSelector "fadeToValue:duration:") retVoid [argCDouble (fromIntegral value), argCDouble (fromIntegral duration)]

-- | minimum
--
-- The minimum value this metaparameter can be set to
--
-- ObjC selector: @- minimum@
minimum_ :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> IO CDouble
minimum_ phaseNumberMetaParameter  =
  sendMsg phaseNumberMetaParameter (mkSelector "minimum") retCDouble []

-- | maximum
--
-- The maximum value this metaparameter can be set to
--
-- ObjC selector: @- maximum@
maximum_ :: IsPHASENumberMetaParameter phaseNumberMetaParameter => phaseNumberMetaParameter -> IO CDouble
maximum_ phaseNumberMetaParameter  =
  sendMsg phaseNumberMetaParameter (mkSelector "maximum") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fadeToValue:duration:@
fadeToValue_durationSelector :: Selector
fadeToValue_durationSelector = mkSelector "fadeToValue:duration:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @maximum@
maximumSelector :: Selector
maximumSelector = mkSelector "maximum"

