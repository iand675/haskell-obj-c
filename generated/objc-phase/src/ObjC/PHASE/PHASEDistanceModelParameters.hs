{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDistanceModelParameters
--
-- Distance model parameters.
--
-- Generated bindings for @PHASEDistanceModelParameters@.
module ObjC.PHASE.PHASEDistanceModelParameters
  ( PHASEDistanceModelParameters
  , IsPHASEDistanceModelParameters(..)
  , init_
  , new
  , fadeOutParameters
  , setFadeOutParameters
  , initSelector
  , newSelector
  , fadeOutParametersSelector
  , setFadeOutParametersSelector


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
init_ :: IsPHASEDistanceModelParameters phaseDistanceModelParameters => phaseDistanceModelParameters -> IO (Id PHASEDistanceModelParameters)
init_ phaseDistanceModelParameters  =
  sendMsg phaseDistanceModelParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEDistanceModelParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEDistanceModelParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | fadeOutParameters
--
-- Fade out parameters (optional).
--
-- ObjC selector: @- fadeOutParameters@
fadeOutParameters :: IsPHASEDistanceModelParameters phaseDistanceModelParameters => phaseDistanceModelParameters -> IO (Id PHASEDistanceModelFadeOutParameters)
fadeOutParameters phaseDistanceModelParameters  =
  sendMsg phaseDistanceModelParameters (mkSelector "fadeOutParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fadeOutParameters
--
-- Fade out parameters (optional).
--
-- ObjC selector: @- setFadeOutParameters:@
setFadeOutParameters :: (IsPHASEDistanceModelParameters phaseDistanceModelParameters, IsPHASEDistanceModelFadeOutParameters value) => phaseDistanceModelParameters -> value -> IO ()
setFadeOutParameters phaseDistanceModelParameters  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseDistanceModelParameters (mkSelector "setFadeOutParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fadeOutParameters@
fadeOutParametersSelector :: Selector
fadeOutParametersSelector = mkSelector "fadeOutParameters"

-- | @Selector@ for @setFadeOutParameters:@
setFadeOutParametersSelector :: Selector
setFadeOutParametersSelector = mkSelector "setFadeOutParameters:"

