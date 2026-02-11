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

-- | @- initWithSubbandParameters:@
initWithSubbandParameters :: (IsPHASEConeDirectivityModelParameters phaseConeDirectivityModelParameters, IsNSArray subbandParameters) => phaseConeDirectivityModelParameters -> subbandParameters -> IO (Id PHASEConeDirectivityModelParameters)
initWithSubbandParameters phaseConeDirectivityModelParameters  subbandParameters =
withObjCPtr subbandParameters $ \raw_subbandParameters ->
    sendMsg phaseConeDirectivityModelParameters (mkSelector "initWithSubbandParameters:") (retPtr retVoid) [argPtr (castPtr raw_subbandParameters :: Ptr ())] >>= ownedObject . castPtr

-- | subbandParameters
--
-- An array of subband parameters.
--
-- ObjC selector: @- subbandParameters@
subbandParameters :: IsPHASEConeDirectivityModelParameters phaseConeDirectivityModelParameters => phaseConeDirectivityModelParameters -> IO (Id NSArray)
subbandParameters phaseConeDirectivityModelParameters  =
  sendMsg phaseConeDirectivityModelParameters (mkSelector "subbandParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSubbandParameters:@
initWithSubbandParametersSelector :: Selector
initWithSubbandParametersSelector = mkSelector "initWithSubbandParameters:"

-- | @Selector@ for @subbandParameters@
subbandParametersSelector :: Selector
subbandParametersSelector = mkSelector "subbandParameters"

