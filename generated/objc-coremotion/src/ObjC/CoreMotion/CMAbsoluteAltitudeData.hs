{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAbsoluteAltitudeData@.
module ObjC.CoreMotion.CMAbsoluteAltitudeData
  ( CMAbsoluteAltitudeData
  , IsCMAbsoluteAltitudeData(..)
  , altitude
  , accuracy
  , precision
  , altitudeSelector
  , accuracySelector
  , precisionSelector


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

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- altitude@
altitude :: IsCMAbsoluteAltitudeData cmAbsoluteAltitudeData => cmAbsoluteAltitudeData -> IO CDouble
altitude cmAbsoluteAltitudeData  =
  sendMsg cmAbsoluteAltitudeData (mkSelector "altitude") retCDouble []

-- | @- accuracy@
accuracy :: IsCMAbsoluteAltitudeData cmAbsoluteAltitudeData => cmAbsoluteAltitudeData -> IO CDouble
accuracy cmAbsoluteAltitudeData  =
  sendMsg cmAbsoluteAltitudeData (mkSelector "accuracy") retCDouble []

-- | @- precision@
precision :: IsCMAbsoluteAltitudeData cmAbsoluteAltitudeData => cmAbsoluteAltitudeData -> IO CDouble
precision cmAbsoluteAltitudeData  =
  sendMsg cmAbsoluteAltitudeData (mkSelector "precision") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @accuracy@
accuracySelector :: Selector
accuracySelector = mkSelector "accuracy"

-- | @Selector@ for @precision@
precisionSelector :: Selector
precisionSelector = mkSelector "precision"

