{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAltitudeData@.
module ObjC.CoreMotion.CMAltitudeData
  ( CMAltitudeData
  , IsCMAltitudeData(..)
  , relativeAltitude
  , pressure
  , relativeAltitudeSelector
  , pressureSelector


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

-- | @- relativeAltitude@
relativeAltitude :: IsCMAltitudeData cmAltitudeData => cmAltitudeData -> IO (Id NSNumber)
relativeAltitude cmAltitudeData  =
  sendMsg cmAltitudeData (mkSelector "relativeAltitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pressure@
pressure :: IsCMAltitudeData cmAltitudeData => cmAltitudeData -> IO (Id NSNumber)
pressure cmAltitudeData  =
  sendMsg cmAltitudeData (mkSelector "pressure") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @relativeAltitude@
relativeAltitudeSelector :: Selector
relativeAltitudeSelector = mkSelector "relativeAltitude"

-- | @Selector@ for @pressure@
pressureSelector :: Selector
pressureSelector = mkSelector "pressure"

