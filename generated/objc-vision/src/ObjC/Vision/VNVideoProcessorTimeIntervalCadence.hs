{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that defines a time-based cadence for processing the video stream.
--
-- Generated bindings for @VNVideoProcessorTimeIntervalCadence@.
module ObjC.Vision.VNVideoProcessorTimeIntervalCadence
  ( VNVideoProcessorTimeIntervalCadence
  , IsVNVideoProcessorTimeIntervalCadence(..)
  , init_
  , initWithTimeInterval
  , timeInterval
  , initSelector
  , initWithTimeIntervalSelector
  , timeIntervalSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNVideoProcessorTimeIntervalCadence vnVideoProcessorTimeIntervalCadence => vnVideoProcessorTimeIntervalCadence -> IO (Id VNVideoProcessorTimeIntervalCadence)
init_ vnVideoProcessorTimeIntervalCadence  =
  sendMsg vnVideoProcessorTimeIntervalCadence (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTimeInterval:@
initWithTimeInterval :: IsVNVideoProcessorTimeIntervalCadence vnVideoProcessorTimeIntervalCadence => vnVideoProcessorTimeIntervalCadence -> CDouble -> IO (Id VNVideoProcessorTimeIntervalCadence)
initWithTimeInterval vnVideoProcessorTimeIntervalCadence  timeInterval =
  sendMsg vnVideoProcessorTimeIntervalCadence (mkSelector "initWithTimeInterval:") (retPtr retVoid) [argCDouble (fromIntegral timeInterval)] >>= ownedObject . castPtr

-- | @- timeInterval@
timeInterval :: IsVNVideoProcessorTimeIntervalCadence vnVideoProcessorTimeIntervalCadence => vnVideoProcessorTimeIntervalCadence -> IO CDouble
timeInterval vnVideoProcessorTimeIntervalCadence  =
  sendMsg vnVideoProcessorTimeIntervalCadence (mkSelector "timeInterval") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTimeInterval:@
initWithTimeIntervalSelector :: Selector
initWithTimeIntervalSelector = mkSelector "initWithTimeInterval:"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector
timeIntervalSelector = mkSelector "timeInterval"

