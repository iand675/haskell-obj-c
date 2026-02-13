{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNVideoProcessorTimeIntervalCadence vnVideoProcessorTimeIntervalCadence => vnVideoProcessorTimeIntervalCadence -> IO (Id VNVideoProcessorTimeIntervalCadence)
init_ vnVideoProcessorTimeIntervalCadence =
  sendOwnedMessage vnVideoProcessorTimeIntervalCadence initSelector

-- | @- initWithTimeInterval:@
initWithTimeInterval :: IsVNVideoProcessorTimeIntervalCadence vnVideoProcessorTimeIntervalCadence => vnVideoProcessorTimeIntervalCadence -> CDouble -> IO (Id VNVideoProcessorTimeIntervalCadence)
initWithTimeInterval vnVideoProcessorTimeIntervalCadence timeInterval =
  sendOwnedMessage vnVideoProcessorTimeIntervalCadence initWithTimeIntervalSelector timeInterval

-- | @- timeInterval@
timeInterval :: IsVNVideoProcessorTimeIntervalCadence vnVideoProcessorTimeIntervalCadence => vnVideoProcessorTimeIntervalCadence -> IO CDouble
timeInterval vnVideoProcessorTimeIntervalCadence =
  sendMessage vnVideoProcessorTimeIntervalCadence timeIntervalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNVideoProcessorTimeIntervalCadence)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTimeInterval:@
initWithTimeIntervalSelector :: Selector '[CDouble] (Id VNVideoProcessorTimeIntervalCadence)
initWithTimeIntervalSelector = mkSelector "initWithTimeInterval:"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector '[] CDouble
timeIntervalSelector = mkSelector "timeInterval"

