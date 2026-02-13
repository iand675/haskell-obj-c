{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionStreamCustomClockConfiguration
--
-- A CMIOExtensionStreamCustomClockProperties describes the parameters used to create a custom clock on the host side (as opposed to the stream using hosttime or a linked Core Audio clock.
--
-- Generated bindings for @CMIOExtensionStreamCustomClockConfiguration@.
module ObjC.CoreMediaIO.CMIOExtensionStreamCustomClockConfiguration
  ( CMIOExtensionStreamCustomClockConfiguration
  , IsCMIOExtensionStreamCustomClockConfiguration(..)
  , init_
  , new
  , clockName
  , sourceIdentifier
  , numberOfEventsForRateSmoothing
  , numberOfAveragesForRateSmoothing
  , clockNameSelector
  , initSelector
  , newSelector
  , numberOfAveragesForRateSmoothingSelector
  , numberOfEventsForRateSmoothingSelector
  , sourceIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO (Id CMIOExtensionStreamCustomClockConfiguration)
init_ cmioExtensionStreamCustomClockConfiguration =
  sendOwnedMessage cmioExtensionStreamCustomClockConfiguration initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionStreamCustomClockConfiguration)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamCustomClockConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | clockName
--
-- The name of the clock.
--
-- ObjC selector: @- clockName@
clockName :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO (Id NSString)
clockName cmioExtensionStreamCustomClockConfiguration =
  sendMessage cmioExtensionStreamCustomClockConfiguration clockNameSelector

-- | sourceIdentifier
--
-- The identifier of the entity driving the clock.
--
-- An unique identifier that is used to indicate the entity that is driving the clock. This value is used internally to determine if two custom clocks have the same hardware source, and thus determine whether or not they will drift relative to one another. This parameter is used in the following way: if a device supports multiple active streams that are internally clocked by a common source, then instead of sharing one clock between each stream, a clock per stream can be configured with the sourceIdentifier for each clock set to be the same value.
--
-- ObjC selector: @- sourceIdentifier@
sourceIdentifier :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO (Id NSUUID)
sourceIdentifier cmioExtensionStreamCustomClockConfiguration =
  sendMessage cmioExtensionStreamCustomClockConfiguration sourceIdentifierSelector

-- | numberOfEventsForRateSmoothing
--
-- The number of events to use for rate smoothing; will be > 0.
--
-- ObjC selector: @- numberOfEventsForRateSmoothing@
numberOfEventsForRateSmoothing :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO CUInt
numberOfEventsForRateSmoothing cmioExtensionStreamCustomClockConfiguration =
  sendMessage cmioExtensionStreamCustomClockConfiguration numberOfEventsForRateSmoothingSelector

-- | numberOfAveragesForRateSmoothing
--
-- The number of averages used for rate smoothing; 0 indicates that the default smoothing algorithm is used.
--
-- ObjC selector: @- numberOfAveragesForRateSmoothing@
numberOfAveragesForRateSmoothing :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO CUInt
numberOfAveragesForRateSmoothing cmioExtensionStreamCustomClockConfiguration =
  sendMessage cmioExtensionStreamCustomClockConfiguration numberOfAveragesForRateSmoothingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionStreamCustomClockConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionStreamCustomClockConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @clockName@
clockNameSelector :: Selector '[] (Id NSString)
clockNameSelector = mkSelector "clockName"

-- | @Selector@ for @sourceIdentifier@
sourceIdentifierSelector :: Selector '[] (Id NSUUID)
sourceIdentifierSelector = mkSelector "sourceIdentifier"

-- | @Selector@ for @numberOfEventsForRateSmoothing@
numberOfEventsForRateSmoothingSelector :: Selector '[] CUInt
numberOfEventsForRateSmoothingSelector = mkSelector "numberOfEventsForRateSmoothing"

-- | @Selector@ for @numberOfAveragesForRateSmoothing@
numberOfAveragesForRateSmoothingSelector :: Selector '[] CUInt
numberOfAveragesForRateSmoothingSelector = mkSelector "numberOfAveragesForRateSmoothing"

