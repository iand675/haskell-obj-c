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
  , initSelector
  , newSelector
  , clockNameSelector
  , sourceIdentifierSelector
  , numberOfEventsForRateSmoothingSelector
  , numberOfAveragesForRateSmoothingSelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO (Id CMIOExtensionStreamCustomClockConfiguration)
init_ cmioExtensionStreamCustomClockConfiguration  =
  sendMsg cmioExtensionStreamCustomClockConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionStreamCustomClockConfiguration)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamCustomClockConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | clockName
--
-- The name of the clock.
--
-- ObjC selector: @- clockName@
clockName :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO (Id NSString)
clockName cmioExtensionStreamCustomClockConfiguration  =
  sendMsg cmioExtensionStreamCustomClockConfiguration (mkSelector "clockName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceIdentifier
--
-- The identifier of the entity driving the clock.
--
-- An unique identifier that is used to indicate the entity that is driving the clock. This value is used internally to determine if two custom clocks have the same hardware source, and thus determine whether or not they will drift relative to one another. This parameter is used in the following way: if a device supports multiple active streams that are internally clocked by a common source, then instead of sharing one clock between each stream, a clock per stream can be configured with the sourceIdentifier for each clock set to be the same value.
--
-- ObjC selector: @- sourceIdentifier@
sourceIdentifier :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO (Id NSUUID)
sourceIdentifier cmioExtensionStreamCustomClockConfiguration  =
  sendMsg cmioExtensionStreamCustomClockConfiguration (mkSelector "sourceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | numberOfEventsForRateSmoothing
--
-- The number of events to use for rate smoothing; will be > 0.
--
-- ObjC selector: @- numberOfEventsForRateSmoothing@
numberOfEventsForRateSmoothing :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO CUInt
numberOfEventsForRateSmoothing cmioExtensionStreamCustomClockConfiguration  =
  sendMsg cmioExtensionStreamCustomClockConfiguration (mkSelector "numberOfEventsForRateSmoothing") retCUInt []

-- | numberOfAveragesForRateSmoothing
--
-- The number of averages used for rate smoothing; 0 indicates that the default smoothing algorithm is used.
--
-- ObjC selector: @- numberOfAveragesForRateSmoothing@
numberOfAveragesForRateSmoothing :: IsCMIOExtensionStreamCustomClockConfiguration cmioExtensionStreamCustomClockConfiguration => cmioExtensionStreamCustomClockConfiguration -> IO CUInt
numberOfAveragesForRateSmoothing cmioExtensionStreamCustomClockConfiguration  =
  sendMsg cmioExtensionStreamCustomClockConfiguration (mkSelector "numberOfAveragesForRateSmoothing") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @clockName@
clockNameSelector :: Selector
clockNameSelector = mkSelector "clockName"

-- | @Selector@ for @sourceIdentifier@
sourceIdentifierSelector :: Selector
sourceIdentifierSelector = mkSelector "sourceIdentifier"

-- | @Selector@ for @numberOfEventsForRateSmoothing@
numberOfEventsForRateSmoothingSelector :: Selector
numberOfEventsForRateSmoothingSelector = mkSelector "numberOfEventsForRateSmoothing"

-- | @Selector@ for @numberOfAveragesForRateSmoothing@
numberOfAveragesForRateSmoothingSelector :: Selector
numberOfAveragesForRateSmoothingSelector = mkSelector "numberOfAveragesForRateSmoothing"

