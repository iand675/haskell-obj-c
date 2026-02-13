{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about one of potentially multiple data sources associated with a port.
--
-- Generated bindings for @AVAudioSessionDataSourceDescription@.
module ObjC.AVFAudio.AVAudioSessionDataSourceDescription
  ( AVAudioSessionDataSourceDescription
  , IsAVAudioSessionDataSourceDescription(..)
  , setPreferredPolarPattern_error
  , dataSourceID
  , dataSourceName
  , location
  , orientation
  , supportedPolarPatterns
  , selectedPolarPattern
  , preferredPolarPattern
  , dataSourceIDSelector
  , dataSourceNameSelector
  , locationSelector
  , orientationSelector
  , preferredPolarPatternSelector
  , selectedPolarPatternSelector
  , setPreferredPolarPattern_errorSelector
  , supportedPolarPatternsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Select the desired polar pattern from the set of available patterns. Setting a nil value	will clear the preference.
--
-- Note: If the owning port and data source are part of the active audio route, changing the polar	pattern will likely result in a route reconfiguration. If the owning port and data source are	not part of the active route, selecting a polar pattern will not result in an immediate route	reconfiguration.  Use AVAudioSession's setPreferredInput:error: method to activate the port. Use	setPreferredDataSource:error: to active the data source on the port.	You must call setPreferredInputOrientation:error: on the AVAudioSession if you chose the	AVAudioSessionPolarPatternStereo polar pattern.
--
-- ObjC selector: @- setPreferredPolarPattern:error:@
setPreferredPolarPattern_error :: (IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription, IsNSString pattern_, IsNSError outError) => avAudioSessionDataSourceDescription -> pattern_ -> outError -> IO Bool
setPreferredPolarPattern_error avAudioSessionDataSourceDescription pattern_ outError =
  sendMessage avAudioSessionDataSourceDescription setPreferredPolarPattern_errorSelector (toNSString pattern_) (toNSError outError)

-- | System-assigned ID for the data source.
--
-- ObjC selector: @- dataSourceID@
dataSourceID :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSNumber)
dataSourceID avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription dataSourceIDSelector

-- | Human-readable name for the data source.
--
-- ObjC selector: @- dataSourceName@
dataSourceName :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
dataSourceName avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription dataSourceNameSelector

-- | Describes the general location of a data source. Will be nil for data sources for which the location is not known.
--
-- ObjC selector: @- location@
location :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
location avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription locationSelector

-- | Describes the orientation of a data source.  Will be nil for data sources for which the orientation is not known.
--
-- ObjC selector: @- orientation@
orientation :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
orientation avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription orientationSelector

-- | Array of one or more AVAudioSessionPolarPatterns describing the supported polar patterns for a data source.  Will be nil for data sources that have no selectable patterns.
--
-- ObjC selector: @- supportedPolarPatterns@
supportedPolarPatterns :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSArray)
supportedPolarPatterns avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription supportedPolarPatternsSelector

-- | Describes the currently selected polar pattern.  Will be nil for data sources that have no selectable patterns.
--
-- ObjC selector: @- selectedPolarPattern@
selectedPolarPattern :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
selectedPolarPattern avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription selectedPolarPatternSelector

-- | Describes the preferred polar pattern.  Will be nil for data sources that have no selectable patterns or if no preference has been set.
--
-- ObjC selector: @- preferredPolarPattern@
preferredPolarPattern :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
preferredPolarPattern avAudioSessionDataSourceDescription =
  sendMessage avAudioSessionDataSourceDescription preferredPolarPatternSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPreferredPolarPattern:error:@
setPreferredPolarPattern_errorSelector :: Selector '[Id NSString, Id NSError] Bool
setPreferredPolarPattern_errorSelector = mkSelector "setPreferredPolarPattern:error:"

-- | @Selector@ for @dataSourceID@
dataSourceIDSelector :: Selector '[] (Id NSNumber)
dataSourceIDSelector = mkSelector "dataSourceID"

-- | @Selector@ for @dataSourceName@
dataSourceNameSelector :: Selector '[] (Id NSString)
dataSourceNameSelector = mkSelector "dataSourceName"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id NSString)
locationSelector = mkSelector "location"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] (Id NSString)
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @supportedPolarPatterns@
supportedPolarPatternsSelector :: Selector '[] (Id NSArray)
supportedPolarPatternsSelector = mkSelector "supportedPolarPatterns"

-- | @Selector@ for @selectedPolarPattern@
selectedPolarPatternSelector :: Selector '[] (Id NSString)
selectedPolarPatternSelector = mkSelector "selectedPolarPattern"

-- | @Selector@ for @preferredPolarPattern@
preferredPolarPatternSelector :: Selector '[] (Id NSString)
preferredPolarPatternSelector = mkSelector "preferredPolarPattern"

