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
  , location
  , orientation
  , supportedPolarPatterns
  , selectedPolarPattern
  , preferredPolarPattern
  , setPreferredPolarPattern_errorSelector
  , locationSelector
  , orientationSelector
  , supportedPolarPatternsSelector
  , selectedPolarPatternSelector
  , preferredPolarPatternSelector


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

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Select the desired polar pattern from the set of available patterns. Setting a nil value	will clear the preference.
--
-- Note: If the owning port and data source are part of the active audio route, changing the polar	pattern will likely result in a route reconfiguration. If the owning port and data source are	not part of the active route, selecting a polar pattern will not result in an immediate route	reconfiguration.  Use AVAudioSession's setPreferredInput:error: method to activate the port. Use	setPreferredDataSource:error: to active the data source on the port.	You must call setPreferredInputOrientation:error: on the AVAudioSession if you chose the	AVAudioSessionPolarPatternStereo polar pattern.
--
-- ObjC selector: @- setPreferredPolarPattern:error:@
setPreferredPolarPattern_error :: (IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription, IsNSString pattern_, IsNSError outError) => avAudioSessionDataSourceDescription -> pattern_ -> outError -> IO Bool
setPreferredPolarPattern_error avAudioSessionDataSourceDescription  pattern_ outError =
withObjCPtr pattern_ $ \raw_pattern_ ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSessionDataSourceDescription (mkSelector "setPreferredPolarPattern:error:") retCULong [argPtr (castPtr raw_pattern_ :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Describes the general location of a data source. Will be nil for data sources for which the location is not known.
--
-- ObjC selector: @- location@
location :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
location avAudioSessionDataSourceDescription  =
  sendMsg avAudioSessionDataSourceDescription (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Describes the orientation of a data source.  Will be nil for data sources for which the orientation is not known.
--
-- ObjC selector: @- orientation@
orientation :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
orientation avAudioSessionDataSourceDescription  =
  sendMsg avAudioSessionDataSourceDescription (mkSelector "orientation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of one or more AVAudioSessionPolarPatterns describing the supported polar patterns for a data source.  Will be nil for data sources that have no selectable patterns.
--
-- ObjC selector: @- supportedPolarPatterns@
supportedPolarPatterns :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSArray)
supportedPolarPatterns avAudioSessionDataSourceDescription  =
  sendMsg avAudioSessionDataSourceDescription (mkSelector "supportedPolarPatterns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Describes the currently selected polar pattern.  Will be nil for data sources that have no selectable patterns.
--
-- ObjC selector: @- selectedPolarPattern@
selectedPolarPattern :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
selectedPolarPattern avAudioSessionDataSourceDescription  =
  sendMsg avAudioSessionDataSourceDescription (mkSelector "selectedPolarPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Describes the preferred polar pattern.  Will be nil for data sources that have no selectable patterns or if no preference has been set.
--
-- ObjC selector: @- preferredPolarPattern@
preferredPolarPattern :: IsAVAudioSessionDataSourceDescription avAudioSessionDataSourceDescription => avAudioSessionDataSourceDescription -> IO (Id NSString)
preferredPolarPattern avAudioSessionDataSourceDescription  =
  sendMsg avAudioSessionDataSourceDescription (mkSelector "preferredPolarPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPreferredPolarPattern:error:@
setPreferredPolarPattern_errorSelector :: Selector
setPreferredPolarPattern_errorSelector = mkSelector "setPreferredPolarPattern:error:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @supportedPolarPatterns@
supportedPolarPatternsSelector :: Selector
supportedPolarPatternsSelector = mkSelector "supportedPolarPatterns"

-- | @Selector@ for @selectedPolarPattern@
selectedPolarPatternSelector :: Selector
selectedPolarPatternSelector = mkSelector "selectedPolarPattern"

-- | @Selector@ for @preferredPolarPattern@
preferredPolarPatternSelector :: Selector
preferredPolarPatternSelector = mkSelector "preferredPolarPattern"

