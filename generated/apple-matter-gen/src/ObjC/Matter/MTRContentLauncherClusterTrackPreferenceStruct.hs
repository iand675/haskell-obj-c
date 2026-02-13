{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterTrackPreferenceStruct@.
module ObjC.Matter.MTRContentLauncherClusterTrackPreferenceStruct
  ( MTRContentLauncherClusterTrackPreferenceStruct
  , IsMTRContentLauncherClusterTrackPreferenceStruct(..)
  , languageCode
  , setLanguageCode
  , characteristics
  , setCharacteristics
  , audioOutputIndex
  , setAudioOutputIndex
  , audioOutputIndexSelector
  , characteristicsSelector
  , languageCodeSelector
  , setAudioOutputIndexSelector
  , setCharacteristicsSelector
  , setLanguageCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- languageCode@
languageCode :: IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct => mtrContentLauncherClusterTrackPreferenceStruct -> IO (Id NSString)
languageCode mtrContentLauncherClusterTrackPreferenceStruct =
  sendMessage mtrContentLauncherClusterTrackPreferenceStruct languageCodeSelector

-- | @- setLanguageCode:@
setLanguageCode :: (IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct, IsNSString value) => mtrContentLauncherClusterTrackPreferenceStruct -> value -> IO ()
setLanguageCode mtrContentLauncherClusterTrackPreferenceStruct value =
  sendMessage mtrContentLauncherClusterTrackPreferenceStruct setLanguageCodeSelector (toNSString value)

-- | @- characteristics@
characteristics :: IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct => mtrContentLauncherClusterTrackPreferenceStruct -> IO (Id NSArray)
characteristics mtrContentLauncherClusterTrackPreferenceStruct =
  sendMessage mtrContentLauncherClusterTrackPreferenceStruct characteristicsSelector

-- | @- setCharacteristics:@
setCharacteristics :: (IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct, IsNSArray value) => mtrContentLauncherClusterTrackPreferenceStruct -> value -> IO ()
setCharacteristics mtrContentLauncherClusterTrackPreferenceStruct value =
  sendMessage mtrContentLauncherClusterTrackPreferenceStruct setCharacteristicsSelector (toNSArray value)

-- | @- audioOutputIndex@
audioOutputIndex :: IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct => mtrContentLauncherClusterTrackPreferenceStruct -> IO (Id NSNumber)
audioOutputIndex mtrContentLauncherClusterTrackPreferenceStruct =
  sendMessage mtrContentLauncherClusterTrackPreferenceStruct audioOutputIndexSelector

-- | @- setAudioOutputIndex:@
setAudioOutputIndex :: (IsMTRContentLauncherClusterTrackPreferenceStruct mtrContentLauncherClusterTrackPreferenceStruct, IsNSNumber value) => mtrContentLauncherClusterTrackPreferenceStruct -> value -> IO ()
setAudioOutputIndex mtrContentLauncherClusterTrackPreferenceStruct value =
  sendMessage mtrContentLauncherClusterTrackPreferenceStruct setAudioOutputIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector '[] (Id NSString)
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector '[Id NSString] ()
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @characteristics@
characteristicsSelector :: Selector '[] (Id NSArray)
characteristicsSelector = mkSelector "characteristics"

-- | @Selector@ for @setCharacteristics:@
setCharacteristicsSelector :: Selector '[Id NSArray] ()
setCharacteristicsSelector = mkSelector "setCharacteristics:"

-- | @Selector@ for @audioOutputIndex@
audioOutputIndexSelector :: Selector '[] (Id NSNumber)
audioOutputIndexSelector = mkSelector "audioOutputIndex"

-- | @Selector@ for @setAudioOutputIndex:@
setAudioOutputIndexSelector :: Selector '[Id NSNumber] ()
setAudioOutputIndexSelector = mkSelector "setAudioOutputIndex:"

