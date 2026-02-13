{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlaybackSpeed
--
-- Class used to define a user selectable playback speed in a playback UI.
--
-- Generated bindings for @AVPlaybackSpeed@.
module ObjC.AVKit.AVPlaybackSpeed
  ( AVPlaybackSpeed
  , IsAVPlaybackSpeed(..)
  , init_
  , new
  , initWithRate_localizedName
  , systemDefaultSpeeds
  , rate
  , localizedName
  , localizedNumericName
  , initSelector
  , initWithRate_localizedNameSelector
  , localizedNameSelector
  , localizedNumericNameSelector
  , newSelector
  , rateSelector
  , systemDefaultSpeedsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO (Id AVPlaybackSpeed)
init_ avPlaybackSpeed =
  sendOwnedMessage avPlaybackSpeed initSelector

-- | @+ new@
new :: IO (Id AVPlaybackSpeed)
new  =
  do
    cls' <- getRequiredClass "AVPlaybackSpeed"
    sendOwnedClassMessage cls' newSelector

-- | initWithRate:localizedName:
--
-- @rate@ — The rate to be used when this playback speed is selected.
--
-- @localizedName@ — A localized name to be displayed representing this playback speed in a UI.
--
-- Initializes an AVPlaybackSpeed.
--
-- ObjC selector: @- initWithRate:localizedName:@
initWithRate_localizedName :: (IsAVPlaybackSpeed avPlaybackSpeed, IsNSString localizedName) => avPlaybackSpeed -> CFloat -> localizedName -> IO (Id AVPlaybackSpeed)
initWithRate_localizedName avPlaybackSpeed rate localizedName =
  sendOwnedMessage avPlaybackSpeed initWithRate_localizedNameSelector rate (toNSString localizedName)

-- | systemDefaultSpeeds
--
-- A list of playback speeds to be used by default across the system.
--
-- ObjC selector: @+ systemDefaultSpeeds@
systemDefaultSpeeds :: IO (Id NSArray)
systemDefaultSpeeds  =
  do
    cls' <- getRequiredClass "AVPlaybackSpeed"
    sendClassMessage cls' systemDefaultSpeedsSelector

-- | rate
--
-- The rate associated with this object. When this playback speed is selected this rate will be set in response to the play button being pressed.
--
-- ObjC selector: @- rate@
rate :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO CFloat
rate avPlaybackSpeed =
  sendMessage avPlaybackSpeed rateSelector

-- | localizedName
--
-- A localized name for this playback speed.
--
-- This name will be used to represent this playback speed in playback UIs where more space is available.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO (Id NSString)
localizedName avPlaybackSpeed =
  sendMessage avPlaybackSpeed localizedNameSelector

-- | localizedNumericName
--
-- A localized name for this playback speed used when space is limited.
--
-- This name will be used to represent this playback speed in playback UIs where limited space is available.
--
-- ObjC selector: @- localizedNumericName@
localizedNumericName :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO (Id NSString)
localizedNumericName avPlaybackSpeed =
  sendMessage avPlaybackSpeed localizedNumericNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlaybackSpeed)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlaybackSpeed)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRate:localizedName:@
initWithRate_localizedNameSelector :: Selector '[CFloat, Id NSString] (Id AVPlaybackSpeed)
initWithRate_localizedNameSelector = mkSelector "initWithRate:localizedName:"

-- | @Selector@ for @systemDefaultSpeeds@
systemDefaultSpeedsSelector :: Selector '[] (Id NSArray)
systemDefaultSpeedsSelector = mkSelector "systemDefaultSpeeds"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @localizedNumericName@
localizedNumericNameSelector :: Selector '[] (Id NSString)
localizedNumericNameSelector = mkSelector "localizedNumericName"

