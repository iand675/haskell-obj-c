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
  , newSelector
  , initWithRate_localizedNameSelector
  , systemDefaultSpeedsSelector
  , rateSelector
  , localizedNameSelector
  , localizedNumericNameSelector


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

import ObjC.AVKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO (Id AVPlaybackSpeed)
init_ avPlaybackSpeed  =
  sendMsg avPlaybackSpeed (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlaybackSpeed)
new  =
  do
    cls' <- getRequiredClass "AVPlaybackSpeed"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithRate_localizedName avPlaybackSpeed  rate localizedName =
withObjCPtr localizedName $ \raw_localizedName ->
    sendMsg avPlaybackSpeed (mkSelector "initWithRate:localizedName:") (retPtr retVoid) [argCFloat (fromIntegral rate), argPtr (castPtr raw_localizedName :: Ptr ())] >>= ownedObject . castPtr

-- | systemDefaultSpeeds
--
-- A list of playback speeds to be used by default across the system.
--
-- ObjC selector: @+ systemDefaultSpeeds@
systemDefaultSpeeds :: IO (Id NSArray)
systemDefaultSpeeds  =
  do
    cls' <- getRequiredClass "AVPlaybackSpeed"
    sendClassMsg cls' (mkSelector "systemDefaultSpeeds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rate
--
-- The rate associated with this object. When this playback speed is selected this rate will be set in response to the play button being pressed.
--
-- ObjC selector: @- rate@
rate :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO CFloat
rate avPlaybackSpeed  =
  sendMsg avPlaybackSpeed (mkSelector "rate") retCFloat []

-- | localizedName
--
-- A localized name for this playback speed.
--
-- This name will be used to represent this playback speed in playback UIs where more space is available.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO (Id NSString)
localizedName avPlaybackSpeed  =
  sendMsg avPlaybackSpeed (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedNumericName
--
-- A localized name for this playback speed used when space is limited.
--
-- This name will be used to represent this playback speed in playback UIs where limited space is available.
--
-- ObjC selector: @- localizedNumericName@
localizedNumericName :: IsAVPlaybackSpeed avPlaybackSpeed => avPlaybackSpeed -> IO (Id NSString)
localizedNumericName avPlaybackSpeed  =
  sendMsg avPlaybackSpeed (mkSelector "localizedNumericName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRate:localizedName:@
initWithRate_localizedNameSelector :: Selector
initWithRate_localizedNameSelector = mkSelector "initWithRate:localizedName:"

-- | @Selector@ for @systemDefaultSpeeds@
systemDefaultSpeedsSelector :: Selector
systemDefaultSpeedsSelector = mkSelector "systemDefaultSpeeds"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @localizedNumericName@
localizedNumericNameSelector :: Selector
localizedNumericNameSelector = mkSelector "localizedNumericName"

