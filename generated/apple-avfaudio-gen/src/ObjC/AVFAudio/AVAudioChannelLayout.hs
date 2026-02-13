{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioChannelLayout
--
-- A description of the roles of a set of audio channels.
--
-- This object is a thin wrapper for the AudioChannelLayout structure, described		in <CoreAudio/CoreAudioTypes.h>.
--
-- Generated bindings for @AVAudioChannelLayout@.
module ObjC.AVFAudio.AVAudioChannelLayout
  ( AVAudioChannelLayout
  , IsAVAudioChannelLayout(..)
  , init_
  , initWithLayoutTag
  , initWithLayout
  , isEqual
  , layoutWithLayoutTag
  , layoutWithLayout
  , layoutTag
  , layout
  , channelCount
  , channelCountSelector
  , initSelector
  , initWithLayoutSelector
  , initWithLayoutTagSelector
  , isEqualSelector
  , layoutSelector
  , layoutTagSelector
  , layoutWithLayoutSelector
  , layoutWithLayoutTagSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO (Id AVAudioChannelLayout)
init_ avAudioChannelLayout =
  sendOwnedMessage avAudioChannelLayout initSelector

-- | initWithLayoutTag:
--
-- Initialize from a layout tag.
--
-- @layoutTag@ — The tag.
--
-- Returns nil if the tag is either kAudioChannelLayoutTag_UseChannelDescriptions or		kAudioChannelLayoutTag_UseChannelBitmap.
--
-- ObjC selector: @- initWithLayoutTag:@
initWithLayoutTag :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> CUInt -> IO (Id AVAudioChannelLayout)
initWithLayoutTag avAudioChannelLayout layoutTag =
  sendOwnedMessage avAudioChannelLayout initWithLayoutTagSelector layoutTag

-- | initWithLayout:
--
-- Initialize from an AudioChannelLayout.
--
-- @layout@ — The AudioChannelLayout.
--
-- If the provided layout's tag is kAudioChannelLayoutTag_UseChannelDescriptions, this		initializer attempts to convert it to a more specific tag.
--
-- ObjC selector: @- initWithLayout:@
initWithLayout :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> Const RawId -> IO (Id AVAudioChannelLayout)
initWithLayout avAudioChannelLayout layout =
  sendOwnedMessage avAudioChannelLayout initWithLayoutSelector layout

-- | isEqual:
--
-- Determine whether another AVAudioChannelLayout is exactly equal to this layout.
--
-- @object@ — The AVAudioChannelLayout to compare against.
--
-- The underlying AudioChannelLayoutTag and AudioChannelLayout are compared for equality.
--
-- ObjC selector: @- isEqual:@
isEqual :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> RawId -> IO Bool
isEqual avAudioChannelLayout object =
  sendMessage avAudioChannelLayout isEqualSelector object

-- | layoutWithLayoutTag:
--
-- Create from a layout tag.
--
-- ObjC selector: @+ layoutWithLayoutTag:@
layoutWithLayoutTag :: CUInt -> IO (Id AVAudioChannelLayout)
layoutWithLayoutTag layoutTag =
  do
    cls' <- getRequiredClass "AVAudioChannelLayout"
    sendClassMessage cls' layoutWithLayoutTagSelector layoutTag

-- | layoutWithLayout:
--
-- Create from an AudioChannelLayout
--
-- ObjC selector: @+ layoutWithLayout:@
layoutWithLayout :: Const RawId -> IO (Id AVAudioChannelLayout)
layoutWithLayout layout =
  do
    cls' <- getRequiredClass "AVAudioChannelLayout"
    sendClassMessage cls' layoutWithLayoutSelector layout

-- | layoutTag
--
-- The layout's tag.
--
-- ObjC selector: @- layoutTag@
layoutTag :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO CUInt
layoutTag avAudioChannelLayout =
  sendMessage avAudioChannelLayout layoutTagSelector

-- | layout
--
-- The underlying AudioChannelLayout.
--
-- ObjC selector: @- layout@
layout :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO (Const RawId)
layout avAudioChannelLayout =
  sendMessage avAudioChannelLayout layoutSelector

-- | channelCount
--
-- The number of channels of audio data.
--
-- ObjC selector: @- channelCount@
channelCount :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO CUInt
channelCount avAudioChannelLayout =
  sendMessage avAudioChannelLayout channelCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioChannelLayout)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLayoutTag:@
initWithLayoutTagSelector :: Selector '[CUInt] (Id AVAudioChannelLayout)
initWithLayoutTagSelector = mkSelector "initWithLayoutTag:"

-- | @Selector@ for @initWithLayout:@
initWithLayoutSelector :: Selector '[Const RawId] (Id AVAudioChannelLayout)
initWithLayoutSelector = mkSelector "initWithLayout:"

-- | @Selector@ for @isEqual:@
isEqualSelector :: Selector '[RawId] Bool
isEqualSelector = mkSelector "isEqual:"

-- | @Selector@ for @layoutWithLayoutTag:@
layoutWithLayoutTagSelector :: Selector '[CUInt] (Id AVAudioChannelLayout)
layoutWithLayoutTagSelector = mkSelector "layoutWithLayoutTag:"

-- | @Selector@ for @layoutWithLayout:@
layoutWithLayoutSelector :: Selector '[Const RawId] (Id AVAudioChannelLayout)
layoutWithLayoutSelector = mkSelector "layoutWithLayout:"

-- | @Selector@ for @layoutTag@
layoutTagSelector :: Selector '[] CUInt
layoutTagSelector = mkSelector "layoutTag"

-- | @Selector@ for @layout@
layoutSelector :: Selector '[] (Const RawId)
layoutSelector = mkSelector "layout"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] CUInt
channelCountSelector = mkSelector "channelCount"

