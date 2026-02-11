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
  , initSelector
  , initWithLayoutTagSelector
  , initWithLayoutSelector
  , isEqualSelector
  , layoutWithLayoutTagSelector
  , layoutWithLayoutSelector
  , layoutTagSelector
  , layoutSelector
  , channelCountSelector


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

-- | @- init@
init_ :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO (Id AVAudioChannelLayout)
init_ avAudioChannelLayout  =
    sendMsg avAudioChannelLayout (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithLayoutTag avAudioChannelLayout  layoutTag =
    sendMsg avAudioChannelLayout (mkSelector "initWithLayoutTag:") (retPtr retVoid) [argCUInt layoutTag] >>= ownedObject . castPtr

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
initWithLayout avAudioChannelLayout  layout =
    sendMsg avAudioChannelLayout (mkSelector "initWithLayout:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst layout)) :: Ptr ())] >>= ownedObject . castPtr

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
isEqual avAudioChannelLayout  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioChannelLayout (mkSelector "isEqual:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | layoutWithLayoutTag:
--
-- Create from a layout tag.
--
-- ObjC selector: @+ layoutWithLayoutTag:@
layoutWithLayoutTag :: CUInt -> IO (Id AVAudioChannelLayout)
layoutWithLayoutTag layoutTag =
  do
    cls' <- getRequiredClass "AVAudioChannelLayout"
    sendClassMsg cls' (mkSelector "layoutWithLayoutTag:") (retPtr retVoid) [argCUInt layoutTag] >>= retainedObject . castPtr

-- | layoutWithLayout:
--
-- Create from an AudioChannelLayout
--
-- ObjC selector: @+ layoutWithLayout:@
layoutWithLayout :: Const RawId -> IO (Id AVAudioChannelLayout)
layoutWithLayout layout =
  do
    cls' <- getRequiredClass "AVAudioChannelLayout"
    sendClassMsg cls' (mkSelector "layoutWithLayout:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst layout)) :: Ptr ())] >>= retainedObject . castPtr

-- | layoutTag
--
-- The layout's tag.
--
-- ObjC selector: @- layoutTag@
layoutTag :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO CUInt
layoutTag avAudioChannelLayout  =
    sendMsg avAudioChannelLayout (mkSelector "layoutTag") retCUInt []

-- | layout
--
-- The underlying AudioChannelLayout.
--
-- ObjC selector: @- layout@
layout :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO (Const RawId)
layout avAudioChannelLayout  =
    fmap Const $ fmap (RawId . castPtr) $ sendMsg avAudioChannelLayout (mkSelector "layout") (retPtr retVoid) []

-- | channelCount
--
-- The number of channels of audio data.
--
-- ObjC selector: @- channelCount@
channelCount :: IsAVAudioChannelLayout avAudioChannelLayout => avAudioChannelLayout -> IO CUInt
channelCount avAudioChannelLayout  =
    sendMsg avAudioChannelLayout (mkSelector "channelCount") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLayoutTag:@
initWithLayoutTagSelector :: Selector
initWithLayoutTagSelector = mkSelector "initWithLayoutTag:"

-- | @Selector@ for @initWithLayout:@
initWithLayoutSelector :: Selector
initWithLayoutSelector = mkSelector "initWithLayout:"

-- | @Selector@ for @isEqual:@
isEqualSelector :: Selector
isEqualSelector = mkSelector "isEqual:"

-- | @Selector@ for @layoutWithLayoutTag:@
layoutWithLayoutTagSelector :: Selector
layoutWithLayoutTagSelector = mkSelector "layoutWithLayoutTag:"

-- | @Selector@ for @layoutWithLayout:@
layoutWithLayoutSelector :: Selector
layoutWithLayoutSelector = mkSelector "layoutWithLayout:"

-- | @Selector@ for @layoutTag@
layoutTagSelector :: Selector
layoutTagSelector = mkSelector "layoutTag"

-- | @Selector@ for @layout@
layoutSelector :: Selector
layoutSelector = mkSelector "layout"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector
channelCountSelector = mkSelector "channelCount"

