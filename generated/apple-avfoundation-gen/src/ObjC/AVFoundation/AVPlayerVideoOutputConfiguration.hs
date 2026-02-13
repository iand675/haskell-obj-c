{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerVideoOutputConfiguration
--
-- An AVPlayerVideoOutputConfiguration carries an identifier for the AVPlayerItem the configuration is associated with as well as presentation settings for that item.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerVideoOutputConfiguration@.
module ObjC.AVFoundation.AVPlayerVideoOutputConfiguration
  ( AVPlayerVideoOutputConfiguration
  , IsAVPlayerVideoOutputConfiguration(..)
  , init_
  , new
  , sourcePlayerItem
  , dataChannelDescriptions
  , dataChannelDescriptionsSelector
  , initSelector
  , newSelector
  , sourcePlayerItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerVideoOutputConfiguration avPlayerVideoOutputConfiguration => avPlayerVideoOutputConfiguration -> IO (Id AVPlayerVideoOutputConfiguration)
init_ avPlayerVideoOutputConfiguration =
  sendOwnedMessage avPlayerVideoOutputConfiguration initSelector

-- | @+ new@
new :: IO (Id AVPlayerVideoOutputConfiguration)
new  =
  do
    cls' <- getRequiredClass "AVPlayerVideoOutputConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | sourcePlayerItem
--
-- The AVPlayerItem which is the source of this configuration.
--
-- This AVPlayerItem can be seen as the source of all samples this configuration vended alongside.
--
-- ObjC selector: @- sourcePlayerItem@
sourcePlayerItem :: IsAVPlayerVideoOutputConfiguration avPlayerVideoOutputConfiguration => avPlayerVideoOutputConfiguration -> IO (Id AVPlayerItem)
sourcePlayerItem avPlayerVideoOutputConfiguration =
  sendMessage avPlayerVideoOutputConfiguration sourcePlayerItemSelector

-- | dataChannelDescriptions
--
-- List of data channels, represented as CMTagCollections, selected for this configuration.
--
-- Returns an Array of CMTagCollections
--
-- ObjC selector: @- dataChannelDescriptions@
dataChannelDescriptions :: IsAVPlayerVideoOutputConfiguration avPlayerVideoOutputConfiguration => avPlayerVideoOutputConfiguration -> IO (Id NSArray)
dataChannelDescriptions avPlayerVideoOutputConfiguration =
  sendMessage avPlayerVideoOutputConfiguration dataChannelDescriptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerVideoOutputConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerVideoOutputConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @sourcePlayerItem@
sourcePlayerItemSelector :: Selector '[] (Id AVPlayerItem)
sourcePlayerItemSelector = mkSelector "sourcePlayerItem"

-- | @Selector@ for @dataChannelDescriptions@
dataChannelDescriptionsSelector :: Selector '[] (Id NSArray)
dataChannelDescriptionsSelector = mkSelector "dataChannelDescriptions"

