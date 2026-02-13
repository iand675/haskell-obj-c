{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents an IEEE 802.11 channel.
--
-- The CWChannel class is used by both CWInterface and CWNetwork as a representation of an IEEE 802.11 Wi-Fi channel.
--
-- Generated bindings for @CWChannel@.
module ObjC.CoreWLAN.CWChannel
  ( CWChannel
  , IsCWChannel(..)
  , isEqualToChannel
  , channelNumber
  , channelWidth
  , channelBand
  , channelBandSelector
  , channelNumberSelector
  , channelWidthSelector
  , isEqualToChannelSelector

  -- * Enum types
  , CWChannelBand(CWChannelBand)
  , pattern KCWChannelBandUnknown
  , pattern KCWChannelBand2GHz
  , pattern KCWChannelBand5GHz
  , pattern KCWChannelBand6GHz
  , CWChannelWidth(CWChannelWidth)
  , pattern KCWChannelWidthUnknown
  , pattern KCWChannelWidth20MHz
  , pattern KCWChannelWidth40MHz
  , pattern KCWChannelWidth80MHz
  , pattern KCWChannelWidth160MHz

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreWLAN.Internal.Classes
import ObjC.CoreWLAN.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @channel@ — The CWChannel with which to compare the receiver.
--
-- Returns: YES if the objects are equal, otherwise NO.
--
-- Determine CWChannel equality.
--
-- CWChannel objects are considered equal if all their corresponding properties are equal.
--
-- ObjC selector: @- isEqualToChannel:@
isEqualToChannel :: (IsCWChannel cwChannel, IsCWChannel channel) => cwChannel -> channel -> IO Bool
isEqualToChannel cwChannel channel =
  sendMessage cwChannel isEqualToChannelSelector (toCWChannel channel)

-- | The channel number represented as an integer value.
--
-- ObjC selector: @- channelNumber@
channelNumber :: IsCWChannel cwChannel => cwChannel -> IO CLong
channelNumber cwChannel =
  sendMessage cwChannel channelNumberSelector

-- | The channel width as indicated by the CWChannelWidth type.
--
-- ObjC selector: @- channelWidth@
channelWidth :: IsCWChannel cwChannel => cwChannel -> IO CWChannelWidth
channelWidth cwChannel =
  sendMessage cwChannel channelWidthSelector

-- | The channel band as indicated by the CWChannelBand type.
--
-- ObjC selector: @- channelBand@
channelBand :: IsCWChannel cwChannel => cwChannel -> IO CWChannelBand
channelBand cwChannel =
  sendMessage cwChannel channelBandSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToChannel:@
isEqualToChannelSelector :: Selector '[Id CWChannel] Bool
isEqualToChannelSelector = mkSelector "isEqualToChannel:"

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector '[] CLong
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @channelWidth@
channelWidthSelector :: Selector '[] CWChannelWidth
channelWidthSelector = mkSelector "channelWidth"

-- | @Selector@ for @channelBand@
channelBandSelector :: Selector '[] CWChannelBand
channelBandSelector = mkSelector "channelBand"

