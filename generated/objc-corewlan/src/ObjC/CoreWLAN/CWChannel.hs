{-# LANGUAGE PatternSynonyms #-}
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
  , isEqualToChannelSelector
  , channelNumberSelector
  , channelWidthSelector
  , channelBandSelector

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
isEqualToChannel cwChannel  channel =
withObjCPtr channel $ \raw_channel ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwChannel (mkSelector "isEqualToChannel:") retCULong [argPtr (castPtr raw_channel :: Ptr ())]

-- | The channel number represented as an integer value.
--
-- ObjC selector: @- channelNumber@
channelNumber :: IsCWChannel cwChannel => cwChannel -> IO CLong
channelNumber cwChannel  =
  sendMsg cwChannel (mkSelector "channelNumber") retCLong []

-- | The channel width as indicated by the CWChannelWidth type.
--
-- ObjC selector: @- channelWidth@
channelWidth :: IsCWChannel cwChannel => cwChannel -> IO CWChannelWidth
channelWidth cwChannel  =
  fmap (coerce :: CLong -> CWChannelWidth) $ sendMsg cwChannel (mkSelector "channelWidth") retCLong []

-- | The channel band as indicated by the CWChannelBand type.
--
-- ObjC selector: @- channelBand@
channelBand :: IsCWChannel cwChannel => cwChannel -> IO CWChannelBand
channelBand cwChannel  =
  fmap (coerce :: CLong -> CWChannelBand) $ sendMsg cwChannel (mkSelector "channelBand") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToChannel:@
isEqualToChannelSelector :: Selector
isEqualToChannelSelector = mkSelector "isEqualToChannel:"

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @channelWidth@
channelWidthSelector :: Selector
channelWidthSelector = mkSelector "channelWidth"

-- | @Selector@ for @channelBand@
channelBandSelector :: Selector
channelBandSelector = mkSelector "channelBand"

