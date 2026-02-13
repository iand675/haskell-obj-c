{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRThreadOperationalDataset allows converting between an "expanded" view of the dataset (with the separate fields) and a single-blob NSData view.
--
-- The latter can be used to pass Thread network credentials via MTRCommissioningParameters.
--
-- Generated bindings for @MTRThreadOperationalDataset@.
module ObjC.Matter.MTRThreadOperationalDataset
  ( MTRThreadOperationalDataset
  , IsMTRThreadOperationalDataset(..)
  , init_
  , new
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panID
  , initWithData
  , data_
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panID
  , networkName
  , extendedPANID
  , masterKey
  , psKc
  , channelNumber
  , panID
  , channel
  , setChannel
  , channelNumberSelector
  , channelSelector
  , dataSelector
  , extendedPANIDSelector
  , initSelector
  , initWithDataSelector
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector
  , initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector
  , masterKeySelector
  , networkNameSelector
  , newSelector
  , panIDSelector
  , psKcSelector
  , setChannelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id MTRThreadOperationalDataset)
init_ mtrThreadOperationalDataset =
  sendOwnedMessage mtrThreadOperationalDataset initSelector

-- | @+ new@
new :: IO (Id MTRThreadOperationalDataset)
new  =
  do
    cls' <- getRequiredClass "MTRThreadOperationalDataset"
    sendOwnedClassMessage cls' newSelector

-- | Create a Thread Operational Dataset object with the individual network fields.
--
-- @extendedPANID@ — Must be MTRSizeThreadExtendedPANID bytes.  Otherwise nil                      will be returned.
--
-- @masterKey@ — Must be MTRSizeThreadMasterKey bytes. Otherwise nil will be                  returned.
--
-- @PSKc@ — Must be MTRSizeThreadPSKc bytes.  Otherwise nil will be returned.
--
-- @channelNumber@ — Must be an unsigned 16-bit value.
--
-- @panID@ — Must be MTRSizeThreadPANID bytes.  Otherwise nil will be              returned.  In particular, it's expected to be a 16-bit unsigned              integer stored as 2 bytes in host order.
--
-- ObjC selector: @- initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panID :: (IsMTRThreadOperationalDataset mtrThreadOperationalDataset, IsNSString networkName, IsNSData extendedPANID, IsNSData masterKey, IsNSData psKc, IsNSNumber channelNumber, IsNSData panID) => mtrThreadOperationalDataset -> networkName -> extendedPANID -> masterKey -> psKc -> channelNumber -> panID -> IO (Id MTRThreadOperationalDataset)
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panID mtrThreadOperationalDataset networkName extendedPANID masterKey psKc channelNumber panID =
  sendOwnedMessage mtrThreadOperationalDataset initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector (toNSString networkName) (toNSData extendedPANID) (toNSData masterKey) (toNSData psKc) (toNSNumber channelNumber) (toNSData panID)

-- | Create a Thread Operational Dataset object with a RCP formatted active operational dataset. This initializer will return nil if the input data cannot be parsed correctly
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsMTRThreadOperationalDataset mtrThreadOperationalDataset, IsNSData data_) => mtrThreadOperationalDataset -> data_ -> IO (Id MTRThreadOperationalDataset)
initWithData mtrThreadOperationalDataset data_ =
  sendOwnedMessage mtrThreadOperationalDataset initWithDataSelector (toNSData data_)

-- | Get the underlying data that represents the Thread Active Operational Dataset This can be used for the threadOperationalDataset of MTRCommissioningParameters.
--
-- ObjC selector: @- data@
data_ :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
data_ mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset dataSelector

-- | @- initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panID :: (IsMTRThreadOperationalDataset mtrThreadOperationalDataset, IsNSString networkName, IsNSData extendedPANID, IsNSData masterKey, IsNSData psKc, IsNSData panID) => mtrThreadOperationalDataset -> networkName -> extendedPANID -> masterKey -> psKc -> CUShort -> panID -> IO (Id MTRThreadOperationalDataset)
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panID mtrThreadOperationalDataset networkName extendedPANID masterKey psKc channel panID =
  sendOwnedMessage mtrThreadOperationalDataset initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector (toNSString networkName) (toNSData extendedPANID) (toNSData masterKey) (toNSData psKc) channel (toNSData panID)

-- | The Thread Network name
--
-- ObjC selector: @- networkName@
networkName :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSString)
networkName mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset networkNameSelector

-- | The Thread Network extendended PAN ID
--
-- ObjC selector: @- extendedPANID@
extendedPANID :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
extendedPANID mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset extendedPANIDSelector

-- | The 16 byte Master Key
--
-- ObjC selector: @- masterKey@
masterKey :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
masterKey mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset masterKeySelector

-- | The Thread PSKc
--
-- ObjC selector: @- PSKc@
psKc :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
psKc mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset psKcSelector

-- | The Thread network channel.  Always an unsigned 16-bit integer.
--
-- ObjC selector: @- channelNumber@
channelNumber :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSNumber)
channelNumber mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset channelNumberSelector

-- | A uint16_t stored as 2-bytes in host order representing the Thread PAN ID
--
-- ObjC selector: @- panID@
panID :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO (Id NSData)
panID mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset panIDSelector

-- | @- channel@
channel :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> IO CUShort
channel mtrThreadOperationalDataset =
  sendMessage mtrThreadOperationalDataset channelSelector

-- | @- setChannel:@
setChannel :: IsMTRThreadOperationalDataset mtrThreadOperationalDataset => mtrThreadOperationalDataset -> CUShort -> IO ()
setChannel mtrThreadOperationalDataset value =
  sendMessage mtrThreadOperationalDataset setChannelSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRThreadOperationalDataset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRThreadOperationalDataset)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector :: Selector '[Id NSString, Id NSData, Id NSData, Id NSData, Id NSNumber, Id NSData] (Id MTRThreadOperationalDataset)
initWithNetworkName_extendedPANID_masterKey_PSKc_channelNumber_panIDSelector = mkSelector "initWithNetworkName:extendedPANID:masterKey:PSKc:channelNumber:panID:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id MTRThreadOperationalDataset)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:@
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector :: Selector '[Id NSString, Id NSData, Id NSData, Id NSData, CUShort, Id NSData] (Id MTRThreadOperationalDataset)
initWithNetworkName_extendedPANID_masterKey_PSKc_channel_panIDSelector = mkSelector "initWithNetworkName:extendedPANID:masterKey:PSKc:channel:panID:"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector '[] (Id NSString)
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @extendedPANID@
extendedPANIDSelector :: Selector '[] (Id NSData)
extendedPANIDSelector = mkSelector "extendedPANID"

-- | @Selector@ for @masterKey@
masterKeySelector :: Selector '[] (Id NSData)
masterKeySelector = mkSelector "masterKey"

-- | @Selector@ for @PSKc@
psKcSelector :: Selector '[] (Id NSData)
psKcSelector = mkSelector "PSKc"

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector '[] (Id NSNumber)
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @panID@
panIDSelector :: Selector '[] (Id NSData)
panIDSelector = mkSelector "panID"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] CUShort
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[CUShort] ()
setChannelSelector = mkSelector "setChannel:"

