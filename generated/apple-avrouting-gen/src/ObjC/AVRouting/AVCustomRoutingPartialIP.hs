{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a full or partial IP address.
--
-- Use this class in conjunction with ``knownRouteIPs``.
--
-- Generated bindings for @AVCustomRoutingPartialIP@.
module ObjC.AVRouting.AVCustomRoutingPartialIP
  ( AVCustomRoutingPartialIP
  , IsAVCustomRoutingPartialIP(..)
  , initWithAddress_mask
  , init_
  , new
  , address
  , mask
  , initWithAddress_maskSelector
  , initSelector
  , newSelector
  , addressSelector
  , maskSelector


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

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an IP fragment.  - Parameters:    - address: The IP address.    - mask: The address mask.
--
-- ObjC selector: @- initWithAddress:mask:@
initWithAddress_mask :: (IsAVCustomRoutingPartialIP avCustomRoutingPartialIP, IsNSData address, IsNSData mask) => avCustomRoutingPartialIP -> address -> mask -> IO (Id AVCustomRoutingPartialIP)
initWithAddress_mask avCustomRoutingPartialIP  address mask =
  withObjCPtr address $ \raw_address ->
    withObjCPtr mask $ \raw_mask ->
        sendMsg avCustomRoutingPartialIP (mkSelector "initWithAddress:mask:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ()), argPtr (castPtr raw_mask :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAVCustomRoutingPartialIP avCustomRoutingPartialIP => avCustomRoutingPartialIP -> IO (Id AVCustomRoutingPartialIP)
init_ avCustomRoutingPartialIP  =
    sendMsg avCustomRoutingPartialIP (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCustomRoutingPartialIP)
new  =
  do
    cls' <- getRequiredClass "AVCustomRoutingPartialIP"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A full or partial IP address for a device known to be on the network.
--
-- Use the following code to create a full known IP address.
--
-- ```var anIPAddressInBytes:[Byte] = [192, 168, 10, 5]var address = Data(bytes: anAddressInBytes, length: anAddressInBytes.count)var aMaskInBytes:[Byte] = [255, 255, 255, 255]var mask = Data(bytes: aMaskInBytes, length: aMaskInBytes.count)var partialIP = AVCustomRoutingPartialIP(address: address, mask: mask)```
--
-- ObjC selector: @- address@
address :: IsAVCustomRoutingPartialIP avCustomRoutingPartialIP => avCustomRoutingPartialIP -> IO (Id NSData)
address avCustomRoutingPartialIP  =
    sendMsg avCustomRoutingPartialIP (mkSelector "address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A mask representing how many octets of the IP  address to respect.
--
-- Use this mask to pass the last two bytes of the IP address instead of passing all four bytes.
--
-- ```var anIPAddressInBytes:[Byte] = [0, 0, 10, 5] var address = Data(bytes: anAddressInBytes, length: anAddressInBytes.count) var aMaskInBytes:[Byte] = [0, 0, 255, 255] var mask = Data(bytes: aMaskInBytes, length: aMaskInBytes.count) var partialIP =AVCustomRoutingPartialIP(address: address, mask: mask) ```
--
-- ObjC selector: @- mask@
mask :: IsAVCustomRoutingPartialIP avCustomRoutingPartialIP => avCustomRoutingPartialIP -> IO (Id NSData)
mask avCustomRoutingPartialIP  =
    sendMsg avCustomRoutingPartialIP (mkSelector "mask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddress:mask:@
initWithAddress_maskSelector :: Selector
initWithAddress_maskSelector = mkSelector "initWithAddress:mask:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @mask@
maskSelector :: Selector
maskSelector = mkSelector "mask"

