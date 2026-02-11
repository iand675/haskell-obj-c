{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKAddress@.
module ObjC.MapKit.MKAddress
  ( MKAddress
  , IsMKAddress(..)
  , initWithFullAddress_shortAddress
  , init_
  , new
  , fullAddress
  , shortAddress
  , initWithFullAddress_shortAddressSelector
  , initSelector
  , newSelector
  , fullAddressSelector
  , shortAddressSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFullAddress:shortAddress:@
initWithFullAddress_shortAddress :: (IsMKAddress mkAddress, IsNSString fullAddress, IsNSString shortAddress) => mkAddress -> fullAddress -> shortAddress -> IO (Id MKAddress)
initWithFullAddress_shortAddress mkAddress  fullAddress shortAddress =
withObjCPtr fullAddress $ \raw_fullAddress ->
  withObjCPtr shortAddress $ \raw_shortAddress ->
      sendMsg mkAddress (mkSelector "initWithFullAddress:shortAddress:") (retPtr retVoid) [argPtr (castPtr raw_fullAddress :: Ptr ()), argPtr (castPtr raw_shortAddress :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMKAddress mkAddress => mkAddress -> IO (Id MKAddress)
init_ mkAddress  =
  sendMsg mkAddress (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKAddress)
new  =
  do
    cls' <- getRequiredClass "MKAddress"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- fullAddress@
fullAddress :: IsMKAddress mkAddress => mkAddress -> IO (Id NSString)
fullAddress mkAddress  =
  sendMsg mkAddress (mkSelector "fullAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortAddress@
shortAddress :: IsMKAddress mkAddress => mkAddress -> IO (Id NSString)
shortAddress mkAddress  =
  sendMsg mkAddress (mkSelector "shortAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFullAddress:shortAddress:@
initWithFullAddress_shortAddressSelector :: Selector
initWithFullAddress_shortAddressSelector = mkSelector "initWithFullAddress:shortAddress:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fullAddress@
fullAddressSelector :: Selector
fullAddressSelector = mkSelector "fullAddress"

-- | @Selector@ for @shortAddress@
shortAddressSelector :: Selector
shortAddressSelector = mkSelector "shortAddress"

