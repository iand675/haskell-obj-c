{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VSAppleSubscription@.
module ObjC.VideoSubscriberAccount.VSAppleSubscription
  ( VSAppleSubscription
  , IsVSAppleSubscription(..)
  , initWithCustomerID_productCodes
  , init_
  , new
  , customerID
  , setCustomerID
  , productCodes
  , setProductCodes
  , customerIDSelector
  , initSelector
  , initWithCustomerID_productCodesSelector
  , newSelector
  , productCodesSelector
  , setCustomerIDSelector
  , setProductCodesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCustomerID:productCodes:@
initWithCustomerID_productCodes :: (IsVSAppleSubscription vsAppleSubscription, IsNSString customerID, IsNSArray productCodes) => vsAppleSubscription -> customerID -> productCodes -> IO (Id VSAppleSubscription)
initWithCustomerID_productCodes vsAppleSubscription customerID productCodes =
  sendOwnedMessage vsAppleSubscription initWithCustomerID_productCodesSelector (toNSString customerID) (toNSArray productCodes)

-- | @- init@
init_ :: IsVSAppleSubscription vsAppleSubscription => vsAppleSubscription -> IO (Id VSAppleSubscription)
init_ vsAppleSubscription =
  sendOwnedMessage vsAppleSubscription initSelector

-- | @+ new@
new :: IO (Id VSAppleSubscription)
new  =
  do
    cls' <- getRequiredClass "VSAppleSubscription"
    sendOwnedClassMessage cls' newSelector

-- | @- customerID@
customerID :: IsVSAppleSubscription vsAppleSubscription => vsAppleSubscription -> IO (Id NSString)
customerID vsAppleSubscription =
  sendMessage vsAppleSubscription customerIDSelector

-- | @- setCustomerID:@
setCustomerID :: (IsVSAppleSubscription vsAppleSubscription, IsNSString value) => vsAppleSubscription -> value -> IO ()
setCustomerID vsAppleSubscription value =
  sendMessage vsAppleSubscription setCustomerIDSelector (toNSString value)

-- | @- productCodes@
productCodes :: IsVSAppleSubscription vsAppleSubscription => vsAppleSubscription -> IO (Id NSArray)
productCodes vsAppleSubscription =
  sendMessage vsAppleSubscription productCodesSelector

-- | @- setProductCodes:@
setProductCodes :: (IsVSAppleSubscription vsAppleSubscription, IsNSArray value) => vsAppleSubscription -> value -> IO ()
setProductCodes vsAppleSubscription value =
  sendMessage vsAppleSubscription setProductCodesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCustomerID:productCodes:@
initWithCustomerID_productCodesSelector :: Selector '[Id NSString, Id NSArray] (Id VSAppleSubscription)
initWithCustomerID_productCodesSelector = mkSelector "initWithCustomerID:productCodes:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VSAppleSubscription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VSAppleSubscription)
newSelector = mkSelector "new"

-- | @Selector@ for @customerID@
customerIDSelector :: Selector '[] (Id NSString)
customerIDSelector = mkSelector "customerID"

-- | @Selector@ for @setCustomerID:@
setCustomerIDSelector :: Selector '[Id NSString] ()
setCustomerIDSelector = mkSelector "setCustomerID:"

-- | @Selector@ for @productCodes@
productCodesSelector :: Selector '[] (Id NSArray)
productCodesSelector = mkSelector "productCodes"

-- | @Selector@ for @setProductCodes:@
setProductCodesSelector :: Selector '[Id NSArray] ()
setProductCodesSelector = mkSelector "setProductCodes:"

