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
  , initWithCustomerID_productCodesSelector
  , initSelector
  , newSelector
  , customerIDSelector
  , setCustomerIDSelector
  , productCodesSelector
  , setProductCodesSelector


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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCustomerID:productCodes:@
initWithCustomerID_productCodes :: (IsVSAppleSubscription vsAppleSubscription, IsNSString customerID, IsNSArray productCodes) => vsAppleSubscription -> customerID -> productCodes -> IO (Id VSAppleSubscription)
initWithCustomerID_productCodes vsAppleSubscription  customerID productCodes =
withObjCPtr customerID $ \raw_customerID ->
  withObjCPtr productCodes $ \raw_productCodes ->
      sendMsg vsAppleSubscription (mkSelector "initWithCustomerID:productCodes:") (retPtr retVoid) [argPtr (castPtr raw_customerID :: Ptr ()), argPtr (castPtr raw_productCodes :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVSAppleSubscription vsAppleSubscription => vsAppleSubscription -> IO (Id VSAppleSubscription)
init_ vsAppleSubscription  =
  sendMsg vsAppleSubscription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VSAppleSubscription)
new  =
  do
    cls' <- getRequiredClass "VSAppleSubscription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- customerID@
customerID :: IsVSAppleSubscription vsAppleSubscription => vsAppleSubscription -> IO (Id NSString)
customerID vsAppleSubscription  =
  sendMsg vsAppleSubscription (mkSelector "customerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomerID:@
setCustomerID :: (IsVSAppleSubscription vsAppleSubscription, IsNSString value) => vsAppleSubscription -> value -> IO ()
setCustomerID vsAppleSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAppleSubscription (mkSelector "setCustomerID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productCodes@
productCodes :: IsVSAppleSubscription vsAppleSubscription => vsAppleSubscription -> IO (Id NSArray)
productCodes vsAppleSubscription  =
  sendMsg vsAppleSubscription (mkSelector "productCodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductCodes:@
setProductCodes :: (IsVSAppleSubscription vsAppleSubscription, IsNSArray value) => vsAppleSubscription -> value -> IO ()
setProductCodes vsAppleSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAppleSubscription (mkSelector "setProductCodes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCustomerID:productCodes:@
initWithCustomerID_productCodesSelector :: Selector
initWithCustomerID_productCodesSelector = mkSelector "initWithCustomerID:productCodes:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @customerID@
customerIDSelector :: Selector
customerIDSelector = mkSelector "customerID"

-- | @Selector@ for @setCustomerID:@
setCustomerIDSelector :: Selector
setCustomerIDSelector = mkSelector "setCustomerID:"

-- | @Selector@ for @productCodes@
productCodesSelector :: Selector
productCodesSelector = mkSelector "productCodes"

-- | @Selector@ for @setProductCodes:@
setProductCodesSelector :: Selector
setProductCodesSelector = mkSelector "setProductCodes:"

