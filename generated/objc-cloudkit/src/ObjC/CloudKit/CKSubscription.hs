{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSubscription@.
module ObjC.CloudKit.CKSubscription
  ( CKSubscription
  , IsCKSubscription(..)
  , init_
  , new
  , subscriptionID
  , subscriptionType
  , initSelector
  , newSelector
  , subscriptionIDSelector
  , subscriptionTypeSelector

  -- * Enum types
  , CKSubscriptionType(CKSubscriptionType)
  , pattern CKSubscriptionTypeQuery
  , pattern CKSubscriptionTypeRecordZone
  , pattern CKSubscriptionTypeDatabase

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

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSubscription ckSubscription => ckSubscription -> IO (Id CKSubscription)
init_ ckSubscription  =
  sendMsg ckSubscription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSubscription)
new  =
  do
    cls' <- getRequiredClass "CKSubscription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- subscriptionID@
subscriptionID :: IsCKSubscription ckSubscription => ckSubscription -> IO (Id NSString)
subscriptionID ckSubscription  =
  sendMsg ckSubscription (mkSelector "subscriptionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subscriptionType@
subscriptionType :: IsCKSubscription ckSubscription => ckSubscription -> IO CKSubscriptionType
subscriptionType ckSubscription  =
  fmap (coerce :: CLong -> CKSubscriptionType) $ sendMsg ckSubscription (mkSelector "subscriptionType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @subscriptionID@
subscriptionIDSelector :: Selector
subscriptionIDSelector = mkSelector "subscriptionID"

-- | @Selector@ for @subscriptionType@
subscriptionTypeSelector :: Selector
subscriptionTypeSelector = mkSelector "subscriptionType"

