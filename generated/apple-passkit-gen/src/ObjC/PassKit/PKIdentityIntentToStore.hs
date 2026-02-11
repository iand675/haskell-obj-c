{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Indicates your intention to store an identity element. This covers not only the element value, but also information derived from the element value such as signatures or digests.
--
-- Generated bindings for @PKIdentityIntentToStore@.
module ObjC.PassKit.PKIdentityIntentToStore
  ( PKIdentityIntentToStore
  , IsPKIdentityIntentToStore(..)
  , mayStoreIntentForDays
  , init_
  , new
  , willNotStoreIntent
  , mayStoreIntent
  , mayStoreIntentForDaysSelector
  , initSelector
  , newSelector
  , willNotStoreIntentSelector
  , mayStoreIntentSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Indicates the data element may be stored for no longer than than the provided number of days.
--
-- ObjC selector: @+ mayStoreIntentForDays:@
mayStoreIntentForDays :: CLong -> IO (Id PKIdentityIntentToStore)
mayStoreIntentForDays days =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendClassMsg cls' (mkSelector "mayStoreIntentForDays:") (retPtr retVoid) [argCLong days] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsPKIdentityIntentToStore pkIdentityIntentToStore => pkIdentityIntentToStore -> IO (Id PKIdentityIntentToStore)
init_ pkIdentityIntentToStore  =
    sendMsg pkIdentityIntentToStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKIdentityIntentToStore)
new  =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates the data element returned in the response will not be stored for a period longer than necessary to process the result in realtime.
--
-- ObjC selector: @+ willNotStoreIntent@
willNotStoreIntent :: IO (Id PKIdentityIntentToStore)
willNotStoreIntent  =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendClassMsg cls' (mkSelector "willNotStoreIntent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the data element may be stored for an indefinite length of time.
--
-- ObjC selector: @+ mayStoreIntent@
mayStoreIntent :: IO (Id PKIdentityIntentToStore)
mayStoreIntent  =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendClassMsg cls' (mkSelector "mayStoreIntent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mayStoreIntentForDays:@
mayStoreIntentForDaysSelector :: Selector
mayStoreIntentForDaysSelector = mkSelector "mayStoreIntentForDays:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @willNotStoreIntent@
willNotStoreIntentSelector :: Selector
willNotStoreIntentSelector = mkSelector "willNotStoreIntent"

-- | @Selector@ for @mayStoreIntent@
mayStoreIntentSelector :: Selector
mayStoreIntentSelector = mkSelector "mayStoreIntent"

