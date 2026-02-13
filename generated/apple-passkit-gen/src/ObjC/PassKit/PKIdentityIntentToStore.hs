{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , mayStoreIntentForDaysSelector
  , mayStoreIntentSelector
  , newSelector
  , willNotStoreIntentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' mayStoreIntentForDaysSelector days

-- | @- init@
init_ :: IsPKIdentityIntentToStore pkIdentityIntentToStore => pkIdentityIntentToStore -> IO (Id PKIdentityIntentToStore)
init_ pkIdentityIntentToStore =
  sendOwnedMessage pkIdentityIntentToStore initSelector

-- | @+ new@
new :: IO (Id PKIdentityIntentToStore)
new  =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendOwnedClassMessage cls' newSelector

-- | Indicates the data element returned in the response will not be stored for a period longer than necessary to process the result in realtime.
--
-- ObjC selector: @+ willNotStoreIntent@
willNotStoreIntent :: IO (Id PKIdentityIntentToStore)
willNotStoreIntent  =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendClassMessage cls' willNotStoreIntentSelector

-- | Indicates the data element may be stored for an indefinite length of time.
--
-- ObjC selector: @+ mayStoreIntent@
mayStoreIntent :: IO (Id PKIdentityIntentToStore)
mayStoreIntent  =
  do
    cls' <- getRequiredClass "PKIdentityIntentToStore"
    sendClassMessage cls' mayStoreIntentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mayStoreIntentForDays:@
mayStoreIntentForDaysSelector :: Selector '[CLong] (Id PKIdentityIntentToStore)
mayStoreIntentForDaysSelector = mkSelector "mayStoreIntentForDays:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIdentityIntentToStore)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKIdentityIntentToStore)
newSelector = mkSelector "new"

-- | @Selector@ for @willNotStoreIntent@
willNotStoreIntentSelector :: Selector '[] (Id PKIdentityIntentToStore)
willNotStoreIntentSelector = mkSelector "willNotStoreIntent"

-- | @Selector@ for @mayStoreIntent@
mayStoreIntentSelector :: Selector '[] (Id PKIdentityIntentToStore)
mayStoreIntentSelector = mkSelector "mayStoreIntent"

