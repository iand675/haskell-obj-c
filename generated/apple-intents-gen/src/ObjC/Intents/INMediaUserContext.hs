{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaUserContext@.
module ObjC.Intents.INMediaUserContext
  ( INMediaUserContext
  , IsINMediaUserContext(..)
  , init_
  , subscriptionStatus
  , setSubscriptionStatus
  , numberOfLibraryItems
  , setNumberOfLibraryItems
  , initSelector
  , numberOfLibraryItemsSelector
  , setNumberOfLibraryItemsSelector
  , setSubscriptionStatusSelector
  , subscriptionStatusSelector

  -- * Enum types
  , INMediaUserContextSubscriptionStatus(INMediaUserContextSubscriptionStatus)
  , pattern INMediaUserContextSubscriptionStatusUnknown
  , pattern INMediaUserContextSubscriptionStatusNotSubscribed
  , pattern INMediaUserContextSubscriptionStatusSubscribed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> IO (Id INMediaUserContext)
init_ inMediaUserContext =
  sendOwnedMessage inMediaUserContext initSelector

-- | Used as a signal of user affinity for the app
--
-- ObjC selector: @- subscriptionStatus@
subscriptionStatus :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> IO INMediaUserContextSubscriptionStatus
subscriptionStatus inMediaUserContext =
  sendMessage inMediaUserContext subscriptionStatusSelector

-- | Used as a signal of user affinity for the app
--
-- ObjC selector: @- setSubscriptionStatus:@
setSubscriptionStatus :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> INMediaUserContextSubscriptionStatus -> IO ()
setSubscriptionStatus inMediaUserContext value =
  sendMessage inMediaUserContext setSubscriptionStatusSelector value

-- | Approximate number of relevant items available in the user's library (playlists, songs, podcasts, albums, etc.) - used as a signal of user affinity for the app
--
-- ObjC selector: @- numberOfLibraryItems@
numberOfLibraryItems :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> IO (Id NSNumber)
numberOfLibraryItems inMediaUserContext =
  sendMessage inMediaUserContext numberOfLibraryItemsSelector

-- | Approximate number of relevant items available in the user's library (playlists, songs, podcasts, albums, etc.) - used as a signal of user affinity for the app
--
-- ObjC selector: @- setNumberOfLibraryItems:@
setNumberOfLibraryItems :: (IsINMediaUserContext inMediaUserContext, IsNSNumber value) => inMediaUserContext -> value -> IO ()
setNumberOfLibraryItems inMediaUserContext value =
  sendMessage inMediaUserContext setNumberOfLibraryItemsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INMediaUserContext)
initSelector = mkSelector "init"

-- | @Selector@ for @subscriptionStatus@
subscriptionStatusSelector :: Selector '[] INMediaUserContextSubscriptionStatus
subscriptionStatusSelector = mkSelector "subscriptionStatus"

-- | @Selector@ for @setSubscriptionStatus:@
setSubscriptionStatusSelector :: Selector '[INMediaUserContextSubscriptionStatus] ()
setSubscriptionStatusSelector = mkSelector "setSubscriptionStatus:"

-- | @Selector@ for @numberOfLibraryItems@
numberOfLibraryItemsSelector :: Selector '[] (Id NSNumber)
numberOfLibraryItemsSelector = mkSelector "numberOfLibraryItems"

-- | @Selector@ for @setNumberOfLibraryItems:@
setNumberOfLibraryItemsSelector :: Selector '[Id NSNumber] ()
setNumberOfLibraryItemsSelector = mkSelector "setNumberOfLibraryItems:"

