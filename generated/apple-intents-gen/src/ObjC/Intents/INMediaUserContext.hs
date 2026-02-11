{-# LANGUAGE PatternSynonyms #-}
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
  , subscriptionStatusSelector
  , setSubscriptionStatusSelector
  , numberOfLibraryItemsSelector
  , setNumberOfLibraryItemsSelector

  -- * Enum types
  , INMediaUserContextSubscriptionStatus(INMediaUserContextSubscriptionStatus)
  , pattern INMediaUserContextSubscriptionStatusUnknown
  , pattern INMediaUserContextSubscriptionStatusNotSubscribed
  , pattern INMediaUserContextSubscriptionStatusSubscribed

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> IO (Id INMediaUserContext)
init_ inMediaUserContext  =
    sendMsg inMediaUserContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Used as a signal of user affinity for the app
--
-- ObjC selector: @- subscriptionStatus@
subscriptionStatus :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> IO INMediaUserContextSubscriptionStatus
subscriptionStatus inMediaUserContext  =
    fmap (coerce :: CLong -> INMediaUserContextSubscriptionStatus) $ sendMsg inMediaUserContext (mkSelector "subscriptionStatus") retCLong []

-- | Used as a signal of user affinity for the app
--
-- ObjC selector: @- setSubscriptionStatus:@
setSubscriptionStatus :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> INMediaUserContextSubscriptionStatus -> IO ()
setSubscriptionStatus inMediaUserContext  value =
    sendMsg inMediaUserContext (mkSelector "setSubscriptionStatus:") retVoid [argCLong (coerce value)]

-- | Approximate number of relevant items available in the user's library (playlists, songs, podcasts, albums, etc.) - used as a signal of user affinity for the app
--
-- ObjC selector: @- numberOfLibraryItems@
numberOfLibraryItems :: IsINMediaUserContext inMediaUserContext => inMediaUserContext -> IO (Id NSNumber)
numberOfLibraryItems inMediaUserContext  =
    sendMsg inMediaUserContext (mkSelector "numberOfLibraryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Approximate number of relevant items available in the user's library (playlists, songs, podcasts, albums, etc.) - used as a signal of user affinity for the app
--
-- ObjC selector: @- setNumberOfLibraryItems:@
setNumberOfLibraryItems :: (IsINMediaUserContext inMediaUserContext, IsNSNumber value) => inMediaUserContext -> value -> IO ()
setNumberOfLibraryItems inMediaUserContext  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inMediaUserContext (mkSelector "setNumberOfLibraryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @subscriptionStatus@
subscriptionStatusSelector :: Selector
subscriptionStatusSelector = mkSelector "subscriptionStatus"

-- | @Selector@ for @setSubscriptionStatus:@
setSubscriptionStatusSelector :: Selector
setSubscriptionStatusSelector = mkSelector "setSubscriptionStatus:"

-- | @Selector@ for @numberOfLibraryItems@
numberOfLibraryItemsSelector :: Selector
numberOfLibraryItemsSelector = mkSelector "numberOfLibraryItems"

-- | @Selector@ for @setNumberOfLibraryItems:@
setNumberOfLibraryItemsSelector :: Selector
setNumberOfLibraryItemsSelector = mkSelector "setNumberOfLibraryItems:"

