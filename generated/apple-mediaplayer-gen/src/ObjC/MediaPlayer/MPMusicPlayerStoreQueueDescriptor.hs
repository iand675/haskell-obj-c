{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerStoreQueueDescriptor@.
module ObjC.MediaPlayer.MPMusicPlayerStoreQueueDescriptor
  ( MPMusicPlayerStoreQueueDescriptor
  , IsMPMusicPlayerStoreQueueDescriptor(..)
  , initWithStoreIDs
  , setStartTime_forItemWithStoreID
  , setEndTime_forItemWithStoreID
  , storeIDs
  , setStoreIDs
  , startItemID
  , setStartItemID
  , initWithStoreIDsSelector
  , setEndTime_forItemWithStoreIDSelector
  , setStartItemIDSelector
  , setStartTime_forItemWithStoreIDSelector
  , setStoreIDsSelector
  , startItemIDSelector
  , storeIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithStoreIDs:@
initWithStoreIDs :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSArray storeIDs) => mpMusicPlayerStoreQueueDescriptor -> storeIDs -> IO (Id MPMusicPlayerStoreQueueDescriptor)
initWithStoreIDs mpMusicPlayerStoreQueueDescriptor storeIDs =
  sendOwnedMessage mpMusicPlayerStoreQueueDescriptor initWithStoreIDsSelector (toNSArray storeIDs)

-- | @- setStartTime:forItemWithStoreID:@
setStartTime_forItemWithStoreID :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSString storeID) => mpMusicPlayerStoreQueueDescriptor -> CDouble -> storeID -> IO ()
setStartTime_forItemWithStoreID mpMusicPlayerStoreQueueDescriptor startTime storeID =
  sendMessage mpMusicPlayerStoreQueueDescriptor setStartTime_forItemWithStoreIDSelector startTime (toNSString storeID)

-- | @- setEndTime:forItemWithStoreID:@
setEndTime_forItemWithStoreID :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSString storeID) => mpMusicPlayerStoreQueueDescriptor -> CDouble -> storeID -> IO ()
setEndTime_forItemWithStoreID mpMusicPlayerStoreQueueDescriptor endTime storeID =
  sendMessage mpMusicPlayerStoreQueueDescriptor setEndTime_forItemWithStoreIDSelector endTime (toNSString storeID)

-- | @- storeIDs@
storeIDs :: IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor => mpMusicPlayerStoreQueueDescriptor -> IO (Id NSArray)
storeIDs mpMusicPlayerStoreQueueDescriptor =
  sendMessage mpMusicPlayerStoreQueueDescriptor storeIDsSelector

-- | @- setStoreIDs:@
setStoreIDs :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSArray value) => mpMusicPlayerStoreQueueDescriptor -> value -> IO ()
setStoreIDs mpMusicPlayerStoreQueueDescriptor value =
  sendMessage mpMusicPlayerStoreQueueDescriptor setStoreIDsSelector (toNSArray value)

-- | @- startItemID@
startItemID :: IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor => mpMusicPlayerStoreQueueDescriptor -> IO (Id NSString)
startItemID mpMusicPlayerStoreQueueDescriptor =
  sendMessage mpMusicPlayerStoreQueueDescriptor startItemIDSelector

-- | @- setStartItemID:@
setStartItemID :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSString value) => mpMusicPlayerStoreQueueDescriptor -> value -> IO ()
setStartItemID mpMusicPlayerStoreQueueDescriptor value =
  sendMessage mpMusicPlayerStoreQueueDescriptor setStartItemIDSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStoreIDs:@
initWithStoreIDsSelector :: Selector '[Id NSArray] (Id MPMusicPlayerStoreQueueDescriptor)
initWithStoreIDsSelector = mkSelector "initWithStoreIDs:"

-- | @Selector@ for @setStartTime:forItemWithStoreID:@
setStartTime_forItemWithStoreIDSelector :: Selector '[CDouble, Id NSString] ()
setStartTime_forItemWithStoreIDSelector = mkSelector "setStartTime:forItemWithStoreID:"

-- | @Selector@ for @setEndTime:forItemWithStoreID:@
setEndTime_forItemWithStoreIDSelector :: Selector '[CDouble, Id NSString] ()
setEndTime_forItemWithStoreIDSelector = mkSelector "setEndTime:forItemWithStoreID:"

-- | @Selector@ for @storeIDs@
storeIDsSelector :: Selector '[] (Id NSArray)
storeIDsSelector = mkSelector "storeIDs"

-- | @Selector@ for @setStoreIDs:@
setStoreIDsSelector :: Selector '[Id NSArray] ()
setStoreIDsSelector = mkSelector "setStoreIDs:"

-- | @Selector@ for @startItemID@
startItemIDSelector :: Selector '[] (Id NSString)
startItemIDSelector = mkSelector "startItemID"

-- | @Selector@ for @setStartItemID:@
setStartItemIDSelector :: Selector '[Id NSString] ()
setStartItemIDSelector = mkSelector "setStartItemID:"

