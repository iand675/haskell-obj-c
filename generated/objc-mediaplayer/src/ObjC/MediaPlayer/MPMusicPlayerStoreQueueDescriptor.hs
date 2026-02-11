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
  , setStartTime_forItemWithStoreIDSelector
  , setEndTime_forItemWithStoreIDSelector
  , storeIDsSelector
  , setStoreIDsSelector
  , startItemIDSelector
  , setStartItemIDSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithStoreIDs:@
initWithStoreIDs :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSArray storeIDs) => mpMusicPlayerStoreQueueDescriptor -> storeIDs -> IO (Id MPMusicPlayerStoreQueueDescriptor)
initWithStoreIDs mpMusicPlayerStoreQueueDescriptor  storeIDs =
withObjCPtr storeIDs $ \raw_storeIDs ->
    sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "initWithStoreIDs:") (retPtr retVoid) [argPtr (castPtr raw_storeIDs :: Ptr ())] >>= ownedObject . castPtr

-- | @- setStartTime:forItemWithStoreID:@
setStartTime_forItemWithStoreID :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSString storeID) => mpMusicPlayerStoreQueueDescriptor -> CDouble -> storeID -> IO ()
setStartTime_forItemWithStoreID mpMusicPlayerStoreQueueDescriptor  startTime storeID =
withObjCPtr storeID $ \raw_storeID ->
    sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "setStartTime:forItemWithStoreID:") retVoid [argCDouble (fromIntegral startTime), argPtr (castPtr raw_storeID :: Ptr ())]

-- | @- setEndTime:forItemWithStoreID:@
setEndTime_forItemWithStoreID :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSString storeID) => mpMusicPlayerStoreQueueDescriptor -> CDouble -> storeID -> IO ()
setEndTime_forItemWithStoreID mpMusicPlayerStoreQueueDescriptor  endTime storeID =
withObjCPtr storeID $ \raw_storeID ->
    sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "setEndTime:forItemWithStoreID:") retVoid [argCDouble (fromIntegral endTime), argPtr (castPtr raw_storeID :: Ptr ())]

-- | @- storeIDs@
storeIDs :: IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor => mpMusicPlayerStoreQueueDescriptor -> IO (Id NSArray)
storeIDs mpMusicPlayerStoreQueueDescriptor  =
  sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "storeIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStoreIDs:@
setStoreIDs :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSArray value) => mpMusicPlayerStoreQueueDescriptor -> value -> IO ()
setStoreIDs mpMusicPlayerStoreQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "setStoreIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startItemID@
startItemID :: IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor => mpMusicPlayerStoreQueueDescriptor -> IO (Id NSString)
startItemID mpMusicPlayerStoreQueueDescriptor  =
  sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "startItemID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartItemID:@
setStartItemID :: (IsMPMusicPlayerStoreQueueDescriptor mpMusicPlayerStoreQueueDescriptor, IsNSString value) => mpMusicPlayerStoreQueueDescriptor -> value -> IO ()
setStartItemID mpMusicPlayerStoreQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMusicPlayerStoreQueueDescriptor (mkSelector "setStartItemID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStoreIDs:@
initWithStoreIDsSelector :: Selector
initWithStoreIDsSelector = mkSelector "initWithStoreIDs:"

-- | @Selector@ for @setStartTime:forItemWithStoreID:@
setStartTime_forItemWithStoreIDSelector :: Selector
setStartTime_forItemWithStoreIDSelector = mkSelector "setStartTime:forItemWithStoreID:"

-- | @Selector@ for @setEndTime:forItemWithStoreID:@
setEndTime_forItemWithStoreIDSelector :: Selector
setEndTime_forItemWithStoreIDSelector = mkSelector "setEndTime:forItemWithStoreID:"

-- | @Selector@ for @storeIDs@
storeIDsSelector :: Selector
storeIDsSelector = mkSelector "storeIDs"

-- | @Selector@ for @setStoreIDs:@
setStoreIDsSelector :: Selector
setStoreIDsSelector = mkSelector "setStoreIDs:"

-- | @Selector@ for @startItemID@
startItemIDSelector :: Selector
startItemIDSelector = mkSelector "startItemID"

-- | @Selector@ for @setStartItemID:@
setStartItemIDSelector :: Selector
setStartItemIDSelector = mkSelector "setStartItemID:"

