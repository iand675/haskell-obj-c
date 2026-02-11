{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUpdateMediaAffinityIntent@.
module ObjC.Intents.INUpdateMediaAffinityIntent
  ( INUpdateMediaAffinityIntent
  , IsINUpdateMediaAffinityIntent(..)
  , initWithMediaItems_mediaSearch_affinityType
  , mediaItems
  , mediaSearch
  , affinityType
  , initWithMediaItems_mediaSearch_affinityTypeSelector
  , mediaItemsSelector
  , mediaSearchSelector
  , affinityTypeSelector

  -- * Enum types
  , INMediaAffinityType(INMediaAffinityType)
  , pattern INMediaAffinityTypeUnknown
  , pattern INMediaAffinityTypeLike
  , pattern INMediaAffinityTypeDislike

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

-- | @- initWithMediaItems:mediaSearch:affinityType:@
initWithMediaItems_mediaSearch_affinityType :: (IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent, IsNSArray mediaItems, IsINMediaSearch mediaSearch) => inUpdateMediaAffinityIntent -> mediaItems -> mediaSearch -> INMediaAffinityType -> IO (Id INUpdateMediaAffinityIntent)
initWithMediaItems_mediaSearch_affinityType inUpdateMediaAffinityIntent  mediaItems mediaSearch affinityType =
withObjCPtr mediaItems $ \raw_mediaItems ->
  withObjCPtr mediaSearch $ \raw_mediaSearch ->
      sendMsg inUpdateMediaAffinityIntent (mkSelector "initWithMediaItems:mediaSearch:affinityType:") (retPtr retVoid) [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr raw_mediaSearch :: Ptr ()), argCLong (coerce affinityType)] >>= ownedObject . castPtr

-- | @- mediaItems@
mediaItems :: IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent => inUpdateMediaAffinityIntent -> IO (Id NSArray)
mediaItems inUpdateMediaAffinityIntent  =
  sendMsg inUpdateMediaAffinityIntent (mkSelector "mediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaSearch@
mediaSearch :: IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent => inUpdateMediaAffinityIntent -> IO (Id INMediaSearch)
mediaSearch inUpdateMediaAffinityIntent  =
  sendMsg inUpdateMediaAffinityIntent (mkSelector "mediaSearch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- affinityType@
affinityType :: IsINUpdateMediaAffinityIntent inUpdateMediaAffinityIntent => inUpdateMediaAffinityIntent -> IO INMediaAffinityType
affinityType inUpdateMediaAffinityIntent  =
  fmap (coerce :: CLong -> INMediaAffinityType) $ sendMsg inUpdateMediaAffinityIntent (mkSelector "affinityType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaSearch:affinityType:@
initWithMediaItems_mediaSearch_affinityTypeSelector :: Selector
initWithMediaItems_mediaSearch_affinityTypeSelector = mkSelector "initWithMediaItems:mediaSearch:affinityType:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector
mediaSearchSelector = mkSelector "mediaSearch"

-- | @Selector@ for @affinityType@
affinityTypeSelector :: Selector
affinityTypeSelector = mkSelector "affinityType"

