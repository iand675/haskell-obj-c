{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForMediaIntent@.
module ObjC.Intents.INSearchForMediaIntent
  ( INSearchForMediaIntent
  , IsINSearchForMediaIntent(..)
  , initWithMediaItems_mediaSearch
  , mediaItems
  , mediaSearch
  , initWithMediaItems_mediaSearchSelector
  , mediaItemsSelector
  , mediaSearchSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithMediaItems:mediaSearch:@
initWithMediaItems_mediaSearch :: (IsINSearchForMediaIntent inSearchForMediaIntent, IsNSArray mediaItems, IsINMediaSearch mediaSearch) => inSearchForMediaIntent -> mediaItems -> mediaSearch -> IO (Id INSearchForMediaIntent)
initWithMediaItems_mediaSearch inSearchForMediaIntent  mediaItems mediaSearch =
withObjCPtr mediaItems $ \raw_mediaItems ->
  withObjCPtr mediaSearch $ \raw_mediaSearch ->
      sendMsg inSearchForMediaIntent (mkSelector "initWithMediaItems:mediaSearch:") (retPtr retVoid) [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr raw_mediaSearch :: Ptr ())] >>= ownedObject . castPtr

-- | @- mediaItems@
mediaItems :: IsINSearchForMediaIntent inSearchForMediaIntent => inSearchForMediaIntent -> IO (Id NSArray)
mediaItems inSearchForMediaIntent  =
  sendMsg inSearchForMediaIntent (mkSelector "mediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaSearch@
mediaSearch :: IsINSearchForMediaIntent inSearchForMediaIntent => inSearchForMediaIntent -> IO (Id INMediaSearch)
mediaSearch inSearchForMediaIntent  =
  sendMsg inSearchForMediaIntent (mkSelector "mediaSearch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaSearch:@
initWithMediaItems_mediaSearchSelector :: Selector
initWithMediaItems_mediaSearchSelector = mkSelector "initWithMediaItems:mediaSearch:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector
mediaSearchSelector = mkSelector "mediaSearch"

