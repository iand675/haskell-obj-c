{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaIntent@.
module ObjC.Intents.INAddMediaIntent
  ( INAddMediaIntent
  , IsINAddMediaIntent(..)
  , initWithMediaItems_mediaSearch_mediaDestination
  , mediaItems
  , mediaSearch
  , mediaDestination
  , initWithMediaItems_mediaSearch_mediaDestinationSelector
  , mediaItemsSelector
  , mediaSearchSelector
  , mediaDestinationSelector


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

-- | @- initWithMediaItems:mediaSearch:mediaDestination:@
initWithMediaItems_mediaSearch_mediaDestination :: (IsINAddMediaIntent inAddMediaIntent, IsNSArray mediaItems, IsINMediaSearch mediaSearch, IsINMediaDestination mediaDestination) => inAddMediaIntent -> mediaItems -> mediaSearch -> mediaDestination -> IO (Id INAddMediaIntent)
initWithMediaItems_mediaSearch_mediaDestination inAddMediaIntent  mediaItems mediaSearch mediaDestination =
withObjCPtr mediaItems $ \raw_mediaItems ->
  withObjCPtr mediaSearch $ \raw_mediaSearch ->
    withObjCPtr mediaDestination $ \raw_mediaDestination ->
        sendMsg inAddMediaIntent (mkSelector "initWithMediaItems:mediaSearch:mediaDestination:") (retPtr retVoid) [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr raw_mediaSearch :: Ptr ()), argPtr (castPtr raw_mediaDestination :: Ptr ())] >>= ownedObject . castPtr

-- | @- mediaItems@
mediaItems :: IsINAddMediaIntent inAddMediaIntent => inAddMediaIntent -> IO (Id NSArray)
mediaItems inAddMediaIntent  =
  sendMsg inAddMediaIntent (mkSelector "mediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaSearch@
mediaSearch :: IsINAddMediaIntent inAddMediaIntent => inAddMediaIntent -> IO (Id INMediaSearch)
mediaSearch inAddMediaIntent  =
  sendMsg inAddMediaIntent (mkSelector "mediaSearch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaDestination@
mediaDestination :: IsINAddMediaIntent inAddMediaIntent => inAddMediaIntent -> IO (Id INMediaDestination)
mediaDestination inAddMediaIntent  =
  sendMsg inAddMediaIntent (mkSelector "mediaDestination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMediaItems:mediaSearch:mediaDestination:@
initWithMediaItems_mediaSearch_mediaDestinationSelector :: Selector
initWithMediaItems_mediaSearch_mediaDestinationSelector = mkSelector "initWithMediaItems:mediaSearch:mediaDestination:"

-- | @Selector@ for @mediaItems@
mediaItemsSelector :: Selector
mediaItemsSelector = mkSelector "mediaItems"

-- | @Selector@ for @mediaSearch@
mediaSearchSelector :: Selector
mediaSearchSelector = mkSelector "mediaSearch"

-- | @Selector@ for @mediaDestination@
mediaDestinationSelector :: Selector
mediaDestinationSelector = mkSelector "mediaDestination"

