{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMMediaList@.
module ObjC.WebKit.DOMMediaList
  ( DOMMediaList
  , IsDOMMediaList(..)
  , item
  , deleteMedium
  , appendMedium
  , mediaText
  , setMediaText
  , length_
  , appendMediumSelector
  , deleteMediumSelector
  , itemSelector
  , lengthSelector
  , mediaTextSelector
  , setMediaTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- item:@
item :: IsDOMMediaList domMediaList => domMediaList -> CUInt -> IO (Id NSString)
item domMediaList index =
  sendMessage domMediaList itemSelector index

-- | @- deleteMedium:@
deleteMedium :: (IsDOMMediaList domMediaList, IsNSString oldMedium) => domMediaList -> oldMedium -> IO ()
deleteMedium domMediaList oldMedium =
  sendMessage domMediaList deleteMediumSelector (toNSString oldMedium)

-- | @- appendMedium:@
appendMedium :: (IsDOMMediaList domMediaList, IsNSString newMedium) => domMediaList -> newMedium -> IO ()
appendMedium domMediaList newMedium =
  sendMessage domMediaList appendMediumSelector (toNSString newMedium)

-- | @- mediaText@
mediaText :: IsDOMMediaList domMediaList => domMediaList -> IO (Id NSString)
mediaText domMediaList =
  sendMessage domMediaList mediaTextSelector

-- | @- setMediaText:@
setMediaText :: (IsDOMMediaList domMediaList, IsNSString value) => domMediaList -> value -> IO ()
setMediaText domMediaList value =
  sendMessage domMediaList setMediaTextSelector (toNSString value)

-- | @- length@
length_ :: IsDOMMediaList domMediaList => domMediaList -> IO CUInt
length_ domMediaList =
  sendMessage domMediaList lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id NSString)
itemSelector = mkSelector "item:"

-- | @Selector@ for @deleteMedium:@
deleteMediumSelector :: Selector '[Id NSString] ()
deleteMediumSelector = mkSelector "deleteMedium:"

-- | @Selector@ for @appendMedium:@
appendMediumSelector :: Selector '[Id NSString] ()
appendMediumSelector = mkSelector "appendMedium:"

-- | @Selector@ for @mediaText@
mediaTextSelector :: Selector '[] (Id NSString)
mediaTextSelector = mkSelector "mediaText"

-- | @Selector@ for @setMediaText:@
setMediaTextSelector :: Selector '[Id NSString] ()
setMediaTextSelector = mkSelector "setMediaText:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

