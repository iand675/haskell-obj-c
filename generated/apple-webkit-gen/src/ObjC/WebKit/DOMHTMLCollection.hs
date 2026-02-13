{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLCollection@.
module ObjC.WebKit.DOMHTMLCollection
  ( DOMHTMLCollection
  , IsDOMHTMLCollection(..)
  , item
  , namedItem
  , tags
  , length_
  , itemSelector
  , lengthSelector
  , namedItemSelector
  , tagsSelector


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
item :: IsDOMHTMLCollection domhtmlCollection => domhtmlCollection -> CUInt -> IO (Id DOMNode)
item domhtmlCollection index =
  sendMessage domhtmlCollection itemSelector index

-- | @- namedItem:@
namedItem :: (IsDOMHTMLCollection domhtmlCollection, IsNSString name) => domhtmlCollection -> name -> IO (Id DOMNode)
namedItem domhtmlCollection name =
  sendMessage domhtmlCollection namedItemSelector (toNSString name)

-- | @- tags:@
tags :: (IsDOMHTMLCollection domhtmlCollection, IsNSString name) => domhtmlCollection -> name -> IO (Id DOMNodeList)
tags domhtmlCollection name =
  sendMessage domhtmlCollection tagsSelector (toNSString name)

-- | @- length@
length_ :: IsDOMHTMLCollection domhtmlCollection => domhtmlCollection -> IO CUInt
length_ domhtmlCollection =
  sendMessage domhtmlCollection lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMNode)
itemSelector = mkSelector "item:"

-- | @Selector@ for @namedItem:@
namedItemSelector :: Selector '[Id NSString] (Id DOMNode)
namedItemSelector = mkSelector "namedItem:"

-- | @Selector@ for @tags:@
tagsSelector :: Selector '[Id NSString] (Id DOMNodeList)
tagsSelector = mkSelector "tags:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

