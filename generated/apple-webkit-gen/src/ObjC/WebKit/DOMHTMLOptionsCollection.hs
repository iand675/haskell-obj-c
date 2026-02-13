{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLOptionsCollection@.
module ObjC.WebKit.DOMHTMLOptionsCollection
  ( DOMHTMLOptionsCollection
  , IsDOMHTMLOptionsCollection(..)
  , namedItem
  , add_index
  , remove
  , item
  , selectedIndex
  , setSelectedIndex
  , length_
  , setLength
  , add_indexSelector
  , itemSelector
  , lengthSelector
  , namedItemSelector
  , removeSelector
  , selectedIndexSelector
  , setLengthSelector
  , setSelectedIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- namedItem:@
namedItem :: (IsDOMHTMLOptionsCollection domhtmlOptionsCollection, IsNSString name) => domhtmlOptionsCollection -> name -> IO (Id DOMNode)
namedItem domhtmlOptionsCollection name =
  sendMessage domhtmlOptionsCollection namedItemSelector (toNSString name)

-- | @- add:index:@
add_index :: (IsDOMHTMLOptionsCollection domhtmlOptionsCollection, IsDOMHTMLOptionElement option) => domhtmlOptionsCollection -> option -> CUInt -> IO ()
add_index domhtmlOptionsCollection option index =
  sendMessage domhtmlOptionsCollection add_indexSelector (toDOMHTMLOptionElement option) index

-- | @- remove:@
remove :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CUInt -> IO ()
remove domhtmlOptionsCollection index =
  sendMessage domhtmlOptionsCollection removeSelector index

-- | @- item:@
item :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CUInt -> IO (Id DOMNode)
item domhtmlOptionsCollection index =
  sendMessage domhtmlOptionsCollection itemSelector index

-- | @- selectedIndex@
selectedIndex :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> IO CInt
selectedIndex domhtmlOptionsCollection =
  sendMessage domhtmlOptionsCollection selectedIndexSelector

-- | @- setSelectedIndex:@
setSelectedIndex :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CInt -> IO ()
setSelectedIndex domhtmlOptionsCollection value =
  sendMessage domhtmlOptionsCollection setSelectedIndexSelector value

-- | @- length@
length_ :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> IO CUInt
length_ domhtmlOptionsCollection =
  sendMessage domhtmlOptionsCollection lengthSelector

-- | @- setLength:@
setLength :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CUInt -> IO ()
setLength domhtmlOptionsCollection value =
  sendMessage domhtmlOptionsCollection setLengthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @namedItem:@
namedItemSelector :: Selector '[Id NSString] (Id DOMNode)
namedItemSelector = mkSelector "namedItem:"

-- | @Selector@ for @add:index:@
add_indexSelector :: Selector '[Id DOMHTMLOptionElement, CUInt] ()
add_indexSelector = mkSelector "add:index:"

-- | @Selector@ for @remove:@
removeSelector :: Selector '[CUInt] ()
removeSelector = mkSelector "remove:"

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMNode)
itemSelector = mkSelector "item:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CInt
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CInt] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector '[CUInt] ()
setLengthSelector = mkSelector "setLength:"

