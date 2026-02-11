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
  , namedItemSelector
  , add_indexSelector
  , removeSelector
  , itemSelector
  , selectedIndexSelector
  , setSelectedIndexSelector
  , lengthSelector
  , setLengthSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- namedItem:@
namedItem :: (IsDOMHTMLOptionsCollection domhtmlOptionsCollection, IsNSString name) => domhtmlOptionsCollection -> name -> IO (Id DOMNode)
namedItem domhtmlOptionsCollection  name =
withObjCPtr name $ \raw_name ->
    sendMsg domhtmlOptionsCollection (mkSelector "namedItem:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- add:index:@
add_index :: (IsDOMHTMLOptionsCollection domhtmlOptionsCollection, IsDOMHTMLOptionElement option) => domhtmlOptionsCollection -> option -> CUInt -> IO ()
add_index domhtmlOptionsCollection  option index =
withObjCPtr option $ \raw_option ->
    sendMsg domhtmlOptionsCollection (mkSelector "add:index:") retVoid [argPtr (castPtr raw_option :: Ptr ()), argCUInt (fromIntegral index)]

-- | @- remove:@
remove :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CUInt -> IO ()
remove domhtmlOptionsCollection  index =
  sendMsg domhtmlOptionsCollection (mkSelector "remove:") retVoid [argCUInt (fromIntegral index)]

-- | @- item:@
item :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CUInt -> IO (Id DOMNode)
item domhtmlOptionsCollection  index =
  sendMsg domhtmlOptionsCollection (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- selectedIndex@
selectedIndex :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> IO CInt
selectedIndex domhtmlOptionsCollection  =
  sendMsg domhtmlOptionsCollection (mkSelector "selectedIndex") retCInt []

-- | @- setSelectedIndex:@
setSelectedIndex :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CInt -> IO ()
setSelectedIndex domhtmlOptionsCollection  value =
  sendMsg domhtmlOptionsCollection (mkSelector "setSelectedIndex:") retVoid [argCInt (fromIntegral value)]

-- | @- length@
length_ :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> IO CUInt
length_ domhtmlOptionsCollection  =
  sendMsg domhtmlOptionsCollection (mkSelector "length") retCUInt []

-- | @- setLength:@
setLength :: IsDOMHTMLOptionsCollection domhtmlOptionsCollection => domhtmlOptionsCollection -> CUInt -> IO ()
setLength domhtmlOptionsCollection  value =
  sendMsg domhtmlOptionsCollection (mkSelector "setLength:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @namedItem:@
namedItemSelector :: Selector
namedItemSelector = mkSelector "namedItem:"

-- | @Selector@ for @add:index:@
add_indexSelector :: Selector
add_indexSelector = mkSelector "add:index:"

-- | @Selector@ for @remove:@
removeSelector :: Selector
removeSelector = mkSelector "remove:"

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector
setLengthSelector = mkSelector "setLength:"

