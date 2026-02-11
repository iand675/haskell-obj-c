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
  , namedItemSelector
  , tagsSelector
  , lengthSelector


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

-- | @- item:@
item :: IsDOMHTMLCollection domhtmlCollection => domhtmlCollection -> CUInt -> IO (Id DOMNode)
item domhtmlCollection  index =
  sendMsg domhtmlCollection (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- namedItem:@
namedItem :: (IsDOMHTMLCollection domhtmlCollection, IsNSString name) => domhtmlCollection -> name -> IO (Id DOMNode)
namedItem domhtmlCollection  name =
withObjCPtr name $ \raw_name ->
    sendMsg domhtmlCollection (mkSelector "namedItem:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- tags:@
tags :: (IsDOMHTMLCollection domhtmlCollection, IsNSString name) => domhtmlCollection -> name -> IO (Id DOMNodeList)
tags domhtmlCollection  name =
withObjCPtr name $ \raw_name ->
    sendMsg domhtmlCollection (mkSelector "tags:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMHTMLCollection domhtmlCollection => domhtmlCollection -> IO CUInt
length_ domhtmlCollection  =
  sendMsg domhtmlCollection (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @namedItem:@
namedItemSelector :: Selector
namedItemSelector = mkSelector "namedItem:"

-- | @Selector@ for @tags:@
tagsSelector :: Selector
tagsSelector = mkSelector "tags:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

