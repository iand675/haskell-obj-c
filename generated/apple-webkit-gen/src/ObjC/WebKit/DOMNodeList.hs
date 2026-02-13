{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMNodeList@.
module ObjC.WebKit.DOMNodeList
  ( DOMNodeList
  , IsDOMNodeList(..)
  , item
  , length_
  , itemSelector
  , lengthSelector


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
item :: IsDOMNodeList domNodeList => domNodeList -> CUInt -> IO (Id DOMNode)
item domNodeList index =
  sendMessage domNodeList itemSelector index

-- | @- length@
length_ :: IsDOMNodeList domNodeList => domNodeList -> IO CUInt
length_ domNodeList =
  sendMessage domNodeList lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMNode)
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

