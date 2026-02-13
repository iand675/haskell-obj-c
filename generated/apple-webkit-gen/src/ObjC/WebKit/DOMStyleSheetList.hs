{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMStyleSheetList@.
module ObjC.WebKit.DOMStyleSheetList
  ( DOMStyleSheetList
  , IsDOMStyleSheetList(..)
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
item :: IsDOMStyleSheetList domStyleSheetList => domStyleSheetList -> CUInt -> IO (Id DOMStyleSheet)
item domStyleSheetList index =
  sendMessage domStyleSheetList itemSelector index

-- | @- length@
length_ :: IsDOMStyleSheetList domStyleSheetList => domStyleSheetList -> IO CUInt
length_ domStyleSheetList =
  sendMessage domStyleSheetList lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMStyleSheet)
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

