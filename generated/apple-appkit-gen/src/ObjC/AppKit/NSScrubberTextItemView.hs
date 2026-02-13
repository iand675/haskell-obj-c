{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberTextItemView
--
-- A simple @NSScrubberItemView@ for displaying text. The -fittingSize method can be used to measure the smallest size for the view which fits the title without truncating.
--
-- Generated bindings for @NSScrubberTextItemView@.
module ObjC.AppKit.NSScrubberTextItemView
  ( NSScrubberTextItemView
  , IsNSScrubberTextItemView(..)
  , textField
  , title
  , setTitle
  , setTitleSelector
  , textFieldSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- textField@
textField :: IsNSScrubberTextItemView nsScrubberTextItemView => nsScrubberTextItemView -> IO (Id NSTextField)
textField nsScrubberTextItemView =
  sendMessage nsScrubberTextItemView textFieldSelector

-- | @- title@
title :: IsNSScrubberTextItemView nsScrubberTextItemView => nsScrubberTextItemView -> IO (Id NSString)
title nsScrubberTextItemView =
  sendMessage nsScrubberTextItemView titleSelector

-- | @- setTitle:@
setTitle :: (IsNSScrubberTextItemView nsScrubberTextItemView, IsNSString value) => nsScrubberTextItemView -> value -> IO ()
setTitle nsScrubberTextItemView value =
  sendMessage nsScrubberTextItemView setTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textField@
textFieldSelector :: Selector '[] (Id NSTextField)
textFieldSelector = mkSelector "textField"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

