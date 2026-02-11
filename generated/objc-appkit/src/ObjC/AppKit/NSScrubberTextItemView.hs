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
  , textFieldSelector
  , titleSelector
  , setTitleSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- textField@
textField :: IsNSScrubberTextItemView nsScrubberTextItemView => nsScrubberTextItemView -> IO (Id NSTextField)
textField nsScrubberTextItemView  =
  sendMsg nsScrubberTextItemView (mkSelector "textField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSScrubberTextItemView nsScrubberTextItemView => nsScrubberTextItemView -> IO (Id NSString)
title nsScrubberTextItemView  =
  sendMsg nsScrubberTextItemView (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSScrubberTextItemView nsScrubberTextItemView, IsNSString value) => nsScrubberTextItemView -> value -> IO ()
setTitle nsScrubberTextItemView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScrubberTextItemView (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textField@
textFieldSelector :: Selector
textFieldSelector = mkSelector "textField"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

