{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKBackForwardListItem object represents a webpage in the back-forward list of a web view.
--
-- Generated bindings for @WKBackForwardListItem@.
module ObjC.WebKit.WKBackForwardListItem
  ( WKBackForwardListItem
  , IsWKBackForwardListItem(..)
  , init_
  , url
  , title
  , initialURL
  , initSelector
  , urlSelector
  , titleSelector
  , initialURLSelector


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

-- | @- init@
init_ :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id WKBackForwardListItem)
init_ wkBackForwardListItem  =
  sendMsg wkBackForwardListItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The URL of the webpage represented by this item.
--
-- ObjC selector: @- URL@
url :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id NSURL)
url wkBackForwardListItem  =
  sendMsg wkBackForwardListItem (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title of the webpage represented by this item.
--
-- ObjC selector: @- title@
title :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id NSString)
title wkBackForwardListItem  =
  sendMsg wkBackForwardListItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL of the initial request that created this item.
--
-- ObjC selector: @- initialURL@
initialURL :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id NSURL)
initialURL wkBackForwardListItem  =
  sendMsg wkBackForwardListItem (mkSelector "initialURL") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @initialURL@
initialURLSelector :: Selector
initialURLSelector = mkSelector "initialURL"

