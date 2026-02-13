{-# LANGUAGE DataKinds #-}
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
  , initialURLSelector
  , titleSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id WKBackForwardListItem)
init_ wkBackForwardListItem =
  sendOwnedMessage wkBackForwardListItem initSelector

-- | The URL of the webpage represented by this item.
--
-- ObjC selector: @- URL@
url :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id NSURL)
url wkBackForwardListItem =
  sendMessage wkBackForwardListItem urlSelector

-- | The title of the webpage represented by this item.
--
-- ObjC selector: @- title@
title :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id NSString)
title wkBackForwardListItem =
  sendMessage wkBackForwardListItem titleSelector

-- | The URL of the initial request that created this item.
--
-- ObjC selector: @- initialURL@
initialURL :: IsWKBackForwardListItem wkBackForwardListItem => wkBackForwardListItem -> IO (Id NSURL)
initialURL wkBackForwardListItem =
  sendOwnedMessage wkBackForwardListItem initialURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKBackForwardListItem)
initSelector = mkSelector "init"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @initialURL@
initialURLSelector :: Selector '[] (Id NSURL)
initialURLSelector = mkSelector "initialURL"

