{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A rich visual representation of a link.
--
-- ``LPLinkView`` presents a link based on its available metadata. Use it to show a link’s title and icon, associated images, inline audio, video playback, and maps in a familiar and consistent style.
--
-- ## Present a rich link
--
-- To present a rich link in your app, create an ``LPLinkView``, passing an ``LPLinkMetadata`` instance into its initializer. Then add the ``LPLinkView`` to your view.
--
-- For example, to present links in a table view, add an ``LPLinkView`` instance as a subview when populating each cell.
--
-- ```swift let linkView = LPLinkView(metadata: metadata) cell.contentView.addSubview(linkView) linkView.sizeToFit() ```
--
-- ``LPLinkView`` has an intrinsic size, but it also responds to <doc://com.apple.documentation/documentation/uikit/uiview/1622630-sizetofit> to present a layout at any size.
--
-- Generated bindings for @LPLinkView@.
module ObjC.LinkPresentation.LPLinkView
  ( LPLinkView
  , IsLPLinkView(..)
  , initWithCoder
  , encodeWithCoder
  , initWithURL
  , initWithMetadata
  , metadata
  , setMetadata
  , encodeWithCoderSelector
  , initWithCoderSelector
  , initWithMetadataSelector
  , initWithURLSelector
  , metadataSelector
  , setMetadataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LinkPresentation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsLPLinkView lpLinkView, IsNSCoder coder) => lpLinkView -> coder -> IO (Id LPLinkView)
initWithCoder lpLinkView coder =
  sendOwnedMessage lpLinkView initWithCoderSelector (toNSCoder coder)

-- | @- encodeWithCoder:@
encodeWithCoder :: (IsLPLinkView lpLinkView, IsNSCoder coder) => lpLinkView -> coder -> IO ()
encodeWithCoder lpLinkView coder =
  sendMessage lpLinkView encodeWithCoderSelector (toNSCoder coder)

-- | Initializes a placeholder link view without metadata for a given URL.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsLPLinkView lpLinkView, IsNSURL url) => lpLinkView -> url -> IO (Id LPLinkView)
initWithURL lpLinkView url =
  sendOwnedMessage lpLinkView initWithURLSelector (toNSURL url)

-- | Initializes a link view with specified metadata.
--
-- ObjC selector: @- initWithMetadata:@
initWithMetadata :: (IsLPLinkView lpLinkView, IsLPLinkMetadata metadata) => lpLinkView -> metadata -> IO (Id LPLinkView)
initWithMetadata lpLinkView metadata =
  sendOwnedMessage lpLinkView initWithMetadataSelector (toLPLinkMetadata metadata)

-- | The metadata from which to generate a rich presentation.
--
-- This can either be generated automatically from a URL by LPMetadataProvider, or manually constructed with the desired data.
--
-- ObjC selector: @- metadata@
metadata :: IsLPLinkView lpLinkView => lpLinkView -> IO (Id LPLinkMetadata)
metadata lpLinkView =
  sendMessage lpLinkView metadataSelector

-- | The metadata from which to generate a rich presentation.
--
-- This can either be generated automatically from a URL by LPMetadataProvider, or manually constructed with the desired data.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsLPLinkView lpLinkView, IsLPLinkMetadata value) => lpLinkView -> value -> IO ()
setMetadata lpLinkView value =
  sendMessage lpLinkView setMetadataSelector (toLPLinkMetadata value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id LPLinkView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector '[Id NSCoder] ()
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id LPLinkView)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithMetadata:@
initWithMetadataSelector :: Selector '[Id LPLinkMetadata] (Id LPLinkView)
initWithMetadataSelector = mkSelector "initWithMetadata:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id LPLinkMetadata)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id LPLinkMetadata] ()
setMetadataSelector = mkSelector "setMetadata:"

