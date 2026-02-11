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
  , initWithCoderSelector
  , encodeWithCoderSelector
  , initWithURLSelector
  , initWithMetadataSelector
  , metadataSelector
  , setMetadataSelector


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

import ObjC.LinkPresentation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsLPLinkView lpLinkView, IsNSCoder coder) => lpLinkView -> coder -> IO (Id LPLinkView)
initWithCoder lpLinkView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg lpLinkView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- encodeWithCoder:@
encodeWithCoder :: (IsLPLinkView lpLinkView, IsNSCoder coder) => lpLinkView -> coder -> IO ()
encodeWithCoder lpLinkView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg lpLinkView (mkSelector "encodeWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | Initializes a placeholder link view without metadata for a given URL.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsLPLinkView lpLinkView, IsNSURL url) => lpLinkView -> url -> IO (Id LPLinkView)
initWithURL lpLinkView  url =
withObjCPtr url $ \raw_url ->
    sendMsg lpLinkView (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a link view with specified metadata.
--
-- ObjC selector: @- initWithMetadata:@
initWithMetadata :: (IsLPLinkView lpLinkView, IsLPLinkMetadata metadata) => lpLinkView -> metadata -> IO (Id LPLinkView)
initWithMetadata lpLinkView  metadata =
withObjCPtr metadata $ \raw_metadata ->
    sendMsg lpLinkView (mkSelector "initWithMetadata:") (retPtr retVoid) [argPtr (castPtr raw_metadata :: Ptr ())] >>= ownedObject . castPtr

-- | The metadata from which to generate a rich presentation.
--
-- This can either be generated automatically from a URL by LPMetadataProvider, or manually constructed with the desired data.
--
-- ObjC selector: @- metadata@
metadata :: IsLPLinkView lpLinkView => lpLinkView -> IO (Id LPLinkMetadata)
metadata lpLinkView  =
  sendMsg lpLinkView (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The metadata from which to generate a rich presentation.
--
-- This can either be generated automatically from a URL by LPMetadataProvider, or manually constructed with the desired data.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsLPLinkView lpLinkView, IsLPLinkMetadata value) => lpLinkView -> value -> IO ()
setMetadata lpLinkView  value =
withObjCPtr value $ \raw_value ->
    sendMsg lpLinkView (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithMetadata:@
initWithMetadataSelector :: Selector
initWithMetadataSelector = mkSelector "initWithMetadata:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

