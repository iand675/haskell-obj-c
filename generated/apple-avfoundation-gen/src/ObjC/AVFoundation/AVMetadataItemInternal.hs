{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataItem
--
-- AVMetadataItem represents an item of metadata associated with an audiovisual asset or with one of its tracks.
--
-- '	AVMetadataItems have keys that accord with the specification of the container format from which they're drawn. Full details of the metadata formats, metadata keys, and metadata keyspaces supported by AVFoundation are available among the defines in AVMetadataFormat.h.
--
-- Note that arrays of AVMetadataItems vended by AVAsset and other classes are "lazy", similar to array-based keys that support key-value observing, meaning that you can obtain objects from those arrays without incurring overhead for items you don't ultimately inspect.
--
-- AVMetadataItem conforms to NSMutableCopying, but for some "lazy" instances of AVMetadataItem, creating a mutable copy can cause properties to load synchronously.  This can cause the calling thread to block while synchronous I/O is performed.  To avoid the possiblity of blocking, which should be avoided on the main thread or when running on one of Swift's concurrency threads, ensure that the @value@ and @extraAttributes@ properties are loaded prior to making a mutable copy.  This can be done using the methods of AVAsynchronousKeyValueLoading, either to synchronously check whether loading has already occurred or to asynchronously load the property values.
--
-- You can filter arrays of AVMetadataItems by locale or by key and keySpace via the category AVMetadataItemArrayFiltering defined below.
--
-- Generated bindings for @AVMetadataItemInternal@.
module ObjC.AVFoundation.AVMetadataItemInternal
  ( AVMetadataItemInternal
  , IsAVMetadataItemInternal(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

