{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibArtwork class represents a media item artwork.
--
-- Generated bindings for @ITLibArtwork@.
module ObjC.ITunesLibrary.ITLibArtwork
  ( ITLibArtwork
  , IsITLibArtwork(..)
  , imageData
  , imageDataFormat
  , imageDataFormatSelector
  , imageDataSelector

  -- * Enum types
  , ITLibArtworkFormat(ITLibArtworkFormat)
  , pattern ITLibArtworkFormatNone
  , pattern ITLibArtworkFormatBitmap
  , pattern ITLibArtworkFormatJPEG
  , pattern ITLibArtworkFormatJPEG2000
  , pattern ITLibArtworkFormatGIF
  , pattern ITLibArtworkFormatPNG
  , pattern ITLibArtworkFormatBMP
  , pattern ITLibArtworkFormatTIFF
  , pattern ITLibArtworkFormatPICT

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The data (bytes) of this artwork image.
--
-- ObjC selector: @- imageData@
imageData :: IsITLibArtwork itLibArtwork => itLibArtwork -> IO (Id NSData)
imageData itLibArtwork =
  sendMessage itLibArtwork imageDataSelector

-- | The fortmat of the data returned by the imageData method.
--
-- ObjC selector: @- imageDataFormat@
imageDataFormat :: IsITLibArtwork itLibArtwork => itLibArtwork -> IO ITLibArtworkFormat
imageDataFormat itLibArtwork =
  sendMessage itLibArtwork imageDataFormatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageData@
imageDataSelector :: Selector '[] (Id NSData)
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @imageDataFormat@
imageDataFormatSelector :: Selector '[] ITLibArtworkFormat
imageDataFormatSelector = mkSelector "imageDataFormat"

