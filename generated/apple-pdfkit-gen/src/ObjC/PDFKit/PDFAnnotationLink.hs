{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationLink@.
module ObjC.PDFKit.PDFAnnotationLink
  ( PDFAnnotationLink
  , IsPDFAnnotationLink(..)
  , destination
  , setDestination
  , url
  , setURL
  , destinationSelector
  , setDestinationSelector
  , setURLSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- destination@
destination :: IsPDFAnnotationLink pdfAnnotationLink => pdfAnnotationLink -> IO (Id PDFDestination)
destination pdfAnnotationLink =
  sendMessage pdfAnnotationLink destinationSelector

-- | @- setDestination:@
setDestination :: (IsPDFAnnotationLink pdfAnnotationLink, IsPDFDestination destination) => pdfAnnotationLink -> destination -> IO ()
setDestination pdfAnnotationLink destination =
  sendMessage pdfAnnotationLink setDestinationSelector (toPDFDestination destination)

-- | @- URL@
url :: IsPDFAnnotationLink pdfAnnotationLink => pdfAnnotationLink -> IO (Id NSURL)
url pdfAnnotationLink =
  sendMessage pdfAnnotationLink urlSelector

-- | @- setURL:@
setURL :: (IsPDFAnnotationLink pdfAnnotationLink, IsNSURL url) => pdfAnnotationLink -> url -> IO ()
setURL pdfAnnotationLink url =
  sendMessage pdfAnnotationLink setURLSelector (toNSURL url)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id PDFDestination)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[Id PDFDestination] ()
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

