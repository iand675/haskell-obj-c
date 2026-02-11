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
  , urlSelector
  , setURLSelector


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

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- destination@
destination :: IsPDFAnnotationLink pdfAnnotationLink => pdfAnnotationLink -> IO (Id PDFDestination)
destination pdfAnnotationLink  =
  sendMsg pdfAnnotationLink (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestination:@
setDestination :: (IsPDFAnnotationLink pdfAnnotationLink, IsPDFDestination destination) => pdfAnnotationLink -> destination -> IO ()
setDestination pdfAnnotationLink  destination =
withObjCPtr destination $ \raw_destination ->
    sendMsg pdfAnnotationLink (mkSelector "setDestination:") retVoid [argPtr (castPtr raw_destination :: Ptr ())]

-- | @- URL@
url :: IsPDFAnnotationLink pdfAnnotationLink => pdfAnnotationLink -> IO (Id NSURL)
url pdfAnnotationLink  =
  sendMsg pdfAnnotationLink (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsPDFAnnotationLink pdfAnnotationLink, IsNSURL url) => pdfAnnotationLink -> url -> IO ()
setURL pdfAnnotationLink  url =
withObjCPtr url $ \raw_url ->
    sendMsg pdfAnnotationLink (mkSelector "setURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

