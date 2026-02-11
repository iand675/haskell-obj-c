{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFActionURL@.
module ObjC.PDFKit.PDFActionURL
  ( PDFActionURL
  , IsPDFActionURL(..)
  , initWithURL
  , url
  , setURL
  , initWithURLSelector
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

-- | @- initWithURL:@
initWithURL :: (IsPDFActionURL pdfActionURL, IsNSURL url) => pdfActionURL -> url -> IO (Id PDFActionURL)
initWithURL pdfActionURL  url =
withObjCPtr url $ \raw_url ->
    sendMsg pdfActionURL (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- URL@
url :: IsPDFActionURL pdfActionURL => pdfActionURL -> IO (Id NSURL)
url pdfActionURL  =
  sendMsg pdfActionURL (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsPDFActionURL pdfActionURL, IsNSURL value) => pdfActionURL -> value -> IO ()
setURL pdfActionURL  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfActionURL (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

