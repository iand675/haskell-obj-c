{-# LANGUAGE DataKinds #-}
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

-- | @- initWithURL:@
initWithURL :: (IsPDFActionURL pdfActionURL, IsNSURL url) => pdfActionURL -> url -> IO (Id PDFActionURL)
initWithURL pdfActionURL url =
  sendOwnedMessage pdfActionURL initWithURLSelector (toNSURL url)

-- | @- URL@
url :: IsPDFActionURL pdfActionURL => pdfActionURL -> IO (Id NSURL)
url pdfActionURL =
  sendMessage pdfActionURL urlSelector

-- | @- setURL:@
setURL :: (IsPDFActionURL pdfActionURL, IsNSURL value) => pdfActionURL -> value -> IO ()
setURL pdfActionURL value =
  sendMessage pdfActionURL setURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id PDFActionURL)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

