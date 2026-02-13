{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFActionGoTo@.
module ObjC.PDFKit.PDFActionGoTo
  ( PDFActionGoTo
  , IsPDFActionGoTo(..)
  , initWithDestination
  , destination
  , setDestination
  , destinationSelector
  , initWithDestinationSelector
  , setDestinationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDestination:@
initWithDestination :: (IsPDFActionGoTo pdfActionGoTo, IsPDFDestination destination) => pdfActionGoTo -> destination -> IO (Id PDFActionGoTo)
initWithDestination pdfActionGoTo destination =
  sendOwnedMessage pdfActionGoTo initWithDestinationSelector (toPDFDestination destination)

-- | @- destination@
destination :: IsPDFActionGoTo pdfActionGoTo => pdfActionGoTo -> IO (Id PDFDestination)
destination pdfActionGoTo =
  sendMessage pdfActionGoTo destinationSelector

-- | @- setDestination:@
setDestination :: (IsPDFActionGoTo pdfActionGoTo, IsPDFDestination value) => pdfActionGoTo -> value -> IO ()
setDestination pdfActionGoTo value =
  sendMessage pdfActionGoTo setDestinationSelector (toPDFDestination value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestination:@
initWithDestinationSelector :: Selector '[Id PDFDestination] (Id PDFActionGoTo)
initWithDestinationSelector = mkSelector "initWithDestination:"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id PDFDestination)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[Id PDFDestination] ()
setDestinationSelector = mkSelector "setDestination:"

