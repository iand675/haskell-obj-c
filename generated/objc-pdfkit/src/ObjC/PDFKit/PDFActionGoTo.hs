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
  , initWithDestinationSelector
  , destinationSelector
  , setDestinationSelector


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

-- | @- initWithDestination:@
initWithDestination :: (IsPDFActionGoTo pdfActionGoTo, IsPDFDestination destination) => pdfActionGoTo -> destination -> IO (Id PDFActionGoTo)
initWithDestination pdfActionGoTo  destination =
withObjCPtr destination $ \raw_destination ->
    sendMsg pdfActionGoTo (mkSelector "initWithDestination:") (retPtr retVoid) [argPtr (castPtr raw_destination :: Ptr ())] >>= ownedObject . castPtr

-- | @- destination@
destination :: IsPDFActionGoTo pdfActionGoTo => pdfActionGoTo -> IO (Id PDFDestination)
destination pdfActionGoTo  =
  sendMsg pdfActionGoTo (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestination:@
setDestination :: (IsPDFActionGoTo pdfActionGoTo, IsPDFDestination value) => pdfActionGoTo -> value -> IO ()
setDestination pdfActionGoTo  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfActionGoTo (mkSelector "setDestination:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestination:@
initWithDestinationSelector :: Selector
initWithDestinationSelector = mkSelector "initWithDestination:"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

