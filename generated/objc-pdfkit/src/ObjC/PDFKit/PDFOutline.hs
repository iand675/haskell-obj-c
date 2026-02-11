{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFOutline@.
module ObjC.PDFKit.PDFOutline
  ( PDFOutline
  , IsPDFOutline(..)
  , init_
  , childAtIndex
  , insertChild_atIndex
  , removeFromParent
  , document
  , numberOfChildren
  , index
  , label
  , setLabel
  , isOpen
  , setIsOpen
  , destination
  , setDestination
  , initSelector
  , childAtIndexSelector
  , insertChild_atIndexSelector
  , removeFromParentSelector
  , documentSelector
  , numberOfChildrenSelector
  , indexSelector
  , labelSelector
  , setLabelSelector
  , isOpenSelector
  , setIsOpenSelector
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

-- | @- init@
init_ :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id PDFOutline)
init_ pdfOutline  =
  sendMsg pdfOutline (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- childAtIndex:@
childAtIndex :: IsPDFOutline pdfOutline => pdfOutline -> CULong -> IO (Id PDFOutline)
childAtIndex pdfOutline  index =
  sendMsg pdfOutline (mkSelector "childAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- insertChild:atIndex:@
insertChild_atIndex :: (IsPDFOutline pdfOutline, IsPDFOutline child) => pdfOutline -> child -> CULong -> IO ()
insertChild_atIndex pdfOutline  child index =
withObjCPtr child $ \raw_child ->
    sendMsg pdfOutline (mkSelector "insertChild:atIndex:") retVoid [argPtr (castPtr raw_child :: Ptr ()), argCULong (fromIntegral index)]

-- | @- removeFromParent@
removeFromParent :: IsPDFOutline pdfOutline => pdfOutline -> IO ()
removeFromParent pdfOutline  =
  sendMsg pdfOutline (mkSelector "removeFromParent") retVoid []

-- | @- document@
document :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id PDFDocument)
document pdfOutline  =
  sendMsg pdfOutline (mkSelector "document") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfChildren@
numberOfChildren :: IsPDFOutline pdfOutline => pdfOutline -> IO CULong
numberOfChildren pdfOutline  =
  sendMsg pdfOutline (mkSelector "numberOfChildren") retCULong []

-- | @- index@
index :: IsPDFOutline pdfOutline => pdfOutline -> IO CULong
index pdfOutline  =
  sendMsg pdfOutline (mkSelector "index") retCULong []

-- | @- label@
label :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id NSString)
label pdfOutline  =
  sendMsg pdfOutline (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsPDFOutline pdfOutline, IsNSString value) => pdfOutline -> value -> IO ()
setLabel pdfOutline  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfOutline (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isOpen@
isOpen :: IsPDFOutline pdfOutline => pdfOutline -> IO Bool
isOpen pdfOutline  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfOutline (mkSelector "isOpen") retCULong []

-- | @- setIsOpen:@
setIsOpen :: IsPDFOutline pdfOutline => pdfOutline -> Bool -> IO ()
setIsOpen pdfOutline  value =
  sendMsg pdfOutline (mkSelector "setIsOpen:") retVoid [argCULong (if value then 1 else 0)]

-- | @- destination@
destination :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id PDFDestination)
destination pdfOutline  =
  sendMsg pdfOutline (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestination:@
setDestination :: (IsPDFOutline pdfOutline, IsPDFDestination value) => pdfOutline -> value -> IO ()
setDestination pdfOutline  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfOutline (mkSelector "setDestination:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @childAtIndex:@
childAtIndexSelector :: Selector
childAtIndexSelector = mkSelector "childAtIndex:"

-- | @Selector@ for @insertChild:atIndex:@
insertChild_atIndexSelector :: Selector
insertChild_atIndexSelector = mkSelector "insertChild:atIndex:"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @document@
documentSelector :: Selector
documentSelector = mkSelector "document"

-- | @Selector@ for @numberOfChildren@
numberOfChildrenSelector :: Selector
numberOfChildrenSelector = mkSelector "numberOfChildren"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @setIsOpen:@
setIsOpenSelector :: Selector
setIsOpenSelector = mkSelector "setIsOpen:"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

