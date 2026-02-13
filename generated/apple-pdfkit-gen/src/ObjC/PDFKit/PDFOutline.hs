{-# LANGUAGE DataKinds #-}
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
  , parent
  , numberOfChildren
  , index
  , label
  , setLabel
  , isOpen
  , setIsOpen
  , destination
  , setDestination
  , action
  , setAction
  , actionSelector
  , childAtIndexSelector
  , destinationSelector
  , documentSelector
  , indexSelector
  , initSelector
  , insertChild_atIndexSelector
  , isOpenSelector
  , labelSelector
  , numberOfChildrenSelector
  , parentSelector
  , removeFromParentSelector
  , setActionSelector
  , setDestinationSelector
  , setIsOpenSelector
  , setLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id PDFOutline)
init_ pdfOutline =
  sendOwnedMessage pdfOutline initSelector

-- | @- childAtIndex:@
childAtIndex :: IsPDFOutline pdfOutline => pdfOutline -> CULong -> IO (Id PDFOutline)
childAtIndex pdfOutline index =
  sendMessage pdfOutline childAtIndexSelector index

-- | @- insertChild:atIndex:@
insertChild_atIndex :: (IsPDFOutline pdfOutline, IsPDFOutline child) => pdfOutline -> child -> CULong -> IO ()
insertChild_atIndex pdfOutline child index =
  sendMessage pdfOutline insertChild_atIndexSelector (toPDFOutline child) index

-- | @- removeFromParent@
removeFromParent :: IsPDFOutline pdfOutline => pdfOutline -> IO ()
removeFromParent pdfOutline =
  sendMessage pdfOutline removeFromParentSelector

-- | @- document@
document :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id PDFDocument)
document pdfOutline =
  sendMessage pdfOutline documentSelector

-- | @- parent@
parent :: IsPDFOutline pdfOutline => pdfOutline -> IO RawId
parent pdfOutline =
  sendMessage pdfOutline parentSelector

-- | @- numberOfChildren@
numberOfChildren :: IsPDFOutline pdfOutline => pdfOutline -> IO CULong
numberOfChildren pdfOutline =
  sendMessage pdfOutline numberOfChildrenSelector

-- | @- index@
index :: IsPDFOutline pdfOutline => pdfOutline -> IO CULong
index pdfOutline =
  sendMessage pdfOutline indexSelector

-- | @- label@
label :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id NSString)
label pdfOutline =
  sendMessage pdfOutline labelSelector

-- | @- setLabel:@
setLabel :: (IsPDFOutline pdfOutline, IsNSString value) => pdfOutline -> value -> IO ()
setLabel pdfOutline value =
  sendMessage pdfOutline setLabelSelector (toNSString value)

-- | @- isOpen@
isOpen :: IsPDFOutline pdfOutline => pdfOutline -> IO Bool
isOpen pdfOutline =
  sendMessage pdfOutline isOpenSelector

-- | @- setIsOpen:@
setIsOpen :: IsPDFOutline pdfOutline => pdfOutline -> Bool -> IO ()
setIsOpen pdfOutline value =
  sendMessage pdfOutline setIsOpenSelector value

-- | @- destination@
destination :: IsPDFOutline pdfOutline => pdfOutline -> IO (Id PDFDestination)
destination pdfOutline =
  sendMessage pdfOutline destinationSelector

-- | @- setDestination:@
setDestination :: (IsPDFOutline pdfOutline, IsPDFDestination value) => pdfOutline -> value -> IO ()
setDestination pdfOutline value =
  sendMessage pdfOutline setDestinationSelector (toPDFDestination value)

-- | @- action@
action :: IsPDFOutline pdfOutline => pdfOutline -> IO RawId
action pdfOutline =
  sendMessage pdfOutline actionSelector

-- | @- setAction:@
setAction :: IsPDFOutline pdfOutline => pdfOutline -> RawId -> IO ()
setAction pdfOutline value =
  sendMessage pdfOutline setActionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PDFOutline)
initSelector = mkSelector "init"

-- | @Selector@ for @childAtIndex:@
childAtIndexSelector :: Selector '[CULong] (Id PDFOutline)
childAtIndexSelector = mkSelector "childAtIndex:"

-- | @Selector@ for @insertChild:atIndex:@
insertChild_atIndexSelector :: Selector '[Id PDFOutline, CULong] ()
insertChild_atIndexSelector = mkSelector "insertChild:atIndex:"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector '[] ()
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @document@
documentSelector :: Selector '[] (Id PDFDocument)
documentSelector = mkSelector "document"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] RawId
parentSelector = mkSelector "parent"

-- | @Selector@ for @numberOfChildren@
numberOfChildrenSelector :: Selector '[] CULong
numberOfChildrenSelector = mkSelector "numberOfChildren"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector '[] Bool
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @setIsOpen:@
setIsOpenSelector :: Selector '[Bool] ()
setIsOpenSelector = mkSelector "setIsOpen:"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id PDFDestination)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[Id PDFDestination] ()
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] RawId
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[RawId] ()
setActionSelector = mkSelector "setAction:"

