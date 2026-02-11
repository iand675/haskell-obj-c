{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXCategoricalDataAxisDescriptor@.
module ObjC.Accessibility.AXCategoricalDataAxisDescriptor
  ( AXCategoricalDataAxisDescriptor
  , IsAXCategoricalDataAxisDescriptor(..)
  , initWithTitle_categoryOrder
  , initWithAttributedTitle_categoryOrder
  , init_
  , new
  , categoryOrder
  , setCategoryOrder
  , initWithTitle_categoryOrderSelector
  , initWithAttributedTitle_categoryOrderSelector
  , initSelector
  , newSelector
  , categoryOrderSelector
  , setCategoryOrderSelector


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

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:categoryOrder:@
initWithTitle_categoryOrder :: (IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor, IsNSString title, IsNSArray categoryOrder) => axCategoricalDataAxisDescriptor -> title -> categoryOrder -> IO (Id AXCategoricalDataAxisDescriptor)
initWithTitle_categoryOrder axCategoricalDataAxisDescriptor  title categoryOrder =
withObjCPtr title $ \raw_title ->
  withObjCPtr categoryOrder $ \raw_categoryOrder ->
      sendMsg axCategoricalDataAxisDescriptor (mkSelector "initWithTitle:categoryOrder:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_categoryOrder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedTitle:categoryOrder:@
initWithAttributedTitle_categoryOrder :: (IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor, IsNSAttributedString attributedTitle, IsNSArray categoryOrder) => axCategoricalDataAxisDescriptor -> attributedTitle -> categoryOrder -> IO (Id AXCategoricalDataAxisDescriptor)
initWithAttributedTitle_categoryOrder axCategoricalDataAxisDescriptor  attributedTitle categoryOrder =
withObjCPtr attributedTitle $ \raw_attributedTitle ->
  withObjCPtr categoryOrder $ \raw_categoryOrder ->
      sendMsg axCategoricalDataAxisDescriptor (mkSelector "initWithAttributedTitle:categoryOrder:") (retPtr retVoid) [argPtr (castPtr raw_attributedTitle :: Ptr ()), argPtr (castPtr raw_categoryOrder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor => axCategoricalDataAxisDescriptor -> IO (Id AXCategoricalDataAxisDescriptor)
init_ axCategoricalDataAxisDescriptor  =
  sendMsg axCategoricalDataAxisDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXCategoricalDataAxisDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXCategoricalDataAxisDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The order of the category values for this axis. This list should contain every possible category value for this axis, in the order they are displayed visually in the graph or legend. For example, if your categorical axis represented 'blood type', and the legend contained 'AB, A, B, O' in that order, you would provide an array containing "AB", "A", "B" and "O" in the same order.
--
-- ObjC selector: @- categoryOrder@
categoryOrder :: IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor => axCategoricalDataAxisDescriptor -> IO (Id NSArray)
categoryOrder axCategoricalDataAxisDescriptor  =
  sendMsg axCategoricalDataAxisDescriptor (mkSelector "categoryOrder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The order of the category values for this axis. This list should contain every possible category value for this axis, in the order they are displayed visually in the graph or legend. For example, if your categorical axis represented 'blood type', and the legend contained 'AB, A, B, O' in that order, you would provide an array containing "AB", "A", "B" and "O" in the same order.
--
-- ObjC selector: @- setCategoryOrder:@
setCategoryOrder :: (IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor, IsNSArray value) => axCategoricalDataAxisDescriptor -> value -> IO ()
setCategoryOrder axCategoricalDataAxisDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg axCategoricalDataAxisDescriptor (mkSelector "setCategoryOrder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:categoryOrder:@
initWithTitle_categoryOrderSelector :: Selector
initWithTitle_categoryOrderSelector = mkSelector "initWithTitle:categoryOrder:"

-- | @Selector@ for @initWithAttributedTitle:categoryOrder:@
initWithAttributedTitle_categoryOrderSelector :: Selector
initWithAttributedTitle_categoryOrderSelector = mkSelector "initWithAttributedTitle:categoryOrder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @categoryOrder@
categoryOrderSelector :: Selector
categoryOrderSelector = mkSelector "categoryOrder"

-- | @Selector@ for @setCategoryOrder:@
setCategoryOrderSelector :: Selector
setCategoryOrderSelector = mkSelector "setCategoryOrder:"

