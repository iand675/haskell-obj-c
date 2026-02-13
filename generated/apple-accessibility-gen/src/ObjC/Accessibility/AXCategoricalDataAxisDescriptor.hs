{-# LANGUAGE DataKinds #-}
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
  , categoryOrderSelector
  , initSelector
  , initWithAttributedTitle_categoryOrderSelector
  , initWithTitle_categoryOrderSelector
  , newSelector
  , setCategoryOrderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:categoryOrder:@
initWithTitle_categoryOrder :: (IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor, IsNSString title, IsNSArray categoryOrder) => axCategoricalDataAxisDescriptor -> title -> categoryOrder -> IO (Id AXCategoricalDataAxisDescriptor)
initWithTitle_categoryOrder axCategoricalDataAxisDescriptor title categoryOrder =
  sendOwnedMessage axCategoricalDataAxisDescriptor initWithTitle_categoryOrderSelector (toNSString title) (toNSArray categoryOrder)

-- | @- initWithAttributedTitle:categoryOrder:@
initWithAttributedTitle_categoryOrder :: (IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor, IsNSAttributedString attributedTitle, IsNSArray categoryOrder) => axCategoricalDataAxisDescriptor -> attributedTitle -> categoryOrder -> IO (Id AXCategoricalDataAxisDescriptor)
initWithAttributedTitle_categoryOrder axCategoricalDataAxisDescriptor attributedTitle categoryOrder =
  sendOwnedMessage axCategoricalDataAxisDescriptor initWithAttributedTitle_categoryOrderSelector (toNSAttributedString attributedTitle) (toNSArray categoryOrder)

-- | @- init@
init_ :: IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor => axCategoricalDataAxisDescriptor -> IO (Id AXCategoricalDataAxisDescriptor)
init_ axCategoricalDataAxisDescriptor =
  sendOwnedMessage axCategoricalDataAxisDescriptor initSelector

-- | @+ new@
new :: IO (Id AXCategoricalDataAxisDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXCategoricalDataAxisDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | The order of the category values for this axis. This list should contain every possible category value for this axis, in the order they are displayed visually in the graph or legend. For example, if your categorical axis represented 'blood type', and the legend contained 'AB, A, B, O' in that order, you would provide an array containing "AB", "A", "B" and "O" in the same order.
--
-- ObjC selector: @- categoryOrder@
categoryOrder :: IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor => axCategoricalDataAxisDescriptor -> IO (Id NSArray)
categoryOrder axCategoricalDataAxisDescriptor =
  sendMessage axCategoricalDataAxisDescriptor categoryOrderSelector

-- | The order of the category values for this axis. This list should contain every possible category value for this axis, in the order they are displayed visually in the graph or legend. For example, if your categorical axis represented 'blood type', and the legend contained 'AB, A, B, O' in that order, you would provide an array containing "AB", "A", "B" and "O" in the same order.
--
-- ObjC selector: @- setCategoryOrder:@
setCategoryOrder :: (IsAXCategoricalDataAxisDescriptor axCategoricalDataAxisDescriptor, IsNSArray value) => axCategoricalDataAxisDescriptor -> value -> IO ()
setCategoryOrder axCategoricalDataAxisDescriptor value =
  sendMessage axCategoricalDataAxisDescriptor setCategoryOrderSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:categoryOrder:@
initWithTitle_categoryOrderSelector :: Selector '[Id NSString, Id NSArray] (Id AXCategoricalDataAxisDescriptor)
initWithTitle_categoryOrderSelector = mkSelector "initWithTitle:categoryOrder:"

-- | @Selector@ for @initWithAttributedTitle:categoryOrder:@
initWithAttributedTitle_categoryOrderSelector :: Selector '[Id NSAttributedString, Id NSArray] (Id AXCategoricalDataAxisDescriptor)
initWithAttributedTitle_categoryOrderSelector = mkSelector "initWithAttributedTitle:categoryOrder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXCategoricalDataAxisDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXCategoricalDataAxisDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @categoryOrder@
categoryOrderSelector :: Selector '[] (Id NSArray)
categoryOrderSelector = mkSelector "categoryOrder"

-- | @Selector@ for @setCategoryOrder:@
setCategoryOrderSelector :: Selector '[Id NSArray] ()
setCategoryOrderSelector = mkSelector "setCategoryOrder:"

