{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSearchFieldCell@.
module ObjC.AppKit.NSSearchFieldCell
  ( NSSearchFieldCell
  , IsNSSearchFieldCell(..)
  , initTextCell
  , initWithCoder
  , initImageCell
  , resetSearchButtonCell
  , resetCancelButtonCell
  , searchTextRectForBounds
  , searchButtonRectForBounds
  , cancelButtonRectForBounds
  , searchButtonCell
  , setSearchButtonCell
  , cancelButtonCell
  , setCancelButtonCell
  , searchMenuTemplate
  , setSearchMenuTemplate
  , sendsWholeSearchString
  , setSendsWholeSearchString
  , maximumRecents
  , setMaximumRecents
  , recentSearches
  , setRecentSearches
  , recentsAutosaveName
  , setRecentsAutosaveName
  , sendsSearchStringImmediately
  , setSendsSearchStringImmediately
  , cancelButtonCellSelector
  , cancelButtonRectForBoundsSelector
  , initImageCellSelector
  , initTextCellSelector
  , initWithCoderSelector
  , maximumRecentsSelector
  , recentSearchesSelector
  , recentsAutosaveNameSelector
  , resetCancelButtonCellSelector
  , resetSearchButtonCellSelector
  , searchButtonCellSelector
  , searchButtonRectForBoundsSelector
  , searchMenuTemplateSelector
  , searchTextRectForBoundsSelector
  , sendsSearchStringImmediatelySelector
  , sendsWholeSearchStringSelector
  , setCancelButtonCellSelector
  , setMaximumRecentsSelector
  , setRecentSearchesSelector
  , setRecentsAutosaveNameSelector
  , setSearchButtonCellSelector
  , setSearchMenuTemplateSelector
  , setSendsSearchStringImmediatelySelector
  , setSendsWholeSearchStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSString string) => nsSearchFieldCell -> string -> IO (Id NSSearchFieldCell)
initTextCell nsSearchFieldCell string =
  sendOwnedMessage nsSearchFieldCell initTextCellSelector (toNSString string)

-- | @- initWithCoder:@
initWithCoder :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSCoder coder) => nsSearchFieldCell -> coder -> IO (Id NSSearchFieldCell)
initWithCoder nsSearchFieldCell coder =
  sendOwnedMessage nsSearchFieldCell initWithCoderSelector (toNSCoder coder)

-- | @- initImageCell:@
initImageCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSImage image) => nsSearchFieldCell -> image -> IO (Id NSSearchFieldCell)
initImageCell nsSearchFieldCell image =
  sendOwnedMessage nsSearchFieldCell initImageCellSelector (toNSImage image)

-- | @- resetSearchButtonCell@
resetSearchButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO ()
resetSearchButtonCell nsSearchFieldCell =
  sendMessage nsSearchFieldCell resetSearchButtonCellSelector

-- | @- resetCancelButtonCell@
resetCancelButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO ()
resetCancelButtonCell nsSearchFieldCell =
  sendMessage nsSearchFieldCell resetCancelButtonCellSelector

-- | @- searchTextRectForBounds:@
searchTextRectForBounds :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> NSRect -> IO NSRect
searchTextRectForBounds nsSearchFieldCell rect =
  sendMessage nsSearchFieldCell searchTextRectForBoundsSelector rect

-- | @- searchButtonRectForBounds:@
searchButtonRectForBounds :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> NSRect -> IO NSRect
searchButtonRectForBounds nsSearchFieldCell rect =
  sendMessage nsSearchFieldCell searchButtonRectForBoundsSelector rect

-- | @- cancelButtonRectForBounds:@
cancelButtonRectForBounds :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> NSRect -> IO NSRect
cancelButtonRectForBounds nsSearchFieldCell rect =
  sendMessage nsSearchFieldCell cancelButtonRectForBoundsSelector rect

-- | @- searchButtonCell@
searchButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSButtonCell)
searchButtonCell nsSearchFieldCell =
  sendMessage nsSearchFieldCell searchButtonCellSelector

-- | @- setSearchButtonCell:@
setSearchButtonCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSButtonCell value) => nsSearchFieldCell -> value -> IO ()
setSearchButtonCell nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setSearchButtonCellSelector (toNSButtonCell value)

-- | @- cancelButtonCell@
cancelButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSButtonCell)
cancelButtonCell nsSearchFieldCell =
  sendMessage nsSearchFieldCell cancelButtonCellSelector

-- | @- setCancelButtonCell:@
setCancelButtonCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSButtonCell value) => nsSearchFieldCell -> value -> IO ()
setCancelButtonCell nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setCancelButtonCellSelector (toNSButtonCell value)

-- | @- searchMenuTemplate@
searchMenuTemplate :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSMenu)
searchMenuTemplate nsSearchFieldCell =
  sendMessage nsSearchFieldCell searchMenuTemplateSelector

-- | @- setSearchMenuTemplate:@
setSearchMenuTemplate :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSMenu value) => nsSearchFieldCell -> value -> IO ()
setSearchMenuTemplate nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setSearchMenuTemplateSelector (toNSMenu value)

-- | @- sendsWholeSearchString@
sendsWholeSearchString :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO Bool
sendsWholeSearchString nsSearchFieldCell =
  sendMessage nsSearchFieldCell sendsWholeSearchStringSelector

-- | @- setSendsWholeSearchString:@
setSendsWholeSearchString :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> Bool -> IO ()
setSendsWholeSearchString nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setSendsWholeSearchStringSelector value

-- | @- maximumRecents@
maximumRecents :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO CLong
maximumRecents nsSearchFieldCell =
  sendMessage nsSearchFieldCell maximumRecentsSelector

-- | @- setMaximumRecents:@
setMaximumRecents :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> CLong -> IO ()
setMaximumRecents nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setMaximumRecentsSelector value

-- | @- recentSearches@
recentSearches :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSArray)
recentSearches nsSearchFieldCell =
  sendMessage nsSearchFieldCell recentSearchesSelector

-- | @- setRecentSearches:@
setRecentSearches :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSArray value) => nsSearchFieldCell -> value -> IO ()
setRecentSearches nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setRecentSearchesSelector (toNSArray value)

-- | @- recentsAutosaveName@
recentsAutosaveName :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSString)
recentsAutosaveName nsSearchFieldCell =
  sendMessage nsSearchFieldCell recentsAutosaveNameSelector

-- | @- setRecentsAutosaveName:@
setRecentsAutosaveName :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSString value) => nsSearchFieldCell -> value -> IO ()
setRecentsAutosaveName nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setRecentsAutosaveNameSelector (toNSString value)

-- | @- sendsSearchStringImmediately@
sendsSearchStringImmediately :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO Bool
sendsSearchStringImmediately nsSearchFieldCell =
  sendMessage nsSearchFieldCell sendsSearchStringImmediatelySelector

-- | @- setSendsSearchStringImmediately:@
setSendsSearchStringImmediately :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> Bool -> IO ()
setSendsSearchStringImmediately nsSearchFieldCell value =
  sendMessage nsSearchFieldCell setSendsSearchStringImmediatelySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSSearchFieldCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSSearchFieldCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSSearchFieldCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @resetSearchButtonCell@
resetSearchButtonCellSelector :: Selector '[] ()
resetSearchButtonCellSelector = mkSelector "resetSearchButtonCell"

-- | @Selector@ for @resetCancelButtonCell@
resetCancelButtonCellSelector :: Selector '[] ()
resetCancelButtonCellSelector = mkSelector "resetCancelButtonCell"

-- | @Selector@ for @searchTextRectForBounds:@
searchTextRectForBoundsSelector :: Selector '[NSRect] NSRect
searchTextRectForBoundsSelector = mkSelector "searchTextRectForBounds:"

-- | @Selector@ for @searchButtonRectForBounds:@
searchButtonRectForBoundsSelector :: Selector '[NSRect] NSRect
searchButtonRectForBoundsSelector = mkSelector "searchButtonRectForBounds:"

-- | @Selector@ for @cancelButtonRectForBounds:@
cancelButtonRectForBoundsSelector :: Selector '[NSRect] NSRect
cancelButtonRectForBoundsSelector = mkSelector "cancelButtonRectForBounds:"

-- | @Selector@ for @searchButtonCell@
searchButtonCellSelector :: Selector '[] (Id NSButtonCell)
searchButtonCellSelector = mkSelector "searchButtonCell"

-- | @Selector@ for @setSearchButtonCell:@
setSearchButtonCellSelector :: Selector '[Id NSButtonCell] ()
setSearchButtonCellSelector = mkSelector "setSearchButtonCell:"

-- | @Selector@ for @cancelButtonCell@
cancelButtonCellSelector :: Selector '[] (Id NSButtonCell)
cancelButtonCellSelector = mkSelector "cancelButtonCell"

-- | @Selector@ for @setCancelButtonCell:@
setCancelButtonCellSelector :: Selector '[Id NSButtonCell] ()
setCancelButtonCellSelector = mkSelector "setCancelButtonCell:"

-- | @Selector@ for @searchMenuTemplate@
searchMenuTemplateSelector :: Selector '[] (Id NSMenu)
searchMenuTemplateSelector = mkSelector "searchMenuTemplate"

-- | @Selector@ for @setSearchMenuTemplate:@
setSearchMenuTemplateSelector :: Selector '[Id NSMenu] ()
setSearchMenuTemplateSelector = mkSelector "setSearchMenuTemplate:"

-- | @Selector@ for @sendsWholeSearchString@
sendsWholeSearchStringSelector :: Selector '[] Bool
sendsWholeSearchStringSelector = mkSelector "sendsWholeSearchString"

-- | @Selector@ for @setSendsWholeSearchString:@
setSendsWholeSearchStringSelector :: Selector '[Bool] ()
setSendsWholeSearchStringSelector = mkSelector "setSendsWholeSearchString:"

-- | @Selector@ for @maximumRecents@
maximumRecentsSelector :: Selector '[] CLong
maximumRecentsSelector = mkSelector "maximumRecents"

-- | @Selector@ for @setMaximumRecents:@
setMaximumRecentsSelector :: Selector '[CLong] ()
setMaximumRecentsSelector = mkSelector "setMaximumRecents:"

-- | @Selector@ for @recentSearches@
recentSearchesSelector :: Selector '[] (Id NSArray)
recentSearchesSelector = mkSelector "recentSearches"

-- | @Selector@ for @setRecentSearches:@
setRecentSearchesSelector :: Selector '[Id NSArray] ()
setRecentSearchesSelector = mkSelector "setRecentSearches:"

-- | @Selector@ for @recentsAutosaveName@
recentsAutosaveNameSelector :: Selector '[] (Id NSString)
recentsAutosaveNameSelector = mkSelector "recentsAutosaveName"

-- | @Selector@ for @setRecentsAutosaveName:@
setRecentsAutosaveNameSelector :: Selector '[Id NSString] ()
setRecentsAutosaveNameSelector = mkSelector "setRecentsAutosaveName:"

-- | @Selector@ for @sendsSearchStringImmediately@
sendsSearchStringImmediatelySelector :: Selector '[] Bool
sendsSearchStringImmediatelySelector = mkSelector "sendsSearchStringImmediately"

-- | @Selector@ for @setSendsSearchStringImmediately:@
setSendsSearchStringImmediatelySelector :: Selector '[Bool] ()
setSendsSearchStringImmediatelySelector = mkSelector "setSendsSearchStringImmediately:"

