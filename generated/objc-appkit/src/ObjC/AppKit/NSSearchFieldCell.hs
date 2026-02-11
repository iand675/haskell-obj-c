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
  , initTextCellSelector
  , initWithCoderSelector
  , initImageCellSelector
  , resetSearchButtonCellSelector
  , resetCancelButtonCellSelector
  , searchTextRectForBoundsSelector
  , searchButtonRectForBoundsSelector
  , cancelButtonRectForBoundsSelector
  , searchButtonCellSelector
  , setSearchButtonCellSelector
  , cancelButtonCellSelector
  , setCancelButtonCellSelector
  , searchMenuTemplateSelector
  , setSearchMenuTemplateSelector
  , sendsWholeSearchStringSelector
  , setSendsWholeSearchStringSelector
  , maximumRecentsSelector
  , setMaximumRecentsSelector
  , recentSearchesSelector
  , setRecentSearchesSelector
  , recentsAutosaveNameSelector
  , setRecentsAutosaveNameSelector
  , sendsSearchStringImmediatelySelector
  , setSendsSearchStringImmediatelySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSString string) => nsSearchFieldCell -> string -> IO (Id NSSearchFieldCell)
initTextCell nsSearchFieldCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsSearchFieldCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSCoder coder) => nsSearchFieldCell -> coder -> IO (Id NSSearchFieldCell)
initWithCoder nsSearchFieldCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsSearchFieldCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSImage image) => nsSearchFieldCell -> image -> IO (Id NSSearchFieldCell)
initImageCell nsSearchFieldCell  image =
withObjCPtr image $ \raw_image ->
    sendMsg nsSearchFieldCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetSearchButtonCell@
resetSearchButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO ()
resetSearchButtonCell nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "resetSearchButtonCell") retVoid []

-- | @- resetCancelButtonCell@
resetCancelButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO ()
resetCancelButtonCell nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "resetCancelButtonCell") retVoid []

-- | @- searchTextRectForBounds:@
searchTextRectForBounds :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> NSRect -> IO NSRect
searchTextRectForBounds nsSearchFieldCell  rect =
  sendMsgStret nsSearchFieldCell (mkSelector "searchTextRectForBounds:") retNSRect [argNSRect rect]

-- | @- searchButtonRectForBounds:@
searchButtonRectForBounds :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> NSRect -> IO NSRect
searchButtonRectForBounds nsSearchFieldCell  rect =
  sendMsgStret nsSearchFieldCell (mkSelector "searchButtonRectForBounds:") retNSRect [argNSRect rect]

-- | @- cancelButtonRectForBounds:@
cancelButtonRectForBounds :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> NSRect -> IO NSRect
cancelButtonRectForBounds nsSearchFieldCell  rect =
  sendMsgStret nsSearchFieldCell (mkSelector "cancelButtonRectForBounds:") retNSRect [argNSRect rect]

-- | @- searchButtonCell@
searchButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSButtonCell)
searchButtonCell nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "searchButtonCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchButtonCell:@
setSearchButtonCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSButtonCell value) => nsSearchFieldCell -> value -> IO ()
setSearchButtonCell nsSearchFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSearchFieldCell (mkSelector "setSearchButtonCell:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cancelButtonCell@
cancelButtonCell :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSButtonCell)
cancelButtonCell nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "cancelButtonCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCancelButtonCell:@
setCancelButtonCell :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSButtonCell value) => nsSearchFieldCell -> value -> IO ()
setCancelButtonCell nsSearchFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSearchFieldCell (mkSelector "setCancelButtonCell:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- searchMenuTemplate@
searchMenuTemplate :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSMenu)
searchMenuTemplate nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "searchMenuTemplate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchMenuTemplate:@
setSearchMenuTemplate :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSMenu value) => nsSearchFieldCell -> value -> IO ()
setSearchMenuTemplate nsSearchFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSearchFieldCell (mkSelector "setSearchMenuTemplate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sendsWholeSearchString@
sendsWholeSearchString :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO Bool
sendsWholeSearchString nsSearchFieldCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSearchFieldCell (mkSelector "sendsWholeSearchString") retCULong []

-- | @- setSendsWholeSearchString:@
setSendsWholeSearchString :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> Bool -> IO ()
setSendsWholeSearchString nsSearchFieldCell  value =
  sendMsg nsSearchFieldCell (mkSelector "setSendsWholeSearchString:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maximumRecents@
maximumRecents :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO CLong
maximumRecents nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "maximumRecents") retCLong []

-- | @- setMaximumRecents:@
setMaximumRecents :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> CLong -> IO ()
setMaximumRecents nsSearchFieldCell  value =
  sendMsg nsSearchFieldCell (mkSelector "setMaximumRecents:") retVoid [argCLong (fromIntegral value)]

-- | @- recentSearches@
recentSearches :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSArray)
recentSearches nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "recentSearches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecentSearches:@
setRecentSearches :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSArray value) => nsSearchFieldCell -> value -> IO ()
setRecentSearches nsSearchFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSearchFieldCell (mkSelector "setRecentSearches:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recentsAutosaveName@
recentsAutosaveName :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO (Id NSString)
recentsAutosaveName nsSearchFieldCell  =
  sendMsg nsSearchFieldCell (mkSelector "recentsAutosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecentsAutosaveName:@
setRecentsAutosaveName :: (IsNSSearchFieldCell nsSearchFieldCell, IsNSString value) => nsSearchFieldCell -> value -> IO ()
setRecentsAutosaveName nsSearchFieldCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSearchFieldCell (mkSelector "setRecentsAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sendsSearchStringImmediately@
sendsSearchStringImmediately :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> IO Bool
sendsSearchStringImmediately nsSearchFieldCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSearchFieldCell (mkSelector "sendsSearchStringImmediately") retCULong []

-- | @- setSendsSearchStringImmediately:@
setSendsSearchStringImmediately :: IsNSSearchFieldCell nsSearchFieldCell => nsSearchFieldCell -> Bool -> IO ()
setSendsSearchStringImmediately nsSearchFieldCell  value =
  sendMsg nsSearchFieldCell (mkSelector "setSendsSearchStringImmediately:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @resetSearchButtonCell@
resetSearchButtonCellSelector :: Selector
resetSearchButtonCellSelector = mkSelector "resetSearchButtonCell"

-- | @Selector@ for @resetCancelButtonCell@
resetCancelButtonCellSelector :: Selector
resetCancelButtonCellSelector = mkSelector "resetCancelButtonCell"

-- | @Selector@ for @searchTextRectForBounds:@
searchTextRectForBoundsSelector :: Selector
searchTextRectForBoundsSelector = mkSelector "searchTextRectForBounds:"

-- | @Selector@ for @searchButtonRectForBounds:@
searchButtonRectForBoundsSelector :: Selector
searchButtonRectForBoundsSelector = mkSelector "searchButtonRectForBounds:"

-- | @Selector@ for @cancelButtonRectForBounds:@
cancelButtonRectForBoundsSelector :: Selector
cancelButtonRectForBoundsSelector = mkSelector "cancelButtonRectForBounds:"

-- | @Selector@ for @searchButtonCell@
searchButtonCellSelector :: Selector
searchButtonCellSelector = mkSelector "searchButtonCell"

-- | @Selector@ for @setSearchButtonCell:@
setSearchButtonCellSelector :: Selector
setSearchButtonCellSelector = mkSelector "setSearchButtonCell:"

-- | @Selector@ for @cancelButtonCell@
cancelButtonCellSelector :: Selector
cancelButtonCellSelector = mkSelector "cancelButtonCell"

-- | @Selector@ for @setCancelButtonCell:@
setCancelButtonCellSelector :: Selector
setCancelButtonCellSelector = mkSelector "setCancelButtonCell:"

-- | @Selector@ for @searchMenuTemplate@
searchMenuTemplateSelector :: Selector
searchMenuTemplateSelector = mkSelector "searchMenuTemplate"

-- | @Selector@ for @setSearchMenuTemplate:@
setSearchMenuTemplateSelector :: Selector
setSearchMenuTemplateSelector = mkSelector "setSearchMenuTemplate:"

-- | @Selector@ for @sendsWholeSearchString@
sendsWholeSearchStringSelector :: Selector
sendsWholeSearchStringSelector = mkSelector "sendsWholeSearchString"

-- | @Selector@ for @setSendsWholeSearchString:@
setSendsWholeSearchStringSelector :: Selector
setSendsWholeSearchStringSelector = mkSelector "setSendsWholeSearchString:"

-- | @Selector@ for @maximumRecents@
maximumRecentsSelector :: Selector
maximumRecentsSelector = mkSelector "maximumRecents"

-- | @Selector@ for @setMaximumRecents:@
setMaximumRecentsSelector :: Selector
setMaximumRecentsSelector = mkSelector "setMaximumRecents:"

-- | @Selector@ for @recentSearches@
recentSearchesSelector :: Selector
recentSearchesSelector = mkSelector "recentSearches"

-- | @Selector@ for @setRecentSearches:@
setRecentSearchesSelector :: Selector
setRecentSearchesSelector = mkSelector "setRecentSearches:"

-- | @Selector@ for @recentsAutosaveName@
recentsAutosaveNameSelector :: Selector
recentsAutosaveNameSelector = mkSelector "recentsAutosaveName"

-- | @Selector@ for @setRecentsAutosaveName:@
setRecentsAutosaveNameSelector :: Selector
setRecentsAutosaveNameSelector = mkSelector "setRecentsAutosaveName:"

-- | @Selector@ for @sendsSearchStringImmediately@
sendsSearchStringImmediatelySelector :: Selector
sendsSearchStringImmediatelySelector = mkSelector "sendsSearchStringImmediately"

-- | @Selector@ for @setSendsSearchStringImmediately:@
setSendsSearchStringImmediatelySelector :: Selector
setSendsSearchStringImmediatelySelector = mkSelector "setSendsSearchStringImmediately:"

