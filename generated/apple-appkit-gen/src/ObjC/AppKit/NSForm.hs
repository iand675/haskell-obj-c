{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSForm@.
module ObjC.AppKit.NSForm
  ( NSForm
  , IsNSForm(..)
  , indexOfSelectedItem
  , setEntryWidth
  , setInterlineSpacing
  , setBordered
  , setBezeled
  , setTitleAlignment
  , setTextAlignment
  , setTitleFont
  , setTextFont
  , cellAtIndex
  , drawCellAtIndex
  , addEntry
  , insertEntry_atIndex
  , removeEntryAtIndex
  , indexOfCellWithTag
  , selectTextAtIndex
  , setFrameSize
  , setTitleBaseWritingDirection
  , setTextBaseWritingDirection
  , setPreferredTextFieldWidth
  , preferredTextFieldWidth
  , addEntrySelector
  , cellAtIndexSelector
  , drawCellAtIndexSelector
  , indexOfCellWithTagSelector
  , indexOfSelectedItemSelector
  , insertEntry_atIndexSelector
  , preferredTextFieldWidthSelector
  , removeEntryAtIndexSelector
  , selectTextAtIndexSelector
  , setBezeledSelector
  , setBorderedSelector
  , setEntryWidthSelector
  , setFrameSizeSelector
  , setInterlineSpacingSelector
  , setPreferredTextFieldWidthSelector
  , setTextAlignmentSelector
  , setTextBaseWritingDirectionSelector
  , setTextFontSelector
  , setTitleAlignmentSelector
  , setTitleBaseWritingDirectionSelector
  , setTitleFontSelector

  -- * Enum types
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
  , NSWritingDirection(NSWritingDirection)
  , pattern NSWritingDirectionNatural
  , pattern NSWritingDirectionLeftToRight
  , pattern NSWritingDirectionRightToLeft

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSForm nsForm => nsForm -> IO CLong
indexOfSelectedItem nsForm =
  sendMessage nsForm indexOfSelectedItemSelector

-- | @- setEntryWidth:@
setEntryWidth :: IsNSForm nsForm => nsForm -> CDouble -> IO ()
setEntryWidth nsForm width =
  sendMessage nsForm setEntryWidthSelector width

-- | @- setInterlineSpacing:@
setInterlineSpacing :: IsNSForm nsForm => nsForm -> CDouble -> IO ()
setInterlineSpacing nsForm spacing =
  sendMessage nsForm setInterlineSpacingSelector spacing

-- | @- setBordered:@
setBordered :: IsNSForm nsForm => nsForm -> Bool -> IO ()
setBordered nsForm flag =
  sendMessage nsForm setBorderedSelector flag

-- | @- setBezeled:@
setBezeled :: IsNSForm nsForm => nsForm -> Bool -> IO ()
setBezeled nsForm flag =
  sendMessage nsForm setBezeledSelector flag

-- | @- setTitleAlignment:@
setTitleAlignment :: IsNSForm nsForm => nsForm -> NSTextAlignment -> IO ()
setTitleAlignment nsForm mode =
  sendMessage nsForm setTitleAlignmentSelector mode

-- | @- setTextAlignment:@
setTextAlignment :: IsNSForm nsForm => nsForm -> NSTextAlignment -> IO ()
setTextAlignment nsForm mode =
  sendMessage nsForm setTextAlignmentSelector mode

-- | @- setTitleFont:@
setTitleFont :: (IsNSForm nsForm, IsNSFont fontObj) => nsForm -> fontObj -> IO ()
setTitleFont nsForm fontObj =
  sendMessage nsForm setTitleFontSelector (toNSFont fontObj)

-- | @- setTextFont:@
setTextFont :: (IsNSForm nsForm, IsNSFont fontObj) => nsForm -> fontObj -> IO ()
setTextFont nsForm fontObj =
  sendMessage nsForm setTextFontSelector (toNSFont fontObj)

-- | @- cellAtIndex:@
cellAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO RawId
cellAtIndex nsForm index =
  sendMessage nsForm cellAtIndexSelector index

-- | @- drawCellAtIndex:@
drawCellAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO ()
drawCellAtIndex nsForm index =
  sendMessage nsForm drawCellAtIndexSelector index

-- | @- addEntry:@
addEntry :: (IsNSForm nsForm, IsNSString title) => nsForm -> title -> IO (Id NSFormCell)
addEntry nsForm title =
  sendMessage nsForm addEntrySelector (toNSString title)

-- | @- insertEntry:atIndex:@
insertEntry_atIndex :: (IsNSForm nsForm, IsNSString title) => nsForm -> title -> CLong -> IO (Id NSFormCell)
insertEntry_atIndex nsForm title index =
  sendMessage nsForm insertEntry_atIndexSelector (toNSString title) index

-- | @- removeEntryAtIndex:@
removeEntryAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO ()
removeEntryAtIndex nsForm index =
  sendMessage nsForm removeEntryAtIndexSelector index

-- | @- indexOfCellWithTag:@
indexOfCellWithTag :: IsNSForm nsForm => nsForm -> CLong -> IO CLong
indexOfCellWithTag nsForm tag =
  sendMessage nsForm indexOfCellWithTagSelector tag

-- | @- selectTextAtIndex:@
selectTextAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO ()
selectTextAtIndex nsForm index =
  sendMessage nsForm selectTextAtIndexSelector index

-- | @- setFrameSize:@
setFrameSize :: IsNSForm nsForm => nsForm -> NSSize -> IO ()
setFrameSize nsForm newSize =
  sendMessage nsForm setFrameSizeSelector newSize

-- | @- setTitleBaseWritingDirection:@
setTitleBaseWritingDirection :: IsNSForm nsForm => nsForm -> NSWritingDirection -> IO ()
setTitleBaseWritingDirection nsForm writingDirection =
  sendMessage nsForm setTitleBaseWritingDirectionSelector writingDirection

-- | @- setTextBaseWritingDirection:@
setTextBaseWritingDirection :: IsNSForm nsForm => nsForm -> NSWritingDirection -> IO ()
setTextBaseWritingDirection nsForm writingDirection =
  sendMessage nsForm setTextBaseWritingDirectionSelector writingDirection

-- | @- setPreferredTextFieldWidth:@
setPreferredTextFieldWidth :: IsNSForm nsForm => nsForm -> CDouble -> IO ()
setPreferredTextFieldWidth nsForm preferredWidth =
  sendMessage nsForm setPreferredTextFieldWidthSelector preferredWidth

-- | @- preferredTextFieldWidth@
preferredTextFieldWidth :: IsNSForm nsForm => nsForm -> IO CDouble
preferredTextFieldWidth nsForm =
  sendMessage nsForm preferredTextFieldWidthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector '[] CLong
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @setEntryWidth:@
setEntryWidthSelector :: Selector '[CDouble] ()
setEntryWidthSelector = mkSelector "setEntryWidth:"

-- | @Selector@ for @setInterlineSpacing:@
setInterlineSpacingSelector :: Selector '[CDouble] ()
setInterlineSpacingSelector = mkSelector "setInterlineSpacing:"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector '[Bool] ()
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector '[Bool] ()
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @setTitleAlignment:@
setTitleAlignmentSelector :: Selector '[NSTextAlignment] ()
setTitleAlignmentSelector = mkSelector "setTitleAlignment:"

-- | @Selector@ for @setTextAlignment:@
setTextAlignmentSelector :: Selector '[NSTextAlignment] ()
setTextAlignmentSelector = mkSelector "setTextAlignment:"

-- | @Selector@ for @setTitleFont:@
setTitleFontSelector :: Selector '[Id NSFont] ()
setTitleFontSelector = mkSelector "setTitleFont:"

-- | @Selector@ for @setTextFont:@
setTextFontSelector :: Selector '[Id NSFont] ()
setTextFontSelector = mkSelector "setTextFont:"

-- | @Selector@ for @cellAtIndex:@
cellAtIndexSelector :: Selector '[CLong] RawId
cellAtIndexSelector = mkSelector "cellAtIndex:"

-- | @Selector@ for @drawCellAtIndex:@
drawCellAtIndexSelector :: Selector '[CLong] ()
drawCellAtIndexSelector = mkSelector "drawCellAtIndex:"

-- | @Selector@ for @addEntry:@
addEntrySelector :: Selector '[Id NSString] (Id NSFormCell)
addEntrySelector = mkSelector "addEntry:"

-- | @Selector@ for @insertEntry:atIndex:@
insertEntry_atIndexSelector :: Selector '[Id NSString, CLong] (Id NSFormCell)
insertEntry_atIndexSelector = mkSelector "insertEntry:atIndex:"

-- | @Selector@ for @removeEntryAtIndex:@
removeEntryAtIndexSelector :: Selector '[CLong] ()
removeEntryAtIndexSelector = mkSelector "removeEntryAtIndex:"

-- | @Selector@ for @indexOfCellWithTag:@
indexOfCellWithTagSelector :: Selector '[CLong] CLong
indexOfCellWithTagSelector = mkSelector "indexOfCellWithTag:"

-- | @Selector@ for @selectTextAtIndex:@
selectTextAtIndexSelector :: Selector '[CLong] ()
selectTextAtIndexSelector = mkSelector "selectTextAtIndex:"

-- | @Selector@ for @setFrameSize:@
setFrameSizeSelector :: Selector '[NSSize] ()
setFrameSizeSelector = mkSelector "setFrameSize:"

-- | @Selector@ for @setTitleBaseWritingDirection:@
setTitleBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setTitleBaseWritingDirectionSelector = mkSelector "setTitleBaseWritingDirection:"

-- | @Selector@ for @setTextBaseWritingDirection:@
setTextBaseWritingDirectionSelector :: Selector '[NSWritingDirection] ()
setTextBaseWritingDirectionSelector = mkSelector "setTextBaseWritingDirection:"

-- | @Selector@ for @setPreferredTextFieldWidth:@
setPreferredTextFieldWidthSelector :: Selector '[CDouble] ()
setPreferredTextFieldWidthSelector = mkSelector "setPreferredTextFieldWidth:"

-- | @Selector@ for @preferredTextFieldWidth@
preferredTextFieldWidthSelector :: Selector '[] CDouble
preferredTextFieldWidthSelector = mkSelector "preferredTextFieldWidth"

