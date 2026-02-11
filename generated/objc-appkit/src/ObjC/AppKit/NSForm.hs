{-# LANGUAGE PatternSynonyms #-}
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
  , indexOfSelectedItemSelector
  , setEntryWidthSelector
  , setInterlineSpacingSelector
  , setBorderedSelector
  , setBezeledSelector
  , setTitleAlignmentSelector
  , setTextAlignmentSelector
  , setTitleFontSelector
  , setTextFontSelector
  , cellAtIndexSelector
  , drawCellAtIndexSelector
  , addEntrySelector
  , insertEntry_atIndexSelector
  , removeEntryAtIndexSelector
  , indexOfCellWithTagSelector
  , selectTextAtIndexSelector
  , setFrameSizeSelector
  , setTitleBaseWritingDirectionSelector
  , setTextBaseWritingDirectionSelector
  , setPreferredTextFieldWidthSelector
  , preferredTextFieldWidthSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSForm nsForm => nsForm -> IO CLong
indexOfSelectedItem nsForm  =
  sendMsg nsForm (mkSelector "indexOfSelectedItem") retCLong []

-- | @- setEntryWidth:@
setEntryWidth :: IsNSForm nsForm => nsForm -> CDouble -> IO ()
setEntryWidth nsForm  width =
  sendMsg nsForm (mkSelector "setEntryWidth:") retVoid [argCDouble (fromIntegral width)]

-- | @- setInterlineSpacing:@
setInterlineSpacing :: IsNSForm nsForm => nsForm -> CDouble -> IO ()
setInterlineSpacing nsForm  spacing =
  sendMsg nsForm (mkSelector "setInterlineSpacing:") retVoid [argCDouble (fromIntegral spacing)]

-- | @- setBordered:@
setBordered :: IsNSForm nsForm => nsForm -> Bool -> IO ()
setBordered nsForm  flag =
  sendMsg nsForm (mkSelector "setBordered:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- setBezeled:@
setBezeled :: IsNSForm nsForm => nsForm -> Bool -> IO ()
setBezeled nsForm  flag =
  sendMsg nsForm (mkSelector "setBezeled:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- setTitleAlignment:@
setTitleAlignment :: IsNSForm nsForm => nsForm -> NSTextAlignment -> IO ()
setTitleAlignment nsForm  mode =
  sendMsg nsForm (mkSelector "setTitleAlignment:") retVoid [argCLong (coerce mode)]

-- | @- setTextAlignment:@
setTextAlignment :: IsNSForm nsForm => nsForm -> NSTextAlignment -> IO ()
setTextAlignment nsForm  mode =
  sendMsg nsForm (mkSelector "setTextAlignment:") retVoid [argCLong (coerce mode)]

-- | @- setTitleFont:@
setTitleFont :: (IsNSForm nsForm, IsNSFont fontObj) => nsForm -> fontObj -> IO ()
setTitleFont nsForm  fontObj =
withObjCPtr fontObj $ \raw_fontObj ->
    sendMsg nsForm (mkSelector "setTitleFont:") retVoid [argPtr (castPtr raw_fontObj :: Ptr ())]

-- | @- setTextFont:@
setTextFont :: (IsNSForm nsForm, IsNSFont fontObj) => nsForm -> fontObj -> IO ()
setTextFont nsForm  fontObj =
withObjCPtr fontObj $ \raw_fontObj ->
    sendMsg nsForm (mkSelector "setTextFont:") retVoid [argPtr (castPtr raw_fontObj :: Ptr ())]

-- | @- cellAtIndex:@
cellAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO RawId
cellAtIndex nsForm  index =
  fmap (RawId . castPtr) $ sendMsg nsForm (mkSelector "cellAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)]

-- | @- drawCellAtIndex:@
drawCellAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO ()
drawCellAtIndex nsForm  index =
  sendMsg nsForm (mkSelector "drawCellAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- addEntry:@
addEntry :: (IsNSForm nsForm, IsNSString title) => nsForm -> title -> IO (Id NSFormCell)
addEntry nsForm  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsForm (mkSelector "addEntry:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertEntry:atIndex:@
insertEntry_atIndex :: (IsNSForm nsForm, IsNSString title) => nsForm -> title -> CLong -> IO (Id NSFormCell)
insertEntry_atIndex nsForm  title index =
withObjCPtr title $ \raw_title ->
    sendMsg nsForm (mkSelector "insertEntry:atIndex:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- removeEntryAtIndex:@
removeEntryAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO ()
removeEntryAtIndex nsForm  index =
  sendMsg nsForm (mkSelector "removeEntryAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- indexOfCellWithTag:@
indexOfCellWithTag :: IsNSForm nsForm => nsForm -> CLong -> IO CLong
indexOfCellWithTag nsForm  tag =
  sendMsg nsForm (mkSelector "indexOfCellWithTag:") retCLong [argCLong (fromIntegral tag)]

-- | @- selectTextAtIndex:@
selectTextAtIndex :: IsNSForm nsForm => nsForm -> CLong -> IO ()
selectTextAtIndex nsForm  index =
  sendMsg nsForm (mkSelector "selectTextAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- setFrameSize:@
setFrameSize :: IsNSForm nsForm => nsForm -> NSSize -> IO ()
setFrameSize nsForm  newSize =
  sendMsg nsForm (mkSelector "setFrameSize:") retVoid [argNSSize newSize]

-- | @- setTitleBaseWritingDirection:@
setTitleBaseWritingDirection :: IsNSForm nsForm => nsForm -> NSWritingDirection -> IO ()
setTitleBaseWritingDirection nsForm  writingDirection =
  sendMsg nsForm (mkSelector "setTitleBaseWritingDirection:") retVoid [argCLong (coerce writingDirection)]

-- | @- setTextBaseWritingDirection:@
setTextBaseWritingDirection :: IsNSForm nsForm => nsForm -> NSWritingDirection -> IO ()
setTextBaseWritingDirection nsForm  writingDirection =
  sendMsg nsForm (mkSelector "setTextBaseWritingDirection:") retVoid [argCLong (coerce writingDirection)]

-- | @- setPreferredTextFieldWidth:@
setPreferredTextFieldWidth :: IsNSForm nsForm => nsForm -> CDouble -> IO ()
setPreferredTextFieldWidth nsForm  preferredWidth =
  sendMsg nsForm (mkSelector "setPreferredTextFieldWidth:") retVoid [argCDouble (fromIntegral preferredWidth)]

-- | @- preferredTextFieldWidth@
preferredTextFieldWidth :: IsNSForm nsForm => nsForm -> IO CDouble
preferredTextFieldWidth nsForm  =
  sendMsg nsForm (mkSelector "preferredTextFieldWidth") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @setEntryWidth:@
setEntryWidthSelector :: Selector
setEntryWidthSelector = mkSelector "setEntryWidth:"

-- | @Selector@ for @setInterlineSpacing:@
setInterlineSpacingSelector :: Selector
setInterlineSpacingSelector = mkSelector "setInterlineSpacing:"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @setBezeled:@
setBezeledSelector :: Selector
setBezeledSelector = mkSelector "setBezeled:"

-- | @Selector@ for @setTitleAlignment:@
setTitleAlignmentSelector :: Selector
setTitleAlignmentSelector = mkSelector "setTitleAlignment:"

-- | @Selector@ for @setTextAlignment:@
setTextAlignmentSelector :: Selector
setTextAlignmentSelector = mkSelector "setTextAlignment:"

-- | @Selector@ for @setTitleFont:@
setTitleFontSelector :: Selector
setTitleFontSelector = mkSelector "setTitleFont:"

-- | @Selector@ for @setTextFont:@
setTextFontSelector :: Selector
setTextFontSelector = mkSelector "setTextFont:"

-- | @Selector@ for @cellAtIndex:@
cellAtIndexSelector :: Selector
cellAtIndexSelector = mkSelector "cellAtIndex:"

-- | @Selector@ for @drawCellAtIndex:@
drawCellAtIndexSelector :: Selector
drawCellAtIndexSelector = mkSelector "drawCellAtIndex:"

-- | @Selector@ for @addEntry:@
addEntrySelector :: Selector
addEntrySelector = mkSelector "addEntry:"

-- | @Selector@ for @insertEntry:atIndex:@
insertEntry_atIndexSelector :: Selector
insertEntry_atIndexSelector = mkSelector "insertEntry:atIndex:"

-- | @Selector@ for @removeEntryAtIndex:@
removeEntryAtIndexSelector :: Selector
removeEntryAtIndexSelector = mkSelector "removeEntryAtIndex:"

-- | @Selector@ for @indexOfCellWithTag:@
indexOfCellWithTagSelector :: Selector
indexOfCellWithTagSelector = mkSelector "indexOfCellWithTag:"

-- | @Selector@ for @selectTextAtIndex:@
selectTextAtIndexSelector :: Selector
selectTextAtIndexSelector = mkSelector "selectTextAtIndex:"

-- | @Selector@ for @setFrameSize:@
setFrameSizeSelector :: Selector
setFrameSizeSelector = mkSelector "setFrameSize:"

-- | @Selector@ for @setTitleBaseWritingDirection:@
setTitleBaseWritingDirectionSelector :: Selector
setTitleBaseWritingDirectionSelector = mkSelector "setTitleBaseWritingDirection:"

-- | @Selector@ for @setTextBaseWritingDirection:@
setTextBaseWritingDirectionSelector :: Selector
setTextBaseWritingDirectionSelector = mkSelector "setTextBaseWritingDirection:"

-- | @Selector@ for @setPreferredTextFieldWidth:@
setPreferredTextFieldWidthSelector :: Selector
setPreferredTextFieldWidthSelector = mkSelector "setPreferredTextFieldWidth:"

-- | @Selector@ for @preferredTextFieldWidth@
preferredTextFieldWidthSelector :: Selector
preferredTextFieldWidthSelector = mkSelector "preferredTextFieldWidth"

