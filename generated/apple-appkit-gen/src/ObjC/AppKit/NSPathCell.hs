{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPathCell@.
module ObjC.AppKit.NSPathCell
  ( NSPathCell
  , IsNSPathCell(..)
  , setObjectValue
  , rectOfPathComponentCell_withFrame_inView
  , pathComponentCellAtPoint_withFrame_inView
  , mouseEntered_withFrame_inView
  , mouseExited_withFrame_inView
  , pathStyle
  , setPathStyle
  , url
  , setURL
  , allowedTypes
  , setAllowedTypes
  , delegate
  , setDelegate
  , pathComponentCellClass
  , pathComponentCells
  , setPathComponentCells
  , clickedPathComponentCell
  , doubleAction
  , setDoubleAction
  , backgroundColor
  , setBackgroundColor
  , placeholderString
  , setPlaceholderString
  , placeholderAttributedString
  , setPlaceholderAttributedString
  , allowedTypesSelector
  , backgroundColorSelector
  , clickedPathComponentCellSelector
  , delegateSelector
  , doubleActionSelector
  , mouseEntered_withFrame_inViewSelector
  , mouseExited_withFrame_inViewSelector
  , pathComponentCellAtPoint_withFrame_inViewSelector
  , pathComponentCellClassSelector
  , pathComponentCellsSelector
  , pathStyleSelector
  , placeholderAttributedStringSelector
  , placeholderStringSelector
  , rectOfPathComponentCell_withFrame_inViewSelector
  , setAllowedTypesSelector
  , setBackgroundColorSelector
  , setDelegateSelector
  , setDoubleActionSelector
  , setObjectValueSelector
  , setPathComponentCellsSelector
  , setPathStyleSelector
  , setPlaceholderAttributedStringSelector
  , setPlaceholderStringSelector
  , setURLSelector
  , urlSelector

  -- * Enum types
  , NSPathStyle(NSPathStyle)
  , pattern NSPathStyleStandard
  , pattern NSPathStylePopUp
  , pattern NSPathStyleNavigationBar

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

-- | @- setObjectValue:@
setObjectValue :: IsNSPathCell nsPathCell => nsPathCell -> RawId -> IO ()
setObjectValue nsPathCell obj_ =
  sendMessage nsPathCell setObjectValueSelector obj_

-- | @- rectOfPathComponentCell:withFrame:inView:@
rectOfPathComponentCell_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSPathComponentCell cell, IsNSView view) => nsPathCell -> cell -> NSRect -> view -> IO NSRect
rectOfPathComponentCell_withFrame_inView nsPathCell cell frame view =
  sendMessage nsPathCell rectOfPathComponentCell_withFrame_inViewSelector (toNSPathComponentCell cell) frame (toNSView view)

-- | @- pathComponentCellAtPoint:withFrame:inView:@
pathComponentCellAtPoint_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSView view) => nsPathCell -> NSPoint -> NSRect -> view -> IO (Id NSPathComponentCell)
pathComponentCellAtPoint_withFrame_inView nsPathCell point frame view =
  sendMessage nsPathCell pathComponentCellAtPoint_withFrame_inViewSelector point frame (toNSView view)

-- | @- mouseEntered:withFrame:inView:@
mouseEntered_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSEvent event, IsNSView view) => nsPathCell -> event -> NSRect -> view -> IO ()
mouseEntered_withFrame_inView nsPathCell event frame view =
  sendMessage nsPathCell mouseEntered_withFrame_inViewSelector (toNSEvent event) frame (toNSView view)

-- | @- mouseExited:withFrame:inView:@
mouseExited_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSEvent event, IsNSView view) => nsPathCell -> event -> NSRect -> view -> IO ()
mouseExited_withFrame_inView nsPathCell event frame view =
  sendMessage nsPathCell mouseExited_withFrame_inViewSelector (toNSEvent event) frame (toNSView view)

-- | @- pathStyle@
pathStyle :: IsNSPathCell nsPathCell => nsPathCell -> IO NSPathStyle
pathStyle nsPathCell =
  sendMessage nsPathCell pathStyleSelector

-- | @- setPathStyle:@
setPathStyle :: IsNSPathCell nsPathCell => nsPathCell -> NSPathStyle -> IO ()
setPathStyle nsPathCell value =
  sendMessage nsPathCell setPathStyleSelector value

-- | @- URL@
url :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSURL)
url nsPathCell =
  sendMessage nsPathCell urlSelector

-- | @- setURL:@
setURL :: (IsNSPathCell nsPathCell, IsNSURL value) => nsPathCell -> value -> IO ()
setURL nsPathCell value =
  sendMessage nsPathCell setURLSelector (toNSURL value)

-- | @- allowedTypes@
allowedTypes :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSArray)
allowedTypes nsPathCell =
  sendMessage nsPathCell allowedTypesSelector

-- | @- setAllowedTypes:@
setAllowedTypes :: (IsNSPathCell nsPathCell, IsNSArray value) => nsPathCell -> value -> IO ()
setAllowedTypes nsPathCell value =
  sendMessage nsPathCell setAllowedTypesSelector (toNSArray value)

-- | @- delegate@
delegate :: IsNSPathCell nsPathCell => nsPathCell -> IO RawId
delegate nsPathCell =
  sendMessage nsPathCell delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSPathCell nsPathCell => nsPathCell -> RawId -> IO ()
setDelegate nsPathCell value =
  sendMessage nsPathCell setDelegateSelector value

-- | @+ pathComponentCellClass@
pathComponentCellClass :: IO Class
pathComponentCellClass  =
  do
    cls' <- getRequiredClass "NSPathCell"
    sendClassMessage cls' pathComponentCellClassSelector

-- | @- pathComponentCells@
pathComponentCells :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSArray)
pathComponentCells nsPathCell =
  sendMessage nsPathCell pathComponentCellsSelector

-- | @- setPathComponentCells:@
setPathComponentCells :: (IsNSPathCell nsPathCell, IsNSArray value) => nsPathCell -> value -> IO ()
setPathComponentCells nsPathCell value =
  sendMessage nsPathCell setPathComponentCellsSelector (toNSArray value)

-- | @- clickedPathComponentCell@
clickedPathComponentCell :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSPathComponentCell)
clickedPathComponentCell nsPathCell =
  sendMessage nsPathCell clickedPathComponentCellSelector

-- | @- doubleAction@
doubleAction :: IsNSPathCell nsPathCell => nsPathCell -> IO Sel
doubleAction nsPathCell =
  sendMessage nsPathCell doubleActionSelector

-- | @- setDoubleAction:@
setDoubleAction :: IsNSPathCell nsPathCell => nsPathCell -> Sel -> IO ()
setDoubleAction nsPathCell value =
  sendMessage nsPathCell setDoubleActionSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSColor)
backgroundColor nsPathCell =
  sendMessage nsPathCell backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSPathCell nsPathCell, IsNSColor value) => nsPathCell -> value -> IO ()
setBackgroundColor nsPathCell value =
  sendMessage nsPathCell setBackgroundColorSelector (toNSColor value)

-- | @- placeholderString@
placeholderString :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSString)
placeholderString nsPathCell =
  sendMessage nsPathCell placeholderStringSelector

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSPathCell nsPathCell, IsNSString value) => nsPathCell -> value -> IO ()
setPlaceholderString nsPathCell value =
  sendMessage nsPathCell setPlaceholderStringSelector (toNSString value)

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSAttributedString)
placeholderAttributedString nsPathCell =
  sendMessage nsPathCell placeholderAttributedStringSelector

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSPathCell nsPathCell, IsNSAttributedString value) => nsPathCell -> value -> IO ()
setPlaceholderAttributedString nsPathCell value =
  sendMessage nsPathCell setPlaceholderAttributedStringSelector (toNSAttributedString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector '[RawId] ()
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @rectOfPathComponentCell:withFrame:inView:@
rectOfPathComponentCell_withFrame_inViewSelector :: Selector '[Id NSPathComponentCell, NSRect, Id NSView] NSRect
rectOfPathComponentCell_withFrame_inViewSelector = mkSelector "rectOfPathComponentCell:withFrame:inView:"

-- | @Selector@ for @pathComponentCellAtPoint:withFrame:inView:@
pathComponentCellAtPoint_withFrame_inViewSelector :: Selector '[NSPoint, NSRect, Id NSView] (Id NSPathComponentCell)
pathComponentCellAtPoint_withFrame_inViewSelector = mkSelector "pathComponentCellAtPoint:withFrame:inView:"

-- | @Selector@ for @mouseEntered:withFrame:inView:@
mouseEntered_withFrame_inViewSelector :: Selector '[Id NSEvent, NSRect, Id NSView] ()
mouseEntered_withFrame_inViewSelector = mkSelector "mouseEntered:withFrame:inView:"

-- | @Selector@ for @mouseExited:withFrame:inView:@
mouseExited_withFrame_inViewSelector :: Selector '[Id NSEvent, NSRect, Id NSView] ()
mouseExited_withFrame_inViewSelector = mkSelector "mouseExited:withFrame:inView:"

-- | @Selector@ for @pathStyle@
pathStyleSelector :: Selector '[] NSPathStyle
pathStyleSelector = mkSelector "pathStyle"

-- | @Selector@ for @setPathStyle:@
setPathStyleSelector :: Selector '[NSPathStyle] ()
setPathStyleSelector = mkSelector "setPathStyle:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @allowedTypes@
allowedTypesSelector :: Selector '[] (Id NSArray)
allowedTypesSelector = mkSelector "allowedTypes"

-- | @Selector@ for @setAllowedTypes:@
setAllowedTypesSelector :: Selector '[Id NSArray] ()
setAllowedTypesSelector = mkSelector "setAllowedTypes:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @pathComponentCellClass@
pathComponentCellClassSelector :: Selector '[] Class
pathComponentCellClassSelector = mkSelector "pathComponentCellClass"

-- | @Selector@ for @pathComponentCells@
pathComponentCellsSelector :: Selector '[] (Id NSArray)
pathComponentCellsSelector = mkSelector "pathComponentCells"

-- | @Selector@ for @setPathComponentCells:@
setPathComponentCellsSelector :: Selector '[Id NSArray] ()
setPathComponentCellsSelector = mkSelector "setPathComponentCells:"

-- | @Selector@ for @clickedPathComponentCell@
clickedPathComponentCellSelector :: Selector '[] (Id NSPathComponentCell)
clickedPathComponentCellSelector = mkSelector "clickedPathComponentCell"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector '[] Sel
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector '[Sel] ()
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @placeholderString@
placeholderStringSelector :: Selector '[] (Id NSString)
placeholderStringSelector = mkSelector "placeholderString"

-- | @Selector@ for @setPlaceholderString:@
setPlaceholderStringSelector :: Selector '[Id NSString] ()
setPlaceholderStringSelector = mkSelector "setPlaceholderString:"

-- | @Selector@ for @placeholderAttributedString@
placeholderAttributedStringSelector :: Selector '[] (Id NSAttributedString)
placeholderAttributedStringSelector = mkSelector "placeholderAttributedString"

-- | @Selector@ for @setPlaceholderAttributedString:@
setPlaceholderAttributedStringSelector :: Selector '[Id NSAttributedString] ()
setPlaceholderAttributedStringSelector = mkSelector "setPlaceholderAttributedString:"

