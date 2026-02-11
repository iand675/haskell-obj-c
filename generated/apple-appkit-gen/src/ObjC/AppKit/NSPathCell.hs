{-# LANGUAGE PatternSynonyms #-}
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
  , setObjectValueSelector
  , rectOfPathComponentCell_withFrame_inViewSelector
  , pathComponentCellAtPoint_withFrame_inViewSelector
  , mouseEntered_withFrame_inViewSelector
  , mouseExited_withFrame_inViewSelector
  , pathStyleSelector
  , setPathStyleSelector
  , urlSelector
  , setURLSelector
  , allowedTypesSelector
  , setAllowedTypesSelector
  , delegateSelector
  , setDelegateSelector
  , pathComponentCellClassSelector
  , pathComponentCellsSelector
  , setPathComponentCellsSelector
  , clickedPathComponentCellSelector
  , doubleActionSelector
  , setDoubleActionSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , placeholderStringSelector
  , setPlaceholderStringSelector
  , placeholderAttributedStringSelector
  , setPlaceholderAttributedStringSelector

  -- * Enum types
  , NSPathStyle(NSPathStyle)
  , pattern NSPathStyleStandard
  , pattern NSPathStylePopUp
  , pattern NSPathStyleNavigationBar

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- setObjectValue:@
setObjectValue :: IsNSPathCell nsPathCell => nsPathCell -> RawId -> IO ()
setObjectValue nsPathCell  obj_ =
    sendMsg nsPathCell (mkSelector "setObjectValue:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @- rectOfPathComponentCell:withFrame:inView:@
rectOfPathComponentCell_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSPathComponentCell cell, IsNSView view) => nsPathCell -> cell -> NSRect -> view -> IO NSRect
rectOfPathComponentCell_withFrame_inView nsPathCell  cell frame view =
  withObjCPtr cell $ \raw_cell ->
    withObjCPtr view $ \raw_view ->
        sendMsgStret nsPathCell (mkSelector "rectOfPathComponentCell:withFrame:inView:") retNSRect [argPtr (castPtr raw_cell :: Ptr ()), argNSRect frame, argPtr (castPtr raw_view :: Ptr ())]

-- | @- pathComponentCellAtPoint:withFrame:inView:@
pathComponentCellAtPoint_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSView view) => nsPathCell -> NSPoint -> NSRect -> view -> IO (Id NSPathComponentCell)
pathComponentCellAtPoint_withFrame_inView nsPathCell  point frame view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsPathCell (mkSelector "pathComponentCellAtPoint:withFrame:inView:") (retPtr retVoid) [argNSPoint point, argNSRect frame, argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- mouseEntered:withFrame:inView:@
mouseEntered_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSEvent event, IsNSView view) => nsPathCell -> event -> NSRect -> view -> IO ()
mouseEntered_withFrame_inView nsPathCell  event frame view =
  withObjCPtr event $ \raw_event ->
    withObjCPtr view $ \raw_view ->
        sendMsg nsPathCell (mkSelector "mouseEntered:withFrame:inView:") retVoid [argPtr (castPtr raw_event :: Ptr ()), argNSRect frame, argPtr (castPtr raw_view :: Ptr ())]

-- | @- mouseExited:withFrame:inView:@
mouseExited_withFrame_inView :: (IsNSPathCell nsPathCell, IsNSEvent event, IsNSView view) => nsPathCell -> event -> NSRect -> view -> IO ()
mouseExited_withFrame_inView nsPathCell  event frame view =
  withObjCPtr event $ \raw_event ->
    withObjCPtr view $ \raw_view ->
        sendMsg nsPathCell (mkSelector "mouseExited:withFrame:inView:") retVoid [argPtr (castPtr raw_event :: Ptr ()), argNSRect frame, argPtr (castPtr raw_view :: Ptr ())]

-- | @- pathStyle@
pathStyle :: IsNSPathCell nsPathCell => nsPathCell -> IO NSPathStyle
pathStyle nsPathCell  =
    fmap (coerce :: CLong -> NSPathStyle) $ sendMsg nsPathCell (mkSelector "pathStyle") retCLong []

-- | @- setPathStyle:@
setPathStyle :: IsNSPathCell nsPathCell => nsPathCell -> NSPathStyle -> IO ()
setPathStyle nsPathCell  value =
    sendMsg nsPathCell (mkSelector "setPathStyle:") retVoid [argCLong (coerce value)]

-- | @- URL@
url :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSURL)
url nsPathCell  =
    sendMsg nsPathCell (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsNSPathCell nsPathCell, IsNSURL value) => nsPathCell -> value -> IO ()
setURL nsPathCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPathCell (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowedTypes@
allowedTypes :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSArray)
allowedTypes nsPathCell  =
    sendMsg nsPathCell (mkSelector "allowedTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAllowedTypes:@
setAllowedTypes :: (IsNSPathCell nsPathCell, IsNSArray value) => nsPathCell -> value -> IO ()
setAllowedTypes nsPathCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPathCell (mkSelector "setAllowedTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSPathCell nsPathCell => nsPathCell -> IO RawId
delegate nsPathCell  =
    fmap (RawId . castPtr) $ sendMsg nsPathCell (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSPathCell nsPathCell => nsPathCell -> RawId -> IO ()
setDelegate nsPathCell  value =
    sendMsg nsPathCell (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @+ pathComponentCellClass@
pathComponentCellClass :: IO Class
pathComponentCellClass  =
  do
    cls' <- getRequiredClass "NSPathCell"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "pathComponentCellClass") (retPtr retVoid) []

-- | @- pathComponentCells@
pathComponentCells :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSArray)
pathComponentCells nsPathCell  =
    sendMsg nsPathCell (mkSelector "pathComponentCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPathComponentCells:@
setPathComponentCells :: (IsNSPathCell nsPathCell, IsNSArray value) => nsPathCell -> value -> IO ()
setPathComponentCells nsPathCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPathCell (mkSelector "setPathComponentCells:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clickedPathComponentCell@
clickedPathComponentCell :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSPathComponentCell)
clickedPathComponentCell nsPathCell  =
    sendMsg nsPathCell (mkSelector "clickedPathComponentCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- doubleAction@
doubleAction :: IsNSPathCell nsPathCell => nsPathCell -> IO Selector
doubleAction nsPathCell  =
    fmap (Selector . castPtr) $ sendMsg nsPathCell (mkSelector "doubleAction") (retPtr retVoid) []

-- | @- setDoubleAction:@
setDoubleAction :: IsNSPathCell nsPathCell => nsPathCell -> Selector -> IO ()
setDoubleAction nsPathCell  value =
    sendMsg nsPathCell (mkSelector "setDoubleAction:") retVoid [argPtr (unSelector value)]

-- | @- backgroundColor@
backgroundColor :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSColor)
backgroundColor nsPathCell  =
    sendMsg nsPathCell (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSPathCell nsPathCell, IsNSColor value) => nsPathCell -> value -> IO ()
setBackgroundColor nsPathCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPathCell (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- placeholderString@
placeholderString :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSString)
placeholderString nsPathCell  =
    sendMsg nsPathCell (mkSelector "placeholderString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSPathCell nsPathCell, IsNSString value) => nsPathCell -> value -> IO ()
setPlaceholderString nsPathCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPathCell (mkSelector "setPlaceholderString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSPathCell nsPathCell => nsPathCell -> IO (Id NSAttributedString)
placeholderAttributedString nsPathCell  =
    sendMsg nsPathCell (mkSelector "placeholderAttributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSPathCell nsPathCell, IsNSAttributedString value) => nsPathCell -> value -> IO ()
setPlaceholderAttributedString nsPathCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPathCell (mkSelector "setPlaceholderAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @rectOfPathComponentCell:withFrame:inView:@
rectOfPathComponentCell_withFrame_inViewSelector :: Selector
rectOfPathComponentCell_withFrame_inViewSelector = mkSelector "rectOfPathComponentCell:withFrame:inView:"

-- | @Selector@ for @pathComponentCellAtPoint:withFrame:inView:@
pathComponentCellAtPoint_withFrame_inViewSelector :: Selector
pathComponentCellAtPoint_withFrame_inViewSelector = mkSelector "pathComponentCellAtPoint:withFrame:inView:"

-- | @Selector@ for @mouseEntered:withFrame:inView:@
mouseEntered_withFrame_inViewSelector :: Selector
mouseEntered_withFrame_inViewSelector = mkSelector "mouseEntered:withFrame:inView:"

-- | @Selector@ for @mouseExited:withFrame:inView:@
mouseExited_withFrame_inViewSelector :: Selector
mouseExited_withFrame_inViewSelector = mkSelector "mouseExited:withFrame:inView:"

-- | @Selector@ for @pathStyle@
pathStyleSelector :: Selector
pathStyleSelector = mkSelector "pathStyle"

-- | @Selector@ for @setPathStyle:@
setPathStyleSelector :: Selector
setPathStyleSelector = mkSelector "setPathStyle:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @allowedTypes@
allowedTypesSelector :: Selector
allowedTypesSelector = mkSelector "allowedTypes"

-- | @Selector@ for @setAllowedTypes:@
setAllowedTypesSelector :: Selector
setAllowedTypesSelector = mkSelector "setAllowedTypes:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @pathComponentCellClass@
pathComponentCellClassSelector :: Selector
pathComponentCellClassSelector = mkSelector "pathComponentCellClass"

-- | @Selector@ for @pathComponentCells@
pathComponentCellsSelector :: Selector
pathComponentCellsSelector = mkSelector "pathComponentCells"

-- | @Selector@ for @setPathComponentCells:@
setPathComponentCellsSelector :: Selector
setPathComponentCellsSelector = mkSelector "setPathComponentCells:"

-- | @Selector@ for @clickedPathComponentCell@
clickedPathComponentCellSelector :: Selector
clickedPathComponentCellSelector = mkSelector "clickedPathComponentCell"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @placeholderString@
placeholderStringSelector :: Selector
placeholderStringSelector = mkSelector "placeholderString"

-- | @Selector@ for @setPlaceholderString:@
setPlaceholderStringSelector :: Selector
setPlaceholderStringSelector = mkSelector "setPlaceholderString:"

-- | @Selector@ for @placeholderAttributedString@
placeholderAttributedStringSelector :: Selector
placeholderAttributedStringSelector = mkSelector "placeholderAttributedString"

-- | @Selector@ for @setPlaceholderAttributedString:@
setPlaceholderAttributedStringSelector :: Selector
setPlaceholderAttributedStringSelector = mkSelector "setPlaceholderAttributedString:"

