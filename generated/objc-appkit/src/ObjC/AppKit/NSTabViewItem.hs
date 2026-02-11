{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTabViewItem@.
module ObjC.AppKit.NSTabViewItem
  ( NSTabViewItem
  , IsNSTabViewItem(..)
  , tabViewItemWithViewController
  , initWithIdentifier
  , drawLabel_inRect
  , sizeOfLabel
  , identifier
  , setIdentifier
  , color
  , setColor
  , label
  , setLabel
  , view
  , setView
  , tabState
  , tabView
  , initialFirstResponder
  , setInitialFirstResponder
  , tabViewItemWithViewControllerSelector
  , initWithIdentifierSelector
  , drawLabel_inRectSelector
  , sizeOfLabelSelector
  , identifierSelector
  , setIdentifierSelector
  , colorSelector
  , setColorSelector
  , labelSelector
  , setLabelSelector
  , viewSelector
  , setViewSelector
  , tabStateSelector
  , tabViewSelector
  , initialFirstResponderSelector
  , setInitialFirstResponderSelector

  -- * Enum types
  , NSTabState(NSTabState)
  , pattern NSSelectedTab
  , pattern NSBackgroundTab
  , pattern NSPressedTab

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

-- | Creates an autoreleased TabViewItem that wraps the provided ViewController. The viewController is set as the tab view item’s @-viewController@ property, which sets several of the tab view item’s other properties.
--
-- @viewController@ — The view controller to wrap, used to set the viewController property
--
-- ObjC selector: @+ tabViewItemWithViewController:@
tabViewItemWithViewController :: IsNSViewController viewController => viewController -> IO (Id NSTabViewItem)
tabViewItemWithViewController viewController =
  do
    cls' <- getRequiredClass "NSTabViewItem"
    withObjCPtr viewController $ \raw_viewController ->
      sendClassMsg cls' (mkSelector "tabViewItemWithViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithIdentifier:@
initWithIdentifier :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> RawId -> IO (Id NSTabViewItem)
initWithIdentifier nsTabViewItem  identifier =
  sendMsg nsTabViewItem (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr (unRawId identifier) :: Ptr ())] >>= ownedObject . castPtr

-- | @- drawLabel:inRect:@
drawLabel_inRect :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> Bool -> NSRect -> IO ()
drawLabel_inRect nsTabViewItem  shouldTruncateLabel labelRect =
  sendMsg nsTabViewItem (mkSelector "drawLabel:inRect:") retVoid [argCULong (if shouldTruncateLabel then 1 else 0), argNSRect labelRect]

-- | @- sizeOfLabel:@
sizeOfLabel :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> Bool -> IO NSSize
sizeOfLabel nsTabViewItem  computeMin =
  sendMsgStret nsTabViewItem (mkSelector "sizeOfLabel:") retNSSize [argCULong (if computeMin then 1 else 0)]

-- | @- identifier@
identifier :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO RawId
identifier nsTabViewItem  =
  fmap (RawId . castPtr) $ sendMsg nsTabViewItem (mkSelector "identifier") (retPtr retVoid) []

-- | @- setIdentifier:@
setIdentifier :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> RawId -> IO ()
setIdentifier nsTabViewItem  value =
  sendMsg nsTabViewItem (mkSelector "setIdentifier:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- color@
color :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSColor)
color nsTabViewItem  =
  sendMsg nsTabViewItem (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsNSTabViewItem nsTabViewItem, IsNSColor value) => nsTabViewItem -> value -> IO ()
setColor nsTabViewItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTabViewItem (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSString)
label nsTabViewItem  =
  sendMsg nsTabViewItem (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsNSTabViewItem nsTabViewItem, IsNSString value) => nsTabViewItem -> value -> IO ()
setLabel nsTabViewItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTabViewItem (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- view@
view :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSView)
view nsTabViewItem  =
  sendMsg nsTabViewItem (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSTabViewItem nsTabViewItem, IsNSView value) => nsTabViewItem -> value -> IO ()
setView nsTabViewItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTabViewItem (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tabState@
tabState :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO NSTabState
tabState nsTabViewItem  =
  fmap (coerce :: CULong -> NSTabState) $ sendMsg nsTabViewItem (mkSelector "tabState") retCULong []

-- | @- tabView@
tabView :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSTabView)
tabView nsTabViewItem  =
  sendMsg nsTabViewItem (mkSelector "tabView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initialFirstResponder@
initialFirstResponder :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSView)
initialFirstResponder nsTabViewItem  =
  sendMsg nsTabViewItem (mkSelector "initialFirstResponder") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialFirstResponder:@
setInitialFirstResponder :: (IsNSTabViewItem nsTabViewItem, IsNSView value) => nsTabViewItem -> value -> IO ()
setInitialFirstResponder nsTabViewItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTabViewItem (mkSelector "setInitialFirstResponder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tabViewItemWithViewController:@
tabViewItemWithViewControllerSelector :: Selector
tabViewItemWithViewControllerSelector = mkSelector "tabViewItemWithViewController:"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @drawLabel:inRect:@
drawLabel_inRectSelector :: Selector
drawLabel_inRectSelector = mkSelector "drawLabel:inRect:"

-- | @Selector@ for @sizeOfLabel:@
sizeOfLabelSelector :: Selector
sizeOfLabelSelector = mkSelector "sizeOfLabel:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @tabState@
tabStateSelector :: Selector
tabStateSelector = mkSelector "tabState"

-- | @Selector@ for @tabView@
tabViewSelector :: Selector
tabViewSelector = mkSelector "tabView"

-- | @Selector@ for @initialFirstResponder@
initialFirstResponderSelector :: Selector
initialFirstResponderSelector = mkSelector "initialFirstResponder"

-- | @Selector@ for @setInitialFirstResponder:@
setInitialFirstResponderSelector :: Selector
setInitialFirstResponderSelector = mkSelector "setInitialFirstResponder:"

