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
  , image
  , setImage
  , view
  , setView
  , viewController
  , setViewController
  , tabState
  , tabView
  , initialFirstResponder
  , setInitialFirstResponder
  , toolTip
  , setToolTip
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
  , imageSelector
  , setImageSelector
  , viewSelector
  , setViewSelector
  , viewControllerSelector
  , setViewControllerSelector
  , tabStateSelector
  , tabViewSelector
  , initialFirstResponderSelector
  , setInitialFirstResponderSelector
  , toolTipSelector
  , setToolTipSelector

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

-- | Get and set the image for this tab view item. The image may only be used in certain tab view styles and options.  The default value is nil.
--
-- ObjC selector: @- image@
image :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSImage)
image nsTabViewItem  =
    sendMsg nsTabViewItem (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get and set the image for this tab view item. The image may only be used in certain tab view styles and options.  The default value is nil.
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSTabViewItem nsTabViewItem, IsNSImage value) => nsTabViewItem -> value -> IO ()
setImage nsTabViewItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTabViewItem (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- view@
view :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSView)
view nsTabViewItem  =
    sendMsg nsTabViewItem (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSTabViewItem nsTabViewItem, IsNSView value) => nsTabViewItem -> value -> IO ()
setView nsTabViewItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTabViewItem (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The view controller wrapped by the tab view item. This property must be set if the tab view item will be added to an NSTabViewController, but can also be used if the tab view item is added to an NSTabView.  If this is set, the tab view item will forward @-view@ calls onto the viewController. Setting a viewController will also set the following properties on the tab view item: @-identifier@ from the address of the viewController, @-label@ from the viewController's title, and @-image@ based on the classname as the view controller. An image named "ViewControllerClassName-TabViewItem" will be searched for first, followed by "ViewControllerClassName". It will search first using +[NSImage imageNamed:], then in @viewController.nibBundle,@ and lastly in the bundle containing the view controller's class. As defined by: -[NSImage imageNamed:imageName], -[viewController.nibBundle imageForResource:imageName], -[[NSBundle bundleForClass:[viewController class]] imageForResource:imageName]. One pass with imageName as [NSStringFromClass([viewController class]) stringByAppendingString:"-TabViewItem"], followed by imageName as NSStringFromClass([viewController class]).
--
-- ObjC selector: @- viewController@
viewController :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSViewController)
viewController nsTabViewItem  =
    sendMsg nsTabViewItem (mkSelector "viewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view controller wrapped by the tab view item. This property must be set if the tab view item will be added to an NSTabViewController, but can also be used if the tab view item is added to an NSTabView.  If this is set, the tab view item will forward @-view@ calls onto the viewController. Setting a viewController will also set the following properties on the tab view item: @-identifier@ from the address of the viewController, @-label@ from the viewController's title, and @-image@ based on the classname as the view controller. An image named "ViewControllerClassName-TabViewItem" will be searched for first, followed by "ViewControllerClassName". It will search first using +[NSImage imageNamed:], then in @viewController.nibBundle,@ and lastly in the bundle containing the view controller's class. As defined by: -[NSImage imageNamed:imageName], -[viewController.nibBundle imageForResource:imageName], -[[NSBundle bundleForClass:[viewController class]] imageForResource:imageName]. One pass with imageName as [NSStringFromClass([viewController class]) stringByAppendingString:"-TabViewItem"], followed by imageName as NSStringFromClass([viewController class]).
--
-- ObjC selector: @- setViewController:@
setViewController :: (IsNSTabViewItem nsTabViewItem, IsNSViewController value) => nsTabViewItem -> value -> IO ()
setViewController nsTabViewItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTabViewItem (mkSelector "setViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @- toolTip@
toolTip :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSString)
toolTip nsTabViewItem  =
    sendMsg nsTabViewItem (mkSelector "toolTip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToolTip:@
setToolTip :: (IsNSTabViewItem nsTabViewItem, IsNSString value) => nsTabViewItem -> value -> IO ()
setToolTip nsTabViewItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTabViewItem (mkSelector "setToolTip:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @setViewController:@
setViewControllerSelector :: Selector
setViewControllerSelector = mkSelector "setViewController:"

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

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector
setToolTipSelector = mkSelector "setToolTip:"

