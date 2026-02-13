{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , colorSelector
  , drawLabel_inRectSelector
  , identifierSelector
  , imageSelector
  , initWithIdentifierSelector
  , initialFirstResponderSelector
  , labelSelector
  , setColorSelector
  , setIdentifierSelector
  , setImageSelector
  , setInitialFirstResponderSelector
  , setLabelSelector
  , setToolTipSelector
  , setViewControllerSelector
  , setViewSelector
  , sizeOfLabelSelector
  , tabStateSelector
  , tabViewItemWithViewControllerSelector
  , tabViewSelector
  , toolTipSelector
  , viewControllerSelector
  , viewSelector

  -- * Enum types
  , NSTabState(NSTabState)
  , pattern NSSelectedTab
  , pattern NSBackgroundTab
  , pattern NSPressedTab

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

-- | Creates an autoreleased TabViewItem that wraps the provided ViewController. The viewController is set as the tab view item’s @-viewController@ property, which sets several of the tab view item’s other properties.
--
-- @viewController@ — The view controller to wrap, used to set the viewController property
--
-- ObjC selector: @+ tabViewItemWithViewController:@
tabViewItemWithViewController :: IsNSViewController viewController => viewController -> IO (Id NSTabViewItem)
tabViewItemWithViewController viewController =
  do
    cls' <- getRequiredClass "NSTabViewItem"
    sendClassMessage cls' tabViewItemWithViewControllerSelector (toNSViewController viewController)

-- | @- initWithIdentifier:@
initWithIdentifier :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> RawId -> IO (Id NSTabViewItem)
initWithIdentifier nsTabViewItem identifier =
  sendOwnedMessage nsTabViewItem initWithIdentifierSelector identifier

-- | @- drawLabel:inRect:@
drawLabel_inRect :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> Bool -> NSRect -> IO ()
drawLabel_inRect nsTabViewItem shouldTruncateLabel labelRect =
  sendMessage nsTabViewItem drawLabel_inRectSelector shouldTruncateLabel labelRect

-- | @- sizeOfLabel:@
sizeOfLabel :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> Bool -> IO NSSize
sizeOfLabel nsTabViewItem computeMin =
  sendMessage nsTabViewItem sizeOfLabelSelector computeMin

-- | @- identifier@
identifier :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO RawId
identifier nsTabViewItem =
  sendMessage nsTabViewItem identifierSelector

-- | @- setIdentifier:@
setIdentifier :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> RawId -> IO ()
setIdentifier nsTabViewItem value =
  sendMessage nsTabViewItem setIdentifierSelector value

-- | @- color@
color :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSColor)
color nsTabViewItem =
  sendMessage nsTabViewItem colorSelector

-- | @- setColor:@
setColor :: (IsNSTabViewItem nsTabViewItem, IsNSColor value) => nsTabViewItem -> value -> IO ()
setColor nsTabViewItem value =
  sendMessage nsTabViewItem setColorSelector (toNSColor value)

-- | @- label@
label :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSString)
label nsTabViewItem =
  sendMessage nsTabViewItem labelSelector

-- | @- setLabel:@
setLabel :: (IsNSTabViewItem nsTabViewItem, IsNSString value) => nsTabViewItem -> value -> IO ()
setLabel nsTabViewItem value =
  sendMessage nsTabViewItem setLabelSelector (toNSString value)

-- | Get and set the image for this tab view item. The image may only be used in certain tab view styles and options.  The default value is nil.
--
-- ObjC selector: @- image@
image :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSImage)
image nsTabViewItem =
  sendMessage nsTabViewItem imageSelector

-- | Get and set the image for this tab view item. The image may only be used in certain tab view styles and options.  The default value is nil.
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSTabViewItem nsTabViewItem, IsNSImage value) => nsTabViewItem -> value -> IO ()
setImage nsTabViewItem value =
  sendMessage nsTabViewItem setImageSelector (toNSImage value)

-- | @- view@
view :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSView)
view nsTabViewItem =
  sendMessage nsTabViewItem viewSelector

-- | @- setView:@
setView :: (IsNSTabViewItem nsTabViewItem, IsNSView value) => nsTabViewItem -> value -> IO ()
setView nsTabViewItem value =
  sendMessage nsTabViewItem setViewSelector (toNSView value)

-- | The view controller wrapped by the tab view item. This property must be set if the tab view item will be added to an NSTabViewController, but can also be used if the tab view item is added to an NSTabView.  If this is set, the tab view item will forward @-view@ calls onto the viewController. Setting a viewController will also set the following properties on the tab view item: @-identifier@ from the address of the viewController, @-label@ from the viewController's title, and @-image@ based on the classname as the view controller. An image named "ViewControllerClassName-TabViewItem" will be searched for first, followed by "ViewControllerClassName". It will search first using +[NSImage imageNamed:], then in @viewController.nibBundle,@ and lastly in the bundle containing the view controller's class. As defined by: -[NSImage imageNamed:imageName], -[viewController.nibBundle imageForResource:imageName], -[[NSBundle bundleForClass:[viewController class]] imageForResource:imageName]. One pass with imageName as [NSStringFromClass([viewController class]) stringByAppendingString:"-TabViewItem"], followed by imageName as NSStringFromClass([viewController class]).
--
-- ObjC selector: @- viewController@
viewController :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSViewController)
viewController nsTabViewItem =
  sendMessage nsTabViewItem viewControllerSelector

-- | The view controller wrapped by the tab view item. This property must be set if the tab view item will be added to an NSTabViewController, but can also be used if the tab view item is added to an NSTabView.  If this is set, the tab view item will forward @-view@ calls onto the viewController. Setting a viewController will also set the following properties on the tab view item: @-identifier@ from the address of the viewController, @-label@ from the viewController's title, and @-image@ based on the classname as the view controller. An image named "ViewControllerClassName-TabViewItem" will be searched for first, followed by "ViewControllerClassName". It will search first using +[NSImage imageNamed:], then in @viewController.nibBundle,@ and lastly in the bundle containing the view controller's class. As defined by: -[NSImage imageNamed:imageName], -[viewController.nibBundle imageForResource:imageName], -[[NSBundle bundleForClass:[viewController class]] imageForResource:imageName]. One pass with imageName as [NSStringFromClass([viewController class]) stringByAppendingString:"-TabViewItem"], followed by imageName as NSStringFromClass([viewController class]).
--
-- ObjC selector: @- setViewController:@
setViewController :: (IsNSTabViewItem nsTabViewItem, IsNSViewController value) => nsTabViewItem -> value -> IO ()
setViewController nsTabViewItem value =
  sendMessage nsTabViewItem setViewControllerSelector (toNSViewController value)

-- | @- tabState@
tabState :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO NSTabState
tabState nsTabViewItem =
  sendMessage nsTabViewItem tabStateSelector

-- | @- tabView@
tabView :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSTabView)
tabView nsTabViewItem =
  sendMessage nsTabViewItem tabViewSelector

-- | @- initialFirstResponder@
initialFirstResponder :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSView)
initialFirstResponder nsTabViewItem =
  sendOwnedMessage nsTabViewItem initialFirstResponderSelector

-- | @- setInitialFirstResponder:@
setInitialFirstResponder :: (IsNSTabViewItem nsTabViewItem, IsNSView value) => nsTabViewItem -> value -> IO ()
setInitialFirstResponder nsTabViewItem value =
  sendMessage nsTabViewItem setInitialFirstResponderSelector (toNSView value)

-- | @- toolTip@
toolTip :: IsNSTabViewItem nsTabViewItem => nsTabViewItem -> IO (Id NSString)
toolTip nsTabViewItem =
  sendMessage nsTabViewItem toolTipSelector

-- | @- setToolTip:@
setToolTip :: (IsNSTabViewItem nsTabViewItem, IsNSString value) => nsTabViewItem -> value -> IO ()
setToolTip nsTabViewItem value =
  sendMessage nsTabViewItem setToolTipSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tabViewItemWithViewController:@
tabViewItemWithViewControllerSelector :: Selector '[Id NSViewController] (Id NSTabViewItem)
tabViewItemWithViewControllerSelector = mkSelector "tabViewItemWithViewController:"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[RawId] (Id NSTabViewItem)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @drawLabel:inRect:@
drawLabel_inRectSelector :: Selector '[Bool, NSRect] ()
drawLabel_inRectSelector = mkSelector "drawLabel:inRect:"

-- | @Selector@ for @sizeOfLabel:@
sizeOfLabelSelector :: Selector '[Bool] NSSize
sizeOfLabelSelector = mkSelector "sizeOfLabel:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] RawId
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[RawId] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @viewController@
viewControllerSelector :: Selector '[] (Id NSViewController)
viewControllerSelector = mkSelector "viewController"

-- | @Selector@ for @setViewController:@
setViewControllerSelector :: Selector '[Id NSViewController] ()
setViewControllerSelector = mkSelector "setViewController:"

-- | @Selector@ for @tabState@
tabStateSelector :: Selector '[] NSTabState
tabStateSelector = mkSelector "tabState"

-- | @Selector@ for @tabView@
tabViewSelector :: Selector '[] (Id NSTabView)
tabViewSelector = mkSelector "tabView"

-- | @Selector@ for @initialFirstResponder@
initialFirstResponderSelector :: Selector '[] (Id NSView)
initialFirstResponderSelector = mkSelector "initialFirstResponder"

-- | @Selector@ for @setInitialFirstResponder:@
setInitialFirstResponderSelector :: Selector '[Id NSView] ()
setInitialFirstResponderSelector = mkSelector "setInitialFirstResponder:"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] (Id NSString)
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector '[Id NSString] ()
setToolTipSelector = mkSelector "setToolTip:"

