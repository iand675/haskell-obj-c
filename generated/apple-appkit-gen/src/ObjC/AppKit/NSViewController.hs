{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSViewController@.
module ObjC.AppKit.NSViewController
  ( NSViewController
  , IsNSViewController(..)
  , initWithNibName_bundle
  , initWithCoder
  , loadView
  , loadViewIfNeeded
  , commitEditingWithDelegate_didCommitSelector_contextInfo
  , commitEditing
  , discardEditing
  , viewDidLoad
  , viewWillAppear
  , viewDidAppear
  , viewWillDisappear
  , viewDidDisappear
  , updateViewConstraints
  , viewWillLayout
  , viewDidLayout
  , addChildViewController
  , removeFromParentViewController
  , insertChildViewController_atIndex
  , removeChildViewControllerAtIndex
  , preferredContentSizeDidChangeForViewController
  , viewWillTransitionToSize
  , presentViewControllerAsSheet
  , presentViewControllerAsModalWindow
  , presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior
  , presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContent
  , transitionFromViewController_toViewController_options_completionHandler
  , presentViewController_animator
  , dismissViewController
  , dismissController
  , nibName
  , nibBundle
  , representedObject
  , setRepresentedObject
  , title
  , setTitle
  , view
  , setView
  , viewIfLoaded
  , viewLoaded
  , preferredContentSize
  , setPreferredContentSize
  , extensionContext
  , sourceItemView
  , setSourceItemView
  , preferredScreenOrigin
  , setPreferredScreenOrigin
  , preferredMinimumSize
  , preferredMaximumSize
  , storyboard
  , parentViewController
  , childViewControllers
  , setChildViewControllers
  , presentedViewControllers
  , presentingViewController
  , addChildViewControllerSelector
  , childViewControllersSelector
  , commitEditingSelector
  , commitEditingWithDelegate_didCommitSelector_contextInfoSelector
  , discardEditingSelector
  , dismissControllerSelector
  , dismissViewControllerSelector
  , extensionContextSelector
  , initWithCoderSelector
  , initWithNibName_bundleSelector
  , insertChildViewController_atIndexSelector
  , loadViewIfNeededSelector
  , loadViewSelector
  , nibBundleSelector
  , nibNameSelector
  , parentViewControllerSelector
  , preferredContentSizeDidChangeForViewControllerSelector
  , preferredContentSizeSelector
  , preferredMaximumSizeSelector
  , preferredMinimumSizeSelector
  , preferredScreenOriginSelector
  , presentViewControllerAsModalWindowSelector
  , presentViewControllerAsSheetSelector
  , presentViewController_animatorSelector
  , presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector
  , presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector
  , presentedViewControllersSelector
  , presentingViewControllerSelector
  , removeChildViewControllerAtIndexSelector
  , removeFromParentViewControllerSelector
  , representedObjectSelector
  , setChildViewControllersSelector
  , setPreferredContentSizeSelector
  , setPreferredScreenOriginSelector
  , setRepresentedObjectSelector
  , setSourceItemViewSelector
  , setTitleSelector
  , setViewSelector
  , sourceItemViewSelector
  , storyboardSelector
  , titleSelector
  , transitionFromViewController_toViewController_options_completionHandlerSelector
  , updateViewConstraintsSelector
  , viewDidAppearSelector
  , viewDidDisappearSelector
  , viewDidLayoutSelector
  , viewDidLoadSelector
  , viewIfLoadedSelector
  , viewLoadedSelector
  , viewSelector
  , viewWillAppearSelector
  , viewWillDisappearSelector
  , viewWillLayoutSelector
  , viewWillTransitionToSizeSelector

  -- * Enum types
  , NSPopoverBehavior(NSPopoverBehavior)
  , pattern NSPopoverBehaviorApplicationDefined
  , pattern NSPopoverBehaviorTransient
  , pattern NSPopoverBehaviorSemitransient
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge
  , NSViewControllerTransitionOptions(NSViewControllerTransitionOptions)
  , pattern NSViewControllerTransitionNone
  , pattern NSViewControllerTransitionCrossfade
  , pattern NSViewControllerTransitionSlideUp
  , pattern NSViewControllerTransitionSlideDown
  , pattern NSViewControllerTransitionSlideLeft
  , pattern NSViewControllerTransitionSlideRight
  , pattern NSViewControllerTransitionSlideForward
  , pattern NSViewControllerTransitionSlideBackward
  , pattern NSViewControllerTransitionAllowUserInteraction

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsNSViewController nsViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => nsViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id NSViewController)
initWithNibName_bundle nsViewController nibNameOrNil nibBundleOrNil =
  sendOwnedMessage nsViewController initWithNibName_bundleSelector (toNSString nibNameOrNil) (toNSBundle nibBundleOrNil)

-- | @- initWithCoder:@
initWithCoder :: (IsNSViewController nsViewController, IsNSCoder coder) => nsViewController -> coder -> IO (Id NSViewController)
initWithCoder nsViewController coder =
  sendOwnedMessage nsViewController initWithCoderSelector (toNSCoder coder)

-- | @- loadView@
loadView :: IsNSViewController nsViewController => nsViewController -> IO ()
loadView nsViewController =
  sendMessage nsViewController loadViewSelector

-- | @- loadViewIfNeeded@
loadViewIfNeeded :: IsNSViewController nsViewController => nsViewController -> IO ()
loadViewIfNeeded nsViewController =
  sendMessage nsViewController loadViewIfNeededSelector

-- | @- commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfo :: IsNSViewController nsViewController => nsViewController -> RawId -> Sel -> Ptr () -> IO ()
commitEditingWithDelegate_didCommitSelector_contextInfo nsViewController delegate didCommitSelector contextInfo =
  sendMessage nsViewController commitEditingWithDelegate_didCommitSelector_contextInfoSelector delegate didCommitSelector contextInfo

-- | @- commitEditing@
commitEditing :: IsNSViewController nsViewController => nsViewController -> IO Bool
commitEditing nsViewController =
  sendMessage nsViewController commitEditingSelector

-- | @- discardEditing@
discardEditing :: IsNSViewController nsViewController => nsViewController -> IO ()
discardEditing nsViewController =
  sendMessage nsViewController discardEditingSelector

-- | @- viewDidLoad@
viewDidLoad :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidLoad nsViewController =
  sendMessage nsViewController viewDidLoadSelector

-- | @- viewWillAppear@
viewWillAppear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewWillAppear nsViewController =
  sendMessage nsViewController viewWillAppearSelector

-- | @- viewDidAppear@
viewDidAppear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidAppear nsViewController =
  sendMessage nsViewController viewDidAppearSelector

-- | @- viewWillDisappear@
viewWillDisappear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewWillDisappear nsViewController =
  sendMessage nsViewController viewWillDisappearSelector

-- | @- viewDidDisappear@
viewDidDisappear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidDisappear nsViewController =
  sendMessage nsViewController viewDidDisappearSelector

-- | @- updateViewConstraints@
updateViewConstraints :: IsNSViewController nsViewController => nsViewController -> IO ()
updateViewConstraints nsViewController =
  sendMessage nsViewController updateViewConstraintsSelector

-- | @- viewWillLayout@
viewWillLayout :: IsNSViewController nsViewController => nsViewController -> IO ()
viewWillLayout nsViewController =
  sendMessage nsViewController viewWillLayoutSelector

-- | @- viewDidLayout@
viewDidLayout :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidLayout nsViewController =
  sendMessage nsViewController viewDidLayoutSelector

-- | @- addChildViewController:@
addChildViewController :: (IsNSViewController nsViewController, IsNSViewController childViewController) => nsViewController -> childViewController -> IO ()
addChildViewController nsViewController childViewController =
  sendMessage nsViewController addChildViewControllerSelector (toNSViewController childViewController)

-- | @- removeFromParentViewController@
removeFromParentViewController :: IsNSViewController nsViewController => nsViewController -> IO ()
removeFromParentViewController nsViewController =
  sendMessage nsViewController removeFromParentViewControllerSelector

-- | @- insertChildViewController:atIndex:@
insertChildViewController_atIndex :: (IsNSViewController nsViewController, IsNSViewController childViewController) => nsViewController -> childViewController -> CLong -> IO ()
insertChildViewController_atIndex nsViewController childViewController index =
  sendMessage nsViewController insertChildViewController_atIndexSelector (toNSViewController childViewController) index

-- | @- removeChildViewControllerAtIndex:@
removeChildViewControllerAtIndex :: IsNSViewController nsViewController => nsViewController -> CLong -> IO ()
removeChildViewControllerAtIndex nsViewController index =
  sendMessage nsViewController removeChildViewControllerAtIndexSelector index

-- | @- preferredContentSizeDidChangeForViewController:@
preferredContentSizeDidChangeForViewController :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
preferredContentSizeDidChangeForViewController nsViewController viewController =
  sendMessage nsViewController preferredContentSizeDidChangeForViewControllerSelector (toNSViewController viewController)

-- | @- viewWillTransitionToSize:@
viewWillTransitionToSize :: IsNSViewController nsViewController => nsViewController -> NSSize -> IO ()
viewWillTransitionToSize nsViewController newSize =
  sendMessage nsViewController viewWillTransitionToSizeSelector newSize

-- | @- presentViewControllerAsSheet:@
presentViewControllerAsSheet :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
presentViewControllerAsSheet nsViewController viewController =
  sendMessage nsViewController presentViewControllerAsSheetSelector (toNSViewController viewController)

-- | @- presentViewControllerAsModalWindow:@
presentViewControllerAsModalWindow :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
presentViewControllerAsModalWindow nsViewController viewController =
  sendMessage nsViewController presentViewControllerAsModalWindowSelector (toNSViewController viewController)

-- | @- presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior :: (IsNSViewController nsViewController, IsNSViewController viewController, IsNSView positioningView) => nsViewController -> viewController -> NSRect -> positioningView -> NSRectEdge -> NSPopoverBehavior -> IO ()
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior nsViewController viewController positioningRect positioningView preferredEdge behavior =
  sendMessage nsViewController presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector (toNSViewController viewController) positioningRect (toNSView positioningView) preferredEdge behavior

-- | @- presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContent :: (IsNSViewController nsViewController, IsNSViewController viewController, IsNSView positioningView) => nsViewController -> viewController -> NSRect -> positioningView -> NSRectEdge -> NSPopoverBehavior -> Bool -> IO ()
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContent nsViewController viewController positioningRect positioningView preferredEdge behavior hasFullSizeContent =
  sendMessage nsViewController presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector (toNSViewController viewController) positioningRect (toNSView positioningView) preferredEdge behavior hasFullSizeContent

-- | @- transitionFromViewController:toViewController:options:completionHandler:@
transitionFromViewController_toViewController_options_completionHandler :: (IsNSViewController nsViewController, IsNSViewController fromViewController, IsNSViewController toViewController) => nsViewController -> fromViewController -> toViewController -> NSViewControllerTransitionOptions -> Ptr () -> IO ()
transitionFromViewController_toViewController_options_completionHandler nsViewController fromViewController toViewController options completion =
  sendMessage nsViewController transitionFromViewController_toViewController_options_completionHandlerSelector (toNSViewController fromViewController) (toNSViewController toViewController) options completion

-- | @- presentViewController:animator:@
presentViewController_animator :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> RawId -> IO ()
presentViewController_animator nsViewController viewController animator =
  sendMessage nsViewController presentViewController_animatorSelector (toNSViewController viewController) animator

-- | @- dismissViewController:@
dismissViewController :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
dismissViewController nsViewController viewController =
  sendMessage nsViewController dismissViewControllerSelector (toNSViewController viewController)

-- | @- dismissController:@
dismissController :: IsNSViewController nsViewController => nsViewController -> RawId -> IO ()
dismissController nsViewController sender =
  sendMessage nsViewController dismissControllerSelector sender

-- | @- nibName@
nibName :: IsNSViewController nsViewController => nsViewController -> IO (Id NSString)
nibName nsViewController =
  sendMessage nsViewController nibNameSelector

-- | @- nibBundle@
nibBundle :: IsNSViewController nsViewController => nsViewController -> IO (Id NSBundle)
nibBundle nsViewController =
  sendMessage nsViewController nibBundleSelector

-- | @- representedObject@
representedObject :: IsNSViewController nsViewController => nsViewController -> IO RawId
representedObject nsViewController =
  sendMessage nsViewController representedObjectSelector

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSViewController nsViewController => nsViewController -> RawId -> IO ()
setRepresentedObject nsViewController value =
  sendMessage nsViewController setRepresentedObjectSelector value

-- | @- title@
title :: IsNSViewController nsViewController => nsViewController -> IO (Id NSString)
title nsViewController =
  sendMessage nsViewController titleSelector

-- | @- setTitle:@
setTitle :: (IsNSViewController nsViewController, IsNSString value) => nsViewController -> value -> IO ()
setTitle nsViewController value =
  sendMessage nsViewController setTitleSelector (toNSString value)

-- | @- view@
view :: IsNSViewController nsViewController => nsViewController -> IO (Id NSView)
view nsViewController =
  sendMessage nsViewController viewSelector

-- | @- setView:@
setView :: (IsNSViewController nsViewController, IsNSView value) => nsViewController -> value -> IO ()
setView nsViewController value =
  sendMessage nsViewController setViewSelector (toNSView value)

-- | @- viewIfLoaded@
viewIfLoaded :: IsNSViewController nsViewController => nsViewController -> IO (Id NSView)
viewIfLoaded nsViewController =
  sendMessage nsViewController viewIfLoadedSelector

-- | @- viewLoaded@
viewLoaded :: IsNSViewController nsViewController => nsViewController -> IO Bool
viewLoaded nsViewController =
  sendMessage nsViewController viewLoadedSelector

-- | @- preferredContentSize@
preferredContentSize :: IsNSViewController nsViewController => nsViewController -> IO NSSize
preferredContentSize nsViewController =
  sendMessage nsViewController preferredContentSizeSelector

-- | @- setPreferredContentSize:@
setPreferredContentSize :: IsNSViewController nsViewController => nsViewController -> NSSize -> IO ()
setPreferredContentSize nsViewController value =
  sendMessage nsViewController setPreferredContentSizeSelector value

-- | @- extensionContext@
extensionContext :: IsNSViewController nsViewController => nsViewController -> IO (Id NSExtensionContext)
extensionContext nsViewController =
  sendMessage nsViewController extensionContextSelector

-- | @- sourceItemView@
sourceItemView :: IsNSViewController nsViewController => nsViewController -> IO (Id NSView)
sourceItemView nsViewController =
  sendMessage nsViewController sourceItemViewSelector

-- | @- setSourceItemView:@
setSourceItemView :: (IsNSViewController nsViewController, IsNSView value) => nsViewController -> value -> IO ()
setSourceItemView nsViewController value =
  sendMessage nsViewController setSourceItemViewSelector (toNSView value)

-- | @- preferredScreenOrigin@
preferredScreenOrigin :: IsNSViewController nsViewController => nsViewController -> IO NSPoint
preferredScreenOrigin nsViewController =
  sendMessage nsViewController preferredScreenOriginSelector

-- | @- setPreferredScreenOrigin:@
setPreferredScreenOrigin :: IsNSViewController nsViewController => nsViewController -> NSPoint -> IO ()
setPreferredScreenOrigin nsViewController value =
  sendMessage nsViewController setPreferredScreenOriginSelector value

-- | @- preferredMinimumSize@
preferredMinimumSize :: IsNSViewController nsViewController => nsViewController -> IO NSSize
preferredMinimumSize nsViewController =
  sendMessage nsViewController preferredMinimumSizeSelector

-- | @- preferredMaximumSize@
preferredMaximumSize :: IsNSViewController nsViewController => nsViewController -> IO NSSize
preferredMaximumSize nsViewController =
  sendMessage nsViewController preferredMaximumSizeSelector

-- | @- storyboard@
storyboard :: IsNSViewController nsViewController => nsViewController -> IO (Id NSStoryboard)
storyboard nsViewController =
  sendMessage nsViewController storyboardSelector

-- | @- parentViewController@
parentViewController :: IsNSViewController nsViewController => nsViewController -> IO (Id NSViewController)
parentViewController nsViewController =
  sendMessage nsViewController parentViewControllerSelector

-- | @- childViewControllers@
childViewControllers :: IsNSViewController nsViewController => nsViewController -> IO (Id NSArray)
childViewControllers nsViewController =
  sendMessage nsViewController childViewControllersSelector

-- | @- setChildViewControllers:@
setChildViewControllers :: (IsNSViewController nsViewController, IsNSArray value) => nsViewController -> value -> IO ()
setChildViewControllers nsViewController value =
  sendMessage nsViewController setChildViewControllersSelector (toNSArray value)

-- | @- presentedViewControllers@
presentedViewControllers :: IsNSViewController nsViewController => nsViewController -> IO (Id NSArray)
presentedViewControllers nsViewController =
  sendMessage nsViewController presentedViewControllersSelector

-- | @- presentingViewController@
presentingViewController :: IsNSViewController nsViewController => nsViewController -> IO (Id NSViewController)
presentingViewController nsViewController =
  sendMessage nsViewController presentingViewControllerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id NSViewController)
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSViewController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @loadView@
loadViewSelector :: Selector '[] ()
loadViewSelector = mkSelector "loadView"

-- | @Selector@ for @loadViewIfNeeded@
loadViewIfNeededSelector :: Selector '[] ()
loadViewIfNeededSelector = mkSelector "loadViewIfNeeded"

-- | @Selector@ for @commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
commitEditingWithDelegate_didCommitSelector_contextInfoSelector = mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:"

-- | @Selector@ for @commitEditing@
commitEditingSelector :: Selector '[] Bool
commitEditingSelector = mkSelector "commitEditing"

-- | @Selector@ for @discardEditing@
discardEditingSelector :: Selector '[] ()
discardEditingSelector = mkSelector "discardEditing"

-- | @Selector@ for @viewDidLoad@
viewDidLoadSelector :: Selector '[] ()
viewDidLoadSelector = mkSelector "viewDidLoad"

-- | @Selector@ for @viewWillAppear@
viewWillAppearSelector :: Selector '[] ()
viewWillAppearSelector = mkSelector "viewWillAppear"

-- | @Selector@ for @viewDidAppear@
viewDidAppearSelector :: Selector '[] ()
viewDidAppearSelector = mkSelector "viewDidAppear"

-- | @Selector@ for @viewWillDisappear@
viewWillDisappearSelector :: Selector '[] ()
viewWillDisappearSelector = mkSelector "viewWillDisappear"

-- | @Selector@ for @viewDidDisappear@
viewDidDisappearSelector :: Selector '[] ()
viewDidDisappearSelector = mkSelector "viewDidDisappear"

-- | @Selector@ for @updateViewConstraints@
updateViewConstraintsSelector :: Selector '[] ()
updateViewConstraintsSelector = mkSelector "updateViewConstraints"

-- | @Selector@ for @viewWillLayout@
viewWillLayoutSelector :: Selector '[] ()
viewWillLayoutSelector = mkSelector "viewWillLayout"

-- | @Selector@ for @viewDidLayout@
viewDidLayoutSelector :: Selector '[] ()
viewDidLayoutSelector = mkSelector "viewDidLayout"

-- | @Selector@ for @addChildViewController:@
addChildViewControllerSelector :: Selector '[Id NSViewController] ()
addChildViewControllerSelector = mkSelector "addChildViewController:"

-- | @Selector@ for @removeFromParentViewController@
removeFromParentViewControllerSelector :: Selector '[] ()
removeFromParentViewControllerSelector = mkSelector "removeFromParentViewController"

-- | @Selector@ for @insertChildViewController:atIndex:@
insertChildViewController_atIndexSelector :: Selector '[Id NSViewController, CLong] ()
insertChildViewController_atIndexSelector = mkSelector "insertChildViewController:atIndex:"

-- | @Selector@ for @removeChildViewControllerAtIndex:@
removeChildViewControllerAtIndexSelector :: Selector '[CLong] ()
removeChildViewControllerAtIndexSelector = mkSelector "removeChildViewControllerAtIndex:"

-- | @Selector@ for @preferredContentSizeDidChangeForViewController:@
preferredContentSizeDidChangeForViewControllerSelector :: Selector '[Id NSViewController] ()
preferredContentSizeDidChangeForViewControllerSelector = mkSelector "preferredContentSizeDidChangeForViewController:"

-- | @Selector@ for @viewWillTransitionToSize:@
viewWillTransitionToSizeSelector :: Selector '[NSSize] ()
viewWillTransitionToSizeSelector = mkSelector "viewWillTransitionToSize:"

-- | @Selector@ for @presentViewControllerAsSheet:@
presentViewControllerAsSheetSelector :: Selector '[Id NSViewController] ()
presentViewControllerAsSheetSelector = mkSelector "presentViewControllerAsSheet:"

-- | @Selector@ for @presentViewControllerAsModalWindow:@
presentViewControllerAsModalWindowSelector :: Selector '[Id NSViewController] ()
presentViewControllerAsModalWindowSelector = mkSelector "presentViewControllerAsModalWindow:"

-- | @Selector@ for @presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector :: Selector '[Id NSViewController, NSRect, Id NSView, NSRectEdge, NSPopoverBehavior] ()
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector = mkSelector "presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:"

-- | @Selector@ for @presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector :: Selector '[Id NSViewController, NSRect, Id NSView, NSRectEdge, NSPopoverBehavior, Bool] ()
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector = mkSelector "presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:"

-- | @Selector@ for @transitionFromViewController:toViewController:options:completionHandler:@
transitionFromViewController_toViewController_options_completionHandlerSelector :: Selector '[Id NSViewController, Id NSViewController, NSViewControllerTransitionOptions, Ptr ()] ()
transitionFromViewController_toViewController_options_completionHandlerSelector = mkSelector "transitionFromViewController:toViewController:options:completionHandler:"

-- | @Selector@ for @presentViewController:animator:@
presentViewController_animatorSelector :: Selector '[Id NSViewController, RawId] ()
presentViewController_animatorSelector = mkSelector "presentViewController:animator:"

-- | @Selector@ for @dismissViewController:@
dismissViewControllerSelector :: Selector '[Id NSViewController] ()
dismissViewControllerSelector = mkSelector "dismissViewController:"

-- | @Selector@ for @dismissController:@
dismissControllerSelector :: Selector '[RawId] ()
dismissControllerSelector = mkSelector "dismissController:"

-- | @Selector@ for @nibName@
nibNameSelector :: Selector '[] (Id NSString)
nibNameSelector = mkSelector "nibName"

-- | @Selector@ for @nibBundle@
nibBundleSelector :: Selector '[] (Id NSBundle)
nibBundleSelector = mkSelector "nibBundle"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector '[] RawId
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector '[RawId] ()
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @viewIfLoaded@
viewIfLoadedSelector :: Selector '[] (Id NSView)
viewIfLoadedSelector = mkSelector "viewIfLoaded"

-- | @Selector@ for @viewLoaded@
viewLoadedSelector :: Selector '[] Bool
viewLoadedSelector = mkSelector "viewLoaded"

-- | @Selector@ for @preferredContentSize@
preferredContentSizeSelector :: Selector '[] NSSize
preferredContentSizeSelector = mkSelector "preferredContentSize"

-- | @Selector@ for @setPreferredContentSize:@
setPreferredContentSizeSelector :: Selector '[NSSize] ()
setPreferredContentSizeSelector = mkSelector "setPreferredContentSize:"

-- | @Selector@ for @extensionContext@
extensionContextSelector :: Selector '[] (Id NSExtensionContext)
extensionContextSelector = mkSelector "extensionContext"

-- | @Selector@ for @sourceItemView@
sourceItemViewSelector :: Selector '[] (Id NSView)
sourceItemViewSelector = mkSelector "sourceItemView"

-- | @Selector@ for @setSourceItemView:@
setSourceItemViewSelector :: Selector '[Id NSView] ()
setSourceItemViewSelector = mkSelector "setSourceItemView:"

-- | @Selector@ for @preferredScreenOrigin@
preferredScreenOriginSelector :: Selector '[] NSPoint
preferredScreenOriginSelector = mkSelector "preferredScreenOrigin"

-- | @Selector@ for @setPreferredScreenOrigin:@
setPreferredScreenOriginSelector :: Selector '[NSPoint] ()
setPreferredScreenOriginSelector = mkSelector "setPreferredScreenOrigin:"

-- | @Selector@ for @preferredMinimumSize@
preferredMinimumSizeSelector :: Selector '[] NSSize
preferredMinimumSizeSelector = mkSelector "preferredMinimumSize"

-- | @Selector@ for @preferredMaximumSize@
preferredMaximumSizeSelector :: Selector '[] NSSize
preferredMaximumSizeSelector = mkSelector "preferredMaximumSize"

-- | @Selector@ for @storyboard@
storyboardSelector :: Selector '[] (Id NSStoryboard)
storyboardSelector = mkSelector "storyboard"

-- | @Selector@ for @parentViewController@
parentViewControllerSelector :: Selector '[] (Id NSViewController)
parentViewControllerSelector = mkSelector "parentViewController"

-- | @Selector@ for @childViewControllers@
childViewControllersSelector :: Selector '[] (Id NSArray)
childViewControllersSelector = mkSelector "childViewControllers"

-- | @Selector@ for @setChildViewControllers:@
setChildViewControllersSelector :: Selector '[Id NSArray] ()
setChildViewControllersSelector = mkSelector "setChildViewControllers:"

-- | @Selector@ for @presentedViewControllers@
presentedViewControllersSelector :: Selector '[] (Id NSArray)
presentedViewControllersSelector = mkSelector "presentedViewControllers"

-- | @Selector@ for @presentingViewController@
presentingViewControllerSelector :: Selector '[] (Id NSViewController)
presentingViewControllerSelector = mkSelector "presentingViewController"

