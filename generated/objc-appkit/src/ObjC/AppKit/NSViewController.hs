{-# LANGUAGE PatternSynonyms #-}
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
  , viewLoaded
  , preferredContentSize
  , setPreferredContentSize
  , preferredScreenOrigin
  , setPreferredScreenOrigin
  , preferredMinimumSize
  , preferredMaximumSize
  , initWithNibName_bundleSelector
  , initWithCoderSelector
  , loadViewSelector
  , loadViewIfNeededSelector
  , commitEditingWithDelegate_didCommitSelector_contextInfoSelector
  , commitEditingSelector
  , discardEditingSelector
  , viewDidLoadSelector
  , viewWillAppearSelector
  , viewDidAppearSelector
  , viewWillDisappearSelector
  , viewDidDisappearSelector
  , updateViewConstraintsSelector
  , viewWillLayoutSelector
  , viewDidLayoutSelector
  , addChildViewControllerSelector
  , removeFromParentViewControllerSelector
  , insertChildViewController_atIndexSelector
  , removeChildViewControllerAtIndexSelector
  , preferredContentSizeDidChangeForViewControllerSelector
  , viewWillTransitionToSizeSelector
  , presentViewControllerAsSheetSelector
  , presentViewControllerAsModalWindowSelector
  , presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector
  , presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector
  , transitionFromViewController_toViewController_options_completionHandlerSelector
  , presentViewController_animatorSelector
  , dismissViewControllerSelector
  , dismissControllerSelector
  , nibNameSelector
  , nibBundleSelector
  , representedObjectSelector
  , setRepresentedObjectSelector
  , titleSelector
  , setTitleSelector
  , viewSelector
  , setViewSelector
  , viewLoadedSelector
  , preferredContentSizeSelector
  , setPreferredContentSizeSelector
  , preferredScreenOriginSelector
  , setPreferredScreenOriginSelector
  , preferredMinimumSizeSelector
  , preferredMaximumSizeSelector

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsNSViewController nsViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => nsViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id NSViewController)
initWithNibName_bundle nsViewController  nibNameOrNil nibBundleOrNil =
withObjCPtr nibNameOrNil $ \raw_nibNameOrNil ->
  withObjCPtr nibBundleOrNil $ \raw_nibBundleOrNil ->
      sendMsg nsViewController (mkSelector "initWithNibName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibNameOrNil :: Ptr ()), argPtr (castPtr raw_nibBundleOrNil :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSViewController nsViewController, IsNSCoder coder) => nsViewController -> coder -> IO (Id NSViewController)
initWithCoder nsViewController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsViewController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- loadView@
loadView :: IsNSViewController nsViewController => nsViewController -> IO ()
loadView nsViewController  =
  sendMsg nsViewController (mkSelector "loadView") retVoid []

-- | @- loadViewIfNeeded@
loadViewIfNeeded :: IsNSViewController nsViewController => nsViewController -> IO ()
loadViewIfNeeded nsViewController  =
  sendMsg nsViewController (mkSelector "loadViewIfNeeded") retVoid []

-- | @- commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfo :: IsNSViewController nsViewController => nsViewController -> RawId -> Selector -> Ptr () -> IO ()
commitEditingWithDelegate_didCommitSelector_contextInfo nsViewController  delegate didCommitSelector contextInfo =
  sendMsg nsViewController (mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didCommitSelector), argPtr contextInfo]

-- | @- commitEditing@
commitEditing :: IsNSViewController nsViewController => nsViewController -> IO Bool
commitEditing nsViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsViewController (mkSelector "commitEditing") retCULong []

-- | @- discardEditing@
discardEditing :: IsNSViewController nsViewController => nsViewController -> IO ()
discardEditing nsViewController  =
  sendMsg nsViewController (mkSelector "discardEditing") retVoid []

-- | @- viewDidLoad@
viewDidLoad :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidLoad nsViewController  =
  sendMsg nsViewController (mkSelector "viewDidLoad") retVoid []

-- | @- viewWillAppear@
viewWillAppear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewWillAppear nsViewController  =
  sendMsg nsViewController (mkSelector "viewWillAppear") retVoid []

-- | @- viewDidAppear@
viewDidAppear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidAppear nsViewController  =
  sendMsg nsViewController (mkSelector "viewDidAppear") retVoid []

-- | @- viewWillDisappear@
viewWillDisappear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewWillDisappear nsViewController  =
  sendMsg nsViewController (mkSelector "viewWillDisappear") retVoid []

-- | @- viewDidDisappear@
viewDidDisappear :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidDisappear nsViewController  =
  sendMsg nsViewController (mkSelector "viewDidDisappear") retVoid []

-- | @- updateViewConstraints@
updateViewConstraints :: IsNSViewController nsViewController => nsViewController -> IO ()
updateViewConstraints nsViewController  =
  sendMsg nsViewController (mkSelector "updateViewConstraints") retVoid []

-- | @- viewWillLayout@
viewWillLayout :: IsNSViewController nsViewController => nsViewController -> IO ()
viewWillLayout nsViewController  =
  sendMsg nsViewController (mkSelector "viewWillLayout") retVoid []

-- | @- viewDidLayout@
viewDidLayout :: IsNSViewController nsViewController => nsViewController -> IO ()
viewDidLayout nsViewController  =
  sendMsg nsViewController (mkSelector "viewDidLayout") retVoid []

-- | @- addChildViewController:@
addChildViewController :: (IsNSViewController nsViewController, IsNSViewController childViewController) => nsViewController -> childViewController -> IO ()
addChildViewController nsViewController  childViewController =
withObjCPtr childViewController $ \raw_childViewController ->
    sendMsg nsViewController (mkSelector "addChildViewController:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ())]

-- | @- removeFromParentViewController@
removeFromParentViewController :: IsNSViewController nsViewController => nsViewController -> IO ()
removeFromParentViewController nsViewController  =
  sendMsg nsViewController (mkSelector "removeFromParentViewController") retVoid []

-- | @- insertChildViewController:atIndex:@
insertChildViewController_atIndex :: (IsNSViewController nsViewController, IsNSViewController childViewController) => nsViewController -> childViewController -> CLong -> IO ()
insertChildViewController_atIndex nsViewController  childViewController index =
withObjCPtr childViewController $ \raw_childViewController ->
    sendMsg nsViewController (mkSelector "insertChildViewController:atIndex:") retVoid [argPtr (castPtr raw_childViewController :: Ptr ()), argCLong (fromIntegral index)]

-- | @- removeChildViewControllerAtIndex:@
removeChildViewControllerAtIndex :: IsNSViewController nsViewController => nsViewController -> CLong -> IO ()
removeChildViewControllerAtIndex nsViewController  index =
  sendMsg nsViewController (mkSelector "removeChildViewControllerAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- preferredContentSizeDidChangeForViewController:@
preferredContentSizeDidChangeForViewController :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
preferredContentSizeDidChangeForViewController nsViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsViewController (mkSelector "preferredContentSizeDidChangeForViewController:") retVoid [argPtr (castPtr raw_viewController :: Ptr ())]

-- | @- viewWillTransitionToSize:@
viewWillTransitionToSize :: IsNSViewController nsViewController => nsViewController -> NSSize -> IO ()
viewWillTransitionToSize nsViewController  newSize =
  sendMsg nsViewController (mkSelector "viewWillTransitionToSize:") retVoid [argNSSize newSize]

-- | @- presentViewControllerAsSheet:@
presentViewControllerAsSheet :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
presentViewControllerAsSheet nsViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsViewController (mkSelector "presentViewControllerAsSheet:") retVoid [argPtr (castPtr raw_viewController :: Ptr ())]

-- | @- presentViewControllerAsModalWindow:@
presentViewControllerAsModalWindow :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
presentViewControllerAsModalWindow nsViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsViewController (mkSelector "presentViewControllerAsModalWindow:") retVoid [argPtr (castPtr raw_viewController :: Ptr ())]

-- | @- presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior :: (IsNSViewController nsViewController, IsNSViewController viewController, IsNSView positioningView) => nsViewController -> viewController -> NSRect -> positioningView -> NSRectEdge -> NSPopoverBehavior -> IO ()
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior nsViewController  viewController positioningRect positioningView preferredEdge behavior =
withObjCPtr viewController $ \raw_viewController ->
  withObjCPtr positioningView $ \raw_positioningView ->
      sendMsg nsViewController (mkSelector "presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:") retVoid [argPtr (castPtr raw_viewController :: Ptr ()), argNSRect positioningRect, argPtr (castPtr raw_positioningView :: Ptr ()), argCULong (coerce preferredEdge), argCLong (coerce behavior)]

-- | @- presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContent :: (IsNSViewController nsViewController, IsNSViewController viewController, IsNSView positioningView) => nsViewController -> viewController -> NSRect -> positioningView -> NSRectEdge -> NSPopoverBehavior -> Bool -> IO ()
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContent nsViewController  viewController positioningRect positioningView preferredEdge behavior hasFullSizeContent =
withObjCPtr viewController $ \raw_viewController ->
  withObjCPtr positioningView $ \raw_positioningView ->
      sendMsg nsViewController (mkSelector "presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:") retVoid [argPtr (castPtr raw_viewController :: Ptr ()), argNSRect positioningRect, argPtr (castPtr raw_positioningView :: Ptr ()), argCULong (coerce preferredEdge), argCLong (coerce behavior), argCULong (if hasFullSizeContent then 1 else 0)]

-- | @- transitionFromViewController:toViewController:options:completionHandler:@
transitionFromViewController_toViewController_options_completionHandler :: (IsNSViewController nsViewController, IsNSViewController fromViewController, IsNSViewController toViewController) => nsViewController -> fromViewController -> toViewController -> NSViewControllerTransitionOptions -> Ptr () -> IO ()
transitionFromViewController_toViewController_options_completionHandler nsViewController  fromViewController toViewController options completion =
withObjCPtr fromViewController $ \raw_fromViewController ->
  withObjCPtr toViewController $ \raw_toViewController ->
      sendMsg nsViewController (mkSelector "transitionFromViewController:toViewController:options:completionHandler:") retVoid [argPtr (castPtr raw_fromViewController :: Ptr ()), argPtr (castPtr raw_toViewController :: Ptr ()), argCULong (coerce options), argPtr (castPtr completion :: Ptr ())]

-- | @- presentViewController:animator:@
presentViewController_animator :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> RawId -> IO ()
presentViewController_animator nsViewController  viewController animator =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsViewController (mkSelector "presentViewController:animator:") retVoid [argPtr (castPtr raw_viewController :: Ptr ()), argPtr (castPtr (unRawId animator) :: Ptr ())]

-- | @- dismissViewController:@
dismissViewController :: (IsNSViewController nsViewController, IsNSViewController viewController) => nsViewController -> viewController -> IO ()
dismissViewController nsViewController  viewController =
withObjCPtr viewController $ \raw_viewController ->
    sendMsg nsViewController (mkSelector "dismissViewController:") retVoid [argPtr (castPtr raw_viewController :: Ptr ())]

-- | @- dismissController:@
dismissController :: IsNSViewController nsViewController => nsViewController -> RawId -> IO ()
dismissController nsViewController  sender =
  sendMsg nsViewController (mkSelector "dismissController:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- nibName@
nibName :: IsNSViewController nsViewController => nsViewController -> IO (Id NSString)
nibName nsViewController  =
  sendMsg nsViewController (mkSelector "nibName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nibBundle@
nibBundle :: IsNSViewController nsViewController => nsViewController -> IO (Id NSBundle)
nibBundle nsViewController  =
  sendMsg nsViewController (mkSelector "nibBundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- representedObject@
representedObject :: IsNSViewController nsViewController => nsViewController -> IO RawId
representedObject nsViewController  =
  fmap (RawId . castPtr) $ sendMsg nsViewController (mkSelector "representedObject") (retPtr retVoid) []

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSViewController nsViewController => nsViewController -> RawId -> IO ()
setRepresentedObject nsViewController  value =
  sendMsg nsViewController (mkSelector "setRepresentedObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- title@
title :: IsNSViewController nsViewController => nsViewController -> IO (Id NSString)
title nsViewController  =
  sendMsg nsViewController (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSViewController nsViewController, IsNSString value) => nsViewController -> value -> IO ()
setTitle nsViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsViewController (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- view@
view :: IsNSViewController nsViewController => nsViewController -> IO (Id NSView)
view nsViewController  =
  sendMsg nsViewController (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSViewController nsViewController, IsNSView value) => nsViewController -> value -> IO ()
setView nsViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsViewController (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- viewLoaded@
viewLoaded :: IsNSViewController nsViewController => nsViewController -> IO Bool
viewLoaded nsViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsViewController (mkSelector "viewLoaded") retCULong []

-- | @- preferredContentSize@
preferredContentSize :: IsNSViewController nsViewController => nsViewController -> IO NSSize
preferredContentSize nsViewController  =
  sendMsgStret nsViewController (mkSelector "preferredContentSize") retNSSize []

-- | @- setPreferredContentSize:@
setPreferredContentSize :: IsNSViewController nsViewController => nsViewController -> NSSize -> IO ()
setPreferredContentSize nsViewController  value =
  sendMsg nsViewController (mkSelector "setPreferredContentSize:") retVoid [argNSSize value]

-- | @- preferredScreenOrigin@
preferredScreenOrigin :: IsNSViewController nsViewController => nsViewController -> IO NSPoint
preferredScreenOrigin nsViewController  =
  sendMsgStret nsViewController (mkSelector "preferredScreenOrigin") retNSPoint []

-- | @- setPreferredScreenOrigin:@
setPreferredScreenOrigin :: IsNSViewController nsViewController => nsViewController -> NSPoint -> IO ()
setPreferredScreenOrigin nsViewController  value =
  sendMsg nsViewController (mkSelector "setPreferredScreenOrigin:") retVoid [argNSPoint value]

-- | @- preferredMinimumSize@
preferredMinimumSize :: IsNSViewController nsViewController => nsViewController -> IO NSSize
preferredMinimumSize nsViewController  =
  sendMsgStret nsViewController (mkSelector "preferredMinimumSize") retNSSize []

-- | @- preferredMaximumSize@
preferredMaximumSize :: IsNSViewController nsViewController => nsViewController -> IO NSSize
preferredMaximumSize nsViewController  =
  sendMsgStret nsViewController (mkSelector "preferredMaximumSize") retNSSize []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @loadView@
loadViewSelector :: Selector
loadViewSelector = mkSelector "loadView"

-- | @Selector@ for @loadViewIfNeeded@
loadViewIfNeededSelector :: Selector
loadViewIfNeededSelector = mkSelector "loadViewIfNeeded"

-- | @Selector@ for @commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfoSelector :: Selector
commitEditingWithDelegate_didCommitSelector_contextInfoSelector = mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:"

-- | @Selector@ for @commitEditing@
commitEditingSelector :: Selector
commitEditingSelector = mkSelector "commitEditing"

-- | @Selector@ for @discardEditing@
discardEditingSelector :: Selector
discardEditingSelector = mkSelector "discardEditing"

-- | @Selector@ for @viewDidLoad@
viewDidLoadSelector :: Selector
viewDidLoadSelector = mkSelector "viewDidLoad"

-- | @Selector@ for @viewWillAppear@
viewWillAppearSelector :: Selector
viewWillAppearSelector = mkSelector "viewWillAppear"

-- | @Selector@ for @viewDidAppear@
viewDidAppearSelector :: Selector
viewDidAppearSelector = mkSelector "viewDidAppear"

-- | @Selector@ for @viewWillDisappear@
viewWillDisappearSelector :: Selector
viewWillDisappearSelector = mkSelector "viewWillDisappear"

-- | @Selector@ for @viewDidDisappear@
viewDidDisappearSelector :: Selector
viewDidDisappearSelector = mkSelector "viewDidDisappear"

-- | @Selector@ for @updateViewConstraints@
updateViewConstraintsSelector :: Selector
updateViewConstraintsSelector = mkSelector "updateViewConstraints"

-- | @Selector@ for @viewWillLayout@
viewWillLayoutSelector :: Selector
viewWillLayoutSelector = mkSelector "viewWillLayout"

-- | @Selector@ for @viewDidLayout@
viewDidLayoutSelector :: Selector
viewDidLayoutSelector = mkSelector "viewDidLayout"

-- | @Selector@ for @addChildViewController:@
addChildViewControllerSelector :: Selector
addChildViewControllerSelector = mkSelector "addChildViewController:"

-- | @Selector@ for @removeFromParentViewController@
removeFromParentViewControllerSelector :: Selector
removeFromParentViewControllerSelector = mkSelector "removeFromParentViewController"

-- | @Selector@ for @insertChildViewController:atIndex:@
insertChildViewController_atIndexSelector :: Selector
insertChildViewController_atIndexSelector = mkSelector "insertChildViewController:atIndex:"

-- | @Selector@ for @removeChildViewControllerAtIndex:@
removeChildViewControllerAtIndexSelector :: Selector
removeChildViewControllerAtIndexSelector = mkSelector "removeChildViewControllerAtIndex:"

-- | @Selector@ for @preferredContentSizeDidChangeForViewController:@
preferredContentSizeDidChangeForViewControllerSelector :: Selector
preferredContentSizeDidChangeForViewControllerSelector = mkSelector "preferredContentSizeDidChangeForViewController:"

-- | @Selector@ for @viewWillTransitionToSize:@
viewWillTransitionToSizeSelector :: Selector
viewWillTransitionToSizeSelector = mkSelector "viewWillTransitionToSize:"

-- | @Selector@ for @presentViewControllerAsSheet:@
presentViewControllerAsSheetSelector :: Selector
presentViewControllerAsSheetSelector = mkSelector "presentViewControllerAsSheet:"

-- | @Selector@ for @presentViewControllerAsModalWindow:@
presentViewControllerAsModalWindowSelector :: Selector
presentViewControllerAsModalWindowSelector = mkSelector "presentViewControllerAsModalWindow:"

-- | @Selector@ for @presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector :: Selector
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behaviorSelector = mkSelector "presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:"

-- | @Selector@ for @presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:@
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector :: Selector
presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior_hasFullSizeContentSelector = mkSelector "presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:hasFullSizeContent:"

-- | @Selector@ for @transitionFromViewController:toViewController:options:completionHandler:@
transitionFromViewController_toViewController_options_completionHandlerSelector :: Selector
transitionFromViewController_toViewController_options_completionHandlerSelector = mkSelector "transitionFromViewController:toViewController:options:completionHandler:"

-- | @Selector@ for @presentViewController:animator:@
presentViewController_animatorSelector :: Selector
presentViewController_animatorSelector = mkSelector "presentViewController:animator:"

-- | @Selector@ for @dismissViewController:@
dismissViewControllerSelector :: Selector
dismissViewControllerSelector = mkSelector "dismissViewController:"

-- | @Selector@ for @dismissController:@
dismissControllerSelector :: Selector
dismissControllerSelector = mkSelector "dismissController:"

-- | @Selector@ for @nibName@
nibNameSelector :: Selector
nibNameSelector = mkSelector "nibName"

-- | @Selector@ for @nibBundle@
nibBundleSelector :: Selector
nibBundleSelector = mkSelector "nibBundle"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @viewLoaded@
viewLoadedSelector :: Selector
viewLoadedSelector = mkSelector "viewLoaded"

-- | @Selector@ for @preferredContentSize@
preferredContentSizeSelector :: Selector
preferredContentSizeSelector = mkSelector "preferredContentSize"

-- | @Selector@ for @setPreferredContentSize:@
setPreferredContentSizeSelector :: Selector
setPreferredContentSizeSelector = mkSelector "setPreferredContentSize:"

-- | @Selector@ for @preferredScreenOrigin@
preferredScreenOriginSelector :: Selector
preferredScreenOriginSelector = mkSelector "preferredScreenOrigin"

-- | @Selector@ for @setPreferredScreenOrigin:@
setPreferredScreenOriginSelector :: Selector
setPreferredScreenOriginSelector = mkSelector "setPreferredScreenOrigin:"

-- | @Selector@ for @preferredMinimumSize@
preferredMinimumSizeSelector :: Selector
preferredMinimumSizeSelector = mkSelector "preferredMinimumSize"

-- | @Selector@ for @preferredMaximumSize@
preferredMaximumSizeSelector :: Selector
preferredMaximumSizeSelector = mkSelector "preferredMaximumSize"

