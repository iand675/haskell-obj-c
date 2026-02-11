{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that manages interactions between Writing Tools and your custom text view.
--
-- Add a @NSWritingToolsCoordinator@ object to a custom view when you want to add Writing Tools support to that view. The coordinator manages interactions between your view and the Writing Tools UI and back-end capabilities. When creating a coordinator, you supply a delegate object to respond to requests from the system and provide needed information. Your delegate delivers your view’s text to Writing Tools, incorporates suggested changes back into your text storage, and supports the animations that Writing Tools creates to show the state of an operation.
--
-- Create the @NSWritingToolsCoordinator@ object when setting up your UI, and initialize it with a custom object that adopts the ``NSWritingToolsCoordinator/Delegate`` protocol. Add the coordinator to the ``NSView/writingToolsCoordinator`` property of your view. When a coordinator is present on a view, the system adds UI elements to initiate Writing Tools operations.
--
-- When defining the delegate, choose an object from your app that has access to your view and its text storage. You can adopt the ``NSWritingToolsCoordinator/Delegate`` protocol in the view itself, or in another type that your view uses to manage content. During the interactions with Writing Tools, the delegate gets and sets the contents of the view’s text storage and supports Writing Tools behaviors.
--
-- > Note: You don’t need to create an @NSWritingToolsCoordinator@  object if you display text using a <doc://com.apple.documentation/documentation/uikit/uitextview>, ``NSTextField``, ``NSTextView``, <doc://com.apple.documentation/documentation/swiftui/textfield>, or <doc://com.apple.documentation/documentation/swiftui/texteditor> view. Those views already include the required support to handle Writing Tools interactions.
--
-- Generated bindings for @NSWritingToolsCoordinator@.
module ObjC.AppKit.NSWritingToolsCoordinator
  ( NSWritingToolsCoordinator
  , IsNSWritingToolsCoordinator(..)
  , initWithDelegate
  , stopWritingTools
  , updateRange_withText_reason_forContextWithIdentifier
  , updateForReflowedTextInContextWithIdentifier
  , isWritingToolsAvailable
  , view
  , effectContainerView
  , setEffectContainerView
  , decorationContainerView
  , setDecorationContainerView
  , state
  , preferredBehavior
  , setPreferredBehavior
  , behavior
  , preferredResultOptions
  , setPreferredResultOptions
  , resultOptions
  , includesTextListMarkers
  , setIncludesTextListMarkers
  , initWithDelegateSelector
  , stopWritingToolsSelector
  , updateRange_withText_reason_forContextWithIdentifierSelector
  , updateForReflowedTextInContextWithIdentifierSelector
  , isWritingToolsAvailableSelector
  , viewSelector
  , effectContainerViewSelector
  , setEffectContainerViewSelector
  , decorationContainerViewSelector
  , setDecorationContainerViewSelector
  , stateSelector
  , preferredBehaviorSelector
  , setPreferredBehaviorSelector
  , behaviorSelector
  , preferredResultOptionsSelector
  , setPreferredResultOptionsSelector
  , resultOptionsSelector
  , includesTextListMarkersSelector
  , setIncludesTextListMarkersSelector

  -- * Enum types
  , NSWritingToolsBehavior(NSWritingToolsBehavior)
  , pattern NSWritingToolsBehaviorNone
  , pattern NSWritingToolsBehaviorDefault
  , pattern NSWritingToolsBehaviorComplete
  , pattern NSWritingToolsBehaviorLimited
  , NSWritingToolsCoordinatorState(NSWritingToolsCoordinatorState)
  , pattern NSWritingToolsCoordinatorStateInactive
  , pattern NSWritingToolsCoordinatorStateNoninteractive
  , pattern NSWritingToolsCoordinatorStateInteractiveResting
  , pattern NSWritingToolsCoordinatorStateInteractiveStreaming
  , NSWritingToolsCoordinatorTextUpdateReason(NSWritingToolsCoordinatorTextUpdateReason)
  , pattern NSWritingToolsCoordinatorTextUpdateReasonTyping
  , pattern NSWritingToolsCoordinatorTextUpdateReasonUndoRedo
  , NSWritingToolsResultOptions(NSWritingToolsResultOptions)
  , pattern NSWritingToolsResultDefault
  , pattern NSWritingToolsResultPlainText
  , pattern NSWritingToolsResultRichText
  , pattern NSWritingToolsResultList
  , pattern NSWritingToolsResultTable
  , pattern NSWritingToolsResultPresentationIntent

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

-- | Creates a writing tools coordinator and assigns the specified delegate object to it.
--
-- - Parameters:    - delegate: An object capable of handling Writing Tools interactions    for your view. The delegate must be able to modify your view’s text    storage and refresh the view’s layout and appearance.
--
-- Create the coordinator object during your view’s initialization, and assign the object to your view. Assign the coordinator to the ``NSView/writingToolsCoordinator`` property of your view.
--
-- ObjC selector: @- initWithDelegate:@
initWithDelegate :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> RawId -> IO (Id NSWritingToolsCoordinator)
initWithDelegate nsWritingToolsCoordinator  delegate =
  sendMsg nsWritingToolsCoordinator (mkSelector "initWithDelegate:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | Stops the current Writing Tools operation and dismisses the system UI.
--
-- Call this method to abort the current Writing Tools operation. This method dismisses the system’s Writing Tools UI and stops any in-flight interactions with your view. This method does not undo any changes that Writing Tools already made to your view’s content.
--
-- ObjC selector: @- stopWritingTools@
stopWritingTools :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO ()
stopWritingTools nsWritingToolsCoordinator  =
  sendMsg nsWritingToolsCoordinator (mkSelector "stopWritingTools") retVoid []

-- | Informs the coordinator about changes your app made to the text in the specified context object.
--
-- - Parameters:    - range: The range of text to replace. This range is relative to    the starting location of the specified context object’s text in    your view’s text storage. If you initialized the context object    with the entire contents of your view’s text storage, specify the    range of text you’re replacing in your text storage. However, if    you initialized the context object with only a portion of your    view’s text, specify a range that is relative to the starting    location of the context object’s text.    - replacementText: The text that replaces the previous content in    @range@. Specify an empty string to delete the text in the specified range.    - reason: The reason you updated the text.    - contextID: The unique identifier of the context object that    contains the text you modified.
--
-- If you make any changes to the text Writing Tools is evaluating, call this method to report those changes to your view’s coordinator object. You might make changes in response to an undo command or when someone types into the same part of your view’s text. Calling this method keeps the coordinator object informed of any changes, and ensures it delivers accurate information to its delegate. In response, the coordinator refreshes previews and other information related to your view. If the scope of the update is significantly large, the coordinator can optionally cancel the Writing Tools session altogether.
--
-- Use this method to report changes that precisely intersect your context object’s text. The first time you call this method for a context object, report changes only to the original attributed string in that object. If you call this method more than once, report changes to the newly modified version of that string. Don’t use this method to report changes to text that comes before or after the context object. If you make changes before your context object, report those changes separately using the ``updateForReflowedTextInContextWithIdentifier(_:)`` method.
--
-- > Warning: Failure to call this method for a change can cause Writing Tools to deliver inaccurate information to your delegate and lead to data loss.
--
-- ObjC selector: @- updateRange:withText:reason:forContextWithIdentifier:@
updateRange_withText_reason_forContextWithIdentifier :: (IsNSWritingToolsCoordinator nsWritingToolsCoordinator, IsNSAttributedString replacementText, IsNSUUID contextID) => nsWritingToolsCoordinator -> NSRange -> replacementText -> NSWritingToolsCoordinatorTextUpdateReason -> contextID -> IO ()
updateRange_withText_reason_forContextWithIdentifier nsWritingToolsCoordinator  range replacementText reason contextID =
withObjCPtr replacementText $ \raw_replacementText ->
  withObjCPtr contextID $ \raw_contextID ->
      sendMsg nsWritingToolsCoordinator (mkSelector "updateRange:withText:reason:forContextWithIdentifier:") retVoid [argNSRange range, argPtr (castPtr raw_replacementText :: Ptr ()), argCLong (coerce reason), argPtr (castPtr raw_contextID :: Ptr ())]

-- | Informs the coordinator that a change occurred to the view or its text that requires a layout update.
--
-- - Parameters:    - contextID: The unique identifier of the context object affected    by the change. Pass the identifier for the context object that comes    after the changes.
--
-- Use this method to inform Writing Tools when the geometry of your view changes, or when the text that precedes one of your context objects changes. Changes to the view’s geometry or text can affect the flow of any remaining text, and require a layout update. Writing Tools uses this method to refresh any layout-dependent information it’s currently tracking. For example, it uses it to refresh the location of proofreading marks it’s displaying in your view.
--
-- If a text change affects the text inside a context object, call the ``updateRange(_:with:reason:forContextWithIdentifier:)`` method to report that change instead.
--
-- ObjC selector: @- updateForReflowedTextInContextWithIdentifier:@
updateForReflowedTextInContextWithIdentifier :: (IsNSWritingToolsCoordinator nsWritingToolsCoordinator, IsNSUUID contextID) => nsWritingToolsCoordinator -> contextID -> IO ()
updateForReflowedTextInContextWithIdentifier nsWritingToolsCoordinator  contextID =
withObjCPtr contextID $ \raw_contextID ->
    sendMsg nsWritingToolsCoordinator (mkSelector "updateForReflowedTextInContextWithIdentifier:") retVoid [argPtr (castPtr raw_contextID :: Ptr ())]

-- | A Boolean value that indicates whether Writing Tools features are currently available.
--
-- The value of this property is @true@ when Writing Tools features are available, and @false@ when they aren’t. Writing Tools support might be unavailable because of device constraints or because the system isn’t ready to process Writing Tools requests.
--
-- ObjC selector: @+ isWritingToolsAvailable@
isWritingToolsAvailable :: IO Bool
isWritingToolsAvailable  =
  do
    cls' <- getRequiredClass "NSWritingToolsCoordinator"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isWritingToolsAvailable") retCULong []

-- | The view that currently uses the writing tools coordinator.
--
-- Use this property to refer to the view that currently owns the coordinator object. The system updates this property automatically when you assign the coordinator to the ``NSView/writingToolsCoordinator`` property of your view. The value of this property is @nil@ if there is no associated view.
--
-- ObjC selector: @- view@
view :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO (Id NSView)
view nsWritingToolsCoordinator  =
  sendMsg nsWritingToolsCoordinator (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view that Writing Tools uses to display visual effects during the text-rewriting process.
--
-- Writing Tools uses the view in this property to host the visual effects it creates when making interactive changes to your view’s content. These visual effects let people know the state of the text and provide feedback about what’s happening to it. Set this property to a subview that sits visually above, and covers, all of the text in your custom text view. If you don’t assign a value to this property, the coordinator places its own effect view in front of the subviews in your custom view. The default value of this property is @nil@.
--
-- If you display your view’s text using multiple text containers, implement the ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:singleContainerSubrangesOf:in:)`` method to request multiple previews.
--
-- ObjC selector: @- effectContainerView@
effectContainerView :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO (Id NSView)
effectContainerView nsWritingToolsCoordinator  =
  sendMsg nsWritingToolsCoordinator (mkSelector "effectContainerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view that Writing Tools uses to display visual effects during the text-rewriting process.
--
-- Writing Tools uses the view in this property to host the visual effects it creates when making interactive changes to your view’s content. These visual effects let people know the state of the text and provide feedback about what’s happening to it. Set this property to a subview that sits visually above, and covers, all of the text in your custom text view. If you don’t assign a value to this property, the coordinator places its own effect view in front of the subviews in your custom view. The default value of this property is @nil@.
--
-- If you display your view’s text using multiple text containers, implement the ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:singleContainerSubrangesOf:in:)`` method to request multiple previews.
--
-- ObjC selector: @- setEffectContainerView:@
setEffectContainerView :: (IsNSWritingToolsCoordinator nsWritingToolsCoordinator, IsNSView value) => nsWritingToolsCoordinator -> value -> IO ()
setEffectContainerView nsWritingToolsCoordinator  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWritingToolsCoordinator (mkSelector "setEffectContainerView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The view that Writing Tools uses to display background decorations such as proofreading marks.
--
-- Writing Tools uses the view in this property to host proofreading marks and other visual elements that show any suggested changes. Set this property to a subview situated visibly below the text in your custom text view. It's also satisfactory to place this view visually in front of the text. Make sure the size of the view is big enough to cover all of the affected text. If you don’t assign a value to this property, the coordinator places its own decoration view behind the subviews in your custom view. The default value of this property is @nil@.
--
-- If you display your view’s text using multiple text containers, implement the ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:singleContainerSubrangesOf:in:)`` and ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:decorationContainerViewFor:in:)`` methods to provide separate decoration views for each container.
--
-- ObjC selector: @- decorationContainerView@
decorationContainerView :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO (Id NSView)
decorationContainerView nsWritingToolsCoordinator  =
  sendMsg nsWritingToolsCoordinator (mkSelector "decorationContainerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view that Writing Tools uses to display background decorations such as proofreading marks.
--
-- Writing Tools uses the view in this property to host proofreading marks and other visual elements that show any suggested changes. Set this property to a subview situated visibly below the text in your custom text view. It's also satisfactory to place this view visually in front of the text. Make sure the size of the view is big enough to cover all of the affected text. If you don’t assign a value to this property, the coordinator places its own decoration view behind the subviews in your custom view. The default value of this property is @nil@.
--
-- If you display your view’s text using multiple text containers, implement the ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:singleContainerSubrangesOf:in:)`` and ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:decorationContainerViewFor:in:)`` methods to provide separate decoration views for each container.
--
-- ObjC selector: @- setDecorationContainerView:@
setDecorationContainerView :: (IsNSWritingToolsCoordinator nsWritingToolsCoordinator, IsNSView value) => nsWritingToolsCoordinator -> value -> IO ()
setDecorationContainerView nsWritingToolsCoordinator  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWritingToolsCoordinator (mkSelector "setDecorationContainerView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The current level of Writing Tools activity in your view.
--
-- Use this property to determine when Writing Tools is actively making changes to your view. During the course of Writing Tools interactions, the system reports state changes to the delegate’s ``NSWritingToolsCoordinator/Delegate/writingToolsCoordinator(_:willChangeTo:completion:)`` method and updates this property accordingly.
--
-- ObjC selector: @- state@
state :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO NSWritingToolsCoordinatorState
state nsWritingToolsCoordinator  =
  fmap (coerce :: CLong -> NSWritingToolsCoordinatorState) $ sendMsg nsWritingToolsCoordinator (mkSelector "state") retCLong []

-- | The level of Writing Tools support you want the system to provide for your view.
--
-- Use this property to request an inline or panel-based experience, or to disable Writing Tools for your view altogether. The default value of this property is ``NSWritingToolsBehavior/default``.
--
-- ObjC selector: @- preferredBehavior@
preferredBehavior :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO NSWritingToolsBehavior
preferredBehavior nsWritingToolsCoordinator  =
  fmap (coerce :: CLong -> NSWritingToolsBehavior) $ sendMsg nsWritingToolsCoordinator (mkSelector "preferredBehavior") retCLong []

-- | The level of Writing Tools support you want the system to provide for your view.
--
-- Use this property to request an inline or panel-based experience, or to disable Writing Tools for your view altogether. The default value of this property is ``NSWritingToolsBehavior/default``.
--
-- ObjC selector: @- setPreferredBehavior:@
setPreferredBehavior :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> NSWritingToolsBehavior -> IO ()
setPreferredBehavior nsWritingToolsCoordinator  value =
  sendMsg nsWritingToolsCoordinator (mkSelector "setPreferredBehavior:") retVoid [argCLong (coerce value)]

-- | The actual level of Writing Tools support the system provides for your view.
--
-- The system chooses this value based on the device capabilities, and takes the value in the ``preferredBehavior`` property into consideration when making the choice. The value in this property is never the default option, and is instead one of the specific options such as ``NSWritingToolsBehavior/none``, ``NSWritingToolsBehavior/limited``, or ``NSWritingToolsBehavior/complete``.
--
-- ObjC selector: @- behavior@
behavior :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO NSWritingToolsBehavior
behavior nsWritingToolsCoordinator  =
  fmap (coerce :: CLong -> NSWritingToolsBehavior) $ sendMsg nsWritingToolsCoordinator (mkSelector "behavior") retCLong []

-- | The type of content you allow Writing Tools to generate for your custom text view.
--
-- Writing Tools can create plain text or rich text, and it can format text using lists or tables as needed. If your view doesn’t support specific types of content, specify the types you do support in this property. The default value of this property is ``NSWritingToolsResult/default``, which lets the system determine the type of content to generate.
--
-- ObjC selector: @- preferredResultOptions@
preferredResultOptions :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO NSWritingToolsResultOptions
preferredResultOptions nsWritingToolsCoordinator  =
  fmap (coerce :: CULong -> NSWritingToolsResultOptions) $ sendMsg nsWritingToolsCoordinator (mkSelector "preferredResultOptions") retCULong []

-- | The type of content you allow Writing Tools to generate for your custom text view.
--
-- Writing Tools can create plain text or rich text, and it can format text using lists or tables as needed. If your view doesn’t support specific types of content, specify the types you do support in this property. The default value of this property is ``NSWritingToolsResult/default``, which lets the system determine the type of content to generate.
--
-- ObjC selector: @- setPreferredResultOptions:@
setPreferredResultOptions :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> NSWritingToolsResultOptions -> IO ()
setPreferredResultOptions nsWritingToolsCoordinator  value =
  sendMsg nsWritingToolsCoordinator (mkSelector "setPreferredResultOptions:") retVoid [argCULong (coerce value)]

-- | The type of content the system generates for your custom text view.
--
-- This property contains the set of options that Writing Tools outputs for your view. Writing Tools takes the value in the ``NSWritingToolsCoordinator/preferredResultOptions`` property into consideration when determining this value.
--
-- ObjC selector: @- resultOptions@
resultOptions :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO NSWritingToolsResultOptions
resultOptions nsWritingToolsCoordinator  =
  fmap (coerce :: CULong -> NSWritingToolsResultOptions) $ sendMsg nsWritingToolsCoordinator (mkSelector "resultOptions") retCULong []

-- | @- includesTextListMarkers@
includesTextListMarkers :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> IO Bool
includesTextListMarkers nsWritingToolsCoordinator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWritingToolsCoordinator (mkSelector "includesTextListMarkers") retCULong []

-- | @- setIncludesTextListMarkers:@
setIncludesTextListMarkers :: IsNSWritingToolsCoordinator nsWritingToolsCoordinator => nsWritingToolsCoordinator -> Bool -> IO ()
setIncludesTextListMarkers nsWritingToolsCoordinator  value =
  sendMsg nsWritingToolsCoordinator (mkSelector "setIncludesTextListMarkers:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDelegate:@
initWithDelegateSelector :: Selector
initWithDelegateSelector = mkSelector "initWithDelegate:"

-- | @Selector@ for @stopWritingTools@
stopWritingToolsSelector :: Selector
stopWritingToolsSelector = mkSelector "stopWritingTools"

-- | @Selector@ for @updateRange:withText:reason:forContextWithIdentifier:@
updateRange_withText_reason_forContextWithIdentifierSelector :: Selector
updateRange_withText_reason_forContextWithIdentifierSelector = mkSelector "updateRange:withText:reason:forContextWithIdentifier:"

-- | @Selector@ for @updateForReflowedTextInContextWithIdentifier:@
updateForReflowedTextInContextWithIdentifierSelector :: Selector
updateForReflowedTextInContextWithIdentifierSelector = mkSelector "updateForReflowedTextInContextWithIdentifier:"

-- | @Selector@ for @isWritingToolsAvailable@
isWritingToolsAvailableSelector :: Selector
isWritingToolsAvailableSelector = mkSelector "isWritingToolsAvailable"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @effectContainerView@
effectContainerViewSelector :: Selector
effectContainerViewSelector = mkSelector "effectContainerView"

-- | @Selector@ for @setEffectContainerView:@
setEffectContainerViewSelector :: Selector
setEffectContainerViewSelector = mkSelector "setEffectContainerView:"

-- | @Selector@ for @decorationContainerView@
decorationContainerViewSelector :: Selector
decorationContainerViewSelector = mkSelector "decorationContainerView"

-- | @Selector@ for @setDecorationContainerView:@
setDecorationContainerViewSelector :: Selector
setDecorationContainerViewSelector = mkSelector "setDecorationContainerView:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @preferredBehavior@
preferredBehaviorSelector :: Selector
preferredBehaviorSelector = mkSelector "preferredBehavior"

-- | @Selector@ for @setPreferredBehavior:@
setPreferredBehaviorSelector :: Selector
setPreferredBehaviorSelector = mkSelector "setPreferredBehavior:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @preferredResultOptions@
preferredResultOptionsSelector :: Selector
preferredResultOptionsSelector = mkSelector "preferredResultOptions"

-- | @Selector@ for @setPreferredResultOptions:@
setPreferredResultOptionsSelector :: Selector
setPreferredResultOptionsSelector = mkSelector "setPreferredResultOptions:"

-- | @Selector@ for @resultOptions@
resultOptionsSelector :: Selector
resultOptionsSelector = mkSelector "resultOptions"

-- | @Selector@ for @includesTextListMarkers@
includesTextListMarkersSelector :: Selector
includesTextListMarkersSelector = mkSelector "includesTextListMarkers"

-- | @Selector@ for @setIncludesTextListMarkers:@
setIncludesTextListMarkersSelector :: Selector
setIncludesTextListMarkersSelector = mkSelector "setIncludesTextListMarkers:"

