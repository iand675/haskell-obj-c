{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUndoManager@.
module ObjC.Foundation.NSUndoManager
  ( NSUndoManager
  , IsNSUndoManager(..)
  , beginUndoGrouping
  , endUndoGrouping
  , disableUndoRegistration
  , enableUndoRegistration
  , undo
  , redo
  , undoNestedGroup
  , removeAllActions
  , removeAllActionsWithTarget
  , registerUndoWithTarget_selector_object
  , prepareWithInvocationTarget
  , registerUndoWithTarget_handler
  , setActionIsDiscardable
  , setActionName
  , undoActionUserInfoValueForKey
  , redoActionUserInfoValueForKey
  , setActionUserInfoValue_forKey
  , undoMenuTitleForUndoActionName
  , redoMenuTitleForUndoActionName
  , groupingLevel
  , undoRegistrationEnabled
  , groupsByEvent
  , setGroupsByEvent
  , levelsOfUndo
  , setLevelsOfUndo
  , runLoopModes
  , setRunLoopModes
  , canUndo
  , canRedo
  , undoCount
  , redoCount
  , undoing
  , redoing
  , undoActionIsDiscardable
  , redoActionIsDiscardable
  , undoActionName
  , redoActionName
  , undoMenuItemTitle
  , redoMenuItemTitle
  , beginUndoGroupingSelector
  , canRedoSelector
  , canUndoSelector
  , disableUndoRegistrationSelector
  , enableUndoRegistrationSelector
  , endUndoGroupingSelector
  , groupingLevelSelector
  , groupsByEventSelector
  , levelsOfUndoSelector
  , prepareWithInvocationTargetSelector
  , redoActionIsDiscardableSelector
  , redoActionNameSelector
  , redoActionUserInfoValueForKeySelector
  , redoCountSelector
  , redoMenuItemTitleSelector
  , redoMenuTitleForUndoActionNameSelector
  , redoSelector
  , redoingSelector
  , registerUndoWithTarget_handlerSelector
  , registerUndoWithTarget_selector_objectSelector
  , removeAllActionsSelector
  , removeAllActionsWithTargetSelector
  , runLoopModesSelector
  , setActionIsDiscardableSelector
  , setActionNameSelector
  , setActionUserInfoValue_forKeySelector
  , setGroupsByEventSelector
  , setLevelsOfUndoSelector
  , setRunLoopModesSelector
  , undoActionIsDiscardableSelector
  , undoActionNameSelector
  , undoActionUserInfoValueForKeySelector
  , undoCountSelector
  , undoMenuItemTitleSelector
  , undoMenuTitleForUndoActionNameSelector
  , undoNestedGroupSelector
  , undoRegistrationEnabledSelector
  , undoSelector
  , undoingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | Marks the beginning of an undo group.
--
-- All individual undo operations before a subsequent ``endUndoGrouping`` message are grouped together and reversed by a later ``undo`` message. By default undo groups are begun automatically at the start of the event loop, but you can begin your own undo groups with this method, and nest them within other groups.
--
-- This method posts an ``NSUndoManagerCheckpointNotification`` unless a top-level undo is in progress. It posts an ``NSUndoManagerDidOpenUndoGroupNotification`` if a new group was successfully created.
--
-- ObjC selector: @- beginUndoGrouping@
beginUndoGrouping :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
beginUndoGrouping nsUndoManager =
  sendMessage nsUndoManager beginUndoGroupingSelector

-- | Marks the end of an undo group.
--
-- All individual undo operations back to the matching ``beginUndoGrouping`` message are grouped together and reversed by a later ``undo`` or ``undoNestedGroup`` message. Undo groups can be nested, thus providing functionality similar to nested transactions. Raises an ``NSInternalInconsistencyException`` if there’s no ``beginUndoGrouping`` message in effect.
--
-- This method posts an ``NSUndoManagerCheckpointNotification`` and an ``NSUndoManagerDidCloseUndoGroupNotification`` just before the group is closed.
--
-- ObjC selector: @- endUndoGrouping@
endUndoGrouping :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
endUndoGrouping nsUndoManager =
  sendMessage nsUndoManager endUndoGroupingSelector

-- | Disables the recording of undo operations, whether by ``registerUndoWithTarget:selector:object:`` or by invocation-based undo.
--
-- This method can be invoked multiple times by multiple clients. The ``enableUndoRegistration`` method must be invoked an equal number of times to re-enable undo registration.
--
-- ObjC selector: @- disableUndoRegistration@
disableUndoRegistration :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
disableUndoRegistration nsUndoManager =
  sendMessage nsUndoManager disableUndoRegistrationSelector

-- | Enables the recording of undo operations.
--
-- Because undo registration is enabled by default, this is used to balance a prior ``disableUndoRegistration``. Undo registration isn’t actually re-enabled until an enable message balances the last disable message in effect. Raises an NSInternalInconsistencyException if invoked while no disableUndoRegistration() message is in effect.
--
-- ObjC selector: @- enableUndoRegistration@
enableUndoRegistration :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
enableUndoRegistration nsUndoManager =
  sendMessage nsUndoManager enableUndoRegistrationSelector

-- | Closes the top-level undo group if necessary and invokes ``undoNestedGroup``.
--
-- This method also invokes ``endUndoGrouping`` if the nesting level is 1. Raises an ``NSInternalInconsistencyException`` if more than one undo group is open (that is, if the last group isn’t at the top level). This method posts an ``NSUndoManagerCheckpointNotification``.
--
-- ObjC selector: @- undo@
undo :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
undo nsUndoManager =
  sendMessage nsUndoManager undoSelector

-- | Performs the operations in the last group on the redo stack, if there are any, recording them on the undo stack as a single group.
--
-- Raises an ``NSInternalInconsistencyException`` if the method is invoked during an undo operation. This method posts an ``NSUndoManagerCheckpointNotification`` and ``NSUndoManagerWillRedoChangeNotification`` before it performs the redo operation, and it posts the ``NSUndoManagerDidRedoChangeNotification`` after it performs the redo operation.
--
-- ObjC selector: @- redo@
redo :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
redo nsUndoManager =
  sendMessage nsUndoManager redoSelector

-- | Performs the undo operations in the last undo group (whether top-level or nested), recording the operations on the redo stack as a single group.
--
-- Raises an ``NSInternalInconsistencyException`` if any undo operations have been registered since the last ``enableUndoRegistration`` message. This method posts an ``NSUndoManagerCheckpointNotification`` and ``NSUndoManagerWillUndoChangeNotification`` before it performs the undo operation, and it posts an ``NSUndoManagerDidUndoChangeNotification`` after it performs the undo operation.
--
-- ObjC selector: @- undoNestedGroup@
undoNestedGroup :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
undoNestedGroup nsUndoManager =
  sendMessage nsUndoManager undoNestedGroupSelector

-- | Clears the undo and redo stacks and re-enables the receiver.
--
-- ObjC selector: @- removeAllActions@
removeAllActions :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO ()
removeAllActions nsUndoManager =
  sendMessage nsUndoManager removeAllActionsSelector

-- | Clears the undo and redo stacks of all operations involving the specified target as the recipient of the undo message.
--
-- Doesn't re-enable the receiver if it's disabled.
--
-- - Parameter target: The recepient of the undo mesages to be removed.
--
-- ObjC selector: @- removeAllActionsWithTarget:@
removeAllActionsWithTarget :: IsNSUndoManager nsUndoManager => nsUndoManager -> RawId -> IO ()
removeAllActionsWithTarget nsUndoManager target =
  sendMessage nsUndoManager removeAllActionsWithTargetSelector target

-- | Registers the selector of the specified target to implement a single undo operation that the target receives.
--
-- - Parameter target: The target of the undo operation. The undo manager maintains an unowned reference to @target@ to prevent retain cycles. - Parameter selector: The selector for the undo operation. - Parameter object: The argument sent with the selector. The undo manager maintains a strong reference to @object@
--
-- ObjC selector: @- registerUndoWithTarget:selector:object:@
registerUndoWithTarget_selector_object :: IsNSUndoManager nsUndoManager => nsUndoManager -> RawId -> Sel -> RawId -> IO ()
registerUndoWithTarget_selector_object nsUndoManager target selector object =
  sendMessage nsUndoManager registerUndoWithTarget_selector_objectSelector target selector object

-- | Prepares the undo manager for invocation-based undo with the given target as the subject of the next undo operation.
--
-- For example, when called as:
--
-- [[undoManager prepareWithInvocationTarget:target] setFont:oldFont color:oldColor]
--
-- When undo is called, the specified target will be called with
--
-- [target setFont:oldFont color:oldColor]
--
-- - Parameter target: The target of the undo operation. The undo manager maintains a weak reference to @target@. - Returns:  A proxy object that forwards messages to the undo manager for recording as undo actions.
--
-- ObjC selector: @- prepareWithInvocationTarget:@
prepareWithInvocationTarget :: IsNSUndoManager nsUndoManager => nsUndoManager -> RawId -> IO RawId
prepareWithInvocationTarget nsUndoManager target =
  sendMessage nsUndoManager prepareWithInvocationTargetSelector target

-- | Records a single undo operation for a given target so that when an undo is performed, it executes the specified block.
--
-- As with other undo operations, this does not strongly retain target. Care should be taken to avoid introducing retain cycles by other references captured by the block.
--
-- - Parameter target: The target of the undo operation. - Parameter undoHandler: The block to be executed when an operation is undone. The block takes a single argument, the target of the undo operation.
--
-- ObjC selector: @- registerUndoWithTarget:handler:@
registerUndoWithTarget_handler :: IsNSUndoManager nsUndoManager => nsUndoManager -> RawId -> Ptr () -> IO ()
registerUndoWithTarget_handler nsUndoManager target undoHandler =
  sendMessage nsUndoManager registerUndoWithTarget_handlerSelector target undoHandler

-- | Sets whether the next undo or redo action is discardable.
--
-- Specifies that the latest undo action may be safely discarded when a document can not be saved for any reason. An example might be an undo action that changes the viewable area of a document. To find out if an undo group contains only discardable actions, look for the ``NSUndoManagerGroupIsDiscardableKey`` in the @userInfo@ dictionary of the ``NSUndoManagerWillCloseUndoGroupNotification``.
--
-- - Parameter discardable: Specifies if the action is discardable. YES if the next undo or redo action can be discarded; NO otherwise.
--
-- ObjC selector: @- setActionIsDiscardable:@
setActionIsDiscardable :: IsNSUndoManager nsUndoManager => nsUndoManager -> Bool -> IO ()
setActionIsDiscardable nsUndoManager discardable =
  sendMessage nsUndoManager setActionIsDiscardableSelector discardable

-- | Sets the name of the action associated with the Undo or Redo command.
--
-- If @actionName@ is an empty string, the undo manager removes the action name currently associated with the menu command.
--
-- - Parameter actionName: The name of the action.
--
-- ObjC selector: @- setActionName:@
setActionName :: (IsNSUndoManager nsUndoManager, IsNSString actionName) => nsUndoManager -> actionName -> IO ()
setActionName nsUndoManager actionName =
  sendMessage nsUndoManager setActionNameSelector (toNSString actionName)

-- | Get a value from the undo action's user info
--
-- - Parameter key: Which value should be retrieved
--
-- ObjC selector: @- undoActionUserInfoValueForKey:@
undoActionUserInfoValueForKey :: (IsNSUndoManager nsUndoManager, IsNSString key) => nsUndoManager -> key -> IO RawId
undoActionUserInfoValueForKey nsUndoManager key =
  sendMessage nsUndoManager undoActionUserInfoValueForKeySelector (toNSString key)

-- | Get a value from the redo action's user info
--
-- - Parameter key: Which value should be retrieved
--
-- ObjC selector: @- redoActionUserInfoValueForKey:@
redoActionUserInfoValueForKey :: (IsNSUndoManager nsUndoManager, IsNSString key) => nsUndoManager -> key -> IO RawId
redoActionUserInfoValueForKey nsUndoManager key =
  sendMessage nsUndoManager redoActionUserInfoValueForKeySelector (toNSString key)

-- | Set user info for the Undo or Redo command. - Parameter info: Value to be saved in the user info - Parameter key: Key at which the object should be saved
--
-- ObjC selector: @- setActionUserInfoValue:forKey:@
setActionUserInfoValue_forKey :: (IsNSUndoManager nsUndoManager, IsNSString key) => nsUndoManager -> RawId -> key -> IO ()
setActionUserInfoValue_forKey nsUndoManager info key =
  sendMessage nsUndoManager setActionUserInfoValue_forKeySelector info (toNSString key)

-- | Returns the complete, localized title of the Undo menu command for the action identified by the given name.
--
-- Override this method if you want to customize the localization behaviour. This method is invoked by ``undoMenuItemTitle``.
--
-- - Parameter actionName: The name of the undo action. - Returns: The localized title of the undo menu item.
--
-- ObjC selector: @- undoMenuTitleForUndoActionName:@
undoMenuTitleForUndoActionName :: (IsNSUndoManager nsUndoManager, IsNSString actionName) => nsUndoManager -> actionName -> IO (Id NSString)
undoMenuTitleForUndoActionName nsUndoManager actionName =
  sendMessage nsUndoManager undoMenuTitleForUndoActionNameSelector (toNSString actionName)

-- | Returns the complete, localized title of the Redo menu command for the action identified by the given name.
--
-- Override this method if you want to customize the localization behaviour. This method is invoked by ``redoMenuItemTitle``.
--
-- - Parameter actionName: The name of the redo action. - Returns: The localized title of the redo menu item.
--
-- ObjC selector: @- redoMenuTitleForUndoActionName:@
redoMenuTitleForUndoActionName :: (IsNSUndoManager nsUndoManager, IsNSString actionName) => nsUndoManager -> actionName -> IO (Id NSString)
redoMenuTitleForUndoActionName nsUndoManager actionName =
  sendMessage nsUndoManager redoMenuTitleForUndoActionNameSelector (toNSString actionName)

-- | The number of nested undo groups (or redo groups, if Redo was invoked last) in the current event loop.
--
-- An integer indicating the number of nested groups. If @0@ is returned, there is no open undo or redo group.
--
-- ObjC selector: @- groupingLevel@
groupingLevel :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO CLong
groupingLevel nsUndoManager =
  sendMessage nsUndoManager groupingLevelSelector

-- | Whether the recording of undo operations is enabled.
--
-- ObjC selector: @- undoRegistrationEnabled@
undoRegistrationEnabled :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
undoRegistrationEnabled nsUndoManager =
  sendMessage nsUndoManager undoRegistrationEnabledSelector

-- | A Boolean value that indicates whether the receiver automatically creates undo groups around each pass of the run loop.
--
-- If @true@, the receiver automatically creates undo groups around each pass of the run loop. The default is @true@. If you turn automatic grouping off, you must close groups explicitly before invoking either ``undo`` or ``undoNestedGroup``.
--
-- ObjC selector: @- groupsByEvent@
groupsByEvent :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
groupsByEvent nsUndoManager =
  sendMessage nsUndoManager groupsByEventSelector

-- | A Boolean value that indicates whether the receiver automatically creates undo groups around each pass of the run loop.
--
-- If @true@, the receiver automatically creates undo groups around each pass of the run loop. The default is @true@. If you turn automatic grouping off, you must close groups explicitly before invoking either ``undo`` or ``undoNestedGroup``.
--
-- ObjC selector: @- setGroupsByEvent:@
setGroupsByEvent :: IsNSUndoManager nsUndoManager => nsUndoManager -> Bool -> IO ()
setGroupsByEvent nsUndoManager value =
  sendMessage nsUndoManager setGroupsByEventSelector value

-- | The maximum number of top-level undo groups the receiver holds.
--
-- An integer specifying the number of undo groups. A limit of 0 indicates no limit, so old undo groups are never dropped. When ending an undo group results in the number of groups exceeding this limit, the oldest groups are dropped from the stack. The default is 0. If you change the limit to a level below the prior limit, old undo groups are immediately dropped.
--
-- ObjC selector: @- levelsOfUndo@
levelsOfUndo :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO CULong
levelsOfUndo nsUndoManager =
  sendMessage nsUndoManager levelsOfUndoSelector

-- | The maximum number of top-level undo groups the receiver holds.
--
-- An integer specifying the number of undo groups. A limit of 0 indicates no limit, so old undo groups are never dropped. When ending an undo group results in the number of groups exceeding this limit, the oldest groups are dropped from the stack. The default is 0. If you change the limit to a level below the prior limit, old undo groups are immediately dropped.
--
-- ObjC selector: @- setLevelsOfUndo:@
setLevelsOfUndo :: IsNSUndoManager nsUndoManager => nsUndoManager -> CULong -> IO ()
setLevelsOfUndo nsUndoManager value =
  sendMessage nsUndoManager setLevelsOfUndoSelector value

-- | The modes governing the types of input handled during a cycle of the run loop.
--
-- An array of string constants specifying the current run-loop modes. By default, the sole run-loop mode is ``NSDefaultRunLoopMode`` (which excludes data from ``NSConnection`` objects). Some examples of other uses are to limit the input to data received during a mouse-tracking session by setting the mode to ``NSEventTrackingRunLoopMode``, or limit it to data received from a modal panel with ``NSModalPanelRunLoopMode``.
--
-- ObjC selector: @- runLoopModes@
runLoopModes :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO (Id NSArray)
runLoopModes nsUndoManager =
  sendMessage nsUndoManager runLoopModesSelector

-- | The modes governing the types of input handled during a cycle of the run loop.
--
-- An array of string constants specifying the current run-loop modes. By default, the sole run-loop mode is ``NSDefaultRunLoopMode`` (which excludes data from ``NSConnection`` objects). Some examples of other uses are to limit the input to data received during a mouse-tracking session by setting the mode to ``NSEventTrackingRunLoopMode``, or limit it to data received from a modal panel with ``NSModalPanelRunLoopMode``.
--
-- ObjC selector: @- setRunLoopModes:@
setRunLoopModes :: (IsNSUndoManager nsUndoManager, IsNSArray value) => nsUndoManager -> value -> IO ()
setRunLoopModes nsUndoManager value =
  sendMessage nsUndoManager setRunLoopModesSelector (toNSArray value)

-- | Whether the receiver has any actions to undo.
--
-- The return value does not mean you can safely invoke ``undo`` or ``undoNestedGroup`` — you may have to close open undo groups first.
--
-- ObjC selector: @- canUndo@
canUndo :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
canUndo nsUndoManager =
  sendMessage nsUndoManager canUndoSelector

-- | Whether the receiver has any actions to redo.
--
-- Because any undo operation registered clears the redo stack, this method posts an NSUndoManagerCheckpointNotification to allow clients to apply their pending operations before testing the redo stack.
--
-- ObjC selector: @- canRedo@
canRedo :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
canRedo nsUndoManager =
  sendMessage nsUndoManager canRedoSelector

-- | How many times @undo@ can be invoked before there are no more actions left to be undone
--
-- ObjC selector: @- undoCount@
undoCount :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO CULong
undoCount nsUndoManager =
  sendMessage nsUndoManager undoCountSelector

-- | How many times @redo@ can be invoked before there are no more actions left to be redone
--
-- ObjC selector: @- redoCount@
redoCount :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO CULong
redoCount nsUndoManager =
  sendMessage nsUndoManager redoCountSelector

-- | Whether the receiver is in the process of performing its ``undo`` or ``undoNestedGroup`` method.
--
-- ObjC selector: @- undoing@
undoing :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
undoing nsUndoManager =
  sendMessage nsUndoManager undoingSelector

-- | Whether the receiver is in the process of performing its ``redo`` method.
--
-- ObjC selector: @- redoing@
redoing :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
redoing nsUndoManager =
  sendMessage nsUndoManager redoingSelector

-- | Whether the next undo action is discardable.
--
-- Specifies that the latest undo action may be safely discarded when a document can not be saved for any reason. These are typically actions that don’t affect persistent state. An example might be an undo action that changes the viewable area of a document.
--
-- ObjC selector: @- undoActionIsDiscardable@
undoActionIsDiscardable :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
undoActionIsDiscardable nsUndoManager =
  sendMessage nsUndoManager undoActionIsDiscardableSelector

-- | Whether the next redo action is discardable.
--
-- Specifies that the latest redo action may be safely discarded when a document can not be saved for any reason. These are typically actions that don’t affect persistent state. An example might be an redo action that changes the viewable area of a document.
--
-- ObjC selector: @- redoActionIsDiscardable@
redoActionIsDiscardable :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO Bool
redoActionIsDiscardable nsUndoManager =
  sendMessage nsUndoManager redoActionIsDiscardableSelector

-- | The name identifying the undo action.
--
-- The undo action name. Returns an empty string if no action name has been assigned or if there is nothing to undo. For example, if the menu title is “Undo Delete,” the string returned is “Delete.”
--
-- ObjC selector: @- undoActionName@
undoActionName :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO (Id NSString)
undoActionName nsUndoManager =
  sendMessage nsUndoManager undoActionNameSelector

-- | The name identifying the redo action.
--
-- The redo action name. Returns an empty string if no action name has been assigned or if there is nothing to redo. For example, if the menu title is “Redo Delete,” the string returned is “Delete.”
--
-- ObjC selector: @- redoActionName@
redoActionName :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO (Id NSString)
redoActionName nsUndoManager =
  sendMessage nsUndoManager redoActionNameSelector

-- | The complete title of the Undo menu command, for example, “Undo Paste.”
--
-- Returns “Undo” if no action name has been assigned or nil if there is nothing to undo.
--
-- ObjC selector: @- undoMenuItemTitle@
undoMenuItemTitle :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO (Id NSString)
undoMenuItemTitle nsUndoManager =
  sendMessage nsUndoManager undoMenuItemTitleSelector

-- | The complete title of the Redo menu command, for example, “Redo Paste.”
--
-- Returns “Redo” if no action name has been assigned or nil if there is nothing to redo.
--
-- ObjC selector: @- redoMenuItemTitle@
redoMenuItemTitle :: IsNSUndoManager nsUndoManager => nsUndoManager -> IO (Id NSString)
redoMenuItemTitle nsUndoManager =
  sendMessage nsUndoManager redoMenuItemTitleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginUndoGrouping@
beginUndoGroupingSelector :: Selector '[] ()
beginUndoGroupingSelector = mkSelector "beginUndoGrouping"

-- | @Selector@ for @endUndoGrouping@
endUndoGroupingSelector :: Selector '[] ()
endUndoGroupingSelector = mkSelector "endUndoGrouping"

-- | @Selector@ for @disableUndoRegistration@
disableUndoRegistrationSelector :: Selector '[] ()
disableUndoRegistrationSelector = mkSelector "disableUndoRegistration"

-- | @Selector@ for @enableUndoRegistration@
enableUndoRegistrationSelector :: Selector '[] ()
enableUndoRegistrationSelector = mkSelector "enableUndoRegistration"

-- | @Selector@ for @undo@
undoSelector :: Selector '[] ()
undoSelector = mkSelector "undo"

-- | @Selector@ for @redo@
redoSelector :: Selector '[] ()
redoSelector = mkSelector "redo"

-- | @Selector@ for @undoNestedGroup@
undoNestedGroupSelector :: Selector '[] ()
undoNestedGroupSelector = mkSelector "undoNestedGroup"

-- | @Selector@ for @removeAllActions@
removeAllActionsSelector :: Selector '[] ()
removeAllActionsSelector = mkSelector "removeAllActions"

-- | @Selector@ for @removeAllActionsWithTarget:@
removeAllActionsWithTargetSelector :: Selector '[RawId] ()
removeAllActionsWithTargetSelector = mkSelector "removeAllActionsWithTarget:"

-- | @Selector@ for @registerUndoWithTarget:selector:object:@
registerUndoWithTarget_selector_objectSelector :: Selector '[RawId, Sel, RawId] ()
registerUndoWithTarget_selector_objectSelector = mkSelector "registerUndoWithTarget:selector:object:"

-- | @Selector@ for @prepareWithInvocationTarget:@
prepareWithInvocationTargetSelector :: Selector '[RawId] RawId
prepareWithInvocationTargetSelector = mkSelector "prepareWithInvocationTarget:"

-- | @Selector@ for @registerUndoWithTarget:handler:@
registerUndoWithTarget_handlerSelector :: Selector '[RawId, Ptr ()] ()
registerUndoWithTarget_handlerSelector = mkSelector "registerUndoWithTarget:handler:"

-- | @Selector@ for @setActionIsDiscardable:@
setActionIsDiscardableSelector :: Selector '[Bool] ()
setActionIsDiscardableSelector = mkSelector "setActionIsDiscardable:"

-- | @Selector@ for @setActionName:@
setActionNameSelector :: Selector '[Id NSString] ()
setActionNameSelector = mkSelector "setActionName:"

-- | @Selector@ for @undoActionUserInfoValueForKey:@
undoActionUserInfoValueForKeySelector :: Selector '[Id NSString] RawId
undoActionUserInfoValueForKeySelector = mkSelector "undoActionUserInfoValueForKey:"

-- | @Selector@ for @redoActionUserInfoValueForKey:@
redoActionUserInfoValueForKeySelector :: Selector '[Id NSString] RawId
redoActionUserInfoValueForKeySelector = mkSelector "redoActionUserInfoValueForKey:"

-- | @Selector@ for @setActionUserInfoValue:forKey:@
setActionUserInfoValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setActionUserInfoValue_forKeySelector = mkSelector "setActionUserInfoValue:forKey:"

-- | @Selector@ for @undoMenuTitleForUndoActionName:@
undoMenuTitleForUndoActionNameSelector :: Selector '[Id NSString] (Id NSString)
undoMenuTitleForUndoActionNameSelector = mkSelector "undoMenuTitleForUndoActionName:"

-- | @Selector@ for @redoMenuTitleForUndoActionName:@
redoMenuTitleForUndoActionNameSelector :: Selector '[Id NSString] (Id NSString)
redoMenuTitleForUndoActionNameSelector = mkSelector "redoMenuTitleForUndoActionName:"

-- | @Selector@ for @groupingLevel@
groupingLevelSelector :: Selector '[] CLong
groupingLevelSelector = mkSelector "groupingLevel"

-- | @Selector@ for @undoRegistrationEnabled@
undoRegistrationEnabledSelector :: Selector '[] Bool
undoRegistrationEnabledSelector = mkSelector "undoRegistrationEnabled"

-- | @Selector@ for @groupsByEvent@
groupsByEventSelector :: Selector '[] Bool
groupsByEventSelector = mkSelector "groupsByEvent"

-- | @Selector@ for @setGroupsByEvent:@
setGroupsByEventSelector :: Selector '[Bool] ()
setGroupsByEventSelector = mkSelector "setGroupsByEvent:"

-- | @Selector@ for @levelsOfUndo@
levelsOfUndoSelector :: Selector '[] CULong
levelsOfUndoSelector = mkSelector "levelsOfUndo"

-- | @Selector@ for @setLevelsOfUndo:@
setLevelsOfUndoSelector :: Selector '[CULong] ()
setLevelsOfUndoSelector = mkSelector "setLevelsOfUndo:"

-- | @Selector@ for @runLoopModes@
runLoopModesSelector :: Selector '[] (Id NSArray)
runLoopModesSelector = mkSelector "runLoopModes"

-- | @Selector@ for @setRunLoopModes:@
setRunLoopModesSelector :: Selector '[Id NSArray] ()
setRunLoopModesSelector = mkSelector "setRunLoopModes:"

-- | @Selector@ for @canUndo@
canUndoSelector :: Selector '[] Bool
canUndoSelector = mkSelector "canUndo"

-- | @Selector@ for @canRedo@
canRedoSelector :: Selector '[] Bool
canRedoSelector = mkSelector "canRedo"

-- | @Selector@ for @undoCount@
undoCountSelector :: Selector '[] CULong
undoCountSelector = mkSelector "undoCount"

-- | @Selector@ for @redoCount@
redoCountSelector :: Selector '[] CULong
redoCountSelector = mkSelector "redoCount"

-- | @Selector@ for @undoing@
undoingSelector :: Selector '[] Bool
undoingSelector = mkSelector "undoing"

-- | @Selector@ for @redoing@
redoingSelector :: Selector '[] Bool
redoingSelector = mkSelector "redoing"

-- | @Selector@ for @undoActionIsDiscardable@
undoActionIsDiscardableSelector :: Selector '[] Bool
undoActionIsDiscardableSelector = mkSelector "undoActionIsDiscardable"

-- | @Selector@ for @redoActionIsDiscardable@
redoActionIsDiscardableSelector :: Selector '[] Bool
redoActionIsDiscardableSelector = mkSelector "redoActionIsDiscardable"

-- | @Selector@ for @undoActionName@
undoActionNameSelector :: Selector '[] (Id NSString)
undoActionNameSelector = mkSelector "undoActionName"

-- | @Selector@ for @redoActionName@
redoActionNameSelector :: Selector '[] (Id NSString)
redoActionNameSelector = mkSelector "redoActionName"

-- | @Selector@ for @undoMenuItemTitle@
undoMenuItemTitleSelector :: Selector '[] (Id NSString)
undoMenuItemTitleSelector = mkSelector "undoMenuItemTitle"

-- | @Selector@ for @redoMenuItemTitle@
redoMenuItemTitleSelector :: Selector '[] (Id NSString)
redoMenuItemTitleSelector = mkSelector "redoMenuItemTitle"

