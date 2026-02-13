{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUParameterNode
--
-- A node in an audio unit's tree of parameters.
--
-- Nodes are instances of either AUParameterGroup or AUParameter.
--
-- Generated bindings for @AUParameterNode@.
module ObjC.AudioToolbox.AUParameterNode
  ( AUParameterNode
  , IsAUParameterNode(..)
  , displayNameWithLength
  , tokenByAddingParameterObserver
  , tokenByAddingParameterRecordingObserver
  , tokenByAddingParameterAutomationObserver
  , removeParameterObserver
  , identifier
  , keyPath
  , displayName
  , implementorValueObserver
  , setImplementorValueObserver
  , implementorValueProvider
  , setImplementorValueProvider
  , implementorStringFromValueCallback
  , setImplementorStringFromValueCallback
  , implementorValueFromStringCallback
  , setImplementorValueFromStringCallback
  , implementorDisplayNameWithLengthCallback
  , setImplementorDisplayNameWithLengthCallback
  , displayNameSelector
  , displayNameWithLengthSelector
  , identifierSelector
  , implementorDisplayNameWithLengthCallbackSelector
  , implementorStringFromValueCallbackSelector
  , implementorValueFromStringCallbackSelector
  , implementorValueObserverSelector
  , implementorValueProviderSelector
  , keyPathSelector
  , removeParameterObserverSelector
  , setImplementorDisplayNameWithLengthCallbackSelector
  , setImplementorStringFromValueCallbackSelector
  , setImplementorValueFromStringCallbackSelector
  , setImplementorValueObserverSelector
  , setImplementorValueProviderSelector
  , tokenByAddingParameterAutomationObserverSelector
  , tokenByAddingParameterObserverSelector
  , tokenByAddingParameterRecordingObserverSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | displayNameWithLength:
--
-- A version of displayName possibly abbreviated to the given desired length, in characters.
--
-- The default implementation simply returns displayName.
--
-- ObjC selector: @- displayNameWithLength:@
displayNameWithLength :: IsAUParameterNode auParameterNode => auParameterNode -> CLong -> IO (Id NSString)
displayNameWithLength auParameterNode maximumLength =
  sendMessage auParameterNode displayNameWithLengthSelector maximumLength

-- | tokenByAddingParameterObserver:
--
-- Add an observer for a parameter or all parameters in a group/tree.
--
-- An audio unit view can use an AUParameterObserver to be notified of changes		to a single parameter, or to all parameters in a group/tree.
--
-- These callbacks are throttled so as to limit the rate of redundant notifications		in the case of frequent changes to a single parameter.
--
-- This block is called in an arbitrary thread context. It is responsible for thread-safety.		It must not make any calls to add or remove other observers, including itself;		this will deadlock.
--
-- An audio unit's implementation should interact with the parameter object via		implementorValueObserver and implementorValueProvider.
--
-- Parameter observers are bound to a specific parameter instance. If this parameter is		destroyed, e.g. if the parameter tree is re-constructed, the previously set parameter		observers will no longer be valid. Clients can observe changes to the parameter tree		via KVO. See the discussion of -[AUAudioUnit parameterTree].
--
-- @observer@ — A block to call after the value of a parameter has changed.
--
-- Returns: A token which can be passed to removeParameterObserver: or to -[AUParameter setValue:originator:]
--
-- ObjC selector: @- tokenByAddingParameterObserver:@
tokenByAddingParameterObserver :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO RawId
tokenByAddingParameterObserver auParameterNode observer =
  sendMessage auParameterNode tokenByAddingParameterObserverSelector observer

-- | tokenByAddingParameterRecordingObserver:
--
-- Add a recording observer for a parameter or all parameters in a group/tree.
--
-- This is a variant of tokenByAddingParameterAutomationObserver where the callback receives		AURecordedParameterEvents instead of AUParameterAutomationEvents.
--
-- This will be deprecated in favor of tokenByAddingParameterAutomationObserver in a future		release.
--
-- ObjC selector: @- tokenByAddingParameterRecordingObserver:@
tokenByAddingParameterRecordingObserver :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO RawId
tokenByAddingParameterRecordingObserver auParameterNode observer =
  sendMessage auParameterNode tokenByAddingParameterRecordingObserverSelector observer

-- | tokenByAddingParameterAutomationObserver:
--
-- Add a recording observer for a parameter or all parameters in a group/tree.
--
-- An audio unit host can use an AUParameterAutomationObserver or AUParameterRecordingObserver		to capture a series of changes to a parameter value, including the timing of the events, as		generated by a UI gesture in a view, for example.
--
-- Unlike AUParameterObserver, these callbacks are not throttled.
--
-- This block is called in an arbitrary thread context. It is responsible for thread-safety.		It must not make any calls to add or remove other observers, including itself;		this will deadlock.
--
-- An audio unit's engine should interact with the parameter object via		implementorValueObserver and implementorValueProvider.
--
-- @observer@ — A block to call to record the changing of a parameter value.
--
-- Returns: A token which can be passed to removeParameterObserver: or to -[AUParameter		setValue:originator:]
--
-- ObjC selector: @- tokenByAddingParameterAutomationObserver:@
tokenByAddingParameterAutomationObserver :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO RawId
tokenByAddingParameterAutomationObserver auParameterNode observer =
  sendMessage auParameterNode tokenByAddingParameterAutomationObserverSelector observer

-- | removeParameterObserver:
--
-- Remove an observer created with tokenByAddingParameterObserver,		tokenByAddingParameterRecordingObserver, or tokenByAddingParameterAutomationObserver.
--
-- This call will remove the callback corresponding to the supplied token. Note that this		will block until any callbacks currently in flight have completed.
--
-- ObjC selector: @- removeParameterObserver:@
removeParameterObserver :: IsAUParameterNode auParameterNode => auParameterNode -> RawId -> IO ()
removeParameterObserver auParameterNode token =
  sendMessage auParameterNode removeParameterObserverSelector token

-- | identifier
--
-- A non-localized, permanent name for a parameter or group.
--
-- The identifier must be unique for all child nodes under any given parent. From release to		release, an audio unit must not change its parameters' identifiers; this will invalidate any		hosts' documents that refer to the parameters.
--
-- ObjC selector: @- identifier@
identifier :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Id NSString)
identifier auParameterNode =
  sendMessage auParameterNode identifierSelector

-- | keyPath
--
-- Generated by concatenating the identifiers of a node's parents with its own.
--
-- Unless an audio unit specifically documents that its parameter addresses are stable and		persistent, hosts, when recording parameter values, should bind to the keyPath.
--
-- The individual node identifiers in a key path are separated by periods. (".")
--
-- Passing a node's keyPath to -[tree valueForKeyPath:] should return the same node.
--
-- ObjC selector: @- keyPath@
keyPath :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Id NSString)
keyPath auParameterNode =
  sendMessage auParameterNode keyPathSelector

-- | displayName
--
-- A localized name to display for the parameter.
--
-- ObjC selector: @- displayName@
displayName :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Id NSString)
displayName auParameterNode =
  sendMessage auParameterNode displayNameSelector

-- | Called when a parameter changes value.
--
-- This block, used only in an audio unit implementation, receives all externally-generated		changes to parameter values. It should store the new value in its audio signal processing		state (assuming that that state is separate from the AUParameter object).
--
-- ObjC selector: @- implementorValueObserver@
implementorValueObserver :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorValueObserver auParameterNode =
  sendMessage auParameterNode implementorValueObserverSelector

-- | Called when a parameter changes value.
--
-- This block, used only in an audio unit implementation, receives all externally-generated		changes to parameter values. It should store the new value in its audio signal processing		state (assuming that that state is separate from the AUParameter object).
--
-- ObjC selector: @- setImplementorValueObserver:@
setImplementorValueObserver :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorValueObserver auParameterNode value =
  sendMessage auParameterNode setImplementorValueObserverSelector value

-- | Called when a value of a parameter in the tree is known to have a stale value				needing to be refreshed.
--
-- The audio unit should return the current value for this parameter; the AUParameterNode will		store the value.
--
-- ObjC selector: @- implementorValueProvider@
implementorValueProvider :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorValueProvider auParameterNode =
  sendMessage auParameterNode implementorValueProviderSelector

-- | Called when a value of a parameter in the tree is known to have a stale value				needing to be refreshed.
--
-- The audio unit should return the current value for this parameter; the AUParameterNode will		store the value.
--
-- ObjC selector: @- setImplementorValueProvider:@
setImplementorValueProvider :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorValueProvider auParameterNode value =
  sendMessage auParameterNode setImplementorValueProviderSelector value

-- | Called to provide string representations of parameter values.	If value is nil, the callback uses the current value of the parameter.
--
-- ObjC selector: @- implementorStringFromValueCallback@
implementorStringFromValueCallback :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorStringFromValueCallback auParameterNode =
  sendMessage auParameterNode implementorStringFromValueCallbackSelector

-- | Called to provide string representations of parameter values.	If value is nil, the callback uses the current value of the parameter.
--
-- ObjC selector: @- setImplementorStringFromValueCallback:@
setImplementorStringFromValueCallback :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorStringFromValueCallback auParameterNode value =
  sendMessage auParameterNode setImplementorStringFromValueCallbackSelector value

-- | Called to convert string to numeric representations of parameter values.
--
-- ObjC selector: @- implementorValueFromStringCallback@
implementorValueFromStringCallback :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorValueFromStringCallback auParameterNode =
  sendMessage auParameterNode implementorValueFromStringCallbackSelector

-- | Called to convert string to numeric representations of parameter values.
--
-- ObjC selector: @- setImplementorValueFromStringCallback:@
setImplementorValueFromStringCallback :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorValueFromStringCallback auParameterNode value =
  sendMessage auParameterNode setImplementorValueFromStringCallbackSelector value

-- | Called to obtain an abbreviated version of a parameter or group name.
--
-- ObjC selector: @- implementorDisplayNameWithLengthCallback@
implementorDisplayNameWithLengthCallback :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorDisplayNameWithLengthCallback auParameterNode =
  sendMessage auParameterNode implementorDisplayNameWithLengthCallbackSelector

-- | Called to obtain an abbreviated version of a parameter or group name.
--
-- ObjC selector: @- setImplementorDisplayNameWithLengthCallback:@
setImplementorDisplayNameWithLengthCallback :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorDisplayNameWithLengthCallback auParameterNode value =
  sendMessage auParameterNode setImplementorDisplayNameWithLengthCallbackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayNameWithLength:@
displayNameWithLengthSelector :: Selector '[CLong] (Id NSString)
displayNameWithLengthSelector = mkSelector "displayNameWithLength:"

-- | @Selector@ for @tokenByAddingParameterObserver:@
tokenByAddingParameterObserverSelector :: Selector '[Ptr ()] RawId
tokenByAddingParameterObserverSelector = mkSelector "tokenByAddingParameterObserver:"

-- | @Selector@ for @tokenByAddingParameterRecordingObserver:@
tokenByAddingParameterRecordingObserverSelector :: Selector '[Ptr ()] RawId
tokenByAddingParameterRecordingObserverSelector = mkSelector "tokenByAddingParameterRecordingObserver:"

-- | @Selector@ for @tokenByAddingParameterAutomationObserver:@
tokenByAddingParameterAutomationObserverSelector :: Selector '[Ptr ()] RawId
tokenByAddingParameterAutomationObserverSelector = mkSelector "tokenByAddingParameterAutomationObserver:"

-- | @Selector@ for @removeParameterObserver:@
removeParameterObserverSelector :: Selector '[RawId] ()
removeParameterObserverSelector = mkSelector "removeParameterObserver:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector '[] (Id NSString)
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @implementorValueObserver@
implementorValueObserverSelector :: Selector '[] (Ptr ())
implementorValueObserverSelector = mkSelector "implementorValueObserver"

-- | @Selector@ for @setImplementorValueObserver:@
setImplementorValueObserverSelector :: Selector '[Ptr ()] ()
setImplementorValueObserverSelector = mkSelector "setImplementorValueObserver:"

-- | @Selector@ for @implementorValueProvider@
implementorValueProviderSelector :: Selector '[] (Ptr ())
implementorValueProviderSelector = mkSelector "implementorValueProvider"

-- | @Selector@ for @setImplementorValueProvider:@
setImplementorValueProviderSelector :: Selector '[Ptr ()] ()
setImplementorValueProviderSelector = mkSelector "setImplementorValueProvider:"

-- | @Selector@ for @implementorStringFromValueCallback@
implementorStringFromValueCallbackSelector :: Selector '[] (Ptr ())
implementorStringFromValueCallbackSelector = mkSelector "implementorStringFromValueCallback"

-- | @Selector@ for @setImplementorStringFromValueCallback:@
setImplementorStringFromValueCallbackSelector :: Selector '[Ptr ()] ()
setImplementorStringFromValueCallbackSelector = mkSelector "setImplementorStringFromValueCallback:"

-- | @Selector@ for @implementorValueFromStringCallback@
implementorValueFromStringCallbackSelector :: Selector '[] (Ptr ())
implementorValueFromStringCallbackSelector = mkSelector "implementorValueFromStringCallback"

-- | @Selector@ for @setImplementorValueFromStringCallback:@
setImplementorValueFromStringCallbackSelector :: Selector '[Ptr ()] ()
setImplementorValueFromStringCallbackSelector = mkSelector "setImplementorValueFromStringCallback:"

-- | @Selector@ for @implementorDisplayNameWithLengthCallback@
implementorDisplayNameWithLengthCallbackSelector :: Selector '[] (Ptr ())
implementorDisplayNameWithLengthCallbackSelector = mkSelector "implementorDisplayNameWithLengthCallback"

-- | @Selector@ for @setImplementorDisplayNameWithLengthCallback:@
setImplementorDisplayNameWithLengthCallbackSelector :: Selector '[Ptr ()] ()
setImplementorDisplayNameWithLengthCallbackSelector = mkSelector "setImplementorDisplayNameWithLengthCallback:"

