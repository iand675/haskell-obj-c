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
  , displayNameWithLengthSelector
  , tokenByAddingParameterObserverSelector
  , tokenByAddingParameterRecordingObserverSelector
  , tokenByAddingParameterAutomationObserverSelector
  , removeParameterObserverSelector
  , identifierSelector
  , keyPathSelector
  , displayNameSelector
  , implementorValueObserverSelector
  , setImplementorValueObserverSelector
  , implementorValueProviderSelector
  , setImplementorValueProviderSelector
  , implementorStringFromValueCallbackSelector
  , setImplementorStringFromValueCallbackSelector
  , implementorValueFromStringCallbackSelector
  , setImplementorValueFromStringCallbackSelector
  , implementorDisplayNameWithLengthCallbackSelector
  , setImplementorDisplayNameWithLengthCallbackSelector


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
displayNameWithLength auParameterNode  maximumLength =
  sendMsg auParameterNode (mkSelector "displayNameWithLength:") (retPtr retVoid) [argCLong (fromIntegral maximumLength)] >>= retainedObject . castPtr

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
tokenByAddingParameterObserver auParameterNode  observer =
  fmap (RawId . castPtr) $ sendMsg auParameterNode (mkSelector "tokenByAddingParameterObserver:") (retPtr retVoid) [argPtr (castPtr observer :: Ptr ())]

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
tokenByAddingParameterRecordingObserver auParameterNode  observer =
  fmap (RawId . castPtr) $ sendMsg auParameterNode (mkSelector "tokenByAddingParameterRecordingObserver:") (retPtr retVoid) [argPtr (castPtr observer :: Ptr ())]

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
tokenByAddingParameterAutomationObserver auParameterNode  observer =
  fmap (RawId . castPtr) $ sendMsg auParameterNode (mkSelector "tokenByAddingParameterAutomationObserver:") (retPtr retVoid) [argPtr (castPtr observer :: Ptr ())]

-- | removeParameterObserver:
--
-- Remove an observer created with tokenByAddingParameterObserver,		tokenByAddingParameterRecordingObserver, or tokenByAddingParameterAutomationObserver.
--
-- This call will remove the callback corresponding to the supplied token. Note that this		will block until any callbacks currently in flight have completed.
--
-- ObjC selector: @- removeParameterObserver:@
removeParameterObserver :: IsAUParameterNode auParameterNode => auParameterNode -> RawId -> IO ()
removeParameterObserver auParameterNode  token =
  sendMsg auParameterNode (mkSelector "removeParameterObserver:") retVoid [argPtr (castPtr (unRawId token) :: Ptr ())]

-- | identifier
--
-- A non-localized, permanent name for a parameter or group.
--
-- The identifier must be unique for all child nodes under any given parent. From release to		release, an audio unit must not change its parameters' identifiers; this will invalidate any		hosts' documents that refer to the parameters.
--
-- ObjC selector: @- identifier@
identifier :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Id NSString)
identifier auParameterNode  =
  sendMsg auParameterNode (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

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
keyPath auParameterNode  =
  sendMsg auParameterNode (mkSelector "keyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | displayName
--
-- A localized name to display for the parameter.
--
-- ObjC selector: @- displayName@
displayName :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Id NSString)
displayName auParameterNode  =
  sendMsg auParameterNode (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Called when a parameter changes value.
--
-- This block, used only in an audio unit implementation, receives all externally-generated		changes to parameter values. It should store the new value in its audio signal processing		state (assuming that that state is separate from the AUParameter object).
--
-- ObjC selector: @- implementorValueObserver@
implementorValueObserver :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorValueObserver auParameterNode  =
  fmap castPtr $ sendMsg auParameterNode (mkSelector "implementorValueObserver") (retPtr retVoid) []

-- | Called when a parameter changes value.
--
-- This block, used only in an audio unit implementation, receives all externally-generated		changes to parameter values. It should store the new value in its audio signal processing		state (assuming that that state is separate from the AUParameter object).
--
-- ObjC selector: @- setImplementorValueObserver:@
setImplementorValueObserver :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorValueObserver auParameterNode  value =
  sendMsg auParameterNode (mkSelector "setImplementorValueObserver:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called when a value of a parameter in the tree is known to have a stale value				needing to be refreshed.
--
-- The audio unit should return the current value for this parameter; the AUParameterNode will		store the value.
--
-- ObjC selector: @- implementorValueProvider@
implementorValueProvider :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorValueProvider auParameterNode  =
  fmap castPtr $ sendMsg auParameterNode (mkSelector "implementorValueProvider") (retPtr retVoid) []

-- | Called when a value of a parameter in the tree is known to have a stale value				needing to be refreshed.
--
-- The audio unit should return the current value for this parameter; the AUParameterNode will		store the value.
--
-- ObjC selector: @- setImplementorValueProvider:@
setImplementorValueProvider :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorValueProvider auParameterNode  value =
  sendMsg auParameterNode (mkSelector "setImplementorValueProvider:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called to provide string representations of parameter values.	If value is nil, the callback uses the current value of the parameter.
--
-- ObjC selector: @- implementorStringFromValueCallback@
implementorStringFromValueCallback :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorStringFromValueCallback auParameterNode  =
  fmap castPtr $ sendMsg auParameterNode (mkSelector "implementorStringFromValueCallback") (retPtr retVoid) []

-- | Called to provide string representations of parameter values.	If value is nil, the callback uses the current value of the parameter.
--
-- ObjC selector: @- setImplementorStringFromValueCallback:@
setImplementorStringFromValueCallback :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorStringFromValueCallback auParameterNode  value =
  sendMsg auParameterNode (mkSelector "setImplementorStringFromValueCallback:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called to convert string to numeric representations of parameter values.
--
-- ObjC selector: @- implementorValueFromStringCallback@
implementorValueFromStringCallback :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorValueFromStringCallback auParameterNode  =
  fmap castPtr $ sendMsg auParameterNode (mkSelector "implementorValueFromStringCallback") (retPtr retVoid) []

-- | Called to convert string to numeric representations of parameter values.
--
-- ObjC selector: @- setImplementorValueFromStringCallback:@
setImplementorValueFromStringCallback :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorValueFromStringCallback auParameterNode  value =
  sendMsg auParameterNode (mkSelector "setImplementorValueFromStringCallback:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called to obtain an abbreviated version of a parameter or group name.
--
-- ObjC selector: @- implementorDisplayNameWithLengthCallback@
implementorDisplayNameWithLengthCallback :: IsAUParameterNode auParameterNode => auParameterNode -> IO (Ptr ())
implementorDisplayNameWithLengthCallback auParameterNode  =
  fmap castPtr $ sendMsg auParameterNode (mkSelector "implementorDisplayNameWithLengthCallback") (retPtr retVoid) []

-- | Called to obtain an abbreviated version of a parameter or group name.
--
-- ObjC selector: @- setImplementorDisplayNameWithLengthCallback:@
setImplementorDisplayNameWithLengthCallback :: IsAUParameterNode auParameterNode => auParameterNode -> Ptr () -> IO ()
setImplementorDisplayNameWithLengthCallback auParameterNode  value =
  sendMsg auParameterNode (mkSelector "setImplementorDisplayNameWithLengthCallback:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayNameWithLength:@
displayNameWithLengthSelector :: Selector
displayNameWithLengthSelector = mkSelector "displayNameWithLength:"

-- | @Selector@ for @tokenByAddingParameterObserver:@
tokenByAddingParameterObserverSelector :: Selector
tokenByAddingParameterObserverSelector = mkSelector "tokenByAddingParameterObserver:"

-- | @Selector@ for @tokenByAddingParameterRecordingObserver:@
tokenByAddingParameterRecordingObserverSelector :: Selector
tokenByAddingParameterRecordingObserverSelector = mkSelector "tokenByAddingParameterRecordingObserver:"

-- | @Selector@ for @tokenByAddingParameterAutomationObserver:@
tokenByAddingParameterAutomationObserverSelector :: Selector
tokenByAddingParameterAutomationObserverSelector = mkSelector "tokenByAddingParameterAutomationObserver:"

-- | @Selector@ for @removeParameterObserver:@
removeParameterObserverSelector :: Selector
removeParameterObserverSelector = mkSelector "removeParameterObserver:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @implementorValueObserver@
implementorValueObserverSelector :: Selector
implementorValueObserverSelector = mkSelector "implementorValueObserver"

-- | @Selector@ for @setImplementorValueObserver:@
setImplementorValueObserverSelector :: Selector
setImplementorValueObserverSelector = mkSelector "setImplementorValueObserver:"

-- | @Selector@ for @implementorValueProvider@
implementorValueProviderSelector :: Selector
implementorValueProviderSelector = mkSelector "implementorValueProvider"

-- | @Selector@ for @setImplementorValueProvider:@
setImplementorValueProviderSelector :: Selector
setImplementorValueProviderSelector = mkSelector "setImplementorValueProvider:"

-- | @Selector@ for @implementorStringFromValueCallback@
implementorStringFromValueCallbackSelector :: Selector
implementorStringFromValueCallbackSelector = mkSelector "implementorStringFromValueCallback"

-- | @Selector@ for @setImplementorStringFromValueCallback:@
setImplementorStringFromValueCallbackSelector :: Selector
setImplementorStringFromValueCallbackSelector = mkSelector "setImplementorStringFromValueCallback:"

-- | @Selector@ for @implementorValueFromStringCallback@
implementorValueFromStringCallbackSelector :: Selector
implementorValueFromStringCallbackSelector = mkSelector "implementorValueFromStringCallback"

-- | @Selector@ for @setImplementorValueFromStringCallback:@
setImplementorValueFromStringCallbackSelector :: Selector
setImplementorValueFromStringCallbackSelector = mkSelector "setImplementorValueFromStringCallback:"

-- | @Selector@ for @implementorDisplayNameWithLengthCallback@
implementorDisplayNameWithLengthCallbackSelector :: Selector
implementorDisplayNameWithLengthCallbackSelector = mkSelector "implementorDisplayNameWithLengthCallback"

-- | @Selector@ for @setImplementorDisplayNameWithLengthCallback:@
setImplementorDisplayNameWithLengthCallbackSelector :: Selector
setImplementorDisplayNameWithLengthCallbackSelector = mkSelector "setImplementorDisplayNameWithLengthCallback:"

