{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.WebKit.NSObject
  ( NSObject
  , IsNSObject(..)
  , webPlugInContainerLoadRequest_inFrame
  , webPlugInContainerShowStatus
  , webPlugInInitialize
  , webPlugInStart
  , webPlugInStop
  , webPlugInDestroy
  , webPlugInSetIsSelected
  , webPlugInMainResourceDidReceiveResponse
  , webPlugInMainResourceDidReceiveData
  , webPlugInMainResourceDidFailWithError
  , webPlugInMainResourceDidFinishLoading
  , webScriptNameForSelector
  , isSelectorExcludedFromWebScript
  , webScriptNameForKey
  , isKeyExcludedFromWebScript
  , invokeUndefinedMethodFromWebScript_withArguments
  , invokeDefaultMethodWithArguments
  , finalizeForWebScript
  , webPlugInContainerSelectionColor
  , webFrame
  , objectForWebScript
  , webPlugInContainerLoadRequest_inFrameSelector
  , webPlugInContainerShowStatusSelector
  , webPlugInInitializeSelector
  , webPlugInStartSelector
  , webPlugInStopSelector
  , webPlugInDestroySelector
  , webPlugInSetIsSelectedSelector
  , webPlugInMainResourceDidReceiveResponseSelector
  , webPlugInMainResourceDidReceiveDataSelector
  , webPlugInMainResourceDidFailWithErrorSelector
  , webPlugInMainResourceDidFinishLoadingSelector
  , webScriptNameForSelectorSelector
  , isSelectorExcludedFromWebScriptSelector
  , webScriptNameForKeySelector
  , isKeyExcludedFromWebScriptSelector
  , invokeUndefinedMethodFromWebScript_withArgumentsSelector
  , invokeDefaultMethodWithArgumentsSelector
  , finalizeForWebScriptSelector
  , webPlugInContainerSelectionColorSelector
  , webFrameSelector
  , objectForWebScriptSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | webPlugInContainerLoadRequest:inFrame:
--
-- Tell the application to show a URL in a target frame
--
-- @request@ — The request to be loaded.
--
-- @target@ — The target frame. If the frame with the specified target is not    found, a new window is opened and the main frame of the new window is named    with the specified target.  If nil is specified, the frame that contains    the applet is targeted.
--
-- ObjC selector: @- webPlugInContainerLoadRequest:inFrame:@
webPlugInContainerLoadRequest_inFrame :: (IsNSObject nsObject, IsNSURLRequest request, IsNSString target) => nsObject -> request -> target -> IO ()
webPlugInContainerLoadRequest_inFrame nsObject  request target =
withObjCPtr request $ \raw_request ->
  withObjCPtr target $ \raw_target ->
      sendMsg nsObject (mkSelector "webPlugInContainerLoadRequest:inFrame:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_target :: Ptr ())]

-- | webPlugInContainerShowStatus:
--
-- Tell the application to show the specified status message.
--
-- @message@ — The string to be shown.
--
-- ObjC selector: @- webPlugInContainerShowStatus:@
webPlugInContainerShowStatus :: (IsNSObject nsObject, IsNSString message) => nsObject -> message -> IO ()
webPlugInContainerShowStatus nsObject  message =
withObjCPtr message $ \raw_message ->
    sendMsg nsObject (mkSelector "webPlugInContainerShowStatus:") retVoid [argPtr (castPtr raw_message :: Ptr ())]

-- | webPlugInInitialize
--
-- Tell the plug-in to perform one-time initialization.
--
-- This method must be only called once per instance of the plug-in    object and must be called before any other methods in this protocol.
--
-- ObjC selector: @- webPlugInInitialize@
webPlugInInitialize :: IsNSObject nsObject => nsObject -> IO ()
webPlugInInitialize nsObject  =
  sendMsg nsObject (mkSelector "webPlugInInitialize") retVoid []

-- | webPlugInStart
--
-- Tell the plug-in to start normal operation.
--
-- The plug-in usually begins drawing, playing sounds and/or    animation in this method.  This method must be called before calling webPlugInStop.    This method may called more than once, provided that the application has    already called webPlugInInitialize and that each call to webPlugInStart is followed    by a call to webPlugInStop.
--
-- ObjC selector: @- webPlugInStart@
webPlugInStart :: IsNSObject nsObject => nsObject -> IO ()
webPlugInStart nsObject  =
  sendMsg nsObject (mkSelector "webPlugInStart") retVoid []

-- | webPlugInStop
--
-- Tell the plug-in to stop normal operation.
--
-- webPlugInStop must be called before this method.  This method may be    called more than once, provided that the application has already called    webPlugInInitialize and that each call to webPlugInStop is preceded by a call to    webPlugInStart.
--
-- ObjC selector: @- webPlugInStop@
webPlugInStop :: IsNSObject nsObject => nsObject -> IO ()
webPlugInStop nsObject  =
  sendMsg nsObject (mkSelector "webPlugInStop") retVoid []

-- | webPlugInDestroy
--
-- Tell the plug-in perform cleanup and prepare to be deallocated.
--
-- The plug-in typically releases memory and other resources in this    method.  If the plug-in has retained the WebPlugInContainer, it must release    it in this mehthod.  This method must be only called once per instance of the    plug-in object.  No other methods in this interface may be called after the    application has called webPlugInDestroy.
--
-- ObjC selector: @- webPlugInDestroy@
webPlugInDestroy :: IsNSObject nsObject => nsObject -> IO ()
webPlugInDestroy nsObject  =
  sendMsg nsObject (mkSelector "webPlugInDestroy") retVoid []

-- | webPlugInSetIsSelected:
--
-- Informs the plug-in whether or not it is selected.  This is typically    used to allow the plug-in to alter it's appearance when selected.
--
-- ObjC selector: @- webPlugInSetIsSelected:@
webPlugInSetIsSelected :: IsNSObject nsObject => nsObject -> Bool -> IO ()
webPlugInSetIsSelected nsObject  isSelected =
  sendMsg nsObject (mkSelector "webPlugInSetIsSelected:") retVoid [argCULong (if isSelected then 1 else 0)]

-- | webPlugInMainResourceDidReceiveResponse:
--
-- Called on the plug-in when WebKit receives -connection:didReceiveResponse:    for the plug-in's main resource.
--
-- This method is only sent to the plug-in if the    WebPlugInShouldLoadMainResourceKey argument passed to the plug-in was NO.
--
-- ObjC selector: @- webPlugInMainResourceDidReceiveResponse:@
webPlugInMainResourceDidReceiveResponse :: (IsNSObject nsObject, IsNSURLResponse response) => nsObject -> response -> IO ()
webPlugInMainResourceDidReceiveResponse nsObject  response =
withObjCPtr response $ \raw_response ->
    sendMsg nsObject (mkSelector "webPlugInMainResourceDidReceiveResponse:") retVoid [argPtr (castPtr raw_response :: Ptr ())]

-- | webPlugInMainResourceDidReceiveData:
--
-- Called on the plug-in when WebKit recieves -connection:didReceiveData:    for the plug-in's main resource.
--
-- This method is only sent to the plug-in if the    WebPlugInShouldLoadMainResourceKey argument passed to the plug-in was NO.
--
-- ObjC selector: @- webPlugInMainResourceDidReceiveData:@
webPlugInMainResourceDidReceiveData :: (IsNSObject nsObject, IsNSData data_) => nsObject -> data_ -> IO ()
webPlugInMainResourceDidReceiveData nsObject  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsObject (mkSelector "webPlugInMainResourceDidReceiveData:") retVoid [argPtr (castPtr raw_data_ :: Ptr ())]

-- | webPlugInMainResourceDidFailWithError:
--
-- Called on the plug-in when WebKit receives -connection:didFailWithError:    for the plug-in's main resource.
--
-- This method is only sent to the plug-in if the    WebPlugInShouldLoadMainResourceKey argument passed to the plug-in was NO.
--
-- ObjC selector: @- webPlugInMainResourceDidFailWithError:@
webPlugInMainResourceDidFailWithError :: (IsNSObject nsObject, IsNSError error_) => nsObject -> error_ -> IO ()
webPlugInMainResourceDidFailWithError nsObject  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsObject (mkSelector "webPlugInMainResourceDidFailWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | webPlugInMainResourceDidFinishLoading
--
-- Called on the plug-in when WebKit receives -connectionDidFinishLoading:    for the plug-in's main resource.
--
-- This method is only sent to the plug-in if the    WebPlugInShouldLoadMainResourceKey argument passed to the plug-in was NO.
--
-- ObjC selector: @- webPlugInMainResourceDidFinishLoading@
webPlugInMainResourceDidFinishLoading :: IsNSObject nsObject => nsObject -> IO ()
webPlugInMainResourceDidFinishLoading nsObject  =
  sendMsg nsObject (mkSelector "webPlugInMainResourceDidFinishLoading") retVoid []

-- | webScriptNameForSelector:
--
-- @selector@ — The selector that will be exposed to the script environment.
--
-- Use the returned string as the exported name for the selector    in the script environment. It is the responsibility of the class to ensure    uniqueness of the returned name. If nil is returned or this    method is not implemented the default name for the selector will    be used. The default name concatenates the components of the    Objective-C selector name and replaces ':' with '_'.  '_' characters    are escaped with an additional '$', i.e. '_' becomes "$_". '$' are    also escaped, i.e.        Objective-C name        Default script name        moveTo::                move__        moveTo_                 moveTo$_        moveTo$_                moveTo$$$_
--
-- Returns: Returns the name to be used to represent the specified selector in the    scripting environment.
--
-- ObjC selector: @+ webScriptNameForSelector:@
webScriptNameForSelector :: Selector -> IO (Id NSString)
webScriptNameForSelector selector =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "webScriptNameForSelector:") (retPtr retVoid) [argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | isSelectorExcludedFromWebScript:
--
-- @selector@ — The selector the will be exposed to the script environment.
--
-- Return NO to export the selector to the script environment.    Return YES to prevent the selector from being exported to the script environment.     If this method is not implemented on the class no selectors will be exported.
--
-- Returns: Returns YES to hide the selector, NO to export the selector.
--
-- ObjC selector: @+ isSelectorExcludedFromWebScript:@
isSelectorExcludedFromWebScript :: Selector -> IO Bool
isSelectorExcludedFromWebScript selector =
  do
    cls' <- getRequiredClass "NSObject"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isSelectorExcludedFromWebScript:") retCULong [argPtr (unSelector selector)]

-- | webScriptNameForKey:
--
-- @name@ — The name of the instance variable that will be exposed to the    script environment. Only instance variables that meet the export criteria will    be exposed.
--
-- Provide an alternate name for a property.
--
-- Returns: Returns the name to be used to represent the specified property in the    scripting environment.
--
-- ObjC selector: @+ webScriptNameForKey:@
webScriptNameForKey :: Const (Ptr CChar) -> IO (Id NSString)
webScriptNameForKey name =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "webScriptNameForKey:") (retPtr retVoid) [argPtr (unConst name)] >>= retainedObject . castPtr

-- | isKeyExcludedFromWebScript:
--
-- @name@ — The name of the instance variable that will be exposed to the    script environment.
--
-- Return NO to export the property to the script environment.    Return YES to prevent the property from being exported to the script environment.
--
-- Returns: Returns YES to hide the property, NO to export the property.
--
-- ObjC selector: @+ isKeyExcludedFromWebScript:@
isKeyExcludedFromWebScript :: Const (Ptr CChar) -> IO Bool
isKeyExcludedFromWebScript name =
  do
    cls' <- getRequiredClass "NSObject"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isKeyExcludedFromWebScript:") retCULong [argPtr (unConst name)]

-- | invokeUndefinedMethodFromWebScript:withArguments:
--
-- @name@ — The name of the method to invoke.
--
-- @arguments@ — The arguments to pass the method.
--
-- If a script attempts to invoke a method that is not exported,     invokeUndefinedMethodFromWebScript:withArguments: will be called.
--
-- Returns: The return value of the invocation. The value will be converted as appropriate    for the script environment.
--
-- ObjC selector: @- invokeUndefinedMethodFromWebScript:withArguments:@
invokeUndefinedMethodFromWebScript_withArguments :: (IsNSObject nsObject, IsNSString name, IsNSArray arguments) => nsObject -> name -> arguments -> IO RawId
invokeUndefinedMethodFromWebScript_withArguments nsObject  name arguments =
withObjCPtr name $ \raw_name ->
  withObjCPtr arguments $ \raw_arguments ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "invokeUndefinedMethodFromWebScript:withArguments:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())]

-- | invokeDefaultMethodWithArguments:
--
-- @arguments@ — The arguments to pass the method.
--
-- If a script attempts to call an exposed object as a function,     this method will be called.
--
-- Returns: The return value of the call. The value will be converted as appropriate    for the script environment.
--
-- ObjC selector: @- invokeDefaultMethodWithArguments:@
invokeDefaultMethodWithArguments :: (IsNSObject nsObject, IsNSArray arguments) => nsObject -> arguments -> IO RawId
invokeDefaultMethodWithArguments nsObject  arguments =
withObjCPtr arguments $ \raw_arguments ->
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "invokeDefaultMethodWithArguments:") (retPtr retVoid) [argPtr (castPtr raw_arguments :: Ptr ())]

-- | finalizeForWebScript
--
-- finalizeForScript is called on objects exposed to the script    environment just before the script environment garbage collects the object.    Subsequently, any references to WebScriptObjects made by the exposed object will    be invalid and have undefined consequences.
--
-- ObjC selector: @- finalizeForWebScript@
finalizeForWebScript :: IsNSObject nsObject => nsObject -> IO ()
finalizeForWebScript nsObject  =
  sendMsg nsObject (mkSelector "finalizeForWebScript") retVoid []

-- | webPlugInContainerSelectionColor
--
-- The color that should be used for any special drawing when    plug-in is selected.
--
-- ObjC selector: @- webPlugInContainerSelectionColor@
webPlugInContainerSelectionColor :: IsNSObject nsObject => nsObject -> IO (Id NSColor)
webPlugInContainerSelectionColor nsObject  =
  sendMsg nsObject (mkSelector "webPlugInContainerSelectionColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | webFrame
--
-- Allows the plug-in to access the WebFrame that    contains the plug-in.  This method will not be implemented by containers that     are not WebKit based.
--
-- ObjC selector: @- webFrame@
webFrame :: IsNSObject nsObject => nsObject -> IO (Id WebFrame)
webFrame nsObject  =
  sendMsg nsObject (mkSelector "webFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | objectForWebScript
--
-- objectForWebScript is used to expose a plug-in's scripting interface.  The     methods of the object are exposed to the script environment.  See the WebScripting    informal protocol for more details.
--
-- Returns: Returns the object that exposes the plug-in's interface.  The class of this    object can implement methods from the WebScripting informal protocol.
--
-- ObjC selector: @- objectForWebScript@
objectForWebScript :: IsNSObject nsObject => nsObject -> IO RawId
objectForWebScript nsObject  =
  fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "objectForWebScript") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webPlugInContainerLoadRequest:inFrame:@
webPlugInContainerLoadRequest_inFrameSelector :: Selector
webPlugInContainerLoadRequest_inFrameSelector = mkSelector "webPlugInContainerLoadRequest:inFrame:"

-- | @Selector@ for @webPlugInContainerShowStatus:@
webPlugInContainerShowStatusSelector :: Selector
webPlugInContainerShowStatusSelector = mkSelector "webPlugInContainerShowStatus:"

-- | @Selector@ for @webPlugInInitialize@
webPlugInInitializeSelector :: Selector
webPlugInInitializeSelector = mkSelector "webPlugInInitialize"

-- | @Selector@ for @webPlugInStart@
webPlugInStartSelector :: Selector
webPlugInStartSelector = mkSelector "webPlugInStart"

-- | @Selector@ for @webPlugInStop@
webPlugInStopSelector :: Selector
webPlugInStopSelector = mkSelector "webPlugInStop"

-- | @Selector@ for @webPlugInDestroy@
webPlugInDestroySelector :: Selector
webPlugInDestroySelector = mkSelector "webPlugInDestroy"

-- | @Selector@ for @webPlugInSetIsSelected:@
webPlugInSetIsSelectedSelector :: Selector
webPlugInSetIsSelectedSelector = mkSelector "webPlugInSetIsSelected:"

-- | @Selector@ for @webPlugInMainResourceDidReceiveResponse:@
webPlugInMainResourceDidReceiveResponseSelector :: Selector
webPlugInMainResourceDidReceiveResponseSelector = mkSelector "webPlugInMainResourceDidReceiveResponse:"

-- | @Selector@ for @webPlugInMainResourceDidReceiveData:@
webPlugInMainResourceDidReceiveDataSelector :: Selector
webPlugInMainResourceDidReceiveDataSelector = mkSelector "webPlugInMainResourceDidReceiveData:"

-- | @Selector@ for @webPlugInMainResourceDidFailWithError:@
webPlugInMainResourceDidFailWithErrorSelector :: Selector
webPlugInMainResourceDidFailWithErrorSelector = mkSelector "webPlugInMainResourceDidFailWithError:"

-- | @Selector@ for @webPlugInMainResourceDidFinishLoading@
webPlugInMainResourceDidFinishLoadingSelector :: Selector
webPlugInMainResourceDidFinishLoadingSelector = mkSelector "webPlugInMainResourceDidFinishLoading"

-- | @Selector@ for @webScriptNameForSelector:@
webScriptNameForSelectorSelector :: Selector
webScriptNameForSelectorSelector = mkSelector "webScriptNameForSelector:"

-- | @Selector@ for @isSelectorExcludedFromWebScript:@
isSelectorExcludedFromWebScriptSelector :: Selector
isSelectorExcludedFromWebScriptSelector = mkSelector "isSelectorExcludedFromWebScript:"

-- | @Selector@ for @webScriptNameForKey:@
webScriptNameForKeySelector :: Selector
webScriptNameForKeySelector = mkSelector "webScriptNameForKey:"

-- | @Selector@ for @isKeyExcludedFromWebScript:@
isKeyExcludedFromWebScriptSelector :: Selector
isKeyExcludedFromWebScriptSelector = mkSelector "isKeyExcludedFromWebScript:"

-- | @Selector@ for @invokeUndefinedMethodFromWebScript:withArguments:@
invokeUndefinedMethodFromWebScript_withArgumentsSelector :: Selector
invokeUndefinedMethodFromWebScript_withArgumentsSelector = mkSelector "invokeUndefinedMethodFromWebScript:withArguments:"

-- | @Selector@ for @invokeDefaultMethodWithArguments:@
invokeDefaultMethodWithArgumentsSelector :: Selector
invokeDefaultMethodWithArgumentsSelector = mkSelector "invokeDefaultMethodWithArguments:"

-- | @Selector@ for @finalizeForWebScript@
finalizeForWebScriptSelector :: Selector
finalizeForWebScriptSelector = mkSelector "finalizeForWebScript"

-- | @Selector@ for @webPlugInContainerSelectionColor@
webPlugInContainerSelectionColorSelector :: Selector
webPlugInContainerSelectionColorSelector = mkSelector "webPlugInContainerSelectionColor"

-- | @Selector@ for @webFrame@
webFrameSelector :: Selector
webFrameSelector = mkSelector "webFrame"

-- | @Selector@ for @objectForWebScript@
objectForWebScriptSelector :: Selector
objectForWebScriptSelector = mkSelector "objectForWebScript"

