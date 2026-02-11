{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The @SBObject@ class declares methods that can be invoked on any object in a scriptable application. It defines methods for getting elements and properties of an object, as well as setting a given object to a new value.
--
-- Each @SBObject@ is built around an object specifier, which tells Scripting Bridge how to locate the object. Therefore, you can think of an @SBObject@ as a reference to an object in an target application rather than an object itself. To bypass this reference-based approach and force evaluation, use the ``SBObject/get`` method.
--
-- Typically, rather than create @SBObject@ instances explictly, you receive @SBObject@ objects by calling methods of an ``SBApplication`` subclass. For example, if you wanted to get an @SBObject@ representing the current iTunes track, you would use code like this (where @iTunesTrack@ is a subclass of @SBObject@):
--
-- ```objc iTunesApplication *iTunes = [SBApplication applicationWithBundleIdentifier:"com.apple.iTunes"]; iTunesTrack *track = [iTunes currentTrack]; ```
--
-- You can discover the names of dynamically generated classes such as @iTunesApplication@ and @iTunesTrack@ by examining the header file created by the @sdp@ tool. Alternatively, you give these variables the dynamic Objective-C type @id@.
--
-- Generated bindings for @SBObject@.
module ObjC.ScriptingBridge.SBObject
  ( SBObject
  , IsSBObject(..)
  , init_
  , initWithProperties
  , initWithData
  , get
  , lastError
  , initWithElementCode_properties_data
  , propertyWithCode
  , propertyWithClass_code
  , elementArrayWithCode
  , sendEvent_id_parameters
  , setTo
  , initSelector
  , initWithPropertiesSelector
  , initWithDataSelector
  , getSelector
  , lastErrorSelector
  , initWithElementCode_properties_dataSelector
  , propertyWithCodeSelector
  , propertyWithClass_codeSelector
  , elementArrayWithCodeSelector
  , sendEvent_id_parametersSelector
  , setToSelector


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

import ObjC.ScriptingBridge.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes and returns an instance of an @SBObject@ subclass.
--
-- Scripting Bridge does not actually create an object in the target application until you add the object returned from this method to an element array (``SBElementArray``).
--
-- - Returns: An @SBObject@ object or @nil@ if the object could not be initialized.
--
-- ObjC selector: @- init@
init_ :: IsSBObject sbObject => sbObject -> IO (Id SBObject)
init_ sbObject  =
  sendMsg sbObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns an instance of an @SBObject@ subclass initialized with the specified properties.
--
-- Scripting Bridge does not actually create an object in the target application until you add the object returned from this method to an element array (``SBElementArray``).
--
-- - Parameters:   - properties: A dictionary with keys specifying the names of properties (that is, attributes or to-one relationships) and the values for those properties.
--
-- - Returns: An @SBObject@ object or @nil@ if the object could not be initialized.
--
-- ObjC selector: @- initWithProperties:@
initWithProperties :: (IsSBObject sbObject, IsNSDictionary properties) => sbObject -> properties -> IO (Id SBObject)
initWithProperties sbObject  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg sbObject (mkSelector "initWithProperties:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= ownedObject . castPtr

-- | Returns an instance of an @SBObject@ subclass initialized with the given data.
--
-- Scripting Bridge does not actually create an object in the target application until you add the object returned from this method to an element array (``SBElementArray``).
--
-- - Parameters:   - data: An object containing data for the new @SBObject@ object. The data varies according to the type of scripting object to be created.
--
-- - Returns: An @SBObject@ object or @nil@ if the object could not be initialized.
--
-- ObjC selector: @- initWithData:@
initWithData :: IsSBObject sbObject => sbObject -> RawId -> IO (Id SBObject)
initWithData sbObject  data_ =
  sendMsg sbObject (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr (unRawId data_) :: Ptr ())] >>= ownedObject . castPtr

-- | Forces evaluation of the receiver, causing the real object to be returned immediately.
--
-- This method forces the current object reference (the receiver) to be evaluated, resulting in the return of the referenced object. By default, Scripting Bridge deals with references to objects until you actually request some concrete data from them or until you call the @get@ method.
--
-- - Returns: For most properties, the result is a Foundation object such as an @NSString@. For properties with no Foundation equivalent, the result is an @NSAppleEventDescriptor@ or another ``SBObject`` for most elements.
--
-- ObjC selector: @- get@
get :: IsSBObject sbObject => sbObject -> IO RawId
get sbObject  =
  fmap (RawId . castPtr) $ sendMsg sbObject (mkSelector "get") (retPtr retVoid) []

-- | The error from the last event this object sent, or nil if it succeeded.
--
-- ObjC selector: @- lastError@
lastError :: IsSBObject sbObject => sbObject -> IO (Id NSError)
lastError sbObject  =
  sendMsg sbObject (mkSelector "lastError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an instance of an @SBObject@ subclass initialized with the specified properties and data and added to the designated element array.
--
-- Unlike the other initializers of this class, this method not only initializes the @SBObject@ object but adds it to a specified element array. This method is the designated initializer.
--
-- - Parameters:   - code: A four-character code used to identify an element in the target application’s scripting interface. See <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager> for details.
--
-- - properties: A dictionary with <doc://com.apple.documentation/documentation/foundation/nsnumber> keys specifying the four-character codes of properties (that is, attributes or to-one relationships) and the values for those properties. Pass @nil@ if you are initializing the object by @data@ only.
--
-- - data: An object containing data for the new @SBObject@ object. The data varies according to the type of scripting object to be created. Pass @nil@ if you initializing the object by @properties@ only.
--
-- - Returns: An @SBObject@ object or @nil@ if the object could not be initialized.
--
-- ObjC selector: @- initWithElementCode:properties:data:@
initWithElementCode_properties_data :: (IsSBObject sbObject, IsNSDictionary properties) => sbObject -> CUInt -> properties -> RawId -> IO (Id SBObject)
initWithElementCode_properties_data sbObject  code properties data_ =
withObjCPtr properties $ \raw_properties ->
    sendMsg sbObject (mkSelector "initWithElementCode:properties:data:") (retPtr retVoid) [argCUInt (fromIntegral code), argPtr (castPtr raw_properties :: Ptr ()), argPtr (castPtr (unRawId data_) :: Ptr ())] >>= ownedObject . castPtr

-- | Returns an object representing the specified property of the receiver.
--
-- @SBObject@ subclasses use this method to implement application-specific property accessor methods. You should not need to call this method directly.
--
-- - Parameters:   - code: A four-character code that uniquely identifies a property of the receiver.
--
-- - Returns: An object representing the receiver’s property as identified by @code@.
--
-- ObjC selector: @- propertyWithCode:@
propertyWithCode :: IsSBObject sbObject => sbObject -> CUInt -> IO (Id SBObject)
propertyWithCode sbObject  code =
  sendMsg sbObject (mkSelector "propertyWithCode:") (retPtr retVoid) [argCUInt (fromIntegral code)] >>= retainedObject . castPtr

-- | Returns an object of the designated scripting class representing the specified property of the receiver
--
-- @SBObject@ subclasses use this method to implement application-specific property accessor methods. You should not need to call this method directly.
--
-- > Note: This method doesn't retrieve the value of the property. To get the   value, call ``get``.
--
-- - Parameters:   - class: The @SBObject@ subclass with which to instantiate the object.
--
-- - code: A four-character code that uniquely identifies a property of the receiver.
--
-- - Returns: An instance of the designated @class@ that represents the receiver’s property identified by @code@.
--
-- ObjC selector: @- propertyWithClass:code:@
propertyWithClass_code :: IsSBObject sbObject => sbObject -> Class -> CUInt -> IO (Id SBObject)
propertyWithClass_code sbObject  cls code =
  sendMsg sbObject (mkSelector "propertyWithClass:code:") (retPtr retVoid) [argPtr (unClass cls), argCUInt (fromIntegral code)] >>= retainedObject . castPtr

-- | Returns an array containing every child of the receiver with the given class-type code.
--
-- @SBObject@ subclasses use this method to implement application-specific property accessor methods. You should not need to call this method directly.
--
-- > Note: This method doesn't retrieve the value of the property. To get the   value, call ``get``.
--
-- - Parameters:   - code: A four-character code that identifies a scripting class.
--
-- - Returns: An ``SBElementArray`` object containing every child of the receiver whose class matches @code@.
--
-- ObjC selector: @- elementArrayWithCode:@
elementArrayWithCode :: IsSBObject sbObject => sbObject -> CUInt -> IO (Id SBElementArray)
elementArrayWithCode sbObject  code =
  sendMsg sbObject (mkSelector "elementArrayWithCode:") (retPtr retVoid) [argCUInt (fromIntegral code)] >>= retainedObject . castPtr

-- | Sends an Apple event with the given event class, event ID, and format to the target application.
--
-- Scripting Bridge uses this method to communicate with target applications. If the target application responds to this method by sending an Apple event representing an error, the receiver calls its delegate's ``SBApplicationDelegate/eventDidFail:withError:`` method. If no delegate has been assigned, the receiver raises an exception.
--
-- You should rarely have to call this method directly.
--
-- - Parameters:   - eventClass: The event class of the Apple event to be sent.
--
-- - eventID: The event ID of the Apple event to be sent.
--
-- - firstParamCode,...: A list of four-character parameter codes (<doc://com.apple.documentation/documentation/coreservices/desctype>) and object values (@id@) terminated by a zero.
--
-- - Returns: The target application's Apple event sent in reply; it is converted to a Cocoa object of an appropriate type.
--
-- ObjC selector: @- sendEvent:id:parameters:@
sendEvent_id_parameters :: IsSBObject sbObject => sbObject -> CUInt -> CUInt -> CUInt -> IO RawId
sendEvent_id_parameters sbObject  eventClass eventID firstParamCode =
  fmap (RawId . castPtr) $ sendMsg sbObject (mkSelector "sendEvent:id:parameters:") (retPtr retVoid) [argCUInt (fromIntegral eventClass), argCUInt (fromIntegral eventID), argCUInt (fromIntegral firstParamCode)]

-- | Sets the receiver to a specified value.
--
-- You should not call this method directly.
--
-- - Parameters:   - value: The data the receiver should be set to. It can be an <doc://com.apple.documentation/documentation/foundation/nsstring>, <doc://com.apple.documentation/documentation/foundation/nsnumber>, <doc://com.apple.documentation/documentation/foundation/nsarray>, @SBObject@, or any other type of object supported by the Scripting Bridge framework.
--
-- ObjC selector: @- setTo:@
setTo :: IsSBObject sbObject => sbObject -> RawId -> IO ()
setTo sbObject  value =
  sendMsg sbObject (mkSelector "setTo:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProperties:@
initWithPropertiesSelector :: Selector
initWithPropertiesSelector = mkSelector "initWithProperties:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @get@
getSelector :: Selector
getSelector = mkSelector "get"

-- | @Selector@ for @lastError@
lastErrorSelector :: Selector
lastErrorSelector = mkSelector "lastError"

-- | @Selector@ for @initWithElementCode:properties:data:@
initWithElementCode_properties_dataSelector :: Selector
initWithElementCode_properties_dataSelector = mkSelector "initWithElementCode:properties:data:"

-- | @Selector@ for @propertyWithCode:@
propertyWithCodeSelector :: Selector
propertyWithCodeSelector = mkSelector "propertyWithCode:"

-- | @Selector@ for @propertyWithClass:code:@
propertyWithClass_codeSelector :: Selector
propertyWithClass_codeSelector = mkSelector "propertyWithClass:code:"

-- | @Selector@ for @elementArrayWithCode:@
elementArrayWithCodeSelector :: Selector
elementArrayWithCodeSelector = mkSelector "elementArrayWithCode:"

-- | @Selector@ for @sendEvent:id:parameters:@
sendEvent_id_parametersSelector :: Selector
sendEvent_id_parametersSelector = mkSelector "sendEvent:id:parameters:"

-- | @Selector@ for @setTo:@
setToSelector :: Selector
setToSelector = mkSelector "setTo:"

