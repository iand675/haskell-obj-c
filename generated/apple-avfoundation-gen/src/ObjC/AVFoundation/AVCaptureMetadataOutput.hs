{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureMetadataOutput
--
-- AVCaptureMetadataOutput is a concrete subclass of AVCaptureOutput that can be used to process metadata objects from an attached connection.
--
-- Instances of AVCaptureMetadataOutput emit arrays of AVMetadataObject instances (see AVMetadataObject.h), such as detected faces. Applications can access the metadata objects with the captureOutput:didOutputMetadataObjects:fromConnection: delegate method.
--
-- Generated bindings for @AVCaptureMetadataOutput@.
module ObjC.AVFoundation.AVCaptureMetadataOutput
  ( AVCaptureMetadataOutput
  , IsAVCaptureMetadataOutput(..)
  , init_
  , new
  , setMetadataObjectsDelegate_queue
  , metadataObjectsDelegate
  , metadataObjectsCallbackQueue
  , availableMetadataObjectTypes
  , metadataObjectTypes
  , setMetadataObjectTypes
  , requiredMetadataObjectTypesForCinematicVideoCapture
  , initSelector
  , newSelector
  , setMetadataObjectsDelegate_queueSelector
  , metadataObjectsDelegateSelector
  , metadataObjectsCallbackQueueSelector
  , availableMetadataObjectTypesSelector
  , metadataObjectTypesSelector
  , setMetadataObjectTypesSelector
  , requiredMetadataObjectTypesForCinematicVideoCaptureSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureMetadataOutput avCaptureMetadataOutput => avCaptureMetadataOutput -> IO (Id AVCaptureMetadataOutput)
init_ avCaptureMetadataOutput  =
    sendMsg avCaptureMetadataOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureMetadataOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureMetadataOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | setMetadataObjectsDelegate:queue:
--
-- Sets the receiver's delegate that will accept metadata objects and dispatch queue on which the delegate will be called.
--
-- @objectsDelegate@ — An object conforming to the AVCaptureMetadataOutputObjectsDelegate protocol that will receive metadata objects after they are captured.
--
-- @objectsCallbackQueue@ — A dispatch queue on which all delegate methods will be called.
--
-- When new metadata objects are captured in the receiver's connection, they will be vended to the delegate using the captureOutput:didOutputMetadataObjects:fromConnection: delegate method. All delegate methods will be called on the specified dispatch queue.
--
-- Clients that need to minimize the chances of metadata being dropped should specify a queue on which a sufficiently small amount of processing is performed along with receiving metadata objects.
--
-- A serial dispatch queue must be used to guarantee that metadata objects will be delivered in order. The objectsCallbackQueue parameter may not be NULL, except when setting the objectsDelegate to nil otherwise -setMetadataObjectsDelegate:queue: throws an NSInvalidArgumentException.
--
-- ObjC selector: @- setMetadataObjectsDelegate:queue:@
setMetadataObjectsDelegate_queue :: (IsAVCaptureMetadataOutput avCaptureMetadataOutput, IsNSObject objectsCallbackQueue) => avCaptureMetadataOutput -> RawId -> objectsCallbackQueue -> IO ()
setMetadataObjectsDelegate_queue avCaptureMetadataOutput  objectsDelegate objectsCallbackQueue =
  withObjCPtr objectsCallbackQueue $ \raw_objectsCallbackQueue ->
      sendMsg avCaptureMetadataOutput (mkSelector "setMetadataObjectsDelegate:queue:") retVoid [argPtr (castPtr (unRawId objectsDelegate) :: Ptr ()), argPtr (castPtr raw_objectsCallbackQueue :: Ptr ())]

-- | metadataObjectsDelegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVCaptureMetadataOutputObjectsDelegate protocol that will receive metadata objects after they are captured. The delegate is set using the setMetadataObjectsDelegate:queue: method.
--
-- ObjC selector: @- metadataObjectsDelegate@
metadataObjectsDelegate :: IsAVCaptureMetadataOutput avCaptureMetadataOutput => avCaptureMetadataOutput -> IO RawId
metadataObjectsDelegate avCaptureMetadataOutput  =
    fmap (RawId . castPtr) $ sendMsg avCaptureMetadataOutput (mkSelector "metadataObjectsDelegate") (retPtr retVoid) []

-- | metadataObjectsCallbackQueue
--
-- The dispatch queue on which all metadata object delegate methods will be called.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the setMetadataObjectsDelegate:queue: method.
--
-- ObjC selector: @- metadataObjectsCallbackQueue@
metadataObjectsCallbackQueue :: IsAVCaptureMetadataOutput avCaptureMetadataOutput => avCaptureMetadataOutput -> IO (Id NSObject)
metadataObjectsCallbackQueue avCaptureMetadataOutput  =
    sendMsg avCaptureMetadataOutput (mkSelector "metadataObjectsCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | availableMetadataObjectTypes
--
-- Indicates the receiver's supported metadata object types.
--
-- The value of this property is an NSArray of NSStrings corresponding to AVMetadataObjectType strings defined in AVMetadataObject.h -- one for each metadata object type supported by the receiver. Available metadata object types are dependent on the capabilities of the AVCaptureInputPort to which this receiver's AVCaptureConnection is connected. Clients may specify the types of objects they would like to process by calling setMetadataObjectTypes:. This property is key-value observable.
--
-- ObjC selector: @- availableMetadataObjectTypes@
availableMetadataObjectTypes :: IsAVCaptureMetadataOutput avCaptureMetadataOutput => avCaptureMetadataOutput -> IO (Id NSArray)
availableMetadataObjectTypes avCaptureMetadataOutput  =
    sendMsg avCaptureMetadataOutput (mkSelector "availableMetadataObjectTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadataObjectTypes
--
-- Specifies the types of metadata objects that the receiver should present to the client.
--
-- AVCaptureMetadataOutput may detect and emit multiple metadata object types. For apps linked before iOS 7.0, the receiver defaults to capturing face metadata objects if supported (see -availableMetadataObjectTypes). For apps linked on or after iOS 7.0, the receiver captures no metadata objects by default. -setMetadataObjectTypes: throws an NSInvalidArgumentException if any elements in the array are not present in the -availableMetadataObjectTypes array.
--
-- If you've set your AVCaptureMetadataOutput's connected input's @cinematicVideoCaptureEnabled@ property to YES, you must set your @metadataObjectTypes@ property to @requiredMetadataObjectTypesForCinematicVideoCapture@ or an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- metadataObjectTypes@
metadataObjectTypes :: IsAVCaptureMetadataOutput avCaptureMetadataOutput => avCaptureMetadataOutput -> IO (Id NSArray)
metadataObjectTypes avCaptureMetadataOutput  =
    sendMsg avCaptureMetadataOutput (mkSelector "metadataObjectTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadataObjectTypes
--
-- Specifies the types of metadata objects that the receiver should present to the client.
--
-- AVCaptureMetadataOutput may detect and emit multiple metadata object types. For apps linked before iOS 7.0, the receiver defaults to capturing face metadata objects if supported (see -availableMetadataObjectTypes). For apps linked on or after iOS 7.0, the receiver captures no metadata objects by default. -setMetadataObjectTypes: throws an NSInvalidArgumentException if any elements in the array are not present in the -availableMetadataObjectTypes array.
--
-- If you've set your AVCaptureMetadataOutput's connected input's @cinematicVideoCaptureEnabled@ property to YES, you must set your @metadataObjectTypes@ property to @requiredMetadataObjectTypesForCinematicVideoCapture@ or an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- setMetadataObjectTypes:@
setMetadataObjectTypes :: (IsAVCaptureMetadataOutput avCaptureMetadataOutput, IsNSArray value) => avCaptureMetadataOutput -> value -> IO ()
setMetadataObjectTypes avCaptureMetadataOutput  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCaptureMetadataOutput (mkSelector "setMetadataObjectTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The required metadata object types when Cinematic Video capture is enabled.
--
-- Since the Cinematic Video algorithm requires a particular set of metadata objects to function optimally, you must set your ``metadataObjectTypes`` property to this property's returned value if you've set ``AVCaptureDeviceInput/cinematicVideoCaptureEnabled`` to @true@ on the connected device input, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- ObjC selector: @- requiredMetadataObjectTypesForCinematicVideoCapture@
requiredMetadataObjectTypesForCinematicVideoCapture :: IsAVCaptureMetadataOutput avCaptureMetadataOutput => avCaptureMetadataOutput -> IO (Id NSArray)
requiredMetadataObjectTypesForCinematicVideoCapture avCaptureMetadataOutput  =
    sendMsg avCaptureMetadataOutput (mkSelector "requiredMetadataObjectTypesForCinematicVideoCapture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setMetadataObjectsDelegate:queue:@
setMetadataObjectsDelegate_queueSelector :: Selector
setMetadataObjectsDelegate_queueSelector = mkSelector "setMetadataObjectsDelegate:queue:"

-- | @Selector@ for @metadataObjectsDelegate@
metadataObjectsDelegateSelector :: Selector
metadataObjectsDelegateSelector = mkSelector "metadataObjectsDelegate"

-- | @Selector@ for @metadataObjectsCallbackQueue@
metadataObjectsCallbackQueueSelector :: Selector
metadataObjectsCallbackQueueSelector = mkSelector "metadataObjectsCallbackQueue"

-- | @Selector@ for @availableMetadataObjectTypes@
availableMetadataObjectTypesSelector :: Selector
availableMetadataObjectTypesSelector = mkSelector "availableMetadataObjectTypes"

-- | @Selector@ for @metadataObjectTypes@
metadataObjectTypesSelector :: Selector
metadataObjectTypesSelector = mkSelector "metadataObjectTypes"

-- | @Selector@ for @setMetadataObjectTypes:@
setMetadataObjectTypesSelector :: Selector
setMetadataObjectTypesSelector = mkSelector "setMetadataObjectTypes:"

-- | @Selector@ for @requiredMetadataObjectTypesForCinematicVideoCapture@
requiredMetadataObjectTypesForCinematicVideoCaptureSelector :: Selector
requiredMetadataObjectTypesForCinematicVideoCaptureSelector = mkSelector "requiredMetadataObjectTypesForCinematicVideoCapture"

