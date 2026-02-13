{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that requests the delivery and handles the receipt of PushKit notifications.
--
-- A @PKPushRegistry@ object manages only certain types of notifications, such as high-priority notifications needed by a VoIP app. PushKit wakes up your app as needed to deliver incoming notifications and delivers the notifications directly to the push registry object that requested them.
--
-- Every time your app launches, whether in the foreground or in the background, create a push registry object and configure it. Typically, you keep the push registry object running for the duration of your app. Each push registry object delivers incoming notifications to its ``PushKit/PKPushRegistry/delegate`` object, which also handles the responses for registration requests. The listing below shows how to create a push registry object and request VoIP notifications. Always assign an appropriate delegate object before modifying the ``PushKit/PKPushRegistry/desiredPushTypes`` property.
--
-- ```objc - (void) registerForVoIPPushes {    self.voipRegistry = [[PKPushRegistry alloc] initWithQueue:nil];    self.voipRegistry.delegate = self;
--
-- // Initiate registration.    self.voipRegistry.desiredPushTypes = [NSSet setWithObject:PKPushTypeVoIP]; } ```
--
-- Assigning a new value to the ``PushKit/PKPushRegistry/desiredPushTypes`` property registers the push registry object with the PushKit servers. The server reports the success or failure of your registration attempts asynchronously to the push registry, which then reports those results to its delegate object. The push registry also delivers all received notifications to the delegate object. For more information about the delegate methods, see ``PushKit/PKPushRegistryDelegate``.
--
-- ## Topics
--
-- ### Initializing a Push Registry
--
-- - ``PushKit/PKPushRegistry/initWithQueue:``
--
-- ### Receiving the Notification Data
--
-- - ``PushKit/PKPushRegistry/delegate`` - ``PushKit/PKPushRegistryDelegate``
--
-- ### Managing the Push Registry
--
-- - ``PushKit/PKPushRegistry/desiredPushTypes`` - ``PushKit/PKPushRegistry/pushTokenForType:``
--
-- Generated bindings for @PKPushRegistry@.
module ObjC.PushKit.PKPushRegistry
  ( PKPushRegistry
  , IsPKPushRegistry(..)
  , pushTokenForType
  , initWithQueue
  , init_
  , delegate
  , setDelegate
  , desiredPushTypes
  , setDesiredPushTypes
  , delegateSelector
  , desiredPushTypesSelector
  , initSelector
  , initWithQueueSelector
  , pushTokenForTypeSelector
  , setDelegateSelector
  , setDesiredPushTypesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PushKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Retrieves the locally cached push token for the specified push type.
--
-- If registration for a specific push type is successful, the push registry delivers the corresponding push token to its delegate and adds a copy of the token to its local cache. Use this method to retrieve the token at a later time.
--
-- - Parameters:   - type: A push type requested by this push registry object. For a list of possible types, see ``PushKit/PKPushType``.
--
-- - Returns: The push token used to send pushes to the device or @nil@ if no token is available for the specified type.
--
-- ObjC selector: @- pushTokenForType:@
pushTokenForType :: (IsPKPushRegistry pkPushRegistry, IsNSString type_) => pkPushRegistry -> type_ -> IO (Id NSData)
pushTokenForType pkPushRegistry type_ =
  sendMessage pkPushRegistry pushTokenForTypeSelector (toNSString type_)

-- | Creates a push registry with the specified dispatch queue.
--
-- - Parameters:   - queue: The dispatch queue on which to execute the delegate methods. It is recommended that you specify a serial queue for this parameter. Specify @nil@ to execute the delegate methods on the app’s main queue.
--
-- - Returns: A @PKPushRegistry@ object that you can use to register for push tokens and use to receive notifications.
--
-- ObjC selector: @- initWithQueue:@
initWithQueue :: (IsPKPushRegistry pkPushRegistry, IsNSObject queue) => pkPushRegistry -> queue -> IO (Id PKPushRegistry)
initWithQueue pkPushRegistry queue =
  sendOwnedMessage pkPushRegistry initWithQueueSelector (toNSObject queue)

-- | init
--
-- Unavailable, use -initWithQueue: instead.
--
-- ObjC selector: @- init@
init_ :: IsPKPushRegistry pkPushRegistry => pkPushRegistry -> IO (Id PKPushRegistry)
init_ pkPushRegistry =
  sendOwnedMessage pkPushRegistry initSelector

-- | The delegate object that receives notifications coming from the push registry object.
--
-- You must assign a valid object to this property before modifying the ``PushKit/PKPushRegistry/desiredPushTypes`` property. A valid delegate object is required to receive push tokens and payload data from incoming pushes.
--
-- For more information about the methods of the @PKPushRegistryDelegate@ protocol, see ``PushKit/PKPushRegistryDelegate``.
--
-- ObjC selector: @- delegate@
delegate :: IsPKPushRegistry pkPushRegistry => pkPushRegistry -> IO RawId
delegate pkPushRegistry =
  sendMessage pkPushRegistry delegateSelector

-- | The delegate object that receives notifications coming from the push registry object.
--
-- You must assign a valid object to this property before modifying the ``PushKit/PKPushRegistry/desiredPushTypes`` property. A valid delegate object is required to receive push tokens and payload data from incoming pushes.
--
-- For more information about the methods of the @PKPushRegistryDelegate@ protocol, see ``PushKit/PKPushRegistryDelegate``.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsPKPushRegistry pkPushRegistry => pkPushRegistry -> RawId -> IO ()
setDelegate pkPushRegistry value =
  sendMessage pkPushRegistry setDelegateSelector value

-- | Registers the push types for this push registry object.
--
-- When you assign a value to this property, the push registry object makes a registration request with the PushKit server. This request is asynchronous, and the success or failure of the request is reported to your registery's delegate object. For a successful registration, PushKit delivers a push token to the delegate. Use that token to generate push requests from your server.
--
-- For a list of push types that you may include in the set, see ``PushKit/PKPushType``.
--
-- ObjC selector: @- desiredPushTypes@
desiredPushTypes :: IsPKPushRegistry pkPushRegistry => pkPushRegistry -> IO (Id NSSet)
desiredPushTypes pkPushRegistry =
  sendMessage pkPushRegistry desiredPushTypesSelector

-- | Registers the push types for this push registry object.
--
-- When you assign a value to this property, the push registry object makes a registration request with the PushKit server. This request is asynchronous, and the success or failure of the request is reported to your registery's delegate object. For a successful registration, PushKit delivers a push token to the delegate. Use that token to generate push requests from your server.
--
-- For a list of push types that you may include in the set, see ``PushKit/PKPushType``.
--
-- ObjC selector: @- setDesiredPushTypes:@
setDesiredPushTypes :: (IsPKPushRegistry pkPushRegistry, IsNSSet value) => pkPushRegistry -> value -> IO ()
setDesiredPushTypes pkPushRegistry value =
  sendMessage pkPushRegistry setDesiredPushTypesSelector (toNSSet value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pushTokenForType:@
pushTokenForTypeSelector :: Selector '[Id NSString] (Id NSData)
pushTokenForTypeSelector = mkSelector "pushTokenForType:"

-- | @Selector@ for @initWithQueue:@
initWithQueueSelector :: Selector '[Id NSObject] (Id PKPushRegistry)
initWithQueueSelector = mkSelector "initWithQueue:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKPushRegistry)
initSelector = mkSelector "init"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @desiredPushTypes@
desiredPushTypesSelector :: Selector '[] (Id NSSet)
desiredPushTypesSelector = mkSelector "desiredPushTypes"

-- | @Selector@ for @setDesiredPushTypes:@
setDesiredPushTypesSelector :: Selector '[Id NSSet] ()
setDesiredPushTypesSelector = mkSelector "setDesiredPushTypes:"

