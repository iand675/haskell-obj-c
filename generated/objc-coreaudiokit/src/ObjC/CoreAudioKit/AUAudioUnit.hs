{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUAudioUnit
--
-- An audio unit instance.
--
-- AUAudioUnit is a host interface to an audio unit. Hosts can instantiate either version 2 or		version 3 units with this class, and to some extent control whether an audio unit is		instantiated in-process or in a separate extension process.
--
-- Implementors of version 3 audio units can and should subclass AUAudioUnit. To port an		existing version 2 audio unit easily, AUAudioUnitV2Bridge can be subclassed.
--
-- These are the ways in which audio unit components can be registered:
--
-- - (v2) Packaged into a component bundle containing an @AudioComponents@ Info.plist entry,		referring to an @AudioComponentFactoryFunction@. See AudioComponent.h.
--
-- - (v2) AudioComponentRegister(). Associates a component description with an		AudioComponentFactoryFunction. See AudioComponent.h.
--
-- - (v3) Packaged into an app extension containing an AudioComponents Info.plist entry.		The principal class must conform to the AUAudioUnitFactory protocol, which will typically		instantiate an AUAudioUnit subclass.
--
-- - (v3) @+[AUAudioUnit registerSubclass:asComponentDescription:name:version:]@. Associates		a component description with an AUAudioUnit subclass.
--
-- A host need not be aware of the concrete subclass of AUAudioUnit that is being instantiated.		@initWithComponentDescription:options:error:@ ensures that the proper subclass is used.
--
-- When using AUAudioUnit with a v2 audio unit, or the C AudioComponent and AudioUnit API's		with a v3 audio unit, all major pieces of functionality are bridged between the		two API's. This header describes, for each v3 method or property, the v2 equivalent.
--
-- Generated bindings for @AUAudioUnit@.
module ObjC.CoreAudioKit.AUAudioUnit
  ( AUAudioUnit
  , IsAUAudioUnit(..)
  , requestViewControllerWithCompletionHandler
  , supportedViewConfigurations
  , selectViewConfiguration
  , requestViewControllerWithCompletionHandlerSelector
  , supportedViewConfigurationsSelector
  , selectViewConfigurationSelector


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

import ObjC.CoreAudioKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | requestViewControllerWithCompletionHandler:
--
-- Obtains an audio unit's view controller (and thereby a view).
--
-- Asynchronously requests the audio unit's view controller. This method will later call the		completion handler, in a thread/dispatch queue context internal to the implementation, with		a view controller, or nil in the case of an audio unit without a custom view controller.
--
-- ObjC selector: @- requestViewControllerWithCompletionHandler:@
requestViewControllerWithCompletionHandler :: IsAUAudioUnit auAudioUnit => auAudioUnit -> Ptr () -> IO ()
requestViewControllerWithCompletionHandler auAudioUnit  completionHandler =
  sendMsg auAudioUnit (mkSelector "requestViewControllerWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | supportedViewConfigurations
--
-- @availableViewConfigurations@ — The list of all available view configurations supported by the host.
--
-- Returns: A set of indices of view configurations from the availableViewConfigurations array that the		audio unit supports.
--
-- Query the list of supported view configurations.
--
-- The host can query the audio unit for all the view configurations it supports.		Hosts can support multiple configurations in which they can display the user interfaces of		audio units (for example: full screen, normal, live mode, etc). These configurations can be		of different sizes and the host might display its own control surfaces along with the view		of the audio unit. The host will call this method and pass an array of supported		configurations.
--
-- The audio unit should override this method and implement its own logic to report all the 		view configurations it supports. The size of the view in the selected configuration should 		be large enough to fit the view of the audio unit, otherwise it might be truncated and a 		scroll bar might be necessary to navigate it.
--
-- In case an empty set is returned from this method, it is considered that the plugin only 		supports the largest available view configuration.
--
-- ObjC selector: @- supportedViewConfigurations:@
supportedViewConfigurations :: (IsAUAudioUnit auAudioUnit, IsNSArray availableViewConfigurations) => auAudioUnit -> availableViewConfigurations -> IO (Id NSIndexSet)
supportedViewConfigurations auAudioUnit  availableViewConfigurations =
withObjCPtr availableViewConfigurations $ \raw_availableViewConfigurations ->
    sendMsg auAudioUnit (mkSelector "supportedViewConfigurations:") (retPtr retVoid) [argPtr (castPtr raw_availableViewConfigurations :: Ptr ())] >>= retainedObject . castPtr

-- | selectViewConfiguration
--
-- @viewConfiguration@ — The requested view configuration.
--
-- The view configuration passed to this method should be one which was indicated as supported         via supportedViewConfigurations. If any other, unsupported, view configuration is passed or         if supportedViewConfigurations returns an empty set, the audio unit implementation should         fall back to its default (largest available) view configuration.
--
-- Request a view configuration from the audio unit.
--
-- The host can use this method to switch the audio unit's view into a new configuration.		Audio Units should override this method with the logic needed to adapt their view controller 		to the requested configuration.
--
-- ObjC selector: @- selectViewConfiguration:@
selectViewConfiguration :: (IsAUAudioUnit auAudioUnit, IsAUAudioUnitViewConfiguration viewConfiguration) => auAudioUnit -> viewConfiguration -> IO ()
selectViewConfiguration auAudioUnit  viewConfiguration =
withObjCPtr viewConfiguration $ \raw_viewConfiguration ->
    sendMsg auAudioUnit (mkSelector "selectViewConfiguration:") retVoid [argPtr (castPtr raw_viewConfiguration :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestViewControllerWithCompletionHandler:@
requestViewControllerWithCompletionHandlerSelector :: Selector
requestViewControllerWithCompletionHandlerSelector = mkSelector "requestViewControllerWithCompletionHandler:"

-- | @Selector@ for @supportedViewConfigurations:@
supportedViewConfigurationsSelector :: Selector
supportedViewConfigurationsSelector = mkSelector "supportedViewConfigurations:"

-- | @Selector@ for @selectViewConfiguration:@
selectViewConfigurationSelector :: Selector
selectViewConfigurationSelector = mkSelector "selectViewConfiguration:"

