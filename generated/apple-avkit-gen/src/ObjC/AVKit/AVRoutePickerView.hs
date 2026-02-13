{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVRoutePickerView@.
module ObjC.AVKit.AVRoutePickerView
  ( AVRoutePickerView
  , IsAVRoutePickerView(..)
  , routePickerButtonColorForState
  , setRoutePickerButtonColor_forState
  , delegate
  , setDelegate
  , player
  , setPlayer
  , routePickerButtonBordered
  , setRoutePickerButtonBordered
  , activeTintColor
  , setActiveTintColor
  , routePickerButtonStyle
  , setRoutePickerButtonStyle
  , prioritizesVideoDevices
  , setPrioritizesVideoDevices
  , customRoutingController
  , setCustomRoutingController
  , activeTintColorSelector
  , customRoutingControllerSelector
  , delegateSelector
  , playerSelector
  , prioritizesVideoDevicesSelector
  , routePickerButtonBorderedSelector
  , routePickerButtonColorForStateSelector
  , routePickerButtonStyleSelector
  , setActiveTintColorSelector
  , setCustomRoutingControllerSelector
  , setDelegateSelector
  , setPlayerSelector
  , setPrioritizesVideoDevicesSelector
  , setRoutePickerButtonBorderedSelector
  , setRoutePickerButtonColor_forStateSelector
  , setRoutePickerButtonStyleSelector

  -- * Enum types
  , AVRoutePickerViewButtonState(AVRoutePickerViewButtonState)
  , pattern AVRoutePickerViewButtonStateNormal
  , pattern AVRoutePickerViewButtonStateNormalHighlighted
  , pattern AVRoutePickerViewButtonStateActive
  , pattern AVRoutePickerViewButtonStateActiveHighlighted
  , AVRoutePickerViewButtonStyle(AVRoutePickerViewButtonStyle)
  , pattern AVRoutePickerViewButtonStyleSystem
  , pattern AVRoutePickerViewButtonStylePlain
  , pattern AVRoutePickerViewButtonStyleCustom

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVKit.Internal.Classes
import ObjC.AVKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | routePickerButtonColorForState:
--
-- @state@ — The state for which to get the picker button color.
--
-- Returns the color of the picker button for a given state.
--
-- ObjC selector: @- routePickerButtonColorForState:@
routePickerButtonColorForState :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> AVRoutePickerViewButtonState -> IO (Id NSColor)
routePickerButtonColorForState avRoutePickerView state =
  sendMessage avRoutePickerView routePickerButtonColorForStateSelector state

-- | setRoutePickerButtonColor:forState:
--
-- @color@ — The color the button should have for a given state.
--
-- @state@ — The state for which to set the color of the button image.
--
-- Sets the color of the picker button for a given state.
--
-- If set to nil, the default color will be used for the given state.
--
-- ObjC selector: @- setRoutePickerButtonColor:forState:@
setRoutePickerButtonColor_forState :: (IsAVRoutePickerView avRoutePickerView, IsNSColor color) => avRoutePickerView -> color -> AVRoutePickerViewButtonState -> IO ()
setRoutePickerButtonColor_forState avRoutePickerView color state =
  sendMessage avRoutePickerView setRoutePickerButtonColor_forStateSelector (toNSColor color) state

-- | delegate
--
-- The route picker view's delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO RawId
delegate avRoutePickerView =
  sendMessage avRoutePickerView delegateSelector

-- | delegate
--
-- The route picker view's delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> RawId -> IO ()
setDelegate avRoutePickerView value =
  sendMessage avRoutePickerView setDelegateSelector value

-- | player
--
-- The player for which to perform routing operations.
--
-- ObjC selector: @- player@
player :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO (Id AVPlayer)
player avRoutePickerView =
  sendMessage avRoutePickerView playerSelector

-- | player
--
-- The player for which to perform routing operations.
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsAVRoutePickerView avRoutePickerView, IsAVPlayer value) => avRoutePickerView -> value -> IO ()
setPlayer avRoutePickerView value =
  sendMessage avRoutePickerView setPlayerSelector (toAVPlayer value)

-- | routePickerButtonBordered
--
-- Whether or not the picker button has a border. Default is YES.
--
-- ObjC selector: @- routePickerButtonBordered@
routePickerButtonBordered :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO Bool
routePickerButtonBordered avRoutePickerView =
  sendMessage avRoutePickerView routePickerButtonBorderedSelector

-- | routePickerButtonBordered
--
-- Whether or not the picker button has a border. Default is YES.
--
-- ObjC selector: @- setRoutePickerButtonBordered:@
setRoutePickerButtonBordered :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> Bool -> IO ()
setRoutePickerButtonBordered avRoutePickerView value =
  sendMessage avRoutePickerView setRoutePickerButtonBorderedSelector value

-- | activeTintColor
--
-- The view's tint color when AirPlay is active.
--
-- ObjC selector: @- activeTintColor@
activeTintColor :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO (Id NSColor)
activeTintColor avRoutePickerView =
  sendMessage avRoutePickerView activeTintColorSelector

-- | activeTintColor
--
-- The view's tint color when AirPlay is active.
--
-- ObjC selector: @- setActiveTintColor:@
setActiveTintColor :: (IsAVRoutePickerView avRoutePickerView, IsNSColor value) => avRoutePickerView -> value -> IO ()
setActiveTintColor avRoutePickerView value =
  sendMessage avRoutePickerView setActiveTintColorSelector (toNSColor value)

-- | routePickerButtonStyle
--
-- The route picker button style.
--
-- ObjC selector: @- routePickerButtonStyle@
routePickerButtonStyle :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO AVRoutePickerViewButtonStyle
routePickerButtonStyle avRoutePickerView =
  sendMessage avRoutePickerView routePickerButtonStyleSelector

-- | routePickerButtonStyle
--
-- The route picker button style.
--
-- ObjC selector: @- setRoutePickerButtonStyle:@
setRoutePickerButtonStyle :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> AVRoutePickerViewButtonStyle -> IO ()
setRoutePickerButtonStyle avRoutePickerView value =
  sendMessage avRoutePickerView setRoutePickerButtonStyleSelector value

-- | prioritizesVideoDevices
--
-- Whether or not the route picker should sort video capable output devices to the top of the list. Setting this to YES will cause the route picker view to show a videocentric icon.
--
-- ObjC selector: @- prioritizesVideoDevices@
prioritizesVideoDevices :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO Bool
prioritizesVideoDevices avRoutePickerView =
  sendMessage avRoutePickerView prioritizesVideoDevicesSelector

-- | prioritizesVideoDevices
--
-- Whether or not the route picker should sort video capable output devices to the top of the list. Setting this to YES will cause the route picker view to show a videocentric icon.
--
-- ObjC selector: @- setPrioritizesVideoDevices:@
setPrioritizesVideoDevices :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> Bool -> IO ()
setPrioritizesVideoDevices avRoutePickerView value =
  sendMessage avRoutePickerView setPrioritizesVideoDevicesSelector value

-- | customRoutingController
--
-- A controller which enables connection to 3rd party devices (non-airplay) via the picker.
--
-- ObjC selector: @- customRoutingController@
customRoutingController :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO RawId
customRoutingController avRoutePickerView =
  sendMessage avRoutePickerView customRoutingControllerSelector

-- | customRoutingController
--
-- A controller which enables connection to 3rd party devices (non-airplay) via the picker.
--
-- ObjC selector: @- setCustomRoutingController:@
setCustomRoutingController :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> RawId -> IO ()
setCustomRoutingController avRoutePickerView value =
  sendMessage avRoutePickerView setCustomRoutingControllerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @routePickerButtonColorForState:@
routePickerButtonColorForStateSelector :: Selector '[AVRoutePickerViewButtonState] (Id NSColor)
routePickerButtonColorForStateSelector = mkSelector "routePickerButtonColorForState:"

-- | @Selector@ for @setRoutePickerButtonColor:forState:@
setRoutePickerButtonColor_forStateSelector :: Selector '[Id NSColor, AVRoutePickerViewButtonState] ()
setRoutePickerButtonColor_forStateSelector = mkSelector "setRoutePickerButtonColor:forState:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id AVPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector '[Id AVPlayer] ()
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @routePickerButtonBordered@
routePickerButtonBorderedSelector :: Selector '[] Bool
routePickerButtonBorderedSelector = mkSelector "routePickerButtonBordered"

-- | @Selector@ for @setRoutePickerButtonBordered:@
setRoutePickerButtonBorderedSelector :: Selector '[Bool] ()
setRoutePickerButtonBorderedSelector = mkSelector "setRoutePickerButtonBordered:"

-- | @Selector@ for @activeTintColor@
activeTintColorSelector :: Selector '[] (Id NSColor)
activeTintColorSelector = mkSelector "activeTintColor"

-- | @Selector@ for @setActiveTintColor:@
setActiveTintColorSelector :: Selector '[Id NSColor] ()
setActiveTintColorSelector = mkSelector "setActiveTintColor:"

-- | @Selector@ for @routePickerButtonStyle@
routePickerButtonStyleSelector :: Selector '[] AVRoutePickerViewButtonStyle
routePickerButtonStyleSelector = mkSelector "routePickerButtonStyle"

-- | @Selector@ for @setRoutePickerButtonStyle:@
setRoutePickerButtonStyleSelector :: Selector '[AVRoutePickerViewButtonStyle] ()
setRoutePickerButtonStyleSelector = mkSelector "setRoutePickerButtonStyle:"

-- | @Selector@ for @prioritizesVideoDevices@
prioritizesVideoDevicesSelector :: Selector '[] Bool
prioritizesVideoDevicesSelector = mkSelector "prioritizesVideoDevices"

-- | @Selector@ for @setPrioritizesVideoDevices:@
setPrioritizesVideoDevicesSelector :: Selector '[Bool] ()
setPrioritizesVideoDevicesSelector = mkSelector "setPrioritizesVideoDevices:"

-- | @Selector@ for @customRoutingController@
customRoutingControllerSelector :: Selector '[] RawId
customRoutingControllerSelector = mkSelector "customRoutingController"

-- | @Selector@ for @setCustomRoutingController:@
setCustomRoutingControllerSelector :: Selector '[RawId] ()
setCustomRoutingControllerSelector = mkSelector "setCustomRoutingController:"

