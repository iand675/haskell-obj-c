{-# LANGUAGE PatternSynonyms #-}
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
  , routePickerButtonColorForStateSelector
  , setRoutePickerButtonColor_forStateSelector
  , delegateSelector
  , setDelegateSelector
  , playerSelector
  , setPlayerSelector
  , routePickerButtonBorderedSelector
  , setRoutePickerButtonBorderedSelector
  , activeTintColorSelector
  , setActiveTintColorSelector
  , routePickerButtonStyleSelector
  , setRoutePickerButtonStyleSelector
  , prioritizesVideoDevicesSelector
  , setPrioritizesVideoDevicesSelector
  , customRoutingControllerSelector
  , setCustomRoutingControllerSelector

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
routePickerButtonColorForState avRoutePickerView  state =
    sendMsg avRoutePickerView (mkSelector "routePickerButtonColorForState:") (retPtr retVoid) [argCLong (coerce state)] >>= retainedObject . castPtr

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
setRoutePickerButtonColor_forState avRoutePickerView  color state =
  withObjCPtr color $ \raw_color ->
      sendMsg avRoutePickerView (mkSelector "setRoutePickerButtonColor:forState:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argCLong (coerce state)]

-- | delegate
--
-- The route picker view's delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO RawId
delegate avRoutePickerView  =
    fmap (RawId . castPtr) $ sendMsg avRoutePickerView (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The route picker view's delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> RawId -> IO ()
setDelegate avRoutePickerView  value =
    sendMsg avRoutePickerView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | player
--
-- The player for which to perform routing operations.
--
-- ObjC selector: @- player@
player :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO (Id AVPlayer)
player avRoutePickerView  =
    sendMsg avRoutePickerView (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | player
--
-- The player for which to perform routing operations.
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsAVRoutePickerView avRoutePickerView, IsAVPlayer value) => avRoutePickerView -> value -> IO ()
setPlayer avRoutePickerView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avRoutePickerView (mkSelector "setPlayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | routePickerButtonBordered
--
-- Whether or not the picker button has a border. Default is YES.
--
-- ObjC selector: @- routePickerButtonBordered@
routePickerButtonBordered :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO Bool
routePickerButtonBordered avRoutePickerView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avRoutePickerView (mkSelector "routePickerButtonBordered") retCULong []

-- | routePickerButtonBordered
--
-- Whether or not the picker button has a border. Default is YES.
--
-- ObjC selector: @- setRoutePickerButtonBordered:@
setRoutePickerButtonBordered :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> Bool -> IO ()
setRoutePickerButtonBordered avRoutePickerView  value =
    sendMsg avRoutePickerView (mkSelector "setRoutePickerButtonBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | activeTintColor
--
-- The view's tint color when AirPlay is active.
--
-- ObjC selector: @- activeTintColor@
activeTintColor :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO (Id NSColor)
activeTintColor avRoutePickerView  =
    sendMsg avRoutePickerView (mkSelector "activeTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activeTintColor
--
-- The view's tint color when AirPlay is active.
--
-- ObjC selector: @- setActiveTintColor:@
setActiveTintColor :: (IsAVRoutePickerView avRoutePickerView, IsNSColor value) => avRoutePickerView -> value -> IO ()
setActiveTintColor avRoutePickerView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avRoutePickerView (mkSelector "setActiveTintColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | routePickerButtonStyle
--
-- The route picker button style.
--
-- ObjC selector: @- routePickerButtonStyle@
routePickerButtonStyle :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO AVRoutePickerViewButtonStyle
routePickerButtonStyle avRoutePickerView  =
    fmap (coerce :: CLong -> AVRoutePickerViewButtonStyle) $ sendMsg avRoutePickerView (mkSelector "routePickerButtonStyle") retCLong []

-- | routePickerButtonStyle
--
-- The route picker button style.
--
-- ObjC selector: @- setRoutePickerButtonStyle:@
setRoutePickerButtonStyle :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> AVRoutePickerViewButtonStyle -> IO ()
setRoutePickerButtonStyle avRoutePickerView  value =
    sendMsg avRoutePickerView (mkSelector "setRoutePickerButtonStyle:") retVoid [argCLong (coerce value)]

-- | prioritizesVideoDevices
--
-- Whether or not the route picker should sort video capable output devices to the top of the list. Setting this to YES will cause the route picker view to show a videocentric icon.
--
-- ObjC selector: @- prioritizesVideoDevices@
prioritizesVideoDevices :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO Bool
prioritizesVideoDevices avRoutePickerView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avRoutePickerView (mkSelector "prioritizesVideoDevices") retCULong []

-- | prioritizesVideoDevices
--
-- Whether or not the route picker should sort video capable output devices to the top of the list. Setting this to YES will cause the route picker view to show a videocentric icon.
--
-- ObjC selector: @- setPrioritizesVideoDevices:@
setPrioritizesVideoDevices :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> Bool -> IO ()
setPrioritizesVideoDevices avRoutePickerView  value =
    sendMsg avRoutePickerView (mkSelector "setPrioritizesVideoDevices:") retVoid [argCULong (if value then 1 else 0)]

-- | customRoutingController
--
-- A controller which enables connection to 3rd party devices (non-airplay) via the picker.
--
-- ObjC selector: @- customRoutingController@
customRoutingController :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> IO RawId
customRoutingController avRoutePickerView  =
    fmap (RawId . castPtr) $ sendMsg avRoutePickerView (mkSelector "customRoutingController") (retPtr retVoid) []

-- | customRoutingController
--
-- A controller which enables connection to 3rd party devices (non-airplay) via the picker.
--
-- ObjC selector: @- setCustomRoutingController:@
setCustomRoutingController :: IsAVRoutePickerView avRoutePickerView => avRoutePickerView -> RawId -> IO ()
setCustomRoutingController avRoutePickerView  value =
    sendMsg avRoutePickerView (mkSelector "setCustomRoutingController:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @routePickerButtonColorForState:@
routePickerButtonColorForStateSelector :: Selector
routePickerButtonColorForStateSelector = mkSelector "routePickerButtonColorForState:"

-- | @Selector@ for @setRoutePickerButtonColor:forState:@
setRoutePickerButtonColor_forStateSelector :: Selector
setRoutePickerButtonColor_forStateSelector = mkSelector "setRoutePickerButtonColor:forState:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @routePickerButtonBordered@
routePickerButtonBorderedSelector :: Selector
routePickerButtonBorderedSelector = mkSelector "routePickerButtonBordered"

-- | @Selector@ for @setRoutePickerButtonBordered:@
setRoutePickerButtonBorderedSelector :: Selector
setRoutePickerButtonBorderedSelector = mkSelector "setRoutePickerButtonBordered:"

-- | @Selector@ for @activeTintColor@
activeTintColorSelector :: Selector
activeTintColorSelector = mkSelector "activeTintColor"

-- | @Selector@ for @setActiveTintColor:@
setActiveTintColorSelector :: Selector
setActiveTintColorSelector = mkSelector "setActiveTintColor:"

-- | @Selector@ for @routePickerButtonStyle@
routePickerButtonStyleSelector :: Selector
routePickerButtonStyleSelector = mkSelector "routePickerButtonStyle"

-- | @Selector@ for @setRoutePickerButtonStyle:@
setRoutePickerButtonStyleSelector :: Selector
setRoutePickerButtonStyleSelector = mkSelector "setRoutePickerButtonStyle:"

-- | @Selector@ for @prioritizesVideoDevices@
prioritizesVideoDevicesSelector :: Selector
prioritizesVideoDevicesSelector = mkSelector "prioritizesVideoDevices"

-- | @Selector@ for @setPrioritizesVideoDevices:@
setPrioritizesVideoDevicesSelector :: Selector
setPrioritizesVideoDevicesSelector = mkSelector "setPrioritizesVideoDevices:"

-- | @Selector@ for @customRoutingController@
customRoutingControllerSelector :: Selector
customRoutingControllerSelector = mkSelector "customRoutingController"

-- | @Selector@ for @setCustomRoutingController:@
setCustomRoutingControllerSelector :: Selector
setCustomRoutingControllerSelector = mkSelector "setCustomRoutingController:"

