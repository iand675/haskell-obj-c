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
  , routePickerButtonBordered
  , setRoutePickerButtonBordered
  , routePickerButtonStyle
  , setRoutePickerButtonStyle
  , prioritizesVideoDevices
  , setPrioritizesVideoDevices
  , routePickerButtonColorForStateSelector
  , setRoutePickerButtonColor_forStateSelector
  , routePickerButtonBorderedSelector
  , setRoutePickerButtonBorderedSelector
  , routePickerButtonStyleSelector
  , setRoutePickerButtonStyleSelector
  , prioritizesVideoDevicesSelector
  , setPrioritizesVideoDevicesSelector

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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @routePickerButtonColorForState:@
routePickerButtonColorForStateSelector :: Selector
routePickerButtonColorForStateSelector = mkSelector "routePickerButtonColorForState:"

-- | @Selector@ for @setRoutePickerButtonColor:forState:@
setRoutePickerButtonColor_forStateSelector :: Selector
setRoutePickerButtonColor_forStateSelector = mkSelector "setRoutePickerButtonColor:forState:"

-- | @Selector@ for @routePickerButtonBordered@
routePickerButtonBorderedSelector :: Selector
routePickerButtonBorderedSelector = mkSelector "routePickerButtonBordered"

-- | @Selector@ for @setRoutePickerButtonBordered:@
setRoutePickerButtonBorderedSelector :: Selector
setRoutePickerButtonBorderedSelector = mkSelector "setRoutePickerButtonBordered:"

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

