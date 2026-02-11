{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerView
--
-- AVPlayerView is a subclass of NSView that can be used to display the visual content of an AVPlayer object and the standard playback controls.
--
-- Generated bindings for @AVPlayerView@.
module ObjC.AVKit.AVPlayerView
  ( AVPlayerView
  , IsAVPlayerView(..)
  , selectSpeed
  , flashChapterNumber_chapterTitle
  , beginTrimmingWithCompletionHandler
  , player
  , setPlayer
  , controlsStyle
  , setControlsStyle
  , videoGravity
  , setVideoGravity
  , readyForDisplay
  , videoBounds
  , updatesNowPlayingInfoCenter
  , setUpdatesNowPlayingInfoCenter
  , allowsVideoFrameAnalysis
  , setAllowsVideoFrameAnalysis
  , videoFrameAnalysisTypes
  , setVideoFrameAnalysisTypes
  , allowsMagnification
  , setAllowsMagnification
  , magnification
  , setMagnification
  , preferredDisplayDynamicRange
  , setPreferredDisplayDynamicRange
  , allowsPictureInPicturePlayback
  , setAllowsPictureInPicturePlayback
  , canBeginTrimming
  , showsFrameSteppingButtons
  , setShowsFrameSteppingButtons
  , showsSharingServiceButton
  , setShowsSharingServiceButton
  , actionPopUpButtonMenu
  , setActionPopUpButtonMenu
  , showsFullScreenToggleButton
  , setShowsFullScreenToggleButton
  , showsTimecodes
  , setShowsTimecodes
  , selectSpeedSelector
  , flashChapterNumber_chapterTitleSelector
  , beginTrimmingWithCompletionHandlerSelector
  , playerSelector
  , setPlayerSelector
  , controlsStyleSelector
  , setControlsStyleSelector
  , videoGravitySelector
  , setVideoGravitySelector
  , readyForDisplaySelector
  , videoBoundsSelector
  , updatesNowPlayingInfoCenterSelector
  , setUpdatesNowPlayingInfoCenterSelector
  , allowsVideoFrameAnalysisSelector
  , setAllowsVideoFrameAnalysisSelector
  , videoFrameAnalysisTypesSelector
  , setVideoFrameAnalysisTypesSelector
  , allowsMagnificationSelector
  , setAllowsMagnificationSelector
  , magnificationSelector
  , setMagnificationSelector
  , preferredDisplayDynamicRangeSelector
  , setPreferredDisplayDynamicRangeSelector
  , allowsPictureInPicturePlaybackSelector
  , setAllowsPictureInPicturePlaybackSelector
  , canBeginTrimmingSelector
  , showsFrameSteppingButtonsSelector
  , setShowsFrameSteppingButtonsSelector
  , showsSharingServiceButtonSelector
  , setShowsSharingServiceButtonSelector
  , actionPopUpButtonMenuSelector
  , setActionPopUpButtonMenuSelector
  , showsFullScreenToggleButtonSelector
  , setShowsFullScreenToggleButtonSelector
  , showsTimecodesSelector
  , setShowsTimecodesSelector

  -- * Enum types
  , AVDisplayDynamicRange(AVDisplayDynamicRange)
  , pattern AVDisplayDynamicRangeAutomatic
  , pattern AVDisplayDynamicRangeStandard
  , pattern AVDisplayDynamicRangeConstrainedHigh
  , pattern AVDisplayDynamicRangeHigh
  , AVPlayerViewControlsStyle(AVPlayerViewControlsStyle)
  , pattern AVPlayerViewControlsStyleNone
  , pattern AVPlayerViewControlsStyleInline
  , pattern AVPlayerViewControlsStyleFloating
  , pattern AVPlayerViewControlsStyleMinimal
  , pattern AVPlayerViewControlsStyleDefault
  , AVVideoFrameAnalysisType(AVVideoFrameAnalysisType)
  , pattern AVVideoFrameAnalysisTypeNone
  , pattern AVVideoFrameAnalysisTypeDefault
  , pattern AVVideoFrameAnalysisTypeText
  , pattern AVVideoFrameAnalysisTypeSubject
  , pattern AVVideoFrameAnalysisTypeVisualSearch
  , pattern AVVideoFrameAnalysisTypeMachineReadableCode

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

import ObjC.AVKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AVKit.Internal.Enums
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | selectSpeed
--
-- @speed@ — The playback speed to select.
--
-- Sets the input AVPlaybackSpeed as the selected speed.
--
-- Calls to selectSpeed with AVPlaybackSpeeds not contained within the speeds property array will be ignored.
--
-- ObjC selector: @- selectSpeed:@
selectSpeed :: (IsAVPlayerView avPlayerView, IsAVPlaybackSpeed speed) => avPlayerView -> speed -> IO ()
selectSpeed avPlayerView  speed =
withObjCPtr speed $ \raw_speed ->
    sendMsg avPlayerView (mkSelector "selectSpeed:") retVoid [argPtr (castPtr raw_speed :: Ptr ())]

-- | flashChapterNumber:chapterTitle:
--
-- @chapterNumber@ — The chapter number (required).
--
-- @chapterTitle@ — The chapter title (optional).
--
-- Display the provided chapter number and title momentarily.
--
-- ObjC selector: @- flashChapterNumber:chapterTitle:@
flashChapterNumber_chapterTitle :: (IsAVPlayerView avPlayerView, IsNSString chapterTitle) => avPlayerView -> CULong -> chapterTitle -> IO ()
flashChapterNumber_chapterTitle avPlayerView  chapterNumber chapterTitle =
withObjCPtr chapterTitle $ \raw_chapterTitle ->
    sendMsg avPlayerView (mkSelector "flashChapterNumber:chapterTitle:") retVoid [argCULong (fromIntegral chapterNumber), argPtr (castPtr raw_chapterTitle :: Ptr ())]

-- | beginTrimmingWithCompletionHandler:
--
-- @handler@ — A completion handler that is executed when the user selects either the Trim or Cancel button in the trimming UI.
--
-- Sets the controls panel into trimming mode and blocks until the user selects either the Trim or the Cancel button.
--
-- ObjC selector: @- beginTrimmingWithCompletionHandler:@
beginTrimmingWithCompletionHandler :: IsAVPlayerView avPlayerView => avPlayerView -> Ptr () -> IO ()
beginTrimmingWithCompletionHandler avPlayerView  handler =
  sendMsg avPlayerView (mkSelector "beginTrimmingWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | player
--
-- The player from which to source the media content for the view.
--
-- ObjC selector: @- player@
player :: IsAVPlayerView avPlayerView => avPlayerView -> IO (Id AVPlayer)
player avPlayerView  =
  sendMsg avPlayerView (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | player
--
-- The player from which to source the media content for the view.
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsAVPlayerView avPlayerView, IsAVPlayer value) => avPlayerView -> value -> IO ()
setPlayer avPlayerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerView (mkSelector "setPlayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | controlsStyle
--
-- The style of the playback controls pane currently associated with the view.
--
-- After macOS 11, the floating style controls will always be used when presenting in fullscreen and AVPlayerViewControlsStyleNone is not specified.
--
-- ObjC selector: @- controlsStyle@
controlsStyle :: IsAVPlayerView avPlayerView => avPlayerView -> IO AVPlayerViewControlsStyle
controlsStyle avPlayerView  =
  fmap (coerce :: CLong -> AVPlayerViewControlsStyle) $ sendMsg avPlayerView (mkSelector "controlsStyle") retCLong []

-- | controlsStyle
--
-- The style of the playback controls pane currently associated with the view.
--
-- After macOS 11, the floating style controls will always be used when presenting in fullscreen and AVPlayerViewControlsStyleNone is not specified.
--
-- ObjC selector: @- setControlsStyle:@
setControlsStyle :: IsAVPlayerView avPlayerView => avPlayerView -> AVPlayerViewControlsStyle -> IO ()
setControlsStyle avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setControlsStyle:") retVoid [argCLong (coerce value)]

-- | videoGravity
--
-- A string defining how the video is displayed within an AVPlayerLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVPlayerView avPlayerView => avPlayerView -> IO (Id NSString)
videoGravity avPlayerView  =
  sendMsg avPlayerView (mkSelector "videoGravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoGravity
--
-- A string defining how the video is displayed within an AVPlayerLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVPlayerView avPlayerView, IsNSString value) => avPlayerView -> value -> IO ()
setVideoGravity avPlayerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerView (mkSelector "setVideoGravity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | readyForDisplay
--
-- Boolean indicating that the first video frame has been made ready for display for the current item of the associated AVPlayer.
--
-- ObjC selector: @- readyForDisplay@
readyForDisplay :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
readyForDisplay avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "readyForDisplay") retCULong []

-- | videoBounds
--
-- The current size and position of the video image as displayed within the receiver's view's bounds.
--
-- ObjC selector: @- videoBounds@
videoBounds :: IsAVPlayerView avPlayerView => avPlayerView -> IO NSRect
videoBounds avPlayerView  =
  sendMsgStret avPlayerView (mkSelector "videoBounds") retNSRect []

-- | updatesNowPlayingInfoCenter
--
-- Whether or not the now playing info center should be updated. Default is YES.
--
-- ObjC selector: @- updatesNowPlayingInfoCenter@
updatesNowPlayingInfoCenter :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
updatesNowPlayingInfoCenter avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "updatesNowPlayingInfoCenter") retCULong []

-- | updatesNowPlayingInfoCenter
--
-- Whether or not the now playing info center should be updated. Default is YES.
--
-- ObjC selector: @- setUpdatesNowPlayingInfoCenter:@
setUpdatesNowPlayingInfoCenter :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setUpdatesNowPlayingInfoCenter avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setUpdatesNowPlayingInfoCenter:") retVoid [argCULong (if value then 1 else 0)]

-- | allowsVideoFrameAnalysis
--
-- When set to YES, the AVPlayerView will try to find objects, text and people while the media is paused. When an object is found, the user will be able to interact with it selecting and right clicking to present a context menu. Default is YES.
--
-- ObjC selector: @- allowsVideoFrameAnalysis@
allowsVideoFrameAnalysis :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
allowsVideoFrameAnalysis avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "allowsVideoFrameAnalysis") retCULong []

-- | allowsVideoFrameAnalysis
--
-- When set to YES, the AVPlayerView will try to find objects, text and people while the media is paused. When an object is found, the user will be able to interact with it selecting and right clicking to present a context menu. Default is YES.
--
-- ObjC selector: @- setAllowsVideoFrameAnalysis:@
setAllowsVideoFrameAnalysis :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setAllowsVideoFrameAnalysis avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setAllowsVideoFrameAnalysis:") retVoid [argCULong (if value then 1 else 0)]

-- | videoFrameAnalysisTypes
--
-- The types of items AVPlayerView looks for in a paused video frame.
--
-- ObjC selector: @- videoFrameAnalysisTypes@
videoFrameAnalysisTypes :: IsAVPlayerView avPlayerView => avPlayerView -> IO AVVideoFrameAnalysisType
videoFrameAnalysisTypes avPlayerView  =
  fmap (coerce :: CULong -> AVVideoFrameAnalysisType) $ sendMsg avPlayerView (mkSelector "videoFrameAnalysisTypes") retCULong []

-- | videoFrameAnalysisTypes
--
-- The types of items AVPlayerView looks for in a paused video frame.
--
-- ObjC selector: @- setVideoFrameAnalysisTypes:@
setVideoFrameAnalysisTypes :: IsAVPlayerView avPlayerView => avPlayerView -> AVVideoFrameAnalysisType -> IO ()
setVideoFrameAnalysisTypes avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setVideoFrameAnalysisTypes:") retVoid [argCULong (coerce value)]

-- | allowsMagnification
--
-- Whether the magnify gesture will change the video's view magnification.
--
-- The default value is NO. This property only effects whether the magnify gesture triggers magnification. A client can still programmatically change magnification even when the value of this is NO. This behavior matches the behavior of NSScrollView.
--
-- ObjC selector: @- allowsMagnification@
allowsMagnification :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
allowsMagnification avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "allowsMagnification") retCULong []

-- | allowsMagnification
--
-- Whether the magnify gesture will change the video's view magnification.
--
-- The default value is NO. This property only effects whether the magnify gesture triggers magnification. A client can still programmatically change magnification even when the value of this is NO. This behavior matches the behavior of NSScrollView.
--
-- ObjC selector: @- setAllowsMagnification:@
setAllowsMagnification :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setAllowsMagnification avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setAllowsMagnification:") retVoid [argCULong (if value then 1 else 0)]

-- | magnification
--
-- The factor by which the video's view is currently scaled.
--
-- The default value is 1.0. The value cannot be smaller than 1.0 or larger 64.0. Nearest neighbor interpolation will be used once the content has been zoomed past a certain factor.
--
-- ObjC selector: @- magnification@
magnification :: IsAVPlayerView avPlayerView => avPlayerView -> IO CDouble
magnification avPlayerView  =
  sendMsg avPlayerView (mkSelector "magnification") retCDouble []

-- | magnification
--
-- The factor by which the video's view is currently scaled.
--
-- The default value is 1.0. The value cannot be smaller than 1.0 or larger 64.0. Nearest neighbor interpolation will be used once the content has been zoomed past a certain factor.
--
-- ObjC selector: @- setMagnification:@
setMagnification :: IsAVPlayerView avPlayerView => avPlayerView -> CDouble -> IO ()
setMagnification avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setMagnification:") retVoid [argCDouble (fromIntegral value)]

-- | Describes how High Dynamic Range (HDR) video content renders.
--
-- Defaults to ``AVDisplayDynamicRangeAutomatic``.
--
-- - Note: This property will only have effect if the video content supports HDR.
--
-- ObjC selector: @- preferredDisplayDynamicRange@
preferredDisplayDynamicRange :: IsAVPlayerView avPlayerView => avPlayerView -> IO AVDisplayDynamicRange
preferredDisplayDynamicRange avPlayerView  =
  fmap (coerce :: CLong -> AVDisplayDynamicRange) $ sendMsg avPlayerView (mkSelector "preferredDisplayDynamicRange") retCLong []

-- | Describes how High Dynamic Range (HDR) video content renders.
--
-- Defaults to ``AVDisplayDynamicRangeAutomatic``.
--
-- - Note: This property will only have effect if the video content supports HDR.
--
-- ObjC selector: @- setPreferredDisplayDynamicRange:@
setPreferredDisplayDynamicRange :: IsAVPlayerView avPlayerView => avPlayerView -> AVDisplayDynamicRange -> IO ()
setPreferredDisplayDynamicRange avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setPreferredDisplayDynamicRange:") retVoid [argCLong (coerce value)]

-- | allowsPictureInPicturePlayback
--
-- Whether or not the receiver allows Picture in Picture playback. Default is NO.
--
-- ObjC selector: @- allowsPictureInPicturePlayback@
allowsPictureInPicturePlayback :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
allowsPictureInPicturePlayback avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "allowsPictureInPicturePlayback") retCULong []

-- | allowsPictureInPicturePlayback
--
-- Whether or not the receiver allows Picture in Picture playback. Default is NO.
--
-- ObjC selector: @- setAllowsPictureInPicturePlayback:@
setAllowsPictureInPicturePlayback :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setAllowsPictureInPicturePlayback avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setAllowsPictureInPicturePlayback:") retVoid [argCULong (if value then 1 else 0)]

-- | canBeginTrimming
--
-- Whether or not the current media can be trimmed.
--
-- ObjC selector: @- canBeginTrimming@
canBeginTrimming :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
canBeginTrimming avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "canBeginTrimming") retCULong []

-- | showsFrameSteppingButtons
--
-- Replace scanning controls in the playback UI with frame stepping buttons. Default is NO.
--
-- ObjC selector: @- showsFrameSteppingButtons@
showsFrameSteppingButtons :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
showsFrameSteppingButtons avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "showsFrameSteppingButtons") retCULong []

-- | showsFrameSteppingButtons
--
-- Replace scanning controls in the playback UI with frame stepping buttons. Default is NO.
--
-- ObjC selector: @- setShowsFrameSteppingButtons:@
setShowsFrameSteppingButtons :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setShowsFrameSteppingButtons avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setShowsFrameSteppingButtons:") retVoid [argCULong (if value then 1 else 0)]

-- | showsSharingServiceButton
--
-- Whether or not the controls pane will show a sharing service button when the current player item can be shared. Default is NO.
--
-- ObjC selector: @- showsSharingServiceButton@
showsSharingServiceButton :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
showsSharingServiceButton avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "showsSharingServiceButton") retCULong []

-- | showsSharingServiceButton
--
-- Whether or not the controls pane will show a sharing service button when the current player item can be shared. Default is NO.
--
-- ObjC selector: @- setShowsSharingServiceButton:@
setShowsSharingServiceButton :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setShowsSharingServiceButton avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setShowsSharingServiceButton:") retVoid [argCULong (if value then 1 else 0)]

-- | actionPopUpButtonMenu
--
-- Clients can set this property in order to show an action pop up button. Default is nil.
--
-- ObjC selector: @- actionPopUpButtonMenu@
actionPopUpButtonMenu :: IsAVPlayerView avPlayerView => avPlayerView -> IO (Id NSMenu)
actionPopUpButtonMenu avPlayerView  =
  sendMsg avPlayerView (mkSelector "actionPopUpButtonMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | actionPopUpButtonMenu
--
-- Clients can set this property in order to show an action pop up button. Default is nil.
--
-- ObjC selector: @- setActionPopUpButtonMenu:@
setActionPopUpButtonMenu :: (IsAVPlayerView avPlayerView, IsNSMenu value) => avPlayerView -> value -> IO ()
setActionPopUpButtonMenu avPlayerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerView (mkSelector "setActionPopUpButtonMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | showsFullScreenToggleButton
--
-- Whether or not the controls pane will show a full screen toggle button. Default is NO.
--
-- ObjC selector: @- showsFullScreenToggleButton@
showsFullScreenToggleButton :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
showsFullScreenToggleButton avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "showsFullScreenToggleButton") retCULong []

-- | showsFullScreenToggleButton
--
-- Whether or not the controls pane will show a full screen toggle button. Default is NO.
--
-- ObjC selector: @- setShowsFullScreenToggleButton:@
setShowsFullScreenToggleButton :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setShowsFullScreenToggleButton avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setShowsFullScreenToggleButton:") retVoid [argCULong (if value then 1 else 0)]

-- | showsTimecodes
--
-- If timecodes are available, allow the AVPlayerView controls to enter timecode mode. Default is NO.
--
-- ObjC selector: @- showsTimecodes@
showsTimecodes :: IsAVPlayerView avPlayerView => avPlayerView -> IO Bool
showsTimecodes avPlayerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerView (mkSelector "showsTimecodes") retCULong []

-- | showsTimecodes
--
-- If timecodes are available, allow the AVPlayerView controls to enter timecode mode. Default is NO.
--
-- ObjC selector: @- setShowsTimecodes:@
setShowsTimecodes :: IsAVPlayerView avPlayerView => avPlayerView -> Bool -> IO ()
setShowsTimecodes avPlayerView  value =
  sendMsg avPlayerView (mkSelector "setShowsTimecodes:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectSpeed:@
selectSpeedSelector :: Selector
selectSpeedSelector = mkSelector "selectSpeed:"

-- | @Selector@ for @flashChapterNumber:chapterTitle:@
flashChapterNumber_chapterTitleSelector :: Selector
flashChapterNumber_chapterTitleSelector = mkSelector "flashChapterNumber:chapterTitle:"

-- | @Selector@ for @beginTrimmingWithCompletionHandler:@
beginTrimmingWithCompletionHandlerSelector :: Selector
beginTrimmingWithCompletionHandlerSelector = mkSelector "beginTrimmingWithCompletionHandler:"

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @controlsStyle@
controlsStyleSelector :: Selector
controlsStyleSelector = mkSelector "controlsStyle"

-- | @Selector@ for @setControlsStyle:@
setControlsStyleSelector :: Selector
setControlsStyleSelector = mkSelector "setControlsStyle:"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector
setVideoGravitySelector = mkSelector "setVideoGravity:"

-- | @Selector@ for @readyForDisplay@
readyForDisplaySelector :: Selector
readyForDisplaySelector = mkSelector "readyForDisplay"

-- | @Selector@ for @videoBounds@
videoBoundsSelector :: Selector
videoBoundsSelector = mkSelector "videoBounds"

-- | @Selector@ for @updatesNowPlayingInfoCenter@
updatesNowPlayingInfoCenterSelector :: Selector
updatesNowPlayingInfoCenterSelector = mkSelector "updatesNowPlayingInfoCenter"

-- | @Selector@ for @setUpdatesNowPlayingInfoCenter:@
setUpdatesNowPlayingInfoCenterSelector :: Selector
setUpdatesNowPlayingInfoCenterSelector = mkSelector "setUpdatesNowPlayingInfoCenter:"

-- | @Selector@ for @allowsVideoFrameAnalysis@
allowsVideoFrameAnalysisSelector :: Selector
allowsVideoFrameAnalysisSelector = mkSelector "allowsVideoFrameAnalysis"

-- | @Selector@ for @setAllowsVideoFrameAnalysis:@
setAllowsVideoFrameAnalysisSelector :: Selector
setAllowsVideoFrameAnalysisSelector = mkSelector "setAllowsVideoFrameAnalysis:"

-- | @Selector@ for @videoFrameAnalysisTypes@
videoFrameAnalysisTypesSelector :: Selector
videoFrameAnalysisTypesSelector = mkSelector "videoFrameAnalysisTypes"

-- | @Selector@ for @setVideoFrameAnalysisTypes:@
setVideoFrameAnalysisTypesSelector :: Selector
setVideoFrameAnalysisTypesSelector = mkSelector "setVideoFrameAnalysisTypes:"

-- | @Selector@ for @allowsMagnification@
allowsMagnificationSelector :: Selector
allowsMagnificationSelector = mkSelector "allowsMagnification"

-- | @Selector@ for @setAllowsMagnification:@
setAllowsMagnificationSelector :: Selector
setAllowsMagnificationSelector = mkSelector "setAllowsMagnification:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector
setMagnificationSelector = mkSelector "setMagnification:"

-- | @Selector@ for @preferredDisplayDynamicRange@
preferredDisplayDynamicRangeSelector :: Selector
preferredDisplayDynamicRangeSelector = mkSelector "preferredDisplayDynamicRange"

-- | @Selector@ for @setPreferredDisplayDynamicRange:@
setPreferredDisplayDynamicRangeSelector :: Selector
setPreferredDisplayDynamicRangeSelector = mkSelector "setPreferredDisplayDynamicRange:"

-- | @Selector@ for @allowsPictureInPicturePlayback@
allowsPictureInPicturePlaybackSelector :: Selector
allowsPictureInPicturePlaybackSelector = mkSelector "allowsPictureInPicturePlayback"

-- | @Selector@ for @setAllowsPictureInPicturePlayback:@
setAllowsPictureInPicturePlaybackSelector :: Selector
setAllowsPictureInPicturePlaybackSelector = mkSelector "setAllowsPictureInPicturePlayback:"

-- | @Selector@ for @canBeginTrimming@
canBeginTrimmingSelector :: Selector
canBeginTrimmingSelector = mkSelector "canBeginTrimming"

-- | @Selector@ for @showsFrameSteppingButtons@
showsFrameSteppingButtonsSelector :: Selector
showsFrameSteppingButtonsSelector = mkSelector "showsFrameSteppingButtons"

-- | @Selector@ for @setShowsFrameSteppingButtons:@
setShowsFrameSteppingButtonsSelector :: Selector
setShowsFrameSteppingButtonsSelector = mkSelector "setShowsFrameSteppingButtons:"

-- | @Selector@ for @showsSharingServiceButton@
showsSharingServiceButtonSelector :: Selector
showsSharingServiceButtonSelector = mkSelector "showsSharingServiceButton"

-- | @Selector@ for @setShowsSharingServiceButton:@
setShowsSharingServiceButtonSelector :: Selector
setShowsSharingServiceButtonSelector = mkSelector "setShowsSharingServiceButton:"

-- | @Selector@ for @actionPopUpButtonMenu@
actionPopUpButtonMenuSelector :: Selector
actionPopUpButtonMenuSelector = mkSelector "actionPopUpButtonMenu"

-- | @Selector@ for @setActionPopUpButtonMenu:@
setActionPopUpButtonMenuSelector :: Selector
setActionPopUpButtonMenuSelector = mkSelector "setActionPopUpButtonMenu:"

-- | @Selector@ for @showsFullScreenToggleButton@
showsFullScreenToggleButtonSelector :: Selector
showsFullScreenToggleButtonSelector = mkSelector "showsFullScreenToggleButton"

-- | @Selector@ for @setShowsFullScreenToggleButton:@
setShowsFullScreenToggleButtonSelector :: Selector
setShowsFullScreenToggleButtonSelector = mkSelector "setShowsFullScreenToggleButton:"

-- | @Selector@ for @showsTimecodes@
showsTimecodesSelector :: Selector
showsTimecodesSelector = mkSelector "showsTimecodes"

-- | @Selector@ for @setShowsTimecodes:@
setShowsTimecodesSelector :: Selector
setShowsTimecodesSelector = mkSelector "setShowsTimecodes:"

