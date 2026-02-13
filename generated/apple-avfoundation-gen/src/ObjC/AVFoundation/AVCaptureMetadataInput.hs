{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureMetadataInput
--
-- AVCaptureMetadataInput is a concrete subclass of AVCaptureInput that provides a way for clients to supply AVMetadataItems to an AVCaptureSession.
--
-- Instances of AVCaptureMetadataInput are input sources for AVCaptureSession that provide AVMetadataItems to an AVCaptureSession. AVCaptureMetadataInputs present one and only one AVCaptureInputPort, which currently may only be connected to an AVCaptureMovieFileOutput. The metadata supplied over the input port is provided by the client, and must conform to a client-supplied CMFormatDescription. The AVMetadataItems are supplied in an AVTimedMetadataGroup.
--
-- Generated bindings for @AVCaptureMetadataInput@.
module ObjC.AVFoundation.AVCaptureMetadataInput
  ( AVCaptureMetadataInput
  , IsAVCaptureMetadataInput(..)
  , metadataInputWithFormatDescription_clock
  , initWithFormatDescription_clock
  , appendTimedMetadataGroup_error
  , appendTimedMetadataGroup_errorSelector
  , initWithFormatDescription_clockSelector
  , metadataInputWithFormatDescription_clockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | metadataInputWithFormatDescription:clock:
--
-- Returns an AVCaptureMetadataInput instance that allows a client to provide AVTimedMetadataGroups to an AVCaptureSession.
--
-- @desc@ — A CMFormatDescription that defines the metadata to be supplied by the client. Throws an NSInvalidArgumentException if NULL is passed.
--
-- @clock@ — A CMClock that provided the timebase for the supplied samples. Throws an NSInvalidArgumentException if NULL is passed.
--
-- Returns: An AVCaptureMetadataInput instance.
--
-- This method returns an instance of AVCaptureMetadataInput that can be used to capture AVTimedMetadataGroups supplied by the client to an AVCaptureSession.
--
-- ObjC selector: @+ metadataInputWithFormatDescription:clock:@
metadataInputWithFormatDescription_clock :: RawId -> Ptr () -> IO (Id AVCaptureMetadataInput)
metadataInputWithFormatDescription_clock desc clock =
  do
    cls' <- getRequiredClass "AVCaptureMetadataInput"
    sendClassMessage cls' metadataInputWithFormatDescription_clockSelector desc clock

-- | initWithFormatDescription:clock:
--
-- Creates an AVCaptureMetadataInput instance that allows a client to provide AVTimedMetadataGroups to an AVCaptureSession.
--
-- @desc@ — A CMFormatDescription that defines the metadata to be supplied by the client. Throws NSInvalidArgumentException if NULL is passed.
--
-- @clock@ — A CMClock that provided the timebase for the supplied samples. Throws NSInvalidArgumentException if NULL is passed.
--
-- Returns: An AVCaptureMetadataInput instance, or nil, if the device could not be used for capture.
--
-- This method creates an instance of AVCaptureMetadataInput that can be used to capture AVTimedMetadataGroups supplied by the client to an AVCaptureSession.
--
-- ObjC selector: @- initWithFormatDescription:clock:@
initWithFormatDescription_clock :: IsAVCaptureMetadataInput avCaptureMetadataInput => avCaptureMetadataInput -> RawId -> Ptr () -> IO (Id AVCaptureMetadataInput)
initWithFormatDescription_clock avCaptureMetadataInput desc clock =
  sendOwnedMessage avCaptureMetadataInput initWithFormatDescription_clockSelector desc clock

-- | appendTimedMetadataGroup:
--
-- Provides metadata to the AVCaptureSession.
--
-- @metadata@ — An AVTimedMetadataGroup of metadata. Will throw an exception if nil. In order to denote a period of no metadata, an empty AVTimedMetadataGroup should be passed.
--
-- The provided AVTimedMetadataGroup will be provided to the AVCaptureSession. The group's presentation timestamp is expressed in the context of the clock supplied to the initializer. It is not required that the AVTimedMetadataGroup have a duration; an empty AVTimedMetadataGroup can be supplied to denote a period of no metadata.
--
-- ObjC selector: @- appendTimedMetadataGroup:error:@
appendTimedMetadataGroup_error :: (IsAVCaptureMetadataInput avCaptureMetadataInput, IsAVTimedMetadataGroup metadata, IsNSError outError) => avCaptureMetadataInput -> metadata -> outError -> IO Bool
appendTimedMetadataGroup_error avCaptureMetadataInput metadata outError =
  sendMessage avCaptureMetadataInput appendTimedMetadataGroup_errorSelector (toAVTimedMetadataGroup metadata) (toNSError outError)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataInputWithFormatDescription:clock:@
metadataInputWithFormatDescription_clockSelector :: Selector '[RawId, Ptr ()] (Id AVCaptureMetadataInput)
metadataInputWithFormatDescription_clockSelector = mkSelector "metadataInputWithFormatDescription:clock:"

-- | @Selector@ for @initWithFormatDescription:clock:@
initWithFormatDescription_clockSelector :: Selector '[RawId, Ptr ()] (Id AVCaptureMetadataInput)
initWithFormatDescription_clockSelector = mkSelector "initWithFormatDescription:clock:"

-- | @Selector@ for @appendTimedMetadataGroup:error:@
appendTimedMetadataGroup_errorSelector :: Selector '[Id AVTimedMetadataGroup, Id NSError] Bool
appendTimedMetadataGroup_errorSelector = mkSelector "appendTimedMetadataGroup:error:"

