{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSearchableItemAttributeSet@.
module ObjC.CoreSpotlight.CSSearchableItemAttributeSet
  ( CSSearchableItemAttributeSet
  , IsCSSearchableItemAttributeSet(..)
  , initWithItemContentType
  , initWithContentType
  , moveFrom
  , setValue_forCustomKey
  , valueForCustomKey
  , headline
  , setHeadline
  , instructions
  , setInstructions
  , thoroughfare
  , setThoroughfare
  , subThoroughfare
  , setSubThoroughfare
  , postalCode
  , setPostalCode
  , city
  , setCity
  , stateOrProvince
  , setStateOrProvince
  , country
  , setCountry
  , fullyFormattedAddress
  , setFullyFormattedAddress
  , altitude
  , setAltitude
  , latitude
  , setLatitude
  , longitude
  , setLongitude
  , speed
  , setSpeed
  , timestamp
  , setTimestamp
  , imageDirection
  , setImageDirection
  , namedLocation
  , setNamedLocation
  , gpsTrack
  , setGPSTrack
  , gpsStatus
  , setGPSStatus
  , gpsMeasureMode
  , setGPSMeasureMode
  , gpsdop
  , setGPSDOP
  , gpsMapDatum
  , setGPSMapDatum
  , gpsDestLatitude
  , setGPSDestLatitude
  , gpsDestLongitude
  , setGPSDestLongitude
  , gpsDestBearing
  , setGPSDestBearing
  , gpsDestDistance
  , setGPSDestDistance
  , gpsProcessingMethod
  , setGPSProcessingMethod
  , gpsAreaInformation
  , setGPSAreaInformation
  , gpsDateStamp
  , setGPSDateStamp
  , gpsDifferental
  , setGPSDifferental
  , pixelHeight
  , setPixelHeight
  , pixelWidth
  , setPixelWidth
  , pixelCount
  , setPixelCount
  , colorSpace
  , setColorSpace
  , bitsPerSample
  , setBitsPerSample
  , flashOn
  , setFlashOn
  , focalLength
  , setFocalLength
  , focalLength35mm
  , setFocalLength35mm
  , acquisitionMake
  , setAcquisitionMake
  , acquisitionModel
  , setAcquisitionModel
  , cameraOwner
  , setCameraOwner
  , lensModel
  , setLensModel
  , isoSpeed
  , setISOSpeed
  , orientation
  , setOrientation
  , layerNames
  , setLayerNames
  , whiteBalance
  , setWhiteBalance
  , aperture
  , setAperture
  , profileName
  , setProfileName
  , resolutionWidthDPI
  , setResolutionWidthDPI
  , resolutionHeightDPI
  , setResolutionHeightDPI
  , exposureMode
  , setExposureMode
  , exposureTime
  , setExposureTime
  , exifVersion
  , setEXIFVersion
  , exifgpsVersion
  , setEXIFGPSVersion
  , hasAlphaChannel
  , setHasAlphaChannel
  , redEyeOn
  , setRedEyeOn
  , meteringMode
  , setMeteringMode
  , maxAperture
  , setMaxAperture
  , fNumber
  , setFNumber
  , exposureProgram
  , setExposureProgram
  , exposureTimeString
  , setExposureTimeString
  , audioSampleRate
  , setAudioSampleRate
  , audioChannelCount
  , setAudioChannelCount
  , tempo
  , setTempo
  , keySignature
  , setKeySignature
  , timeSignature
  , setTimeSignature
  , audioEncodingApplication
  , setAudioEncodingApplication
  , composer
  , setComposer
  , lyricist
  , setLyricist
  , album
  , setAlbum
  , artist
  , setArtist
  , audioTrackNumber
  , setAudioTrackNumber
  , recordingDate
  , setRecordingDate
  , musicalGenre
  , setMusicalGenre
  , generalMIDISequence
  , setGeneralMIDISequence
  , musicalInstrumentCategory
  , setMusicalInstrumentCategory
  , musicalInstrumentName
  , setMusicalInstrumentName
  , editors
  , setEditors
  , participants
  , setParticipants
  , projects
  , setProjects
  , downloadedDate
  , setDownloadedDate
  , contentSources
  , setContentSources
  , comment
  , setComment
  , copyright
  , setCopyright
  , lastUsedDate
  , setLastUsedDate
  , contentCreationDate
  , setContentCreationDate
  , contentModificationDate
  , setContentModificationDate
  , addedDate
  , setAddedDate
  , duration
  , setDuration
  , contactKeywords
  , setContactKeywords
  , codecs
  , setCodecs
  , mediaTypes
  , setMediaTypes
  , streamable
  , setStreamable
  , totalBitRate
  , setTotalBitRate
  , videoBitRate
  , setVideoBitRate
  , audioBitRate
  , setAudioBitRate
  , deliveryType
  , setDeliveryType
  , organizations
  , setOrganizations
  , role_
  , setRole
  , languages
  , setLanguages
  , rights
  , setRights
  , publishers
  , setPublishers
  , contributors
  , setContributors
  , coverage
  , setCoverage
  , rating
  , setRating
  , ratingDescription
  , setRatingDescription
  , playCount
  , setPlayCount
  , information
  , setInformation
  , director
  , setDirector
  , producer
  , setProducer
  , genre
  , setGenre
  , performers
  , setPerformers
  , originalFormat
  , setOriginalFormat
  , originalSource
  , setOriginalSource
  , local
  , setLocal
  , contentRating
  , setContentRating
  , url
  , setURL
  , accountIdentifier
  , setAccountIdentifier
  , accountHandles
  , setAccountHandles
  , htmlContentData
  , setHTMLContentData
  , textContent
  , setTextContent
  , authors
  , setAuthors
  , primaryRecipients
  , setPrimaryRecipients
  , additionalRecipients
  , setAdditionalRecipients
  , hiddenAdditionalRecipients
  , setHiddenAdditionalRecipients
  , emailHeaders
  , setEmailHeaders
  , mailboxIdentifiers
  , setMailboxIdentifiers
  , authorNames
  , setAuthorNames
  , recipientNames
  , setRecipientNames
  , authorEmailAddresses
  , setAuthorEmailAddresses
  , recipientEmailAddresses
  , setRecipientEmailAddresses
  , authorAddresses
  , setAuthorAddresses
  , recipientAddresses
  , setRecipientAddresses
  , phoneNumbers
  , setPhoneNumbers
  , emailAddresses
  , setEmailAddresses
  , instantMessageAddresses
  , setInstantMessageAddresses
  , likelyJunk
  , setLikelyJunk
  , dueDate
  , setDueDate
  , completionDate
  , setCompletionDate
  , startDate
  , setStartDate
  , endDate
  , setEndDate
  , importantDates
  , setImportantDates
  , allDay
  , setAllDay
  , subject
  , setSubject
  , theme
  , setTheme
  , contentDescription
  , setContentDescription
  , identifier
  , setIdentifier
  , audiences
  , setAudiences
  , fileSize
  , setFileSize
  , pageCount
  , setPageCount
  , pageWidth
  , setPageWidth
  , pageHeight
  , setPageHeight
  , securityMethod
  , setSecurityMethod
  , creator
  , setCreator
  , encodingApplications
  , setEncodingApplications
  , kind
  , setKind
  , fontNames
  , setFontNames
  , containerTitle
  , setContainerTitle
  , containerDisplayName
  , setContainerDisplayName
  , containerIdentifier
  , setContainerIdentifier
  , containerOrder
  , setContainerOrder
  , supportsPhoneCall
  , setSupportsPhoneCall
  , supportsNavigation
  , setSupportsNavigation
  , displayName
  , setDisplayName
  , alternateNames
  , setAlternateNames
  , path
  , setPath
  , contentURL
  , setContentURL
  , thumbnailURL
  , setThumbnailURL
  , thumbnailData
  , setThumbnailData
  , darkThumbnailURL
  , setDarkThumbnailURL
  , relatedUniqueIdentifier
  , setRelatedUniqueIdentifier
  , metadataModificationDate
  , setMetadataModificationDate
  , contentType
  , setContentType
  , contentTypeTree
  , setContentTypeTree
  , keywords
  , setKeywords
  , title
  , setTitle
  , version
  , setVersion
  , initWithItemContentTypeSelector
  , initWithContentTypeSelector
  , moveFromSelector
  , setValue_forCustomKeySelector
  , valueForCustomKeySelector
  , headlineSelector
  , setHeadlineSelector
  , instructionsSelector
  , setInstructionsSelector
  , thoroughfareSelector
  , setThoroughfareSelector
  , subThoroughfareSelector
  , setSubThoroughfareSelector
  , postalCodeSelector
  , setPostalCodeSelector
  , citySelector
  , setCitySelector
  , stateOrProvinceSelector
  , setStateOrProvinceSelector
  , countrySelector
  , setCountrySelector
  , fullyFormattedAddressSelector
  , setFullyFormattedAddressSelector
  , altitudeSelector
  , setAltitudeSelector
  , latitudeSelector
  , setLatitudeSelector
  , longitudeSelector
  , setLongitudeSelector
  , speedSelector
  , setSpeedSelector
  , timestampSelector
  , setTimestampSelector
  , imageDirectionSelector
  , setImageDirectionSelector
  , namedLocationSelector
  , setNamedLocationSelector
  , gpsTrackSelector
  , setGPSTrackSelector
  , gpsStatusSelector
  , setGPSStatusSelector
  , gpsMeasureModeSelector
  , setGPSMeasureModeSelector
  , gpsdopSelector
  , setGPSDOPSelector
  , gpsMapDatumSelector
  , setGPSMapDatumSelector
  , gpsDestLatitudeSelector
  , setGPSDestLatitudeSelector
  , gpsDestLongitudeSelector
  , setGPSDestLongitudeSelector
  , gpsDestBearingSelector
  , setGPSDestBearingSelector
  , gpsDestDistanceSelector
  , setGPSDestDistanceSelector
  , gpsProcessingMethodSelector
  , setGPSProcessingMethodSelector
  , gpsAreaInformationSelector
  , setGPSAreaInformationSelector
  , gpsDateStampSelector
  , setGPSDateStampSelector
  , gpsDifferentalSelector
  , setGPSDifferentalSelector
  , pixelHeightSelector
  , setPixelHeightSelector
  , pixelWidthSelector
  , setPixelWidthSelector
  , pixelCountSelector
  , setPixelCountSelector
  , colorSpaceSelector
  , setColorSpaceSelector
  , bitsPerSampleSelector
  , setBitsPerSampleSelector
  , flashOnSelector
  , setFlashOnSelector
  , focalLengthSelector
  , setFocalLengthSelector
  , focalLength35mmSelector
  , setFocalLength35mmSelector
  , acquisitionMakeSelector
  , setAcquisitionMakeSelector
  , acquisitionModelSelector
  , setAcquisitionModelSelector
  , cameraOwnerSelector
  , setCameraOwnerSelector
  , lensModelSelector
  , setLensModelSelector
  , isoSpeedSelector
  , setISOSpeedSelector
  , orientationSelector
  , setOrientationSelector
  , layerNamesSelector
  , setLayerNamesSelector
  , whiteBalanceSelector
  , setWhiteBalanceSelector
  , apertureSelector
  , setApertureSelector
  , profileNameSelector
  , setProfileNameSelector
  , resolutionWidthDPISelector
  , setResolutionWidthDPISelector
  , resolutionHeightDPISelector
  , setResolutionHeightDPISelector
  , exposureModeSelector
  , setExposureModeSelector
  , exposureTimeSelector
  , setExposureTimeSelector
  , exifVersionSelector
  , setEXIFVersionSelector
  , exifgpsVersionSelector
  , setEXIFGPSVersionSelector
  , hasAlphaChannelSelector
  , setHasAlphaChannelSelector
  , redEyeOnSelector
  , setRedEyeOnSelector
  , meteringModeSelector
  , setMeteringModeSelector
  , maxApertureSelector
  , setMaxApertureSelector
  , fNumberSelector
  , setFNumberSelector
  , exposureProgramSelector
  , setExposureProgramSelector
  , exposureTimeStringSelector
  , setExposureTimeStringSelector
  , audioSampleRateSelector
  , setAudioSampleRateSelector
  , audioChannelCountSelector
  , setAudioChannelCountSelector
  , tempoSelector
  , setTempoSelector
  , keySignatureSelector
  , setKeySignatureSelector
  , timeSignatureSelector
  , setTimeSignatureSelector
  , audioEncodingApplicationSelector
  , setAudioEncodingApplicationSelector
  , composerSelector
  , setComposerSelector
  , lyricistSelector
  , setLyricistSelector
  , albumSelector
  , setAlbumSelector
  , artistSelector
  , setArtistSelector
  , audioTrackNumberSelector
  , setAudioTrackNumberSelector
  , recordingDateSelector
  , setRecordingDateSelector
  , musicalGenreSelector
  , setMusicalGenreSelector
  , generalMIDISequenceSelector
  , setGeneralMIDISequenceSelector
  , musicalInstrumentCategorySelector
  , setMusicalInstrumentCategorySelector
  , musicalInstrumentNameSelector
  , setMusicalInstrumentNameSelector
  , editorsSelector
  , setEditorsSelector
  , participantsSelector
  , setParticipantsSelector
  , projectsSelector
  , setProjectsSelector
  , downloadedDateSelector
  , setDownloadedDateSelector
  , contentSourcesSelector
  , setContentSourcesSelector
  , commentSelector
  , setCommentSelector
  , copyrightSelector
  , setCopyrightSelector
  , lastUsedDateSelector
  , setLastUsedDateSelector
  , contentCreationDateSelector
  , setContentCreationDateSelector
  , contentModificationDateSelector
  , setContentModificationDateSelector
  , addedDateSelector
  , setAddedDateSelector
  , durationSelector
  , setDurationSelector
  , contactKeywordsSelector
  , setContactKeywordsSelector
  , codecsSelector
  , setCodecsSelector
  , mediaTypesSelector
  , setMediaTypesSelector
  , streamableSelector
  , setStreamableSelector
  , totalBitRateSelector
  , setTotalBitRateSelector
  , videoBitRateSelector
  , setVideoBitRateSelector
  , audioBitRateSelector
  , setAudioBitRateSelector
  , deliveryTypeSelector
  , setDeliveryTypeSelector
  , organizationsSelector
  , setOrganizationsSelector
  , roleSelector
  , setRoleSelector
  , languagesSelector
  , setLanguagesSelector
  , rightsSelector
  , setRightsSelector
  , publishersSelector
  , setPublishersSelector
  , contributorsSelector
  , setContributorsSelector
  , coverageSelector
  , setCoverageSelector
  , ratingSelector
  , setRatingSelector
  , ratingDescriptionSelector
  , setRatingDescriptionSelector
  , playCountSelector
  , setPlayCountSelector
  , informationSelector
  , setInformationSelector
  , directorSelector
  , setDirectorSelector
  , producerSelector
  , setProducerSelector
  , genreSelector
  , setGenreSelector
  , performersSelector
  , setPerformersSelector
  , originalFormatSelector
  , setOriginalFormatSelector
  , originalSourceSelector
  , setOriginalSourceSelector
  , localSelector
  , setLocalSelector
  , contentRatingSelector
  , setContentRatingSelector
  , urlSelector
  , setURLSelector
  , accountIdentifierSelector
  , setAccountIdentifierSelector
  , accountHandlesSelector
  , setAccountHandlesSelector
  , htmlContentDataSelector
  , setHTMLContentDataSelector
  , textContentSelector
  , setTextContentSelector
  , authorsSelector
  , setAuthorsSelector
  , primaryRecipientsSelector
  , setPrimaryRecipientsSelector
  , additionalRecipientsSelector
  , setAdditionalRecipientsSelector
  , hiddenAdditionalRecipientsSelector
  , setHiddenAdditionalRecipientsSelector
  , emailHeadersSelector
  , setEmailHeadersSelector
  , mailboxIdentifiersSelector
  , setMailboxIdentifiersSelector
  , authorNamesSelector
  , setAuthorNamesSelector
  , recipientNamesSelector
  , setRecipientNamesSelector
  , authorEmailAddressesSelector
  , setAuthorEmailAddressesSelector
  , recipientEmailAddressesSelector
  , setRecipientEmailAddressesSelector
  , authorAddressesSelector
  , setAuthorAddressesSelector
  , recipientAddressesSelector
  , setRecipientAddressesSelector
  , phoneNumbersSelector
  , setPhoneNumbersSelector
  , emailAddressesSelector
  , setEmailAddressesSelector
  , instantMessageAddressesSelector
  , setInstantMessageAddressesSelector
  , likelyJunkSelector
  , setLikelyJunkSelector
  , dueDateSelector
  , setDueDateSelector
  , completionDateSelector
  , setCompletionDateSelector
  , startDateSelector
  , setStartDateSelector
  , endDateSelector
  , setEndDateSelector
  , importantDatesSelector
  , setImportantDatesSelector
  , allDaySelector
  , setAllDaySelector
  , subjectSelector
  , setSubjectSelector
  , themeSelector
  , setThemeSelector
  , contentDescriptionSelector
  , setContentDescriptionSelector
  , identifierSelector
  , setIdentifierSelector
  , audiencesSelector
  , setAudiencesSelector
  , fileSizeSelector
  , setFileSizeSelector
  , pageCountSelector
  , setPageCountSelector
  , pageWidthSelector
  , setPageWidthSelector
  , pageHeightSelector
  , setPageHeightSelector
  , securityMethodSelector
  , setSecurityMethodSelector
  , creatorSelector
  , setCreatorSelector
  , encodingApplicationsSelector
  , setEncodingApplicationsSelector
  , kindSelector
  , setKindSelector
  , fontNamesSelector
  , setFontNamesSelector
  , containerTitleSelector
  , setContainerTitleSelector
  , containerDisplayNameSelector
  , setContainerDisplayNameSelector
  , containerIdentifierSelector
  , setContainerIdentifierSelector
  , containerOrderSelector
  , setContainerOrderSelector
  , supportsPhoneCallSelector
  , setSupportsPhoneCallSelector
  , supportsNavigationSelector
  , setSupportsNavigationSelector
  , displayNameSelector
  , setDisplayNameSelector
  , alternateNamesSelector
  , setAlternateNamesSelector
  , pathSelector
  , setPathSelector
  , contentURLSelector
  , setContentURLSelector
  , thumbnailURLSelector
  , setThumbnailURLSelector
  , thumbnailDataSelector
  , setThumbnailDataSelector
  , darkThumbnailURLSelector
  , setDarkThumbnailURLSelector
  , relatedUniqueIdentifierSelector
  , setRelatedUniqueIdentifierSelector
  , metadataModificationDateSelector
  , setMetadataModificationDateSelector
  , contentTypeSelector
  , setContentTypeSelector
  , contentTypeTreeSelector
  , setContentTypeTreeSelector
  , keywordsSelector
  , setKeywordsSelector
  , titleSelector
  , setTitleSelector
  , versionSelector
  , setVersionSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- initWithItemContentType:@
initWithItemContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString itemContentType) => csSearchableItemAttributeSet -> itemContentType -> IO (Id CSSearchableItemAttributeSet)
initWithItemContentType csSearchableItemAttributeSet  itemContentType =
withObjCPtr itemContentType $ \raw_itemContentType ->
    sendMsg csSearchableItemAttributeSet (mkSelector "initWithItemContentType:") (retPtr retVoid) [argPtr (castPtr raw_itemContentType :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentType:@
initWithContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsUTType contentType) => csSearchableItemAttributeSet -> contentType -> IO (Id CSSearchableItemAttributeSet)
initWithContentType csSearchableItemAttributeSet  contentType =
withObjCPtr contentType $ \raw_contentType ->
    sendMsg csSearchableItemAttributeSet (mkSelector "initWithContentType:") (retPtr retVoid) [argPtr (castPtr raw_contentType :: Ptr ())] >>= ownedObject . castPtr

-- | @- moveFrom:@
moveFrom :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsCSSearchableItemAttributeSet sourceAttributeSet) => csSearchableItemAttributeSet -> sourceAttributeSet -> IO ()
moveFrom csSearchableItemAttributeSet  sourceAttributeSet =
withObjCPtr sourceAttributeSet $ \raw_sourceAttributeSet ->
    sendMsg csSearchableItemAttributeSet (mkSelector "moveFrom:") retVoid [argPtr (castPtr raw_sourceAttributeSet :: Ptr ())]

-- | @- setValue:forCustomKey:@
setValue_forCustomKey :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsCSCustomAttributeKey key) => csSearchableItemAttributeSet -> RawId -> key -> IO ()
setValue_forCustomKey csSearchableItemAttributeSet  value key =
withObjCPtr key $ \raw_key ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setValue:forCustomKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- valueForCustomKey:@
valueForCustomKey :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsCSCustomAttributeKey key) => csSearchableItemAttributeSet -> key -> IO RawId
valueForCustomKey csSearchableItemAttributeSet  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg csSearchableItemAttributeSet (mkSelector "valueForCustomKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- headline@
headline :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
headline csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "headline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeadline:@
setHeadline :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setHeadline csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setHeadline:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- instructions@
instructions :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
instructions csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "instructions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInstructions:@
setInstructions :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setInstructions csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setInstructions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- thoroughfare@
thoroughfare :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
thoroughfare csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "thoroughfare") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThoroughfare:@
setThoroughfare :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setThoroughfare csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setThoroughfare:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subThoroughfare@
subThoroughfare :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
subThoroughfare csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "subThoroughfare") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubThoroughfare:@
setSubThoroughfare :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setSubThoroughfare csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setSubThoroughfare:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- postalCode@
postalCode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
postalCode csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPostalCode:@
setPostalCode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setPostalCode csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPostalCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- city@
city :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
city csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "city") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCity:@
setCity :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCity csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stateOrProvince@
stateOrProvince :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
stateOrProvince csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "stateOrProvince") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStateOrProvince:@
setStateOrProvince :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setStateOrProvince csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setStateOrProvince:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- country@
country :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
country csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "country") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCountry:@
setCountry :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCountry csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCountry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullyFormattedAddress@
fullyFormattedAddress :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
fullyFormattedAddress csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "fullyFormattedAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFullyFormattedAddress:@
setFullyFormattedAddress :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setFullyFormattedAddress csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFullyFormattedAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- altitude@
altitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
altitude csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "altitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAltitude:@
setAltitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAltitude csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAltitude:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latitude@
latitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
latitude csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "latitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatitude:@
setLatitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLatitude csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLatitude:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- longitude@
longitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
longitude csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "longitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLongitude:@
setLongitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLongitude csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLongitude:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- speed@
speed :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
speed csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "speed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpeed:@
setSpeed :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setSpeed csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timestamp@
timestamp :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
timestamp csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimestamp:@
setTimestamp :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setTimestamp csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageDirection@
imageDirection :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
imageDirection csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "imageDirection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageDirection:@
setImageDirection :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setImageDirection csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setImageDirection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- namedLocation@
namedLocation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
namedLocation csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "namedLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNamedLocation:@
setNamedLocation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setNamedLocation csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setNamedLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSTrack@
gpsTrack :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsTrack csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSTrack:@
setGPSTrack :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSTrack csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSTrack:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSStatus@
gpsStatus :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsStatus csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSStatus:@
setGPSStatus :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSStatus csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSMeasureMode@
gpsMeasureMode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsMeasureMode csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSMeasureMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSMeasureMode:@
setGPSMeasureMode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSMeasureMode csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSMeasureMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDOP@
gpsdop :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsdop csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDOP") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDOP:@
setGPSDOP :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDOP csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDOP:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSMapDatum@
gpsMapDatum :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsMapDatum csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSMapDatum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSMapDatum:@
setGPSMapDatum :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSMapDatum csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSMapDatum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDestLatitude@
gpsDestLatitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestLatitude csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDestLatitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDestLatitude:@
setGPSDestLatitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestLatitude csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDestLatitude:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDestLongitude@
gpsDestLongitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestLongitude csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDestLongitude") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDestLongitude:@
setGPSDestLongitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestLongitude csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDestLongitude:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDestBearing@
gpsDestBearing :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestBearing csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDestBearing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDestBearing:@
setGPSDestBearing :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestBearing csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDestBearing:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDestDistance@
gpsDestDistance :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestDistance csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDestDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDestDistance:@
setGPSDestDistance :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestDistance csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDestDistance:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSProcessingMethod@
gpsProcessingMethod :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsProcessingMethod csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSProcessingMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSProcessingMethod:@
setGPSProcessingMethod :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSProcessingMethod csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSProcessingMethod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSAreaInformation@
gpsAreaInformation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsAreaInformation csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSAreaInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSAreaInformation:@
setGPSAreaInformation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSAreaInformation csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSAreaInformation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDateStamp@
gpsDateStamp :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
gpsDateStamp csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDateStamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDateStamp:@
setGPSDateStamp :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDateStamp csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDateStamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- GPSDifferental@
gpsDifferental :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDifferental csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "GPSDifferental") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGPSDifferental:@
setGPSDifferental :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDifferental csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGPSDifferental:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pixelHeight@
pixelHeight :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pixelHeight csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "pixelHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPixelHeight:@
setPixelHeight :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPixelHeight csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPixelHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pixelWidth@
pixelWidth :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pixelWidth csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "pixelWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPixelWidth:@
setPixelWidth :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPixelWidth csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPixelWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pixelCount@
pixelCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pixelCount csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "pixelCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPixelCount:@
setPixelCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPixelCount csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPixelCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorSpace@
colorSpace :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
colorSpace csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorSpace:@
setColorSpace :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setColorSpace csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setColorSpace:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bitsPerSample@
bitsPerSample :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
bitsPerSample csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "bitsPerSample") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBitsPerSample:@
setBitsPerSample :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setBitsPerSample csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setBitsPerSample:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- flashOn@
flashOn :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
flashOn csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "flashOn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFlashOn:@
setFlashOn :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFlashOn csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFlashOn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- focalLength@
focalLength :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
focalLength csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "focalLength") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFocalLength:@
setFocalLength :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFocalLength csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFocalLength:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- focalLength35mm@
focalLength35mm :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
focalLength35mm csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "focalLength35mm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFocalLength35mm:@
setFocalLength35mm :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFocalLength35mm csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFocalLength35mm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- acquisitionMake@
acquisitionMake :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
acquisitionMake csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "acquisitionMake") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAcquisitionMake:@
setAcquisitionMake :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAcquisitionMake csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAcquisitionMake:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- acquisitionModel@
acquisitionModel :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
acquisitionModel csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "acquisitionModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAcquisitionModel:@
setAcquisitionModel :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAcquisitionModel csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAcquisitionModel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cameraOwner@
cameraOwner :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
cameraOwner csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "cameraOwner") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCameraOwner:@
setCameraOwner :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCameraOwner csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCameraOwner:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lensModel@
lensModel :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
lensModel csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "lensModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLensModel:@
setLensModel :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setLensModel csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLensModel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ISOSpeed@
isoSpeed :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
isoSpeed csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "ISOSpeed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setISOSpeed:@
setISOSpeed :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setISOSpeed csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setISOSpeed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- orientation@
orientation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
orientation csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "orientation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrientation:@
setOrientation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setOrientation csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setOrientation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- layerNames@
layerNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
layerNames csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "layerNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayerNames:@
setLayerNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setLayerNames csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLayerNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- whiteBalance@
whiteBalance :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
whiteBalance csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "whiteBalance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWhiteBalance:@
setWhiteBalance :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setWhiteBalance csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setWhiteBalance:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- aperture@
aperture :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
aperture csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "aperture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAperture:@
setAperture :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAperture csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAperture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- profileName@
profileName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
profileName csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "profileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProfileName:@
setProfileName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setProfileName csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setProfileName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resolutionWidthDPI@
resolutionWidthDPI :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
resolutionWidthDPI csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "resolutionWidthDPI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResolutionWidthDPI:@
setResolutionWidthDPI :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setResolutionWidthDPI csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setResolutionWidthDPI:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resolutionHeightDPI@
resolutionHeightDPI :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
resolutionHeightDPI csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "resolutionHeightDPI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResolutionHeightDPI:@
setResolutionHeightDPI :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setResolutionHeightDPI csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setResolutionHeightDPI:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exposureMode@
exposureMode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
exposureMode csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "exposureMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExposureMode:@
setExposureMode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureMode csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setExposureMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exposureTime@
exposureTime :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
exposureTime csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "exposureTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExposureTime:@
setExposureTime :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureTime csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setExposureTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- EXIFVersion@
exifVersion :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exifVersion csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "EXIFVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEXIFVersion:@
setEXIFVersion :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setEXIFVersion csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEXIFVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- EXIFGPSVersion@
exifgpsVersion :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exifgpsVersion csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "EXIFGPSVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEXIFGPSVersion:@
setEXIFGPSVersion :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setEXIFGPSVersion csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEXIFGPSVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasAlphaChannel@
hasAlphaChannel :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
hasAlphaChannel csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "hasAlphaChannel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHasAlphaChannel:@
setHasAlphaChannel :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setHasAlphaChannel csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setHasAlphaChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- redEyeOn@
redEyeOn :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
redEyeOn csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "redEyeOn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRedEyeOn:@
setRedEyeOn :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setRedEyeOn csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRedEyeOn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- meteringMode@
meteringMode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
meteringMode csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "meteringMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeteringMode:@
setMeteringMode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMeteringMode csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMeteringMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxAperture@
maxAperture :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
maxAperture csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "maxAperture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxAperture:@
setMaxAperture :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setMaxAperture csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMaxAperture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fNumber@
fNumber :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
fNumber csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "fNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFNumber:@
setFNumber :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFNumber csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exposureProgram@
exposureProgram :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exposureProgram csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "exposureProgram") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExposureProgram:@
setExposureProgram :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureProgram csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setExposureProgram:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exposureTimeString@
exposureTimeString :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exposureTimeString csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "exposureTimeString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExposureTimeString:@
setExposureTimeString :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureTimeString csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setExposureTimeString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioSampleRate@
audioSampleRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioSampleRate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "audioSampleRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioSampleRate:@
setAudioSampleRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioSampleRate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAudioSampleRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioChannelCount@
audioChannelCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioChannelCount csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "audioChannelCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioChannelCount:@
setAudioChannelCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioChannelCount csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAudioChannelCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tempo@
tempo :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
tempo csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "tempo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTempo:@
setTempo :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setTempo csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTempo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keySignature@
keySignature :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
keySignature csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "keySignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeySignature:@
setKeySignature :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setKeySignature csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setKeySignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeSignature@
timeSignature :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
timeSignature csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "timeSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeSignature:@
setTimeSignature :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTimeSignature csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTimeSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioEncodingApplication@
audioEncodingApplication :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
audioEncodingApplication csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "audioEncodingApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioEncodingApplication:@
setAudioEncodingApplication :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioEncodingApplication csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAudioEncodingApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- composer@
composer :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
composer csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "composer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setComposer:@
setComposer :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setComposer csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setComposer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lyricist@
lyricist :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
lyricist csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "lyricist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLyricist:@
setLyricist :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setLyricist csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLyricist:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- album@
album :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
album csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "album") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlbum:@
setAlbum :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAlbum csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAlbum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- artist@
artist :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
artist csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "artist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArtist:@
setArtist :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setArtist csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setArtist:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioTrackNumber@
audioTrackNumber :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioTrackNumber csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "audioTrackNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioTrackNumber:@
setAudioTrackNumber :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioTrackNumber csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAudioTrackNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recordingDate@
recordingDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
recordingDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "recordingDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordingDate:@
setRecordingDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setRecordingDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRecordingDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- musicalGenre@
musicalGenre :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
musicalGenre csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "musicalGenre") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMusicalGenre:@
setMusicalGenre :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMusicalGenre csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMusicalGenre:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- generalMIDISequence@
generalMIDISequence :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
generalMIDISequence csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "generalMIDISequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGeneralMIDISequence:@
setGeneralMIDISequence :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGeneralMIDISequence csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGeneralMIDISequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- musicalInstrumentCategory@
musicalInstrumentCategory :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
musicalInstrumentCategory csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "musicalInstrumentCategory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMusicalInstrumentCategory:@
setMusicalInstrumentCategory :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMusicalInstrumentCategory csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMusicalInstrumentCategory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- musicalInstrumentName@
musicalInstrumentName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
musicalInstrumentName csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "musicalInstrumentName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMusicalInstrumentName:@
setMusicalInstrumentName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMusicalInstrumentName csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMusicalInstrumentName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- editors@
editors :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
editors csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "editors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEditors:@
setEditors :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setEditors csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEditors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- participants@
participants :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
participants csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "participants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParticipants:@
setParticipants :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setParticipants csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setParticipants:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- projects@
projects :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
projects csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "projects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProjects:@
setProjects :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setProjects csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setProjects:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- downloadedDate@
downloadedDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
downloadedDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "downloadedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDownloadedDate:@
setDownloadedDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setDownloadedDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDownloadedDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentSources@
contentSources :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contentSources csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentSources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentSources:@
setContentSources :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContentSources csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentSources:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- comment@
comment :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
comment csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "comment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setComment:@
setComment :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setComment csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setComment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- copyright@
copyright :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
copyright csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "copyright") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setCopyright:@
setCopyright :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCopyright csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCopyright:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastUsedDate@
lastUsedDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
lastUsedDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "lastUsedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastUsedDate:@
setLastUsedDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setLastUsedDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLastUsedDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentCreationDate@
contentCreationDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
contentCreationDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentCreationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentCreationDate:@
setContentCreationDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setContentCreationDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentCreationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentModificationDate@
contentModificationDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
contentModificationDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentModificationDate:@
setContentModificationDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setContentModificationDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentModificationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- addedDate@
addedDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
addedDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "addedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAddedDate:@
setAddedDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setAddedDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAddedDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
duration csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setDuration csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contactKeywords@
contactKeywords :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contactKeywords csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contactKeywords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContactKeywords:@
setContactKeywords :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContactKeywords csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContactKeywords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- codecs@
codecs :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
codecs csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "codecs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCodecs:@
setCodecs :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setCodecs csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCodecs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mediaTypes@
mediaTypes :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
mediaTypes csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "mediaTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMediaTypes:@
setMediaTypes :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setMediaTypes csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMediaTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- streamable@
streamable :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
streamable csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "streamable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamable:@
setStreamable :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setStreamable csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setStreamable:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalBitRate@
totalBitRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
totalBitRate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "totalBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTotalBitRate:@
setTotalBitRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setTotalBitRate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTotalBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoBitRate@
videoBitRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
videoBitRate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "videoBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoBitRate:@
setVideoBitRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setVideoBitRate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setVideoBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioBitRate@
audioBitRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioBitRate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "audioBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioBitRate:@
setAudioBitRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioBitRate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAudioBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deliveryType@
deliveryType :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
deliveryType csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "deliveryType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeliveryType:@
setDeliveryType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setDeliveryType csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDeliveryType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- organizations@
organizations :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
organizations csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "organizations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrganizations:@
setOrganizations :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setOrganizations csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setOrganizations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- role@
role_ :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
role_ csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "role") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRole:@
setRole :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRole csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRole:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- languages@
languages :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
languages csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "languages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguages:@
setLanguages :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setLanguages csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLanguages:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rights@
rights :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
rights csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "rights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRights:@
setRights :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRights csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRights:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- publishers@
publishers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
publishers csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "publishers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPublishers:@
setPublishers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPublishers csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPublishers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contributors@
contributors :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contributors csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contributors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContributors:@
setContributors :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContributors csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContributors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coverage@
coverage :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
coverage csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "coverage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoverage:@
setCoverage :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setCoverage csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCoverage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rating@
rating :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
rating csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "rating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRating:@
setRating :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setRating csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRating:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ratingDescription@
ratingDescription :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
ratingDescription csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "ratingDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRatingDescription:@
setRatingDescription :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRatingDescription csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRatingDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- playCount@
playCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
playCount csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "playCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlayCount:@
setPlayCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPlayCount csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPlayCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- information@
information :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
information csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "information") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInformation:@
setInformation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setInformation csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setInformation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- director@
director :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
director csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "director") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirector:@
setDirector :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setDirector csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDirector:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- producer@
producer :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
producer csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "producer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProducer:@
setProducer :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setProducer csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setProducer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- genre@
genre :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
genre csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "genre") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGenre:@
setGenre :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGenre csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setGenre:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- performers@
performers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
performers csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "performers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPerformers:@
setPerformers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPerformers csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPerformers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originalFormat@
originalFormat :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
originalFormat csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "originalFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginalFormat:@
setOriginalFormat :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setOriginalFormat csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setOriginalFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originalSource@
originalSource :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
originalSource csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "originalSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginalSource:@
setOriginalSource :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setOriginalSource csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setOriginalSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- local@
local :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
local csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "local") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocal:@
setLocal :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLocal csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLocal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentRating@
contentRating :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
contentRating csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentRating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentRating:@
setContentRating :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setContentRating csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentRating:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URL@
url :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
url csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setURL csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accountIdentifier@
accountIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
accountIdentifier csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "accountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccountIdentifier:@
setAccountIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAccountIdentifier csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAccountIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accountHandles@
accountHandles :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
accountHandles csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "accountHandles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccountHandles:@
setAccountHandles :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAccountHandles csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAccountHandles:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- HTMLContentData@
htmlContentData :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSData)
htmlContentData csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "HTMLContentData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHTMLContentData:@
setHTMLContentData :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSData value) => csSearchableItemAttributeSet -> value -> IO ()
setHTMLContentData csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setHTMLContentData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textContent@
textContent :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
textContent csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "textContent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextContent:@
setTextContent :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTextContent csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTextContent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authors@
authors :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authors csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "authors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthors:@
setAuthors :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthors csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAuthors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- primaryRecipients@
primaryRecipients :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
primaryRecipients csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "primaryRecipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrimaryRecipients:@
setPrimaryRecipients :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPrimaryRecipients csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPrimaryRecipients:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- additionalRecipients@
additionalRecipients :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
additionalRecipients csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "additionalRecipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdditionalRecipients:@
setAdditionalRecipients :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAdditionalRecipients csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAdditionalRecipients:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hiddenAdditionalRecipients@
hiddenAdditionalRecipients :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
hiddenAdditionalRecipients csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "hiddenAdditionalRecipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHiddenAdditionalRecipients:@
setHiddenAdditionalRecipients :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setHiddenAdditionalRecipients csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setHiddenAdditionalRecipients:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emailHeaders@
emailHeaders :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDictionary)
emailHeaders csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "emailHeaders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmailHeaders:@
setEmailHeaders :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDictionary value) => csSearchableItemAttributeSet -> value -> IO ()
setEmailHeaders csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEmailHeaders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mailboxIdentifiers@
mailboxIdentifiers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
mailboxIdentifiers csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "mailboxIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMailboxIdentifiers:@
setMailboxIdentifiers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setMailboxIdentifiers csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMailboxIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authorNames@
authorNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authorNames csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "authorNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthorNames:@
setAuthorNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthorNames csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAuthorNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recipientNames@
recipientNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
recipientNames csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "recipientNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecipientNames:@
setRecipientNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setRecipientNames csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRecipientNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authorEmailAddresses@
authorEmailAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authorEmailAddresses csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "authorEmailAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthorEmailAddresses:@
setAuthorEmailAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthorEmailAddresses csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAuthorEmailAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recipientEmailAddresses@
recipientEmailAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
recipientEmailAddresses csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "recipientEmailAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecipientEmailAddresses:@
setRecipientEmailAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setRecipientEmailAddresses csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRecipientEmailAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authorAddresses@
authorAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authorAddresses csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "authorAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthorAddresses:@
setAuthorAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthorAddresses csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAuthorAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recipientAddresses@
recipientAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
recipientAddresses csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "recipientAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecipientAddresses:@
setRecipientAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setRecipientAddresses csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRecipientAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneNumbers@
phoneNumbers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
phoneNumbers csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "phoneNumbers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneNumbers:@
setPhoneNumbers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPhoneNumbers csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPhoneNumbers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emailAddresses@
emailAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
emailAddresses csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "emailAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmailAddresses:@
setEmailAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setEmailAddresses csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEmailAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- instantMessageAddresses@
instantMessageAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
instantMessageAddresses csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "instantMessageAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInstantMessageAddresses:@
setInstantMessageAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setInstantMessageAddresses csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setInstantMessageAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- likelyJunk@
likelyJunk :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
likelyJunk csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "likelyJunk") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLikelyJunk:@
setLikelyJunk :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLikelyJunk csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setLikelyJunk:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dueDate@
dueDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
dueDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "dueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDueDate:@
setDueDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setDueDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDueDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- completionDate@
completionDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
completionDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "completionDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletionDate:@
setCompletionDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setCompletionDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCompletionDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startDate@
startDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
startDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartDate:@
setStartDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setStartDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endDate@
endDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
endDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndDate:@
setEndDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setEndDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- importantDates@
importantDates :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
importantDates csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "importantDates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImportantDates:@
setImportantDates :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setImportantDates csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setImportantDates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allDay@
allDay :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
allDay csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "allDay") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAllDay:@
setAllDay :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAllDay csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAllDay:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Subject of the this item.
--
-- ObjC selector: @- subject@
subject :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
subject csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "subject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Subject of the this item.
--
-- ObjC selector: @- setSubject:@
setSubject :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setSubject csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setSubject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- theme@
theme :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
theme csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "theme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTheme:@
setTheme :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTheme csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTheme:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentDescription@
contentDescription :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
contentDescription csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentDescription:@
setContentDescription :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContentDescription csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
identifier csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setIdentifier csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audiences@
audiences :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
audiences csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "audiences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudiences:@
setAudiences :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAudiences csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAudiences:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileSize@
fileSize :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
fileSize csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "fileSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileSize:@
setFileSize :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFileSize csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFileSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pageCount@
pageCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pageCount csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "pageCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageCount:@
setPageCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPageCount csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPageCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pageWidth@
pageWidth :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pageWidth csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "pageWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageWidth:@
setPageWidth :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPageWidth csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPageWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pageHeight@
pageHeight :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pageHeight csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "pageHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageHeight:@
setPageHeight :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPageHeight csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPageHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- securityMethod@
securityMethod :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
securityMethod csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "securityMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSecurityMethod:@
setSecurityMethod :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setSecurityMethod csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setSecurityMethod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- creator@
creator :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
creator csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "creator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCreator:@
setCreator :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCreator csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setCreator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- encodingApplications@
encodingApplications :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
encodingApplications csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "encodingApplications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncodingApplications:@
setEncodingApplications :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setEncodingApplications csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setEncodingApplications:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- kind@
kind :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
kind csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "kind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKind:@
setKind :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setKind csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setKind:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fontNames@
fontNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
fontNames csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "fontNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontNames:@
setFontNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setFontNames csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setFontNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerTitle@
containerTitle :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
containerTitle csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "containerTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerTitle:@
setContainerTitle :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerTitle csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContainerTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerDisplayName@
containerDisplayName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
containerDisplayName csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "containerDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerDisplayName:@
setContainerDisplayName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerDisplayName csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContainerDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerIdentifier@
containerIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
containerIdentifier csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "containerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerIdentifier:@
setContainerIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerIdentifier csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContainerIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerOrder@
containerOrder :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
containerOrder csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "containerOrder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerOrder:@
setContainerOrder :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerOrder csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContainerOrder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportsPhoneCall@
supportsPhoneCall :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
supportsPhoneCall csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "supportsPhoneCall") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportsPhoneCall:@
setSupportsPhoneCall :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setSupportsPhoneCall csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setSupportsPhoneCall:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportsNavigation@
supportsNavigation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
supportsNavigation csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "supportsNavigation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportsNavigation:@
setSupportsNavigation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setSupportsNavigation csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setSupportsNavigation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displayName@
displayName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
displayName csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayName:@
setDisplayName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setDisplayName csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alternateNames@
alternateNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
alternateNames csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "alternateNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlternateNames:@
setAlternateNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAlternateNames csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setAlternateNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- path@
path :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
path csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPath:@
setPath :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setPath csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentURL@
contentURL :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
contentURL csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentURL:@
setContentURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setContentURL csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- thumbnailURL@
thumbnailURL :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
thumbnailURL csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "thumbnailURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThumbnailURL:@
setThumbnailURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setThumbnailURL csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setThumbnailURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- thumbnailData@
thumbnailData :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSData)
thumbnailData csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "thumbnailData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThumbnailData:@
setThumbnailData :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSData value) => csSearchableItemAttributeSet -> value -> IO ()
setThumbnailData csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setThumbnailData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- darkThumbnailURL@
darkThumbnailURL :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
darkThumbnailURL csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "darkThumbnailURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDarkThumbnailURL:@
setDarkThumbnailURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setDarkThumbnailURL csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setDarkThumbnailURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- relatedUniqueIdentifier@
relatedUniqueIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
relatedUniqueIdentifier csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "relatedUniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRelatedUniqueIdentifier:@
setRelatedUniqueIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRelatedUniqueIdentifier csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setRelatedUniqueIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataModificationDate@
metadataModificationDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
metadataModificationDate csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "metadataModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataModificationDate:@
setMetadataModificationDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setMetadataModificationDate csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setMetadataModificationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentType@
contentType :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
contentType csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentType:@
setContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContentType csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentTypeTree@
contentTypeTree :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contentTypeTree csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "contentTypeTree") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentTypeTree:@
setContentTypeTree :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContentTypeTree csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setContentTypeTree:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keywords@
keywords :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
keywords csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "keywords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeywords:@
setKeywords :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setKeywords csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setKeywords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- title@
title :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
title csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTitle csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- version@
version :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
version csSearchableItemAttributeSet  =
  sendMsg csSearchableItemAttributeSet (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersion:@
setVersion :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setVersion csSearchableItemAttributeSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItemAttributeSet (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemContentType:@
initWithItemContentTypeSelector :: Selector
initWithItemContentTypeSelector = mkSelector "initWithItemContentType:"

-- | @Selector@ for @initWithContentType:@
initWithContentTypeSelector :: Selector
initWithContentTypeSelector = mkSelector "initWithContentType:"

-- | @Selector@ for @moveFrom:@
moveFromSelector :: Selector
moveFromSelector = mkSelector "moveFrom:"

-- | @Selector@ for @setValue:forCustomKey:@
setValue_forCustomKeySelector :: Selector
setValue_forCustomKeySelector = mkSelector "setValue:forCustomKey:"

-- | @Selector@ for @valueForCustomKey:@
valueForCustomKeySelector :: Selector
valueForCustomKeySelector = mkSelector "valueForCustomKey:"

-- | @Selector@ for @headline@
headlineSelector :: Selector
headlineSelector = mkSelector "headline"

-- | @Selector@ for @setHeadline:@
setHeadlineSelector :: Selector
setHeadlineSelector = mkSelector "setHeadline:"

-- | @Selector@ for @instructions@
instructionsSelector :: Selector
instructionsSelector = mkSelector "instructions"

-- | @Selector@ for @setInstructions:@
setInstructionsSelector :: Selector
setInstructionsSelector = mkSelector "setInstructions:"

-- | @Selector@ for @thoroughfare@
thoroughfareSelector :: Selector
thoroughfareSelector = mkSelector "thoroughfare"

-- | @Selector@ for @setThoroughfare:@
setThoroughfareSelector :: Selector
setThoroughfareSelector = mkSelector "setThoroughfare:"

-- | @Selector@ for @subThoroughfare@
subThoroughfareSelector :: Selector
subThoroughfareSelector = mkSelector "subThoroughfare"

-- | @Selector@ for @setSubThoroughfare:@
setSubThoroughfareSelector :: Selector
setSubThoroughfareSelector = mkSelector "setSubThoroughfare:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @city@
citySelector :: Selector
citySelector = mkSelector "city"

-- | @Selector@ for @setCity:@
setCitySelector :: Selector
setCitySelector = mkSelector "setCity:"

-- | @Selector@ for @stateOrProvince@
stateOrProvinceSelector :: Selector
stateOrProvinceSelector = mkSelector "stateOrProvince"

-- | @Selector@ for @setStateOrProvince:@
setStateOrProvinceSelector :: Selector
setStateOrProvinceSelector = mkSelector "setStateOrProvince:"

-- | @Selector@ for @country@
countrySelector :: Selector
countrySelector = mkSelector "country"

-- | @Selector@ for @setCountry:@
setCountrySelector :: Selector
setCountrySelector = mkSelector "setCountry:"

-- | @Selector@ for @fullyFormattedAddress@
fullyFormattedAddressSelector :: Selector
fullyFormattedAddressSelector = mkSelector "fullyFormattedAddress"

-- | @Selector@ for @setFullyFormattedAddress:@
setFullyFormattedAddressSelector :: Selector
setFullyFormattedAddressSelector = mkSelector "setFullyFormattedAddress:"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @setAltitude:@
setAltitudeSelector :: Selector
setAltitudeSelector = mkSelector "setAltitude:"

-- | @Selector@ for @latitude@
latitudeSelector :: Selector
latitudeSelector = mkSelector "latitude"

-- | @Selector@ for @setLatitude:@
setLatitudeSelector :: Selector
setLatitudeSelector = mkSelector "setLatitude:"

-- | @Selector@ for @longitude@
longitudeSelector :: Selector
longitudeSelector = mkSelector "longitude"

-- | @Selector@ for @setLongitude:@
setLongitudeSelector :: Selector
setLongitudeSelector = mkSelector "setLongitude:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @setTimestamp:@
setTimestampSelector :: Selector
setTimestampSelector = mkSelector "setTimestamp:"

-- | @Selector@ for @imageDirection@
imageDirectionSelector :: Selector
imageDirectionSelector = mkSelector "imageDirection"

-- | @Selector@ for @setImageDirection:@
setImageDirectionSelector :: Selector
setImageDirectionSelector = mkSelector "setImageDirection:"

-- | @Selector@ for @namedLocation@
namedLocationSelector :: Selector
namedLocationSelector = mkSelector "namedLocation"

-- | @Selector@ for @setNamedLocation:@
setNamedLocationSelector :: Selector
setNamedLocationSelector = mkSelector "setNamedLocation:"

-- | @Selector@ for @GPSTrack@
gpsTrackSelector :: Selector
gpsTrackSelector = mkSelector "GPSTrack"

-- | @Selector@ for @setGPSTrack:@
setGPSTrackSelector :: Selector
setGPSTrackSelector = mkSelector "setGPSTrack:"

-- | @Selector@ for @GPSStatus@
gpsStatusSelector :: Selector
gpsStatusSelector = mkSelector "GPSStatus"

-- | @Selector@ for @setGPSStatus:@
setGPSStatusSelector :: Selector
setGPSStatusSelector = mkSelector "setGPSStatus:"

-- | @Selector@ for @GPSMeasureMode@
gpsMeasureModeSelector :: Selector
gpsMeasureModeSelector = mkSelector "GPSMeasureMode"

-- | @Selector@ for @setGPSMeasureMode:@
setGPSMeasureModeSelector :: Selector
setGPSMeasureModeSelector = mkSelector "setGPSMeasureMode:"

-- | @Selector@ for @GPSDOP@
gpsdopSelector :: Selector
gpsdopSelector = mkSelector "GPSDOP"

-- | @Selector@ for @setGPSDOP:@
setGPSDOPSelector :: Selector
setGPSDOPSelector = mkSelector "setGPSDOP:"

-- | @Selector@ for @GPSMapDatum@
gpsMapDatumSelector :: Selector
gpsMapDatumSelector = mkSelector "GPSMapDatum"

-- | @Selector@ for @setGPSMapDatum:@
setGPSMapDatumSelector :: Selector
setGPSMapDatumSelector = mkSelector "setGPSMapDatum:"

-- | @Selector@ for @GPSDestLatitude@
gpsDestLatitudeSelector :: Selector
gpsDestLatitudeSelector = mkSelector "GPSDestLatitude"

-- | @Selector@ for @setGPSDestLatitude:@
setGPSDestLatitudeSelector :: Selector
setGPSDestLatitudeSelector = mkSelector "setGPSDestLatitude:"

-- | @Selector@ for @GPSDestLongitude@
gpsDestLongitudeSelector :: Selector
gpsDestLongitudeSelector = mkSelector "GPSDestLongitude"

-- | @Selector@ for @setGPSDestLongitude:@
setGPSDestLongitudeSelector :: Selector
setGPSDestLongitudeSelector = mkSelector "setGPSDestLongitude:"

-- | @Selector@ for @GPSDestBearing@
gpsDestBearingSelector :: Selector
gpsDestBearingSelector = mkSelector "GPSDestBearing"

-- | @Selector@ for @setGPSDestBearing:@
setGPSDestBearingSelector :: Selector
setGPSDestBearingSelector = mkSelector "setGPSDestBearing:"

-- | @Selector@ for @GPSDestDistance@
gpsDestDistanceSelector :: Selector
gpsDestDistanceSelector = mkSelector "GPSDestDistance"

-- | @Selector@ for @setGPSDestDistance:@
setGPSDestDistanceSelector :: Selector
setGPSDestDistanceSelector = mkSelector "setGPSDestDistance:"

-- | @Selector@ for @GPSProcessingMethod@
gpsProcessingMethodSelector :: Selector
gpsProcessingMethodSelector = mkSelector "GPSProcessingMethod"

-- | @Selector@ for @setGPSProcessingMethod:@
setGPSProcessingMethodSelector :: Selector
setGPSProcessingMethodSelector = mkSelector "setGPSProcessingMethod:"

-- | @Selector@ for @GPSAreaInformation@
gpsAreaInformationSelector :: Selector
gpsAreaInformationSelector = mkSelector "GPSAreaInformation"

-- | @Selector@ for @setGPSAreaInformation:@
setGPSAreaInformationSelector :: Selector
setGPSAreaInformationSelector = mkSelector "setGPSAreaInformation:"

-- | @Selector@ for @GPSDateStamp@
gpsDateStampSelector :: Selector
gpsDateStampSelector = mkSelector "GPSDateStamp"

-- | @Selector@ for @setGPSDateStamp:@
setGPSDateStampSelector :: Selector
setGPSDateStampSelector = mkSelector "setGPSDateStamp:"

-- | @Selector@ for @GPSDifferental@
gpsDifferentalSelector :: Selector
gpsDifferentalSelector = mkSelector "GPSDifferental"

-- | @Selector@ for @setGPSDifferental:@
setGPSDifferentalSelector :: Selector
setGPSDifferentalSelector = mkSelector "setGPSDifferental:"

-- | @Selector@ for @pixelHeight@
pixelHeightSelector :: Selector
pixelHeightSelector = mkSelector "pixelHeight"

-- | @Selector@ for @setPixelHeight:@
setPixelHeightSelector :: Selector
setPixelHeightSelector = mkSelector "setPixelHeight:"

-- | @Selector@ for @pixelWidth@
pixelWidthSelector :: Selector
pixelWidthSelector = mkSelector "pixelWidth"

-- | @Selector@ for @setPixelWidth:@
setPixelWidthSelector :: Selector
setPixelWidthSelector = mkSelector "setPixelWidth:"

-- | @Selector@ for @pixelCount@
pixelCountSelector :: Selector
pixelCountSelector = mkSelector "pixelCount"

-- | @Selector@ for @setPixelCount:@
setPixelCountSelector :: Selector
setPixelCountSelector = mkSelector "setPixelCount:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector
setColorSpaceSelector = mkSelector "setColorSpace:"

-- | @Selector@ for @bitsPerSample@
bitsPerSampleSelector :: Selector
bitsPerSampleSelector = mkSelector "bitsPerSample"

-- | @Selector@ for @setBitsPerSample:@
setBitsPerSampleSelector :: Selector
setBitsPerSampleSelector = mkSelector "setBitsPerSample:"

-- | @Selector@ for @flashOn@
flashOnSelector :: Selector
flashOnSelector = mkSelector "flashOn"

-- | @Selector@ for @setFlashOn:@
setFlashOnSelector :: Selector
setFlashOnSelector = mkSelector "setFlashOn:"

-- | @Selector@ for @focalLength@
focalLengthSelector :: Selector
focalLengthSelector = mkSelector "focalLength"

-- | @Selector@ for @setFocalLength:@
setFocalLengthSelector :: Selector
setFocalLengthSelector = mkSelector "setFocalLength:"

-- | @Selector@ for @focalLength35mm@
focalLength35mmSelector :: Selector
focalLength35mmSelector = mkSelector "focalLength35mm"

-- | @Selector@ for @setFocalLength35mm:@
setFocalLength35mmSelector :: Selector
setFocalLength35mmSelector = mkSelector "setFocalLength35mm:"

-- | @Selector@ for @acquisitionMake@
acquisitionMakeSelector :: Selector
acquisitionMakeSelector = mkSelector "acquisitionMake"

-- | @Selector@ for @setAcquisitionMake:@
setAcquisitionMakeSelector :: Selector
setAcquisitionMakeSelector = mkSelector "setAcquisitionMake:"

-- | @Selector@ for @acquisitionModel@
acquisitionModelSelector :: Selector
acquisitionModelSelector = mkSelector "acquisitionModel"

-- | @Selector@ for @setAcquisitionModel:@
setAcquisitionModelSelector :: Selector
setAcquisitionModelSelector = mkSelector "setAcquisitionModel:"

-- | @Selector@ for @cameraOwner@
cameraOwnerSelector :: Selector
cameraOwnerSelector = mkSelector "cameraOwner"

-- | @Selector@ for @setCameraOwner:@
setCameraOwnerSelector :: Selector
setCameraOwnerSelector = mkSelector "setCameraOwner:"

-- | @Selector@ for @lensModel@
lensModelSelector :: Selector
lensModelSelector = mkSelector "lensModel"

-- | @Selector@ for @setLensModel:@
setLensModelSelector :: Selector
setLensModelSelector = mkSelector "setLensModel:"

-- | @Selector@ for @ISOSpeed@
isoSpeedSelector :: Selector
isoSpeedSelector = mkSelector "ISOSpeed"

-- | @Selector@ for @setISOSpeed:@
setISOSpeedSelector :: Selector
setISOSpeedSelector = mkSelector "setISOSpeed:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @layerNames@
layerNamesSelector :: Selector
layerNamesSelector = mkSelector "layerNames"

-- | @Selector@ for @setLayerNames:@
setLayerNamesSelector :: Selector
setLayerNamesSelector = mkSelector "setLayerNames:"

-- | @Selector@ for @whiteBalance@
whiteBalanceSelector :: Selector
whiteBalanceSelector = mkSelector "whiteBalance"

-- | @Selector@ for @setWhiteBalance:@
setWhiteBalanceSelector :: Selector
setWhiteBalanceSelector = mkSelector "setWhiteBalance:"

-- | @Selector@ for @aperture@
apertureSelector :: Selector
apertureSelector = mkSelector "aperture"

-- | @Selector@ for @setAperture:@
setApertureSelector :: Selector
setApertureSelector = mkSelector "setAperture:"

-- | @Selector@ for @profileName@
profileNameSelector :: Selector
profileNameSelector = mkSelector "profileName"

-- | @Selector@ for @setProfileName:@
setProfileNameSelector :: Selector
setProfileNameSelector = mkSelector "setProfileName:"

-- | @Selector@ for @resolutionWidthDPI@
resolutionWidthDPISelector :: Selector
resolutionWidthDPISelector = mkSelector "resolutionWidthDPI"

-- | @Selector@ for @setResolutionWidthDPI:@
setResolutionWidthDPISelector :: Selector
setResolutionWidthDPISelector = mkSelector "setResolutionWidthDPI:"

-- | @Selector@ for @resolutionHeightDPI@
resolutionHeightDPISelector :: Selector
resolutionHeightDPISelector = mkSelector "resolutionHeightDPI"

-- | @Selector@ for @setResolutionHeightDPI:@
setResolutionHeightDPISelector :: Selector
setResolutionHeightDPISelector = mkSelector "setResolutionHeightDPI:"

-- | @Selector@ for @exposureMode@
exposureModeSelector :: Selector
exposureModeSelector = mkSelector "exposureMode"

-- | @Selector@ for @setExposureMode:@
setExposureModeSelector :: Selector
setExposureModeSelector = mkSelector "setExposureMode:"

-- | @Selector@ for @exposureTime@
exposureTimeSelector :: Selector
exposureTimeSelector = mkSelector "exposureTime"

-- | @Selector@ for @setExposureTime:@
setExposureTimeSelector :: Selector
setExposureTimeSelector = mkSelector "setExposureTime:"

-- | @Selector@ for @EXIFVersion@
exifVersionSelector :: Selector
exifVersionSelector = mkSelector "EXIFVersion"

-- | @Selector@ for @setEXIFVersion:@
setEXIFVersionSelector :: Selector
setEXIFVersionSelector = mkSelector "setEXIFVersion:"

-- | @Selector@ for @EXIFGPSVersion@
exifgpsVersionSelector :: Selector
exifgpsVersionSelector = mkSelector "EXIFGPSVersion"

-- | @Selector@ for @setEXIFGPSVersion:@
setEXIFGPSVersionSelector :: Selector
setEXIFGPSVersionSelector = mkSelector "setEXIFGPSVersion:"

-- | @Selector@ for @hasAlphaChannel@
hasAlphaChannelSelector :: Selector
hasAlphaChannelSelector = mkSelector "hasAlphaChannel"

-- | @Selector@ for @setHasAlphaChannel:@
setHasAlphaChannelSelector :: Selector
setHasAlphaChannelSelector = mkSelector "setHasAlphaChannel:"

-- | @Selector@ for @redEyeOn@
redEyeOnSelector :: Selector
redEyeOnSelector = mkSelector "redEyeOn"

-- | @Selector@ for @setRedEyeOn:@
setRedEyeOnSelector :: Selector
setRedEyeOnSelector = mkSelector "setRedEyeOn:"

-- | @Selector@ for @meteringMode@
meteringModeSelector :: Selector
meteringModeSelector = mkSelector "meteringMode"

-- | @Selector@ for @setMeteringMode:@
setMeteringModeSelector :: Selector
setMeteringModeSelector = mkSelector "setMeteringMode:"

-- | @Selector@ for @maxAperture@
maxApertureSelector :: Selector
maxApertureSelector = mkSelector "maxAperture"

-- | @Selector@ for @setMaxAperture:@
setMaxApertureSelector :: Selector
setMaxApertureSelector = mkSelector "setMaxAperture:"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @setFNumber:@
setFNumberSelector :: Selector
setFNumberSelector = mkSelector "setFNumber:"

-- | @Selector@ for @exposureProgram@
exposureProgramSelector :: Selector
exposureProgramSelector = mkSelector "exposureProgram"

-- | @Selector@ for @setExposureProgram:@
setExposureProgramSelector :: Selector
setExposureProgramSelector = mkSelector "setExposureProgram:"

-- | @Selector@ for @exposureTimeString@
exposureTimeStringSelector :: Selector
exposureTimeStringSelector = mkSelector "exposureTimeString"

-- | @Selector@ for @setExposureTimeString:@
setExposureTimeStringSelector :: Selector
setExposureTimeStringSelector = mkSelector "setExposureTimeString:"

-- | @Selector@ for @audioSampleRate@
audioSampleRateSelector :: Selector
audioSampleRateSelector = mkSelector "audioSampleRate"

-- | @Selector@ for @setAudioSampleRate:@
setAudioSampleRateSelector :: Selector
setAudioSampleRateSelector = mkSelector "setAudioSampleRate:"

-- | @Selector@ for @audioChannelCount@
audioChannelCountSelector :: Selector
audioChannelCountSelector = mkSelector "audioChannelCount"

-- | @Selector@ for @setAudioChannelCount:@
setAudioChannelCountSelector :: Selector
setAudioChannelCountSelector = mkSelector "setAudioChannelCount:"

-- | @Selector@ for @tempo@
tempoSelector :: Selector
tempoSelector = mkSelector "tempo"

-- | @Selector@ for @setTempo:@
setTempoSelector :: Selector
setTempoSelector = mkSelector "setTempo:"

-- | @Selector@ for @keySignature@
keySignatureSelector :: Selector
keySignatureSelector = mkSelector "keySignature"

-- | @Selector@ for @setKeySignature:@
setKeySignatureSelector :: Selector
setKeySignatureSelector = mkSelector "setKeySignature:"

-- | @Selector@ for @timeSignature@
timeSignatureSelector :: Selector
timeSignatureSelector = mkSelector "timeSignature"

-- | @Selector@ for @setTimeSignature:@
setTimeSignatureSelector :: Selector
setTimeSignatureSelector = mkSelector "setTimeSignature:"

-- | @Selector@ for @audioEncodingApplication@
audioEncodingApplicationSelector :: Selector
audioEncodingApplicationSelector = mkSelector "audioEncodingApplication"

-- | @Selector@ for @setAudioEncodingApplication:@
setAudioEncodingApplicationSelector :: Selector
setAudioEncodingApplicationSelector = mkSelector "setAudioEncodingApplication:"

-- | @Selector@ for @composer@
composerSelector :: Selector
composerSelector = mkSelector "composer"

-- | @Selector@ for @setComposer:@
setComposerSelector :: Selector
setComposerSelector = mkSelector "setComposer:"

-- | @Selector@ for @lyricist@
lyricistSelector :: Selector
lyricistSelector = mkSelector "lyricist"

-- | @Selector@ for @setLyricist:@
setLyricistSelector :: Selector
setLyricistSelector = mkSelector "setLyricist:"

-- | @Selector@ for @album@
albumSelector :: Selector
albumSelector = mkSelector "album"

-- | @Selector@ for @setAlbum:@
setAlbumSelector :: Selector
setAlbumSelector = mkSelector "setAlbum:"

-- | @Selector@ for @artist@
artistSelector :: Selector
artistSelector = mkSelector "artist"

-- | @Selector@ for @setArtist:@
setArtistSelector :: Selector
setArtistSelector = mkSelector "setArtist:"

-- | @Selector@ for @audioTrackNumber@
audioTrackNumberSelector :: Selector
audioTrackNumberSelector = mkSelector "audioTrackNumber"

-- | @Selector@ for @setAudioTrackNumber:@
setAudioTrackNumberSelector :: Selector
setAudioTrackNumberSelector = mkSelector "setAudioTrackNumber:"

-- | @Selector@ for @recordingDate@
recordingDateSelector :: Selector
recordingDateSelector = mkSelector "recordingDate"

-- | @Selector@ for @setRecordingDate:@
setRecordingDateSelector :: Selector
setRecordingDateSelector = mkSelector "setRecordingDate:"

-- | @Selector@ for @musicalGenre@
musicalGenreSelector :: Selector
musicalGenreSelector = mkSelector "musicalGenre"

-- | @Selector@ for @setMusicalGenre:@
setMusicalGenreSelector :: Selector
setMusicalGenreSelector = mkSelector "setMusicalGenre:"

-- | @Selector@ for @generalMIDISequence@
generalMIDISequenceSelector :: Selector
generalMIDISequenceSelector = mkSelector "generalMIDISequence"

-- | @Selector@ for @setGeneralMIDISequence:@
setGeneralMIDISequenceSelector :: Selector
setGeneralMIDISequenceSelector = mkSelector "setGeneralMIDISequence:"

-- | @Selector@ for @musicalInstrumentCategory@
musicalInstrumentCategorySelector :: Selector
musicalInstrumentCategorySelector = mkSelector "musicalInstrumentCategory"

-- | @Selector@ for @setMusicalInstrumentCategory:@
setMusicalInstrumentCategorySelector :: Selector
setMusicalInstrumentCategorySelector = mkSelector "setMusicalInstrumentCategory:"

-- | @Selector@ for @musicalInstrumentName@
musicalInstrumentNameSelector :: Selector
musicalInstrumentNameSelector = mkSelector "musicalInstrumentName"

-- | @Selector@ for @setMusicalInstrumentName:@
setMusicalInstrumentNameSelector :: Selector
setMusicalInstrumentNameSelector = mkSelector "setMusicalInstrumentName:"

-- | @Selector@ for @editors@
editorsSelector :: Selector
editorsSelector = mkSelector "editors"

-- | @Selector@ for @setEditors:@
setEditorsSelector :: Selector
setEditorsSelector = mkSelector "setEditors:"

-- | @Selector@ for @participants@
participantsSelector :: Selector
participantsSelector = mkSelector "participants"

-- | @Selector@ for @setParticipants:@
setParticipantsSelector :: Selector
setParticipantsSelector = mkSelector "setParticipants:"

-- | @Selector@ for @projects@
projectsSelector :: Selector
projectsSelector = mkSelector "projects"

-- | @Selector@ for @setProjects:@
setProjectsSelector :: Selector
setProjectsSelector = mkSelector "setProjects:"

-- | @Selector@ for @downloadedDate@
downloadedDateSelector :: Selector
downloadedDateSelector = mkSelector "downloadedDate"

-- | @Selector@ for @setDownloadedDate:@
setDownloadedDateSelector :: Selector
setDownloadedDateSelector = mkSelector "setDownloadedDate:"

-- | @Selector@ for @contentSources@
contentSourcesSelector :: Selector
contentSourcesSelector = mkSelector "contentSources"

-- | @Selector@ for @setContentSources:@
setContentSourcesSelector :: Selector
setContentSourcesSelector = mkSelector "setContentSources:"

-- | @Selector@ for @comment@
commentSelector :: Selector
commentSelector = mkSelector "comment"

-- | @Selector@ for @setComment:@
setCommentSelector :: Selector
setCommentSelector = mkSelector "setComment:"

-- | @Selector@ for @copyright@
copyrightSelector :: Selector
copyrightSelector = mkSelector "copyright"

-- | @Selector@ for @setCopyright:@
setCopyrightSelector :: Selector
setCopyrightSelector = mkSelector "setCopyright:"

-- | @Selector@ for @lastUsedDate@
lastUsedDateSelector :: Selector
lastUsedDateSelector = mkSelector "lastUsedDate"

-- | @Selector@ for @setLastUsedDate:@
setLastUsedDateSelector :: Selector
setLastUsedDateSelector = mkSelector "setLastUsedDate:"

-- | @Selector@ for @contentCreationDate@
contentCreationDateSelector :: Selector
contentCreationDateSelector = mkSelector "contentCreationDate"

-- | @Selector@ for @setContentCreationDate:@
setContentCreationDateSelector :: Selector
setContentCreationDateSelector = mkSelector "setContentCreationDate:"

-- | @Selector@ for @contentModificationDate@
contentModificationDateSelector :: Selector
contentModificationDateSelector = mkSelector "contentModificationDate"

-- | @Selector@ for @setContentModificationDate:@
setContentModificationDateSelector :: Selector
setContentModificationDateSelector = mkSelector "setContentModificationDate:"

-- | @Selector@ for @addedDate@
addedDateSelector :: Selector
addedDateSelector = mkSelector "addedDate"

-- | @Selector@ for @setAddedDate:@
setAddedDateSelector :: Selector
setAddedDateSelector = mkSelector "setAddedDate:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @contactKeywords@
contactKeywordsSelector :: Selector
contactKeywordsSelector = mkSelector "contactKeywords"

-- | @Selector@ for @setContactKeywords:@
setContactKeywordsSelector :: Selector
setContactKeywordsSelector = mkSelector "setContactKeywords:"

-- | @Selector@ for @codecs@
codecsSelector :: Selector
codecsSelector = mkSelector "codecs"

-- | @Selector@ for @setCodecs:@
setCodecsSelector :: Selector
setCodecsSelector = mkSelector "setCodecs:"

-- | @Selector@ for @mediaTypes@
mediaTypesSelector :: Selector
mediaTypesSelector = mkSelector "mediaTypes"

-- | @Selector@ for @setMediaTypes:@
setMediaTypesSelector :: Selector
setMediaTypesSelector = mkSelector "setMediaTypes:"

-- | @Selector@ for @streamable@
streamableSelector :: Selector
streamableSelector = mkSelector "streamable"

-- | @Selector@ for @setStreamable:@
setStreamableSelector :: Selector
setStreamableSelector = mkSelector "setStreamable:"

-- | @Selector@ for @totalBitRate@
totalBitRateSelector :: Selector
totalBitRateSelector = mkSelector "totalBitRate"

-- | @Selector@ for @setTotalBitRate:@
setTotalBitRateSelector :: Selector
setTotalBitRateSelector = mkSelector "setTotalBitRate:"

-- | @Selector@ for @videoBitRate@
videoBitRateSelector :: Selector
videoBitRateSelector = mkSelector "videoBitRate"

-- | @Selector@ for @setVideoBitRate:@
setVideoBitRateSelector :: Selector
setVideoBitRateSelector = mkSelector "setVideoBitRate:"

-- | @Selector@ for @audioBitRate@
audioBitRateSelector :: Selector
audioBitRateSelector = mkSelector "audioBitRate"

-- | @Selector@ for @setAudioBitRate:@
setAudioBitRateSelector :: Selector
setAudioBitRateSelector = mkSelector "setAudioBitRate:"

-- | @Selector@ for @deliveryType@
deliveryTypeSelector :: Selector
deliveryTypeSelector = mkSelector "deliveryType"

-- | @Selector@ for @setDeliveryType:@
setDeliveryTypeSelector :: Selector
setDeliveryTypeSelector = mkSelector "setDeliveryType:"

-- | @Selector@ for @organizations@
organizationsSelector :: Selector
organizationsSelector = mkSelector "organizations"

-- | @Selector@ for @setOrganizations:@
setOrganizationsSelector :: Selector
setOrganizationsSelector = mkSelector "setOrganizations:"

-- | @Selector@ for @role@
roleSelector :: Selector
roleSelector = mkSelector "role"

-- | @Selector@ for @setRole:@
setRoleSelector :: Selector
setRoleSelector = mkSelector "setRole:"

-- | @Selector@ for @languages@
languagesSelector :: Selector
languagesSelector = mkSelector "languages"

-- | @Selector@ for @setLanguages:@
setLanguagesSelector :: Selector
setLanguagesSelector = mkSelector "setLanguages:"

-- | @Selector@ for @rights@
rightsSelector :: Selector
rightsSelector = mkSelector "rights"

-- | @Selector@ for @setRights:@
setRightsSelector :: Selector
setRightsSelector = mkSelector "setRights:"

-- | @Selector@ for @publishers@
publishersSelector :: Selector
publishersSelector = mkSelector "publishers"

-- | @Selector@ for @setPublishers:@
setPublishersSelector :: Selector
setPublishersSelector = mkSelector "setPublishers:"

-- | @Selector@ for @contributors@
contributorsSelector :: Selector
contributorsSelector = mkSelector "contributors"

-- | @Selector@ for @setContributors:@
setContributorsSelector :: Selector
setContributorsSelector = mkSelector "setContributors:"

-- | @Selector@ for @coverage@
coverageSelector :: Selector
coverageSelector = mkSelector "coverage"

-- | @Selector@ for @setCoverage:@
setCoverageSelector :: Selector
setCoverageSelector = mkSelector "setCoverage:"

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

-- | @Selector@ for @setRating:@
setRatingSelector :: Selector
setRatingSelector = mkSelector "setRating:"

-- | @Selector@ for @ratingDescription@
ratingDescriptionSelector :: Selector
ratingDescriptionSelector = mkSelector "ratingDescription"

-- | @Selector@ for @setRatingDescription:@
setRatingDescriptionSelector :: Selector
setRatingDescriptionSelector = mkSelector "setRatingDescription:"

-- | @Selector@ for @playCount@
playCountSelector :: Selector
playCountSelector = mkSelector "playCount"

-- | @Selector@ for @setPlayCount:@
setPlayCountSelector :: Selector
setPlayCountSelector = mkSelector "setPlayCount:"

-- | @Selector@ for @information@
informationSelector :: Selector
informationSelector = mkSelector "information"

-- | @Selector@ for @setInformation:@
setInformationSelector :: Selector
setInformationSelector = mkSelector "setInformation:"

-- | @Selector@ for @director@
directorSelector :: Selector
directorSelector = mkSelector "director"

-- | @Selector@ for @setDirector:@
setDirectorSelector :: Selector
setDirectorSelector = mkSelector "setDirector:"

-- | @Selector@ for @producer@
producerSelector :: Selector
producerSelector = mkSelector "producer"

-- | @Selector@ for @setProducer:@
setProducerSelector :: Selector
setProducerSelector = mkSelector "setProducer:"

-- | @Selector@ for @genre@
genreSelector :: Selector
genreSelector = mkSelector "genre"

-- | @Selector@ for @setGenre:@
setGenreSelector :: Selector
setGenreSelector = mkSelector "setGenre:"

-- | @Selector@ for @performers@
performersSelector :: Selector
performersSelector = mkSelector "performers"

-- | @Selector@ for @setPerformers:@
setPerformersSelector :: Selector
setPerformersSelector = mkSelector "setPerformers:"

-- | @Selector@ for @originalFormat@
originalFormatSelector :: Selector
originalFormatSelector = mkSelector "originalFormat"

-- | @Selector@ for @setOriginalFormat:@
setOriginalFormatSelector :: Selector
setOriginalFormatSelector = mkSelector "setOriginalFormat:"

-- | @Selector@ for @originalSource@
originalSourceSelector :: Selector
originalSourceSelector = mkSelector "originalSource"

-- | @Selector@ for @setOriginalSource:@
setOriginalSourceSelector :: Selector
setOriginalSourceSelector = mkSelector "setOriginalSource:"

-- | @Selector@ for @local@
localSelector :: Selector
localSelector = mkSelector "local"

-- | @Selector@ for @setLocal:@
setLocalSelector :: Selector
setLocalSelector = mkSelector "setLocal:"

-- | @Selector@ for @contentRating@
contentRatingSelector :: Selector
contentRatingSelector = mkSelector "contentRating"

-- | @Selector@ for @setContentRating:@
setContentRatingSelector :: Selector
setContentRatingSelector = mkSelector "setContentRating:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @accountIdentifier@
accountIdentifierSelector :: Selector
accountIdentifierSelector = mkSelector "accountIdentifier"

-- | @Selector@ for @setAccountIdentifier:@
setAccountIdentifierSelector :: Selector
setAccountIdentifierSelector = mkSelector "setAccountIdentifier:"

-- | @Selector@ for @accountHandles@
accountHandlesSelector :: Selector
accountHandlesSelector = mkSelector "accountHandles"

-- | @Selector@ for @setAccountHandles:@
setAccountHandlesSelector :: Selector
setAccountHandlesSelector = mkSelector "setAccountHandles:"

-- | @Selector@ for @HTMLContentData@
htmlContentDataSelector :: Selector
htmlContentDataSelector = mkSelector "HTMLContentData"

-- | @Selector@ for @setHTMLContentData:@
setHTMLContentDataSelector :: Selector
setHTMLContentDataSelector = mkSelector "setHTMLContentData:"

-- | @Selector@ for @textContent@
textContentSelector :: Selector
textContentSelector = mkSelector "textContent"

-- | @Selector@ for @setTextContent:@
setTextContentSelector :: Selector
setTextContentSelector = mkSelector "setTextContent:"

-- | @Selector@ for @authors@
authorsSelector :: Selector
authorsSelector = mkSelector "authors"

-- | @Selector@ for @setAuthors:@
setAuthorsSelector :: Selector
setAuthorsSelector = mkSelector "setAuthors:"

-- | @Selector@ for @primaryRecipients@
primaryRecipientsSelector :: Selector
primaryRecipientsSelector = mkSelector "primaryRecipients"

-- | @Selector@ for @setPrimaryRecipients:@
setPrimaryRecipientsSelector :: Selector
setPrimaryRecipientsSelector = mkSelector "setPrimaryRecipients:"

-- | @Selector@ for @additionalRecipients@
additionalRecipientsSelector :: Selector
additionalRecipientsSelector = mkSelector "additionalRecipients"

-- | @Selector@ for @setAdditionalRecipients:@
setAdditionalRecipientsSelector :: Selector
setAdditionalRecipientsSelector = mkSelector "setAdditionalRecipients:"

-- | @Selector@ for @hiddenAdditionalRecipients@
hiddenAdditionalRecipientsSelector :: Selector
hiddenAdditionalRecipientsSelector = mkSelector "hiddenAdditionalRecipients"

-- | @Selector@ for @setHiddenAdditionalRecipients:@
setHiddenAdditionalRecipientsSelector :: Selector
setHiddenAdditionalRecipientsSelector = mkSelector "setHiddenAdditionalRecipients:"

-- | @Selector@ for @emailHeaders@
emailHeadersSelector :: Selector
emailHeadersSelector = mkSelector "emailHeaders"

-- | @Selector@ for @setEmailHeaders:@
setEmailHeadersSelector :: Selector
setEmailHeadersSelector = mkSelector "setEmailHeaders:"

-- | @Selector@ for @mailboxIdentifiers@
mailboxIdentifiersSelector :: Selector
mailboxIdentifiersSelector = mkSelector "mailboxIdentifiers"

-- | @Selector@ for @setMailboxIdentifiers:@
setMailboxIdentifiersSelector :: Selector
setMailboxIdentifiersSelector = mkSelector "setMailboxIdentifiers:"

-- | @Selector@ for @authorNames@
authorNamesSelector :: Selector
authorNamesSelector = mkSelector "authorNames"

-- | @Selector@ for @setAuthorNames:@
setAuthorNamesSelector :: Selector
setAuthorNamesSelector = mkSelector "setAuthorNames:"

-- | @Selector@ for @recipientNames@
recipientNamesSelector :: Selector
recipientNamesSelector = mkSelector "recipientNames"

-- | @Selector@ for @setRecipientNames:@
setRecipientNamesSelector :: Selector
setRecipientNamesSelector = mkSelector "setRecipientNames:"

-- | @Selector@ for @authorEmailAddresses@
authorEmailAddressesSelector :: Selector
authorEmailAddressesSelector = mkSelector "authorEmailAddresses"

-- | @Selector@ for @setAuthorEmailAddresses:@
setAuthorEmailAddressesSelector :: Selector
setAuthorEmailAddressesSelector = mkSelector "setAuthorEmailAddresses:"

-- | @Selector@ for @recipientEmailAddresses@
recipientEmailAddressesSelector :: Selector
recipientEmailAddressesSelector = mkSelector "recipientEmailAddresses"

-- | @Selector@ for @setRecipientEmailAddresses:@
setRecipientEmailAddressesSelector :: Selector
setRecipientEmailAddressesSelector = mkSelector "setRecipientEmailAddresses:"

-- | @Selector@ for @authorAddresses@
authorAddressesSelector :: Selector
authorAddressesSelector = mkSelector "authorAddresses"

-- | @Selector@ for @setAuthorAddresses:@
setAuthorAddressesSelector :: Selector
setAuthorAddressesSelector = mkSelector "setAuthorAddresses:"

-- | @Selector@ for @recipientAddresses@
recipientAddressesSelector :: Selector
recipientAddressesSelector = mkSelector "recipientAddresses"

-- | @Selector@ for @setRecipientAddresses:@
setRecipientAddressesSelector :: Selector
setRecipientAddressesSelector = mkSelector "setRecipientAddresses:"

-- | @Selector@ for @phoneNumbers@
phoneNumbersSelector :: Selector
phoneNumbersSelector = mkSelector "phoneNumbers"

-- | @Selector@ for @setPhoneNumbers:@
setPhoneNumbersSelector :: Selector
setPhoneNumbersSelector = mkSelector "setPhoneNumbers:"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @setEmailAddresses:@
setEmailAddressesSelector :: Selector
setEmailAddressesSelector = mkSelector "setEmailAddresses:"

-- | @Selector@ for @instantMessageAddresses@
instantMessageAddressesSelector :: Selector
instantMessageAddressesSelector = mkSelector "instantMessageAddresses"

-- | @Selector@ for @setInstantMessageAddresses:@
setInstantMessageAddressesSelector :: Selector
setInstantMessageAddressesSelector = mkSelector "setInstantMessageAddresses:"

-- | @Selector@ for @likelyJunk@
likelyJunkSelector :: Selector
likelyJunkSelector = mkSelector "likelyJunk"

-- | @Selector@ for @setLikelyJunk:@
setLikelyJunkSelector :: Selector
setLikelyJunkSelector = mkSelector "setLikelyJunk:"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector
dueDateSelector = mkSelector "dueDate"

-- | @Selector@ for @setDueDate:@
setDueDateSelector :: Selector
setDueDateSelector = mkSelector "setDueDate:"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @setCompletionDate:@
setCompletionDateSelector :: Selector
setCompletionDateSelector = mkSelector "setCompletionDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector
setEndDateSelector = mkSelector "setEndDate:"

-- | @Selector@ for @importantDates@
importantDatesSelector :: Selector
importantDatesSelector = mkSelector "importantDates"

-- | @Selector@ for @setImportantDates:@
setImportantDatesSelector :: Selector
setImportantDatesSelector = mkSelector "setImportantDates:"

-- | @Selector@ for @allDay@
allDaySelector :: Selector
allDaySelector = mkSelector "allDay"

-- | @Selector@ for @setAllDay:@
setAllDaySelector :: Selector
setAllDaySelector = mkSelector "setAllDay:"

-- | @Selector@ for @subject@
subjectSelector :: Selector
subjectSelector = mkSelector "subject"

-- | @Selector@ for @setSubject:@
setSubjectSelector :: Selector
setSubjectSelector = mkSelector "setSubject:"

-- | @Selector@ for @theme@
themeSelector :: Selector
themeSelector = mkSelector "theme"

-- | @Selector@ for @setTheme:@
setThemeSelector :: Selector
setThemeSelector = mkSelector "setTheme:"

-- | @Selector@ for @contentDescription@
contentDescriptionSelector :: Selector
contentDescriptionSelector = mkSelector "contentDescription"

-- | @Selector@ for @setContentDescription:@
setContentDescriptionSelector :: Selector
setContentDescriptionSelector = mkSelector "setContentDescription:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @audiences@
audiencesSelector :: Selector
audiencesSelector = mkSelector "audiences"

-- | @Selector@ for @setAudiences:@
setAudiencesSelector :: Selector
setAudiencesSelector = mkSelector "setAudiences:"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @setFileSize:@
setFileSizeSelector :: Selector
setFileSizeSelector = mkSelector "setFileSize:"

-- | @Selector@ for @pageCount@
pageCountSelector :: Selector
pageCountSelector = mkSelector "pageCount"

-- | @Selector@ for @setPageCount:@
setPageCountSelector :: Selector
setPageCountSelector = mkSelector "setPageCount:"

-- | @Selector@ for @pageWidth@
pageWidthSelector :: Selector
pageWidthSelector = mkSelector "pageWidth"

-- | @Selector@ for @setPageWidth:@
setPageWidthSelector :: Selector
setPageWidthSelector = mkSelector "setPageWidth:"

-- | @Selector@ for @pageHeight@
pageHeightSelector :: Selector
pageHeightSelector = mkSelector "pageHeight"

-- | @Selector@ for @setPageHeight:@
setPageHeightSelector :: Selector
setPageHeightSelector = mkSelector "setPageHeight:"

-- | @Selector@ for @securityMethod@
securityMethodSelector :: Selector
securityMethodSelector = mkSelector "securityMethod"

-- | @Selector@ for @setSecurityMethod:@
setSecurityMethodSelector :: Selector
setSecurityMethodSelector = mkSelector "setSecurityMethod:"

-- | @Selector@ for @creator@
creatorSelector :: Selector
creatorSelector = mkSelector "creator"

-- | @Selector@ for @setCreator:@
setCreatorSelector :: Selector
setCreatorSelector = mkSelector "setCreator:"

-- | @Selector@ for @encodingApplications@
encodingApplicationsSelector :: Selector
encodingApplicationsSelector = mkSelector "encodingApplications"

-- | @Selector@ for @setEncodingApplications:@
setEncodingApplicationsSelector :: Selector
setEncodingApplicationsSelector = mkSelector "setEncodingApplications:"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @setKind:@
setKindSelector :: Selector
setKindSelector = mkSelector "setKind:"

-- | @Selector@ for @fontNames@
fontNamesSelector :: Selector
fontNamesSelector = mkSelector "fontNames"

-- | @Selector@ for @setFontNames:@
setFontNamesSelector :: Selector
setFontNamesSelector = mkSelector "setFontNames:"

-- | @Selector@ for @containerTitle@
containerTitleSelector :: Selector
containerTitleSelector = mkSelector "containerTitle"

-- | @Selector@ for @setContainerTitle:@
setContainerTitleSelector :: Selector
setContainerTitleSelector = mkSelector "setContainerTitle:"

-- | @Selector@ for @containerDisplayName@
containerDisplayNameSelector :: Selector
containerDisplayNameSelector = mkSelector "containerDisplayName"

-- | @Selector@ for @setContainerDisplayName:@
setContainerDisplayNameSelector :: Selector
setContainerDisplayNameSelector = mkSelector "setContainerDisplayName:"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @setContainerIdentifier:@
setContainerIdentifierSelector :: Selector
setContainerIdentifierSelector = mkSelector "setContainerIdentifier:"

-- | @Selector@ for @containerOrder@
containerOrderSelector :: Selector
containerOrderSelector = mkSelector "containerOrder"

-- | @Selector@ for @setContainerOrder:@
setContainerOrderSelector :: Selector
setContainerOrderSelector = mkSelector "setContainerOrder:"

-- | @Selector@ for @supportsPhoneCall@
supportsPhoneCallSelector :: Selector
supportsPhoneCallSelector = mkSelector "supportsPhoneCall"

-- | @Selector@ for @setSupportsPhoneCall:@
setSupportsPhoneCallSelector :: Selector
setSupportsPhoneCallSelector = mkSelector "setSupportsPhoneCall:"

-- | @Selector@ for @supportsNavigation@
supportsNavigationSelector :: Selector
supportsNavigationSelector = mkSelector "supportsNavigation"

-- | @Selector@ for @setSupportsNavigation:@
setSupportsNavigationSelector :: Selector
setSupportsNavigationSelector = mkSelector "setSupportsNavigation:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @alternateNames@
alternateNamesSelector :: Selector
alternateNamesSelector = mkSelector "alternateNames"

-- | @Selector@ for @setAlternateNames:@
setAlternateNamesSelector :: Selector
setAlternateNamesSelector = mkSelector "setAlternateNames:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @contentURL@
contentURLSelector :: Selector
contentURLSelector = mkSelector "contentURL"

-- | @Selector@ for @setContentURL:@
setContentURLSelector :: Selector
setContentURLSelector = mkSelector "setContentURL:"

-- | @Selector@ for @thumbnailURL@
thumbnailURLSelector :: Selector
thumbnailURLSelector = mkSelector "thumbnailURL"

-- | @Selector@ for @setThumbnailURL:@
setThumbnailURLSelector :: Selector
setThumbnailURLSelector = mkSelector "setThumbnailURL:"

-- | @Selector@ for @thumbnailData@
thumbnailDataSelector :: Selector
thumbnailDataSelector = mkSelector "thumbnailData"

-- | @Selector@ for @setThumbnailData:@
setThumbnailDataSelector :: Selector
setThumbnailDataSelector = mkSelector "setThumbnailData:"

-- | @Selector@ for @darkThumbnailURL@
darkThumbnailURLSelector :: Selector
darkThumbnailURLSelector = mkSelector "darkThumbnailURL"

-- | @Selector@ for @setDarkThumbnailURL:@
setDarkThumbnailURLSelector :: Selector
setDarkThumbnailURLSelector = mkSelector "setDarkThumbnailURL:"

-- | @Selector@ for @relatedUniqueIdentifier@
relatedUniqueIdentifierSelector :: Selector
relatedUniqueIdentifierSelector = mkSelector "relatedUniqueIdentifier"

-- | @Selector@ for @setRelatedUniqueIdentifier:@
setRelatedUniqueIdentifierSelector :: Selector
setRelatedUniqueIdentifierSelector = mkSelector "setRelatedUniqueIdentifier:"

-- | @Selector@ for @metadataModificationDate@
metadataModificationDateSelector :: Selector
metadataModificationDateSelector = mkSelector "metadataModificationDate"

-- | @Selector@ for @setMetadataModificationDate:@
setMetadataModificationDateSelector :: Selector
setMetadataModificationDateSelector = mkSelector "setMetadataModificationDate:"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @setContentType:@
setContentTypeSelector :: Selector
setContentTypeSelector = mkSelector "setContentType:"

-- | @Selector@ for @contentTypeTree@
contentTypeTreeSelector :: Selector
contentTypeTreeSelector = mkSelector "contentTypeTree"

-- | @Selector@ for @setContentTypeTree:@
setContentTypeTreeSelector :: Selector
setContentTypeTreeSelector = mkSelector "setContentTypeTree:"

-- | @Selector@ for @keywords@
keywordsSelector :: Selector
keywordsSelector = mkSelector "keywords"

-- | @Selector@ for @setKeywords:@
setKeywordsSelector :: Selector
setKeywordsSelector = mkSelector "setKeywords:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

