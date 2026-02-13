{-# LANGUAGE DataKinds #-}
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
  , isPriority
  , textContentSummary
  , transcribedTextContent
  , setTranscribedTextContent
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
  , actionIdentifiers
  , setActionIdentifiers
  , sharedItemContentType
  , setSharedItemContentType
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
  , weakRelatedUniqueIdentifier
  , setWeakRelatedUniqueIdentifier
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
  , userCreated
  , setUserCreated
  , userOwned
  , setUserOwned
  , userCurated
  , setUserCurated
  , rankingHint
  , setRankingHint
  , domainIdentifier
  , setDomainIdentifier
  , accountHandlesSelector
  , accountIdentifierSelector
  , acquisitionMakeSelector
  , acquisitionModelSelector
  , actionIdentifiersSelector
  , addedDateSelector
  , additionalRecipientsSelector
  , albumSelector
  , allDaySelector
  , alternateNamesSelector
  , altitudeSelector
  , apertureSelector
  , artistSelector
  , audiencesSelector
  , audioBitRateSelector
  , audioChannelCountSelector
  , audioEncodingApplicationSelector
  , audioSampleRateSelector
  , audioTrackNumberSelector
  , authorAddressesSelector
  , authorEmailAddressesSelector
  , authorNamesSelector
  , authorsSelector
  , bitsPerSampleSelector
  , cameraOwnerSelector
  , citySelector
  , codecsSelector
  , colorSpaceSelector
  , commentSelector
  , completionDateSelector
  , composerSelector
  , contactKeywordsSelector
  , containerDisplayNameSelector
  , containerIdentifierSelector
  , containerOrderSelector
  , containerTitleSelector
  , contentCreationDateSelector
  , contentDescriptionSelector
  , contentModificationDateSelector
  , contentRatingSelector
  , contentSourcesSelector
  , contentTypeSelector
  , contentTypeTreeSelector
  , contentURLSelector
  , contributorsSelector
  , copyrightSelector
  , countrySelector
  , coverageSelector
  , creatorSelector
  , darkThumbnailURLSelector
  , deliveryTypeSelector
  , directorSelector
  , displayNameSelector
  , domainIdentifierSelector
  , downloadedDateSelector
  , dueDateSelector
  , durationSelector
  , editorsSelector
  , emailAddressesSelector
  , emailHeadersSelector
  , encodingApplicationsSelector
  , endDateSelector
  , exifVersionSelector
  , exifgpsVersionSelector
  , exposureModeSelector
  , exposureProgramSelector
  , exposureTimeSelector
  , exposureTimeStringSelector
  , fNumberSelector
  , fileSizeSelector
  , flashOnSelector
  , focalLength35mmSelector
  , focalLengthSelector
  , fontNamesSelector
  , fullyFormattedAddressSelector
  , generalMIDISequenceSelector
  , genreSelector
  , gpsAreaInformationSelector
  , gpsDateStampSelector
  , gpsDestBearingSelector
  , gpsDestDistanceSelector
  , gpsDestLatitudeSelector
  , gpsDestLongitudeSelector
  , gpsDifferentalSelector
  , gpsMapDatumSelector
  , gpsMeasureModeSelector
  , gpsProcessingMethodSelector
  , gpsStatusSelector
  , gpsTrackSelector
  , gpsdopSelector
  , hasAlphaChannelSelector
  , headlineSelector
  , hiddenAdditionalRecipientsSelector
  , htmlContentDataSelector
  , identifierSelector
  , imageDirectionSelector
  , importantDatesSelector
  , informationSelector
  , initWithContentTypeSelector
  , initWithItemContentTypeSelector
  , instantMessageAddressesSelector
  , instructionsSelector
  , isPrioritySelector
  , isoSpeedSelector
  , keySignatureSelector
  , keywordsSelector
  , kindSelector
  , languagesSelector
  , lastUsedDateSelector
  , latitudeSelector
  , layerNamesSelector
  , lensModelSelector
  , likelyJunkSelector
  , localSelector
  , longitudeSelector
  , lyricistSelector
  , mailboxIdentifiersSelector
  , maxApertureSelector
  , mediaTypesSelector
  , metadataModificationDateSelector
  , meteringModeSelector
  , moveFromSelector
  , musicalGenreSelector
  , musicalInstrumentCategorySelector
  , musicalInstrumentNameSelector
  , namedLocationSelector
  , organizationsSelector
  , orientationSelector
  , originalFormatSelector
  , originalSourceSelector
  , pageCountSelector
  , pageHeightSelector
  , pageWidthSelector
  , participantsSelector
  , pathSelector
  , performersSelector
  , phoneNumbersSelector
  , pixelCountSelector
  , pixelHeightSelector
  , pixelWidthSelector
  , playCountSelector
  , postalCodeSelector
  , primaryRecipientsSelector
  , producerSelector
  , profileNameSelector
  , projectsSelector
  , publishersSelector
  , rankingHintSelector
  , ratingDescriptionSelector
  , ratingSelector
  , recipientAddressesSelector
  , recipientEmailAddressesSelector
  , recipientNamesSelector
  , recordingDateSelector
  , redEyeOnSelector
  , relatedUniqueIdentifierSelector
  , resolutionHeightDPISelector
  , resolutionWidthDPISelector
  , rightsSelector
  , roleSelector
  , securityMethodSelector
  , setAccountHandlesSelector
  , setAccountIdentifierSelector
  , setAcquisitionMakeSelector
  , setAcquisitionModelSelector
  , setActionIdentifiersSelector
  , setAddedDateSelector
  , setAdditionalRecipientsSelector
  , setAlbumSelector
  , setAllDaySelector
  , setAlternateNamesSelector
  , setAltitudeSelector
  , setApertureSelector
  , setArtistSelector
  , setAudiencesSelector
  , setAudioBitRateSelector
  , setAudioChannelCountSelector
  , setAudioEncodingApplicationSelector
  , setAudioSampleRateSelector
  , setAudioTrackNumberSelector
  , setAuthorAddressesSelector
  , setAuthorEmailAddressesSelector
  , setAuthorNamesSelector
  , setAuthorsSelector
  , setBitsPerSampleSelector
  , setCameraOwnerSelector
  , setCitySelector
  , setCodecsSelector
  , setColorSpaceSelector
  , setCommentSelector
  , setCompletionDateSelector
  , setComposerSelector
  , setContactKeywordsSelector
  , setContainerDisplayNameSelector
  , setContainerIdentifierSelector
  , setContainerOrderSelector
  , setContainerTitleSelector
  , setContentCreationDateSelector
  , setContentDescriptionSelector
  , setContentModificationDateSelector
  , setContentRatingSelector
  , setContentSourcesSelector
  , setContentTypeSelector
  , setContentTypeTreeSelector
  , setContentURLSelector
  , setContributorsSelector
  , setCopyrightSelector
  , setCountrySelector
  , setCoverageSelector
  , setCreatorSelector
  , setDarkThumbnailURLSelector
  , setDeliveryTypeSelector
  , setDirectorSelector
  , setDisplayNameSelector
  , setDomainIdentifierSelector
  , setDownloadedDateSelector
  , setDueDateSelector
  , setDurationSelector
  , setEXIFGPSVersionSelector
  , setEXIFVersionSelector
  , setEditorsSelector
  , setEmailAddressesSelector
  , setEmailHeadersSelector
  , setEncodingApplicationsSelector
  , setEndDateSelector
  , setExposureModeSelector
  , setExposureProgramSelector
  , setExposureTimeSelector
  , setExposureTimeStringSelector
  , setFNumberSelector
  , setFileSizeSelector
  , setFlashOnSelector
  , setFocalLength35mmSelector
  , setFocalLengthSelector
  , setFontNamesSelector
  , setFullyFormattedAddressSelector
  , setGPSAreaInformationSelector
  , setGPSDOPSelector
  , setGPSDateStampSelector
  , setGPSDestBearingSelector
  , setGPSDestDistanceSelector
  , setGPSDestLatitudeSelector
  , setGPSDestLongitudeSelector
  , setGPSDifferentalSelector
  , setGPSMapDatumSelector
  , setGPSMeasureModeSelector
  , setGPSProcessingMethodSelector
  , setGPSStatusSelector
  , setGPSTrackSelector
  , setGeneralMIDISequenceSelector
  , setGenreSelector
  , setHTMLContentDataSelector
  , setHasAlphaChannelSelector
  , setHeadlineSelector
  , setHiddenAdditionalRecipientsSelector
  , setISOSpeedSelector
  , setIdentifierSelector
  , setImageDirectionSelector
  , setImportantDatesSelector
  , setInformationSelector
  , setInstantMessageAddressesSelector
  , setInstructionsSelector
  , setKeySignatureSelector
  , setKeywordsSelector
  , setKindSelector
  , setLanguagesSelector
  , setLastUsedDateSelector
  , setLatitudeSelector
  , setLayerNamesSelector
  , setLensModelSelector
  , setLikelyJunkSelector
  , setLocalSelector
  , setLongitudeSelector
  , setLyricistSelector
  , setMailboxIdentifiersSelector
  , setMaxApertureSelector
  , setMediaTypesSelector
  , setMetadataModificationDateSelector
  , setMeteringModeSelector
  , setMusicalGenreSelector
  , setMusicalInstrumentCategorySelector
  , setMusicalInstrumentNameSelector
  , setNamedLocationSelector
  , setOrganizationsSelector
  , setOrientationSelector
  , setOriginalFormatSelector
  , setOriginalSourceSelector
  , setPageCountSelector
  , setPageHeightSelector
  , setPageWidthSelector
  , setParticipantsSelector
  , setPathSelector
  , setPerformersSelector
  , setPhoneNumbersSelector
  , setPixelCountSelector
  , setPixelHeightSelector
  , setPixelWidthSelector
  , setPlayCountSelector
  , setPostalCodeSelector
  , setPrimaryRecipientsSelector
  , setProducerSelector
  , setProfileNameSelector
  , setProjectsSelector
  , setPublishersSelector
  , setRankingHintSelector
  , setRatingDescriptionSelector
  , setRatingSelector
  , setRecipientAddressesSelector
  , setRecipientEmailAddressesSelector
  , setRecipientNamesSelector
  , setRecordingDateSelector
  , setRedEyeOnSelector
  , setRelatedUniqueIdentifierSelector
  , setResolutionHeightDPISelector
  , setResolutionWidthDPISelector
  , setRightsSelector
  , setRoleSelector
  , setSecurityMethodSelector
  , setSharedItemContentTypeSelector
  , setSpeedSelector
  , setStartDateSelector
  , setStateOrProvinceSelector
  , setStreamableSelector
  , setSubThoroughfareSelector
  , setSubjectSelector
  , setSupportsNavigationSelector
  , setSupportsPhoneCallSelector
  , setTempoSelector
  , setTextContentSelector
  , setThemeSelector
  , setThoroughfareSelector
  , setThumbnailDataSelector
  , setThumbnailURLSelector
  , setTimeSignatureSelector
  , setTimestampSelector
  , setTitleSelector
  , setTotalBitRateSelector
  , setTranscribedTextContentSelector
  , setURLSelector
  , setUserCreatedSelector
  , setUserCuratedSelector
  , setUserOwnedSelector
  , setValue_forCustomKeySelector
  , setVersionSelector
  , setVideoBitRateSelector
  , setWeakRelatedUniqueIdentifierSelector
  , setWhiteBalanceSelector
  , sharedItemContentTypeSelector
  , speedSelector
  , startDateSelector
  , stateOrProvinceSelector
  , streamableSelector
  , subThoroughfareSelector
  , subjectSelector
  , supportsNavigationSelector
  , supportsPhoneCallSelector
  , tempoSelector
  , textContentSelector
  , textContentSummarySelector
  , themeSelector
  , thoroughfareSelector
  , thumbnailDataSelector
  , thumbnailURLSelector
  , timeSignatureSelector
  , timestampSelector
  , titleSelector
  , totalBitRateSelector
  , transcribedTextContentSelector
  , urlSelector
  , userCreatedSelector
  , userCuratedSelector
  , userOwnedSelector
  , valueForCustomKeySelector
  , versionSelector
  , videoBitRateSelector
  , weakRelatedUniqueIdentifierSelector
  , whiteBalanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- initWithItemContentType:@
initWithItemContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString itemContentType) => csSearchableItemAttributeSet -> itemContentType -> IO (Id CSSearchableItemAttributeSet)
initWithItemContentType csSearchableItemAttributeSet itemContentType =
  sendOwnedMessage csSearchableItemAttributeSet initWithItemContentTypeSelector (toNSString itemContentType)

-- | @- initWithContentType:@
initWithContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsUTType contentType) => csSearchableItemAttributeSet -> contentType -> IO (Id CSSearchableItemAttributeSet)
initWithContentType csSearchableItemAttributeSet contentType =
  sendOwnedMessage csSearchableItemAttributeSet initWithContentTypeSelector (toUTType contentType)

-- | @- moveFrom:@
moveFrom :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsCSSearchableItemAttributeSet sourceAttributeSet) => csSearchableItemAttributeSet -> sourceAttributeSet -> IO ()
moveFrom csSearchableItemAttributeSet sourceAttributeSet =
  sendMessage csSearchableItemAttributeSet moveFromSelector (toCSSearchableItemAttributeSet sourceAttributeSet)

-- | @- setValue:forCustomKey:@
setValue_forCustomKey :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsCSCustomAttributeKey key) => csSearchableItemAttributeSet -> RawId -> key -> IO ()
setValue_forCustomKey csSearchableItemAttributeSet value key =
  sendMessage csSearchableItemAttributeSet setValue_forCustomKeySelector value (toCSCustomAttributeKey key)

-- | @- valueForCustomKey:@
valueForCustomKey :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsCSCustomAttributeKey key) => csSearchableItemAttributeSet -> key -> IO RawId
valueForCustomKey csSearchableItemAttributeSet key =
  sendMessage csSearchableItemAttributeSet valueForCustomKeySelector (toCSCustomAttributeKey key)

-- | @- headline@
headline :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
headline csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet headlineSelector

-- | @- setHeadline:@
setHeadline :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setHeadline csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setHeadlineSelector (toNSString value)

-- | @- instructions@
instructions :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
instructions csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet instructionsSelector

-- | @- setInstructions:@
setInstructions :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setInstructions csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setInstructionsSelector (toNSString value)

-- | @- thoroughfare@
thoroughfare :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
thoroughfare csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet thoroughfareSelector

-- | @- setThoroughfare:@
setThoroughfare :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setThoroughfare csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setThoroughfareSelector (toNSString value)

-- | @- subThoroughfare@
subThoroughfare :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
subThoroughfare csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet subThoroughfareSelector

-- | @- setSubThoroughfare:@
setSubThoroughfare :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setSubThoroughfare csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSubThoroughfareSelector (toNSString value)

-- | @- postalCode@
postalCode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
postalCode csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet postalCodeSelector

-- | @- setPostalCode:@
setPostalCode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setPostalCode csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPostalCodeSelector (toNSString value)

-- | @- city@
city :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
city csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet citySelector

-- | @- setCity:@
setCity :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCity csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCitySelector (toNSString value)

-- | @- stateOrProvince@
stateOrProvince :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
stateOrProvince csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet stateOrProvinceSelector

-- | @- setStateOrProvince:@
setStateOrProvince :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setStateOrProvince csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setStateOrProvinceSelector (toNSString value)

-- | @- country@
country :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
country csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet countrySelector

-- | @- setCountry:@
setCountry :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCountry csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCountrySelector (toNSString value)

-- | @- fullyFormattedAddress@
fullyFormattedAddress :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
fullyFormattedAddress csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet fullyFormattedAddressSelector

-- | @- setFullyFormattedAddress:@
setFullyFormattedAddress :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setFullyFormattedAddress csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFullyFormattedAddressSelector (toNSString value)

-- | @- altitude@
altitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
altitude csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet altitudeSelector

-- | @- setAltitude:@
setAltitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAltitude csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAltitudeSelector (toNSNumber value)

-- | @- latitude@
latitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
latitude csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet latitudeSelector

-- | @- setLatitude:@
setLatitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLatitude csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLatitudeSelector (toNSNumber value)

-- | @- longitude@
longitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
longitude csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet longitudeSelector

-- | @- setLongitude:@
setLongitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLongitude csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLongitudeSelector (toNSNumber value)

-- | @- speed@
speed :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
speed csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet speedSelector

-- | @- setSpeed:@
setSpeed :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setSpeed csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSpeedSelector (toNSNumber value)

-- | @- timestamp@
timestamp :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
timestamp csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet timestampSelector

-- | @- setTimestamp:@
setTimestamp :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setTimestamp csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTimestampSelector (toNSDate value)

-- | @- imageDirection@
imageDirection :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
imageDirection csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet imageDirectionSelector

-- | @- setImageDirection:@
setImageDirection :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setImageDirection csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setImageDirectionSelector (toNSNumber value)

-- | @- namedLocation@
namedLocation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
namedLocation csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet namedLocationSelector

-- | @- setNamedLocation:@
setNamedLocation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setNamedLocation csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setNamedLocationSelector (toNSString value)

-- | @- GPSTrack@
gpsTrack :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsTrack csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsTrackSelector

-- | @- setGPSTrack:@
setGPSTrack :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSTrack csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSTrackSelector (toNSNumber value)

-- | @- GPSStatus@
gpsStatus :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsStatus csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsStatusSelector

-- | @- setGPSStatus:@
setGPSStatus :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSStatus csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSStatusSelector (toNSString value)

-- | @- GPSMeasureMode@
gpsMeasureMode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsMeasureMode csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsMeasureModeSelector

-- | @- setGPSMeasureMode:@
setGPSMeasureMode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSMeasureMode csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSMeasureModeSelector (toNSString value)

-- | @- GPSDOP@
gpsdop :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsdop csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsdopSelector

-- | @- setGPSDOP:@
setGPSDOP :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDOP csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDOPSelector (toNSNumber value)

-- | @- GPSMapDatum@
gpsMapDatum :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsMapDatum csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsMapDatumSelector

-- | @- setGPSMapDatum:@
setGPSMapDatum :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSMapDatum csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSMapDatumSelector (toNSString value)

-- | @- GPSDestLatitude@
gpsDestLatitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestLatitude csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsDestLatitudeSelector

-- | @- setGPSDestLatitude:@
setGPSDestLatitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestLatitude csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDestLatitudeSelector (toNSNumber value)

-- | @- GPSDestLongitude@
gpsDestLongitude :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestLongitude csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsDestLongitudeSelector

-- | @- setGPSDestLongitude:@
setGPSDestLongitude :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestLongitude csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDestLongitudeSelector (toNSNumber value)

-- | @- GPSDestBearing@
gpsDestBearing :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestBearing csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsDestBearingSelector

-- | @- setGPSDestBearing:@
setGPSDestBearing :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestBearing csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDestBearingSelector (toNSNumber value)

-- | @- GPSDestDistance@
gpsDestDistance :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDestDistance csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsDestDistanceSelector

-- | @- setGPSDestDistance:@
setGPSDestDistance :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDestDistance csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDestDistanceSelector (toNSNumber value)

-- | @- GPSProcessingMethod@
gpsProcessingMethod :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsProcessingMethod csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsProcessingMethodSelector

-- | @- setGPSProcessingMethod:@
setGPSProcessingMethod :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSProcessingMethod csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSProcessingMethodSelector (toNSString value)

-- | @- GPSAreaInformation@
gpsAreaInformation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
gpsAreaInformation csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsAreaInformationSelector

-- | @- setGPSAreaInformation:@
setGPSAreaInformation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSAreaInformation csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSAreaInformationSelector (toNSString value)

-- | @- GPSDateStamp@
gpsDateStamp :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
gpsDateStamp csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsDateStampSelector

-- | @- setGPSDateStamp:@
setGPSDateStamp :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDateStamp csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDateStampSelector (toNSDate value)

-- | @- GPSDifferental@
gpsDifferental :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
gpsDifferental csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet gpsDifferentalSelector

-- | @- setGPSDifferental:@
setGPSDifferental :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGPSDifferental csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGPSDifferentalSelector (toNSNumber value)

-- | @- pixelHeight@
pixelHeight :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pixelHeight csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pixelHeightSelector

-- | @- setPixelHeight:@
setPixelHeight :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPixelHeight csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPixelHeightSelector (toNSNumber value)

-- | @- pixelWidth@
pixelWidth :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pixelWidth csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pixelWidthSelector

-- | @- setPixelWidth:@
setPixelWidth :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPixelWidth csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPixelWidthSelector (toNSNumber value)

-- | @- pixelCount@
pixelCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pixelCount csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pixelCountSelector

-- | @- setPixelCount:@
setPixelCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPixelCount csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPixelCountSelector (toNSNumber value)

-- | @- colorSpace@
colorSpace :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
colorSpace csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet colorSpaceSelector

-- | @- setColorSpace:@
setColorSpace :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setColorSpace csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setColorSpaceSelector (toNSString value)

-- | @- bitsPerSample@
bitsPerSample :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
bitsPerSample csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet bitsPerSampleSelector

-- | @- setBitsPerSample:@
setBitsPerSample :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setBitsPerSample csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setBitsPerSampleSelector (toNSNumber value)

-- | @- flashOn@
flashOn :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
flashOn csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet flashOnSelector

-- | @- setFlashOn:@
setFlashOn :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFlashOn csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFlashOnSelector (toNSNumber value)

-- | @- focalLength@
focalLength :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
focalLength csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet focalLengthSelector

-- | @- setFocalLength:@
setFocalLength :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFocalLength csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFocalLengthSelector (toNSNumber value)

-- | @- focalLength35mm@
focalLength35mm :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
focalLength35mm csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet focalLength35mmSelector

-- | @- setFocalLength35mm:@
setFocalLength35mm :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFocalLength35mm csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFocalLength35mmSelector (toNSNumber value)

-- | @- acquisitionMake@
acquisitionMake :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
acquisitionMake csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet acquisitionMakeSelector

-- | @- setAcquisitionMake:@
setAcquisitionMake :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAcquisitionMake csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAcquisitionMakeSelector (toNSString value)

-- | @- acquisitionModel@
acquisitionModel :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
acquisitionModel csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet acquisitionModelSelector

-- | @- setAcquisitionModel:@
setAcquisitionModel :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAcquisitionModel csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAcquisitionModelSelector (toNSString value)

-- | @- cameraOwner@
cameraOwner :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
cameraOwner csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet cameraOwnerSelector

-- | @- setCameraOwner:@
setCameraOwner :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCameraOwner csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCameraOwnerSelector (toNSString value)

-- | @- lensModel@
lensModel :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
lensModel csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet lensModelSelector

-- | @- setLensModel:@
setLensModel :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setLensModel csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLensModelSelector (toNSString value)

-- | @- ISOSpeed@
isoSpeed :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
isoSpeed csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet isoSpeedSelector

-- | @- setISOSpeed:@
setISOSpeed :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setISOSpeed csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setISOSpeedSelector (toNSNumber value)

-- | @- orientation@
orientation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
orientation csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet orientationSelector

-- | @- setOrientation:@
setOrientation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setOrientation csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setOrientationSelector (toNSNumber value)

-- | @- layerNames@
layerNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
layerNames csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet layerNamesSelector

-- | @- setLayerNames:@
setLayerNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setLayerNames csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLayerNamesSelector (toNSArray value)

-- | @- whiteBalance@
whiteBalance :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
whiteBalance csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet whiteBalanceSelector

-- | @- setWhiteBalance:@
setWhiteBalance :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setWhiteBalance csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setWhiteBalanceSelector (toNSNumber value)

-- | @- aperture@
aperture :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
aperture csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet apertureSelector

-- | @- setAperture:@
setAperture :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAperture csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setApertureSelector (toNSNumber value)

-- | @- profileName@
profileName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
profileName csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet profileNameSelector

-- | @- setProfileName:@
setProfileName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setProfileName csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setProfileNameSelector (toNSString value)

-- | @- resolutionWidthDPI@
resolutionWidthDPI :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
resolutionWidthDPI csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet resolutionWidthDPISelector

-- | @- setResolutionWidthDPI:@
setResolutionWidthDPI :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setResolutionWidthDPI csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setResolutionWidthDPISelector (toNSNumber value)

-- | @- resolutionHeightDPI@
resolutionHeightDPI :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
resolutionHeightDPI csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet resolutionHeightDPISelector

-- | @- setResolutionHeightDPI:@
setResolutionHeightDPI :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setResolutionHeightDPI csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setResolutionHeightDPISelector (toNSNumber value)

-- | @- exposureMode@
exposureMode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
exposureMode csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet exposureModeSelector

-- | @- setExposureMode:@
setExposureMode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureMode csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setExposureModeSelector (toNSNumber value)

-- | @- exposureTime@
exposureTime :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
exposureTime csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet exposureTimeSelector

-- | @- setExposureTime:@
setExposureTime :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureTime csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setExposureTimeSelector (toNSNumber value)

-- | @- EXIFVersion@
exifVersion :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exifVersion csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet exifVersionSelector

-- | @- setEXIFVersion:@
setEXIFVersion :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setEXIFVersion csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEXIFVersionSelector (toNSString value)

-- | @- EXIFGPSVersion@
exifgpsVersion :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exifgpsVersion csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet exifgpsVersionSelector

-- | @- setEXIFGPSVersion:@
setEXIFGPSVersion :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setEXIFGPSVersion csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEXIFGPSVersionSelector (toNSString value)

-- | @- hasAlphaChannel@
hasAlphaChannel :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
hasAlphaChannel csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet hasAlphaChannelSelector

-- | @- setHasAlphaChannel:@
setHasAlphaChannel :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setHasAlphaChannel csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setHasAlphaChannelSelector (toNSNumber value)

-- | @- redEyeOn@
redEyeOn :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
redEyeOn csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet redEyeOnSelector

-- | @- setRedEyeOn:@
setRedEyeOn :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setRedEyeOn csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRedEyeOnSelector (toNSNumber value)

-- | @- meteringMode@
meteringMode :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
meteringMode csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet meteringModeSelector

-- | @- setMeteringMode:@
setMeteringMode :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMeteringMode csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMeteringModeSelector (toNSString value)

-- | @- maxAperture@
maxAperture :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
maxAperture csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet maxApertureSelector

-- | @- setMaxAperture:@
setMaxAperture :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setMaxAperture csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMaxApertureSelector (toNSNumber value)

-- | @- fNumber@
fNumber :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
fNumber csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet fNumberSelector

-- | @- setFNumber:@
setFNumber :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFNumber csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFNumberSelector (toNSNumber value)

-- | @- exposureProgram@
exposureProgram :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exposureProgram csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet exposureProgramSelector

-- | @- setExposureProgram:@
setExposureProgram :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureProgram csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setExposureProgramSelector (toNSString value)

-- | @- exposureTimeString@
exposureTimeString :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
exposureTimeString csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet exposureTimeStringSelector

-- | @- setExposureTimeString:@
setExposureTimeString :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setExposureTimeString csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setExposureTimeStringSelector (toNSString value)

-- | @- audioSampleRate@
audioSampleRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioSampleRate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet audioSampleRateSelector

-- | @- setAudioSampleRate:@
setAudioSampleRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioSampleRate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAudioSampleRateSelector (toNSNumber value)

-- | @- audioChannelCount@
audioChannelCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioChannelCount csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet audioChannelCountSelector

-- | @- setAudioChannelCount:@
setAudioChannelCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioChannelCount csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAudioChannelCountSelector (toNSNumber value)

-- | @- tempo@
tempo :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
tempo csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet tempoSelector

-- | @- setTempo:@
setTempo :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setTempo csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTempoSelector (toNSNumber value)

-- | @- keySignature@
keySignature :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
keySignature csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet keySignatureSelector

-- | @- setKeySignature:@
setKeySignature :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setKeySignature csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setKeySignatureSelector (toNSString value)

-- | @- timeSignature@
timeSignature :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
timeSignature csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet timeSignatureSelector

-- | @- setTimeSignature:@
setTimeSignature :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTimeSignature csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTimeSignatureSelector (toNSString value)

-- | @- audioEncodingApplication@
audioEncodingApplication :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
audioEncodingApplication csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet audioEncodingApplicationSelector

-- | @- setAudioEncodingApplication:@
setAudioEncodingApplication :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioEncodingApplication csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAudioEncodingApplicationSelector (toNSString value)

-- | @- composer@
composer :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
composer csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet composerSelector

-- | @- setComposer:@
setComposer :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setComposer csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setComposerSelector (toNSString value)

-- | @- lyricist@
lyricist :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
lyricist csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet lyricistSelector

-- | @- setLyricist:@
setLyricist :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setLyricist csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLyricistSelector (toNSString value)

-- | @- album@
album :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
album csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet albumSelector

-- | @- setAlbum:@
setAlbum :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAlbum csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAlbumSelector (toNSString value)

-- | @- artist@
artist :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
artist csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet artistSelector

-- | @- setArtist:@
setArtist :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setArtist csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setArtistSelector (toNSString value)

-- | @- audioTrackNumber@
audioTrackNumber :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioTrackNumber csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet audioTrackNumberSelector

-- | @- setAudioTrackNumber:@
setAudioTrackNumber :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioTrackNumber csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAudioTrackNumberSelector (toNSNumber value)

-- | @- recordingDate@
recordingDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
recordingDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet recordingDateSelector

-- | @- setRecordingDate:@
setRecordingDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setRecordingDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRecordingDateSelector (toNSDate value)

-- | @- musicalGenre@
musicalGenre :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
musicalGenre csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet musicalGenreSelector

-- | @- setMusicalGenre:@
setMusicalGenre :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMusicalGenre csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMusicalGenreSelector (toNSString value)

-- | @- generalMIDISequence@
generalMIDISequence :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
generalMIDISequence csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet generalMIDISequenceSelector

-- | @- setGeneralMIDISequence:@
setGeneralMIDISequence :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setGeneralMIDISequence csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGeneralMIDISequenceSelector (toNSNumber value)

-- | @- musicalInstrumentCategory@
musicalInstrumentCategory :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
musicalInstrumentCategory csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet musicalInstrumentCategorySelector

-- | @- setMusicalInstrumentCategory:@
setMusicalInstrumentCategory :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMusicalInstrumentCategory csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMusicalInstrumentCategorySelector (toNSString value)

-- | @- musicalInstrumentName@
musicalInstrumentName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
musicalInstrumentName csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet musicalInstrumentNameSelector

-- | @- setMusicalInstrumentName:@
setMusicalInstrumentName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setMusicalInstrumentName csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMusicalInstrumentNameSelector (toNSString value)

-- | @- editors@
editors :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
editors csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet editorsSelector

-- | @- setEditors:@
setEditors :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setEditors csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEditorsSelector (toNSArray value)

-- | @- participants@
participants :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
participants csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet participantsSelector

-- | @- setParticipants:@
setParticipants :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setParticipants csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setParticipantsSelector (toNSArray value)

-- | @- projects@
projects :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
projects csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet projectsSelector

-- | @- setProjects:@
setProjects :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setProjects csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setProjectsSelector (toNSArray value)

-- | @- downloadedDate@
downloadedDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
downloadedDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet downloadedDateSelector

-- | @- setDownloadedDate:@
setDownloadedDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setDownloadedDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDownloadedDateSelector (toNSDate value)

-- | @- contentSources@
contentSources :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contentSources csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentSourcesSelector

-- | @- setContentSources:@
setContentSources :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContentSources csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentSourcesSelector (toNSArray value)

-- | @- comment@
comment :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
comment csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet commentSelector

-- | @- setComment:@
setComment :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setComment csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCommentSelector (toNSString value)

-- | @- copyright@
copyright :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
copyright csSearchableItemAttributeSet =
  sendOwnedMessage csSearchableItemAttributeSet copyrightSelector

-- | @- setCopyright:@
setCopyright :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCopyright csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCopyrightSelector (toNSString value)

-- | @- lastUsedDate@
lastUsedDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
lastUsedDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet lastUsedDateSelector

-- | @- setLastUsedDate:@
setLastUsedDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setLastUsedDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLastUsedDateSelector (toNSDate value)

-- | @- contentCreationDate@
contentCreationDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
contentCreationDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentCreationDateSelector

-- | @- setContentCreationDate:@
setContentCreationDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setContentCreationDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentCreationDateSelector (toNSDate value)

-- | @- contentModificationDate@
contentModificationDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
contentModificationDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentModificationDateSelector

-- | @- setContentModificationDate:@
setContentModificationDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setContentModificationDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentModificationDateSelector (toNSDate value)

-- | @- addedDate@
addedDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
addedDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet addedDateSelector

-- | @- setAddedDate:@
setAddedDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setAddedDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAddedDateSelector (toNSDate value)

-- | @- duration@
duration :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
duration csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet durationSelector

-- | @- setDuration:@
setDuration :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setDuration csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDurationSelector (toNSNumber value)

-- | @- contactKeywords@
contactKeywords :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contactKeywords csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contactKeywordsSelector

-- | @- setContactKeywords:@
setContactKeywords :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContactKeywords csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContactKeywordsSelector (toNSArray value)

-- | @- codecs@
codecs :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
codecs csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet codecsSelector

-- | @- setCodecs:@
setCodecs :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setCodecs csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCodecsSelector (toNSArray value)

-- | @- mediaTypes@
mediaTypes :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
mediaTypes csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet mediaTypesSelector

-- | @- setMediaTypes:@
setMediaTypes :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setMediaTypes csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMediaTypesSelector (toNSArray value)

-- | @- streamable@
streamable :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
streamable csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet streamableSelector

-- | @- setStreamable:@
setStreamable :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setStreamable csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setStreamableSelector (toNSNumber value)

-- | @- totalBitRate@
totalBitRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
totalBitRate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet totalBitRateSelector

-- | @- setTotalBitRate:@
setTotalBitRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setTotalBitRate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTotalBitRateSelector (toNSNumber value)

-- | @- videoBitRate@
videoBitRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
videoBitRate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet videoBitRateSelector

-- | @- setVideoBitRate:@
setVideoBitRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setVideoBitRate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setVideoBitRateSelector (toNSNumber value)

-- | @- audioBitRate@
audioBitRate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
audioBitRate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet audioBitRateSelector

-- | @- setAudioBitRate:@
setAudioBitRate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAudioBitRate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAudioBitRateSelector (toNSNumber value)

-- | @- deliveryType@
deliveryType :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
deliveryType csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet deliveryTypeSelector

-- | @- setDeliveryType:@
setDeliveryType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setDeliveryType csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDeliveryTypeSelector (toNSNumber value)

-- | @- organizations@
organizations :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
organizations csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet organizationsSelector

-- | @- setOrganizations:@
setOrganizations :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setOrganizations csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setOrganizationsSelector (toNSArray value)

-- | @- role@
role_ :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
role_ csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet roleSelector

-- | @- setRole:@
setRole :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRole csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRoleSelector (toNSString value)

-- | @- languages@
languages :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
languages csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet languagesSelector

-- | @- setLanguages:@
setLanguages :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setLanguages csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLanguagesSelector (toNSArray value)

-- | @- rights@
rights :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
rights csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet rightsSelector

-- | @- setRights:@
setRights :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRights csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRightsSelector (toNSString value)

-- | @- publishers@
publishers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
publishers csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet publishersSelector

-- | @- setPublishers:@
setPublishers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPublishers csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPublishersSelector (toNSArray value)

-- | @- contributors@
contributors :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contributors csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contributorsSelector

-- | @- setContributors:@
setContributors :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContributors csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContributorsSelector (toNSArray value)

-- | @- coverage@
coverage :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
coverage csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet coverageSelector

-- | @- setCoverage:@
setCoverage :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setCoverage csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCoverageSelector (toNSArray value)

-- | @- rating@
rating :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
rating csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet ratingSelector

-- | @- setRating:@
setRating :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setRating csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRatingSelector (toNSNumber value)

-- | @- ratingDescription@
ratingDescription :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
ratingDescription csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet ratingDescriptionSelector

-- | @- setRatingDescription:@
setRatingDescription :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRatingDescription csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRatingDescriptionSelector (toNSString value)

-- | @- playCount@
playCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
playCount csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet playCountSelector

-- | @- setPlayCount:@
setPlayCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPlayCount csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPlayCountSelector (toNSNumber value)

-- | @- information@
information :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
information csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet informationSelector

-- | @- setInformation:@
setInformation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setInformation csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setInformationSelector (toNSString value)

-- | @- director@
director :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
director csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet directorSelector

-- | @- setDirector:@
setDirector :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setDirector csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDirectorSelector (toNSString value)

-- | @- producer@
producer :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
producer csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet producerSelector

-- | @- setProducer:@
setProducer :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setProducer csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setProducerSelector (toNSString value)

-- | @- genre@
genre :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
genre csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet genreSelector

-- | @- setGenre:@
setGenre :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setGenre csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setGenreSelector (toNSString value)

-- | @- performers@
performers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
performers csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet performersSelector

-- | @- setPerformers:@
setPerformers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPerformers csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPerformersSelector (toNSArray value)

-- | @- originalFormat@
originalFormat :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
originalFormat csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet originalFormatSelector

-- | @- setOriginalFormat:@
setOriginalFormat :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setOriginalFormat csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setOriginalFormatSelector (toNSString value)

-- | @- originalSource@
originalSource :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
originalSource csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet originalSourceSelector

-- | @- setOriginalSource:@
setOriginalSource :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setOriginalSource csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setOriginalSourceSelector (toNSString value)

-- | @- local@
local :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
local csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet localSelector

-- | @- setLocal:@
setLocal :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLocal csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLocalSelector (toNSNumber value)

-- | @- contentRating@
contentRating :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
contentRating csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentRatingSelector

-- | @- setContentRating:@
setContentRating :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setContentRating csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentRatingSelector (toNSNumber value)

-- | @- URL@
url :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
url csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet urlSelector

-- | @- setURL:@
setURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setURL csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setURLSelector (toNSURL value)

-- | @- accountIdentifier@
accountIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
accountIdentifier csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet accountIdentifierSelector

-- | @- setAccountIdentifier:@
setAccountIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setAccountIdentifier csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAccountIdentifierSelector (toNSString value)

-- | @- accountHandles@
accountHandles :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
accountHandles csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet accountHandlesSelector

-- | @- setAccountHandles:@
setAccountHandles :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAccountHandles csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAccountHandlesSelector (toNSArray value)

-- | @- HTMLContentData@
htmlContentData :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSData)
htmlContentData csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet htmlContentDataSelector

-- | @- setHTMLContentData:@
setHTMLContentData :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSData value) => csSearchableItemAttributeSet -> value -> IO ()
setHTMLContentData csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setHTMLContentDataSelector (toNSData value)

-- | @- textContent@
textContent :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
textContent csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet textContentSelector

-- | @- setTextContent:@
setTextContent :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTextContent csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTextContentSelector (toNSString value)

-- | @- authors@
authors :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authors csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet authorsSelector

-- | @- setAuthors:@
setAuthors :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthors csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAuthorsSelector (toNSArray value)

-- | @- primaryRecipients@
primaryRecipients :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
primaryRecipients csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet primaryRecipientsSelector

-- | @- setPrimaryRecipients:@
setPrimaryRecipients :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPrimaryRecipients csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPrimaryRecipientsSelector (toNSArray value)

-- | @- additionalRecipients@
additionalRecipients :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
additionalRecipients csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet additionalRecipientsSelector

-- | @- setAdditionalRecipients:@
setAdditionalRecipients :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAdditionalRecipients csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAdditionalRecipientsSelector (toNSArray value)

-- | @- hiddenAdditionalRecipients@
hiddenAdditionalRecipients :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
hiddenAdditionalRecipients csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet hiddenAdditionalRecipientsSelector

-- | @- setHiddenAdditionalRecipients:@
setHiddenAdditionalRecipients :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setHiddenAdditionalRecipients csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setHiddenAdditionalRecipientsSelector (toNSArray value)

-- | @- emailHeaders@
emailHeaders :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDictionary)
emailHeaders csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet emailHeadersSelector

-- | @- setEmailHeaders:@
setEmailHeaders :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDictionary value) => csSearchableItemAttributeSet -> value -> IO ()
setEmailHeaders csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEmailHeadersSelector (toNSDictionary value)

-- | @- mailboxIdentifiers@
mailboxIdentifiers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
mailboxIdentifiers csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet mailboxIdentifiersSelector

-- | @- setMailboxIdentifiers:@
setMailboxIdentifiers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setMailboxIdentifiers csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMailboxIdentifiersSelector (toNSArray value)

-- | @- authorNames@
authorNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authorNames csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet authorNamesSelector

-- | @- setAuthorNames:@
setAuthorNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthorNames csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAuthorNamesSelector (toNSArray value)

-- | @- recipientNames@
recipientNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
recipientNames csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet recipientNamesSelector

-- | @- setRecipientNames:@
setRecipientNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setRecipientNames csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRecipientNamesSelector (toNSArray value)

-- | @- authorEmailAddresses@
authorEmailAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authorEmailAddresses csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet authorEmailAddressesSelector

-- | @- setAuthorEmailAddresses:@
setAuthorEmailAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthorEmailAddresses csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAuthorEmailAddressesSelector (toNSArray value)

-- | @- recipientEmailAddresses@
recipientEmailAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
recipientEmailAddresses csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet recipientEmailAddressesSelector

-- | @- setRecipientEmailAddresses:@
setRecipientEmailAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setRecipientEmailAddresses csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRecipientEmailAddressesSelector (toNSArray value)

-- | @- authorAddresses@
authorAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
authorAddresses csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet authorAddressesSelector

-- | @- setAuthorAddresses:@
setAuthorAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAuthorAddresses csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAuthorAddressesSelector (toNSArray value)

-- | @- recipientAddresses@
recipientAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
recipientAddresses csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet recipientAddressesSelector

-- | @- setRecipientAddresses:@
setRecipientAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setRecipientAddresses csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRecipientAddressesSelector (toNSArray value)

-- | @- phoneNumbers@
phoneNumbers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
phoneNumbers csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet phoneNumbersSelector

-- | @- setPhoneNumbers:@
setPhoneNumbers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setPhoneNumbers csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPhoneNumbersSelector (toNSArray value)

-- | @- emailAddresses@
emailAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
emailAddresses csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet emailAddressesSelector

-- | @- setEmailAddresses:@
setEmailAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setEmailAddresses csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEmailAddressesSelector (toNSArray value)

-- | @- instantMessageAddresses@
instantMessageAddresses :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
instantMessageAddresses csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet instantMessageAddressesSelector

-- | @- setInstantMessageAddresses:@
setInstantMessageAddresses :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setInstantMessageAddresses csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setInstantMessageAddressesSelector (toNSArray value)

-- | @- likelyJunk@
likelyJunk :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
likelyJunk csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet likelyJunkSelector

-- | @- setLikelyJunk:@
setLikelyJunk :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setLikelyJunk csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setLikelyJunkSelector (toNSNumber value)

-- | @- isPriority@
isPriority :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
isPriority csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet isPrioritySelector

-- | @- textContentSummary@
textContentSummary :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
textContentSummary csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet textContentSummarySelector

-- | @- transcribedTextContent@
transcribedTextContent :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
transcribedTextContent csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet transcribedTextContentSelector

-- | @- setTranscribedTextContent:@
setTranscribedTextContent :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setTranscribedTextContent csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTranscribedTextContentSelector value

-- | @- dueDate@
dueDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
dueDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet dueDateSelector

-- | @- setDueDate:@
setDueDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setDueDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDueDateSelector (toNSDate value)

-- | @- completionDate@
completionDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
completionDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet completionDateSelector

-- | @- setCompletionDate:@
setCompletionDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setCompletionDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCompletionDateSelector (toNSDate value)

-- | @- startDate@
startDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
startDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet startDateSelector

-- | @- setStartDate:@
setStartDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setStartDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setStartDateSelector (toNSDate value)

-- | @- endDate@
endDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
endDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet endDateSelector

-- | @- setEndDate:@
setEndDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setEndDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEndDateSelector (toNSDate value)

-- | @- importantDates@
importantDates :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
importantDates csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet importantDatesSelector

-- | @- setImportantDates:@
setImportantDates :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setImportantDates csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setImportantDatesSelector (toNSArray value)

-- | @- allDay@
allDay :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
allDay csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet allDaySelector

-- | @- setAllDay:@
setAllDay :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setAllDay csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAllDaySelector (toNSNumber value)

-- | Subject of the this item.
--
-- ObjC selector: @- subject@
subject :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
subject csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet subjectSelector

-- | Subject of the this item.
--
-- ObjC selector: @- setSubject:@
setSubject :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setSubject csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSubjectSelector (toNSString value)

-- | @- theme@
theme :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
theme csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet themeSelector

-- | @- setTheme:@
setTheme :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTheme csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setThemeSelector (toNSString value)

-- | @- contentDescription@
contentDescription :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
contentDescription csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentDescriptionSelector

-- | @- setContentDescription:@
setContentDescription :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContentDescription csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentDescriptionSelector (toNSString value)

-- | @- identifier@
identifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
identifier csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setIdentifier csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setIdentifierSelector (toNSString value)

-- | @- audiences@
audiences :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
audiences csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet audiencesSelector

-- | @- setAudiences:@
setAudiences :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAudiences csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAudiencesSelector (toNSArray value)

-- | @- fileSize@
fileSize :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
fileSize csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet fileSizeSelector

-- | @- setFileSize:@
setFileSize :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setFileSize csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFileSizeSelector (toNSNumber value)

-- | @- pageCount@
pageCount :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pageCount csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pageCountSelector

-- | @- setPageCount:@
setPageCount :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPageCount csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPageCountSelector (toNSNumber value)

-- | @- pageWidth@
pageWidth :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pageWidth csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pageWidthSelector

-- | @- setPageWidth:@
setPageWidth :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPageWidth csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPageWidthSelector (toNSNumber value)

-- | @- pageHeight@
pageHeight :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
pageHeight csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pageHeightSelector

-- | @- setPageHeight:@
setPageHeight :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setPageHeight csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPageHeightSelector (toNSNumber value)

-- | @- securityMethod@
securityMethod :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
securityMethod csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet securityMethodSelector

-- | @- setSecurityMethod:@
setSecurityMethod :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setSecurityMethod csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSecurityMethodSelector (toNSString value)

-- | @- creator@
creator :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
creator csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet creatorSelector

-- | @- setCreator:@
setCreator :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setCreator csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setCreatorSelector (toNSString value)

-- | @- encodingApplications@
encodingApplications :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
encodingApplications csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet encodingApplicationsSelector

-- | @- setEncodingApplications:@
setEncodingApplications :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setEncodingApplications csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setEncodingApplicationsSelector (toNSArray value)

-- | @- kind@
kind :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
kind csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet kindSelector

-- | @- setKind:@
setKind :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setKind csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setKindSelector (toNSString value)

-- | @- fontNames@
fontNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
fontNames csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet fontNamesSelector

-- | @- setFontNames:@
setFontNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setFontNames csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setFontNamesSelector (toNSArray value)

-- | @- containerTitle@
containerTitle :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
containerTitle csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet containerTitleSelector

-- | @- setContainerTitle:@
setContainerTitle :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerTitle csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContainerTitleSelector (toNSString value)

-- | @- containerDisplayName@
containerDisplayName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
containerDisplayName csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet containerDisplayNameSelector

-- | @- setContainerDisplayName:@
setContainerDisplayName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerDisplayName csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContainerDisplayNameSelector (toNSString value)

-- | @- containerIdentifier@
containerIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
containerIdentifier csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet containerIdentifierSelector

-- | @- setContainerIdentifier:@
setContainerIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerIdentifier csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContainerIdentifierSelector (toNSString value)

-- | @- containerOrder@
containerOrder :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
containerOrder csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet containerOrderSelector

-- | @- setContainerOrder:@
setContainerOrder :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setContainerOrder csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContainerOrderSelector (toNSNumber value)

-- | @- supportsPhoneCall@
supportsPhoneCall :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
supportsPhoneCall csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet supportsPhoneCallSelector

-- | @- setSupportsPhoneCall:@
setSupportsPhoneCall :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setSupportsPhoneCall csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSupportsPhoneCallSelector (toNSNumber value)

-- | @- supportsNavigation@
supportsNavigation :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSNumber)
supportsNavigation csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet supportsNavigationSelector

-- | @- setSupportsNavigation:@
setSupportsNavigation :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSNumber value) => csSearchableItemAttributeSet -> value -> IO ()
setSupportsNavigation csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSupportsNavigationSelector (toNSNumber value)

-- | @- actionIdentifiers@
actionIdentifiers :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
actionIdentifiers csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet actionIdentifiersSelector

-- | @- setActionIdentifiers:@
setActionIdentifiers :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setActionIdentifiers csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setActionIdentifiersSelector (toNSArray value)

-- | @- sharedItemContentType@
sharedItemContentType :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id UTType)
sharedItemContentType csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet sharedItemContentTypeSelector

-- | @- setSharedItemContentType:@
setSharedItemContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsUTType value) => csSearchableItemAttributeSet -> value -> IO ()
setSharedItemContentType csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setSharedItemContentTypeSelector (toUTType value)

-- | @- displayName@
displayName :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
displayName csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet displayNameSelector

-- | @- setDisplayName:@
setDisplayName :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setDisplayName csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDisplayNameSelector (toNSString value)

-- | @- alternateNames@
alternateNames :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
alternateNames csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet alternateNamesSelector

-- | @- setAlternateNames:@
setAlternateNames :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setAlternateNames csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setAlternateNamesSelector (toNSArray value)

-- | @- path@
path :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
path csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet pathSelector

-- | @- setPath:@
setPath :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setPath csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setPathSelector (toNSString value)

-- | @- contentURL@
contentURL :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
contentURL csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentURLSelector

-- | @- setContentURL:@
setContentURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setContentURL csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentURLSelector (toNSURL value)

-- | @- thumbnailURL@
thumbnailURL :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
thumbnailURL csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet thumbnailURLSelector

-- | @- setThumbnailURL:@
setThumbnailURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setThumbnailURL csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setThumbnailURLSelector (toNSURL value)

-- | @- thumbnailData@
thumbnailData :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSData)
thumbnailData csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet thumbnailDataSelector

-- | @- setThumbnailData:@
setThumbnailData :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSData value) => csSearchableItemAttributeSet -> value -> IO ()
setThumbnailData csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setThumbnailDataSelector (toNSData value)

-- | @- darkThumbnailURL@
darkThumbnailURL :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSURL)
darkThumbnailURL csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet darkThumbnailURLSelector

-- | @- setDarkThumbnailURL:@
setDarkThumbnailURL :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSURL value) => csSearchableItemAttributeSet -> value -> IO ()
setDarkThumbnailURL csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDarkThumbnailURLSelector (toNSURL value)

-- | @- relatedUniqueIdentifier@
relatedUniqueIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
relatedUniqueIdentifier csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet relatedUniqueIdentifierSelector

-- | @- setRelatedUniqueIdentifier:@
setRelatedUniqueIdentifier :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setRelatedUniqueIdentifier csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRelatedUniqueIdentifierSelector (toNSString value)

-- | @- weakRelatedUniqueIdentifier@
weakRelatedUniqueIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
weakRelatedUniqueIdentifier csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet weakRelatedUniqueIdentifierSelector

-- | @- setWeakRelatedUniqueIdentifier:@
setWeakRelatedUniqueIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setWeakRelatedUniqueIdentifier csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setWeakRelatedUniqueIdentifierSelector value

-- | @- metadataModificationDate@
metadataModificationDate :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSDate)
metadataModificationDate csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet metadataModificationDateSelector

-- | @- setMetadataModificationDate:@
setMetadataModificationDate :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSDate value) => csSearchableItemAttributeSet -> value -> IO ()
setMetadataModificationDate csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setMetadataModificationDateSelector (toNSDate value)

-- | @- contentType@
contentType :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
contentType csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentTypeSelector

-- | @- setContentType:@
setContentType :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setContentType csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentTypeSelector (toNSString value)

-- | @- contentTypeTree@
contentTypeTree :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
contentTypeTree csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet contentTypeTreeSelector

-- | @- setContentTypeTree:@
setContentTypeTree :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setContentTypeTree csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setContentTypeTreeSelector (toNSArray value)

-- | @- keywords@
keywords :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSArray)
keywords csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet keywordsSelector

-- | @- setKeywords:@
setKeywords :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSArray value) => csSearchableItemAttributeSet -> value -> IO ()
setKeywords csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setKeywordsSelector (toNSArray value)

-- | @- title@
title :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
title csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet titleSelector

-- | @- setTitle:@
setTitle :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setTitle csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setTitleSelector (toNSString value)

-- | @- version@
version :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO (Id NSString)
version csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet versionSelector

-- | @- setVersion:@
setVersion :: (IsCSSearchableItemAttributeSet csSearchableItemAttributeSet, IsNSString value) => csSearchableItemAttributeSet -> value -> IO ()
setVersion csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setVersionSelector (toNSString value)

-- | @- userCreated@
userCreated :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
userCreated csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet userCreatedSelector

-- | @- setUserCreated:@
setUserCreated :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setUserCreated csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setUserCreatedSelector value

-- | @- userOwned@
userOwned :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
userOwned csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet userOwnedSelector

-- | @- setUserOwned:@
setUserOwned :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setUserOwned csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setUserOwnedSelector value

-- | @- userCurated@
userCurated :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
userCurated csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet userCuratedSelector

-- | @- setUserCurated:@
setUserCurated :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setUserCurated csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setUserCuratedSelector value

-- | @- rankingHint@
rankingHint :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
rankingHint csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet rankingHintSelector

-- | @- setRankingHint:@
setRankingHint :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setRankingHint csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setRankingHintSelector value

-- | @- domainIdentifier@
domainIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> IO RawId
domainIdentifier csSearchableItemAttributeSet =
  sendMessage csSearchableItemAttributeSet domainIdentifierSelector

-- | @- setDomainIdentifier:@
setDomainIdentifier :: IsCSSearchableItemAttributeSet csSearchableItemAttributeSet => csSearchableItemAttributeSet -> RawId -> IO ()
setDomainIdentifier csSearchableItemAttributeSet value =
  sendMessage csSearchableItemAttributeSet setDomainIdentifierSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemContentType:@
initWithItemContentTypeSelector :: Selector '[Id NSString] (Id CSSearchableItemAttributeSet)
initWithItemContentTypeSelector = mkSelector "initWithItemContentType:"

-- | @Selector@ for @initWithContentType:@
initWithContentTypeSelector :: Selector '[Id UTType] (Id CSSearchableItemAttributeSet)
initWithContentTypeSelector = mkSelector "initWithContentType:"

-- | @Selector@ for @moveFrom:@
moveFromSelector :: Selector '[Id CSSearchableItemAttributeSet] ()
moveFromSelector = mkSelector "moveFrom:"

-- | @Selector@ for @setValue:forCustomKey:@
setValue_forCustomKeySelector :: Selector '[RawId, Id CSCustomAttributeKey] ()
setValue_forCustomKeySelector = mkSelector "setValue:forCustomKey:"

-- | @Selector@ for @valueForCustomKey:@
valueForCustomKeySelector :: Selector '[Id CSCustomAttributeKey] RawId
valueForCustomKeySelector = mkSelector "valueForCustomKey:"

-- | @Selector@ for @headline@
headlineSelector :: Selector '[] (Id NSString)
headlineSelector = mkSelector "headline"

-- | @Selector@ for @setHeadline:@
setHeadlineSelector :: Selector '[Id NSString] ()
setHeadlineSelector = mkSelector "setHeadline:"

-- | @Selector@ for @instructions@
instructionsSelector :: Selector '[] (Id NSString)
instructionsSelector = mkSelector "instructions"

-- | @Selector@ for @setInstructions:@
setInstructionsSelector :: Selector '[Id NSString] ()
setInstructionsSelector = mkSelector "setInstructions:"

-- | @Selector@ for @thoroughfare@
thoroughfareSelector :: Selector '[] (Id NSString)
thoroughfareSelector = mkSelector "thoroughfare"

-- | @Selector@ for @setThoroughfare:@
setThoroughfareSelector :: Selector '[Id NSString] ()
setThoroughfareSelector = mkSelector "setThoroughfare:"

-- | @Selector@ for @subThoroughfare@
subThoroughfareSelector :: Selector '[] (Id NSString)
subThoroughfareSelector = mkSelector "subThoroughfare"

-- | @Selector@ for @setSubThoroughfare:@
setSubThoroughfareSelector :: Selector '[Id NSString] ()
setSubThoroughfareSelector = mkSelector "setSubThoroughfare:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector '[Id NSString] ()
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @city@
citySelector :: Selector '[] (Id NSString)
citySelector = mkSelector "city"

-- | @Selector@ for @setCity:@
setCitySelector :: Selector '[Id NSString] ()
setCitySelector = mkSelector "setCity:"

-- | @Selector@ for @stateOrProvince@
stateOrProvinceSelector :: Selector '[] (Id NSString)
stateOrProvinceSelector = mkSelector "stateOrProvince"

-- | @Selector@ for @setStateOrProvince:@
setStateOrProvinceSelector :: Selector '[Id NSString] ()
setStateOrProvinceSelector = mkSelector "setStateOrProvince:"

-- | @Selector@ for @country@
countrySelector :: Selector '[] (Id NSString)
countrySelector = mkSelector "country"

-- | @Selector@ for @setCountry:@
setCountrySelector :: Selector '[Id NSString] ()
setCountrySelector = mkSelector "setCountry:"

-- | @Selector@ for @fullyFormattedAddress@
fullyFormattedAddressSelector :: Selector '[] (Id NSString)
fullyFormattedAddressSelector = mkSelector "fullyFormattedAddress"

-- | @Selector@ for @setFullyFormattedAddress:@
setFullyFormattedAddressSelector :: Selector '[Id NSString] ()
setFullyFormattedAddressSelector = mkSelector "setFullyFormattedAddress:"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector '[] (Id NSNumber)
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @setAltitude:@
setAltitudeSelector :: Selector '[Id NSNumber] ()
setAltitudeSelector = mkSelector "setAltitude:"

-- | @Selector@ for @latitude@
latitudeSelector :: Selector '[] (Id NSNumber)
latitudeSelector = mkSelector "latitude"

-- | @Selector@ for @setLatitude:@
setLatitudeSelector :: Selector '[Id NSNumber] ()
setLatitudeSelector = mkSelector "setLatitude:"

-- | @Selector@ for @longitude@
longitudeSelector :: Selector '[] (Id NSNumber)
longitudeSelector = mkSelector "longitude"

-- | @Selector@ for @setLongitude:@
setLongitudeSelector :: Selector '[Id NSNumber] ()
setLongitudeSelector = mkSelector "setLongitude:"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] (Id NSNumber)
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[Id NSNumber] ()
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSDate)
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @setTimestamp:@
setTimestampSelector :: Selector '[Id NSDate] ()
setTimestampSelector = mkSelector "setTimestamp:"

-- | @Selector@ for @imageDirection@
imageDirectionSelector :: Selector '[] (Id NSNumber)
imageDirectionSelector = mkSelector "imageDirection"

-- | @Selector@ for @setImageDirection:@
setImageDirectionSelector :: Selector '[Id NSNumber] ()
setImageDirectionSelector = mkSelector "setImageDirection:"

-- | @Selector@ for @namedLocation@
namedLocationSelector :: Selector '[] (Id NSString)
namedLocationSelector = mkSelector "namedLocation"

-- | @Selector@ for @setNamedLocation:@
setNamedLocationSelector :: Selector '[Id NSString] ()
setNamedLocationSelector = mkSelector "setNamedLocation:"

-- | @Selector@ for @GPSTrack@
gpsTrackSelector :: Selector '[] (Id NSNumber)
gpsTrackSelector = mkSelector "GPSTrack"

-- | @Selector@ for @setGPSTrack:@
setGPSTrackSelector :: Selector '[Id NSNumber] ()
setGPSTrackSelector = mkSelector "setGPSTrack:"

-- | @Selector@ for @GPSStatus@
gpsStatusSelector :: Selector '[] (Id NSString)
gpsStatusSelector = mkSelector "GPSStatus"

-- | @Selector@ for @setGPSStatus:@
setGPSStatusSelector :: Selector '[Id NSString] ()
setGPSStatusSelector = mkSelector "setGPSStatus:"

-- | @Selector@ for @GPSMeasureMode@
gpsMeasureModeSelector :: Selector '[] (Id NSString)
gpsMeasureModeSelector = mkSelector "GPSMeasureMode"

-- | @Selector@ for @setGPSMeasureMode:@
setGPSMeasureModeSelector :: Selector '[Id NSString] ()
setGPSMeasureModeSelector = mkSelector "setGPSMeasureMode:"

-- | @Selector@ for @GPSDOP@
gpsdopSelector :: Selector '[] (Id NSNumber)
gpsdopSelector = mkSelector "GPSDOP"

-- | @Selector@ for @setGPSDOP:@
setGPSDOPSelector :: Selector '[Id NSNumber] ()
setGPSDOPSelector = mkSelector "setGPSDOP:"

-- | @Selector@ for @GPSMapDatum@
gpsMapDatumSelector :: Selector '[] (Id NSString)
gpsMapDatumSelector = mkSelector "GPSMapDatum"

-- | @Selector@ for @setGPSMapDatum:@
setGPSMapDatumSelector :: Selector '[Id NSString] ()
setGPSMapDatumSelector = mkSelector "setGPSMapDatum:"

-- | @Selector@ for @GPSDestLatitude@
gpsDestLatitudeSelector :: Selector '[] (Id NSNumber)
gpsDestLatitudeSelector = mkSelector "GPSDestLatitude"

-- | @Selector@ for @setGPSDestLatitude:@
setGPSDestLatitudeSelector :: Selector '[Id NSNumber] ()
setGPSDestLatitudeSelector = mkSelector "setGPSDestLatitude:"

-- | @Selector@ for @GPSDestLongitude@
gpsDestLongitudeSelector :: Selector '[] (Id NSNumber)
gpsDestLongitudeSelector = mkSelector "GPSDestLongitude"

-- | @Selector@ for @setGPSDestLongitude:@
setGPSDestLongitudeSelector :: Selector '[Id NSNumber] ()
setGPSDestLongitudeSelector = mkSelector "setGPSDestLongitude:"

-- | @Selector@ for @GPSDestBearing@
gpsDestBearingSelector :: Selector '[] (Id NSNumber)
gpsDestBearingSelector = mkSelector "GPSDestBearing"

-- | @Selector@ for @setGPSDestBearing:@
setGPSDestBearingSelector :: Selector '[Id NSNumber] ()
setGPSDestBearingSelector = mkSelector "setGPSDestBearing:"

-- | @Selector@ for @GPSDestDistance@
gpsDestDistanceSelector :: Selector '[] (Id NSNumber)
gpsDestDistanceSelector = mkSelector "GPSDestDistance"

-- | @Selector@ for @setGPSDestDistance:@
setGPSDestDistanceSelector :: Selector '[Id NSNumber] ()
setGPSDestDistanceSelector = mkSelector "setGPSDestDistance:"

-- | @Selector@ for @GPSProcessingMethod@
gpsProcessingMethodSelector :: Selector '[] (Id NSString)
gpsProcessingMethodSelector = mkSelector "GPSProcessingMethod"

-- | @Selector@ for @setGPSProcessingMethod:@
setGPSProcessingMethodSelector :: Selector '[Id NSString] ()
setGPSProcessingMethodSelector = mkSelector "setGPSProcessingMethod:"

-- | @Selector@ for @GPSAreaInformation@
gpsAreaInformationSelector :: Selector '[] (Id NSString)
gpsAreaInformationSelector = mkSelector "GPSAreaInformation"

-- | @Selector@ for @setGPSAreaInformation:@
setGPSAreaInformationSelector :: Selector '[Id NSString] ()
setGPSAreaInformationSelector = mkSelector "setGPSAreaInformation:"

-- | @Selector@ for @GPSDateStamp@
gpsDateStampSelector :: Selector '[] (Id NSDate)
gpsDateStampSelector = mkSelector "GPSDateStamp"

-- | @Selector@ for @setGPSDateStamp:@
setGPSDateStampSelector :: Selector '[Id NSDate] ()
setGPSDateStampSelector = mkSelector "setGPSDateStamp:"

-- | @Selector@ for @GPSDifferental@
gpsDifferentalSelector :: Selector '[] (Id NSNumber)
gpsDifferentalSelector = mkSelector "GPSDifferental"

-- | @Selector@ for @setGPSDifferental:@
setGPSDifferentalSelector :: Selector '[Id NSNumber] ()
setGPSDifferentalSelector = mkSelector "setGPSDifferental:"

-- | @Selector@ for @pixelHeight@
pixelHeightSelector :: Selector '[] (Id NSNumber)
pixelHeightSelector = mkSelector "pixelHeight"

-- | @Selector@ for @setPixelHeight:@
setPixelHeightSelector :: Selector '[Id NSNumber] ()
setPixelHeightSelector = mkSelector "setPixelHeight:"

-- | @Selector@ for @pixelWidth@
pixelWidthSelector :: Selector '[] (Id NSNumber)
pixelWidthSelector = mkSelector "pixelWidth"

-- | @Selector@ for @setPixelWidth:@
setPixelWidthSelector :: Selector '[Id NSNumber] ()
setPixelWidthSelector = mkSelector "setPixelWidth:"

-- | @Selector@ for @pixelCount@
pixelCountSelector :: Selector '[] (Id NSNumber)
pixelCountSelector = mkSelector "pixelCount"

-- | @Selector@ for @setPixelCount:@
setPixelCountSelector :: Selector '[Id NSNumber] ()
setPixelCountSelector = mkSelector "setPixelCount:"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSString)
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @setColorSpace:@
setColorSpaceSelector :: Selector '[Id NSString] ()
setColorSpaceSelector = mkSelector "setColorSpace:"

-- | @Selector@ for @bitsPerSample@
bitsPerSampleSelector :: Selector '[] (Id NSNumber)
bitsPerSampleSelector = mkSelector "bitsPerSample"

-- | @Selector@ for @setBitsPerSample:@
setBitsPerSampleSelector :: Selector '[Id NSNumber] ()
setBitsPerSampleSelector = mkSelector "setBitsPerSample:"

-- | @Selector@ for @flashOn@
flashOnSelector :: Selector '[] (Id NSNumber)
flashOnSelector = mkSelector "flashOn"

-- | @Selector@ for @setFlashOn:@
setFlashOnSelector :: Selector '[Id NSNumber] ()
setFlashOnSelector = mkSelector "setFlashOn:"

-- | @Selector@ for @focalLength@
focalLengthSelector :: Selector '[] (Id NSNumber)
focalLengthSelector = mkSelector "focalLength"

-- | @Selector@ for @setFocalLength:@
setFocalLengthSelector :: Selector '[Id NSNumber] ()
setFocalLengthSelector = mkSelector "setFocalLength:"

-- | @Selector@ for @focalLength35mm@
focalLength35mmSelector :: Selector '[] (Id NSNumber)
focalLength35mmSelector = mkSelector "focalLength35mm"

-- | @Selector@ for @setFocalLength35mm:@
setFocalLength35mmSelector :: Selector '[Id NSNumber] ()
setFocalLength35mmSelector = mkSelector "setFocalLength35mm:"

-- | @Selector@ for @acquisitionMake@
acquisitionMakeSelector :: Selector '[] (Id NSString)
acquisitionMakeSelector = mkSelector "acquisitionMake"

-- | @Selector@ for @setAcquisitionMake:@
setAcquisitionMakeSelector :: Selector '[Id NSString] ()
setAcquisitionMakeSelector = mkSelector "setAcquisitionMake:"

-- | @Selector@ for @acquisitionModel@
acquisitionModelSelector :: Selector '[] (Id NSString)
acquisitionModelSelector = mkSelector "acquisitionModel"

-- | @Selector@ for @setAcquisitionModel:@
setAcquisitionModelSelector :: Selector '[Id NSString] ()
setAcquisitionModelSelector = mkSelector "setAcquisitionModel:"

-- | @Selector@ for @cameraOwner@
cameraOwnerSelector :: Selector '[] (Id NSString)
cameraOwnerSelector = mkSelector "cameraOwner"

-- | @Selector@ for @setCameraOwner:@
setCameraOwnerSelector :: Selector '[Id NSString] ()
setCameraOwnerSelector = mkSelector "setCameraOwner:"

-- | @Selector@ for @lensModel@
lensModelSelector :: Selector '[] (Id NSString)
lensModelSelector = mkSelector "lensModel"

-- | @Selector@ for @setLensModel:@
setLensModelSelector :: Selector '[Id NSString] ()
setLensModelSelector = mkSelector "setLensModel:"

-- | @Selector@ for @ISOSpeed@
isoSpeedSelector :: Selector '[] (Id NSNumber)
isoSpeedSelector = mkSelector "ISOSpeed"

-- | @Selector@ for @setISOSpeed:@
setISOSpeedSelector :: Selector '[Id NSNumber] ()
setISOSpeedSelector = mkSelector "setISOSpeed:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] (Id NSNumber)
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[Id NSNumber] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @layerNames@
layerNamesSelector :: Selector '[] (Id NSArray)
layerNamesSelector = mkSelector "layerNames"

-- | @Selector@ for @setLayerNames:@
setLayerNamesSelector :: Selector '[Id NSArray] ()
setLayerNamesSelector = mkSelector "setLayerNames:"

-- | @Selector@ for @whiteBalance@
whiteBalanceSelector :: Selector '[] (Id NSNumber)
whiteBalanceSelector = mkSelector "whiteBalance"

-- | @Selector@ for @setWhiteBalance:@
setWhiteBalanceSelector :: Selector '[Id NSNumber] ()
setWhiteBalanceSelector = mkSelector "setWhiteBalance:"

-- | @Selector@ for @aperture@
apertureSelector :: Selector '[] (Id NSNumber)
apertureSelector = mkSelector "aperture"

-- | @Selector@ for @setAperture:@
setApertureSelector :: Selector '[Id NSNumber] ()
setApertureSelector = mkSelector "setAperture:"

-- | @Selector@ for @profileName@
profileNameSelector :: Selector '[] (Id NSString)
profileNameSelector = mkSelector "profileName"

-- | @Selector@ for @setProfileName:@
setProfileNameSelector :: Selector '[Id NSString] ()
setProfileNameSelector = mkSelector "setProfileName:"

-- | @Selector@ for @resolutionWidthDPI@
resolutionWidthDPISelector :: Selector '[] (Id NSNumber)
resolutionWidthDPISelector = mkSelector "resolutionWidthDPI"

-- | @Selector@ for @setResolutionWidthDPI:@
setResolutionWidthDPISelector :: Selector '[Id NSNumber] ()
setResolutionWidthDPISelector = mkSelector "setResolutionWidthDPI:"

-- | @Selector@ for @resolutionHeightDPI@
resolutionHeightDPISelector :: Selector '[] (Id NSNumber)
resolutionHeightDPISelector = mkSelector "resolutionHeightDPI"

-- | @Selector@ for @setResolutionHeightDPI:@
setResolutionHeightDPISelector :: Selector '[Id NSNumber] ()
setResolutionHeightDPISelector = mkSelector "setResolutionHeightDPI:"

-- | @Selector@ for @exposureMode@
exposureModeSelector :: Selector '[] (Id NSNumber)
exposureModeSelector = mkSelector "exposureMode"

-- | @Selector@ for @setExposureMode:@
setExposureModeSelector :: Selector '[Id NSNumber] ()
setExposureModeSelector = mkSelector "setExposureMode:"

-- | @Selector@ for @exposureTime@
exposureTimeSelector :: Selector '[] (Id NSNumber)
exposureTimeSelector = mkSelector "exposureTime"

-- | @Selector@ for @setExposureTime:@
setExposureTimeSelector :: Selector '[Id NSNumber] ()
setExposureTimeSelector = mkSelector "setExposureTime:"

-- | @Selector@ for @EXIFVersion@
exifVersionSelector :: Selector '[] (Id NSString)
exifVersionSelector = mkSelector "EXIFVersion"

-- | @Selector@ for @setEXIFVersion:@
setEXIFVersionSelector :: Selector '[Id NSString] ()
setEXIFVersionSelector = mkSelector "setEXIFVersion:"

-- | @Selector@ for @EXIFGPSVersion@
exifgpsVersionSelector :: Selector '[] (Id NSString)
exifgpsVersionSelector = mkSelector "EXIFGPSVersion"

-- | @Selector@ for @setEXIFGPSVersion:@
setEXIFGPSVersionSelector :: Selector '[Id NSString] ()
setEXIFGPSVersionSelector = mkSelector "setEXIFGPSVersion:"

-- | @Selector@ for @hasAlphaChannel@
hasAlphaChannelSelector :: Selector '[] (Id NSNumber)
hasAlphaChannelSelector = mkSelector "hasAlphaChannel"

-- | @Selector@ for @setHasAlphaChannel:@
setHasAlphaChannelSelector :: Selector '[Id NSNumber] ()
setHasAlphaChannelSelector = mkSelector "setHasAlphaChannel:"

-- | @Selector@ for @redEyeOn@
redEyeOnSelector :: Selector '[] (Id NSNumber)
redEyeOnSelector = mkSelector "redEyeOn"

-- | @Selector@ for @setRedEyeOn:@
setRedEyeOnSelector :: Selector '[Id NSNumber] ()
setRedEyeOnSelector = mkSelector "setRedEyeOn:"

-- | @Selector@ for @meteringMode@
meteringModeSelector :: Selector '[] (Id NSString)
meteringModeSelector = mkSelector "meteringMode"

-- | @Selector@ for @setMeteringMode:@
setMeteringModeSelector :: Selector '[Id NSString] ()
setMeteringModeSelector = mkSelector "setMeteringMode:"

-- | @Selector@ for @maxAperture@
maxApertureSelector :: Selector '[] (Id NSNumber)
maxApertureSelector = mkSelector "maxAperture"

-- | @Selector@ for @setMaxAperture:@
setMaxApertureSelector :: Selector '[Id NSNumber] ()
setMaxApertureSelector = mkSelector "setMaxAperture:"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector '[] (Id NSNumber)
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @setFNumber:@
setFNumberSelector :: Selector '[Id NSNumber] ()
setFNumberSelector = mkSelector "setFNumber:"

-- | @Selector@ for @exposureProgram@
exposureProgramSelector :: Selector '[] (Id NSString)
exposureProgramSelector = mkSelector "exposureProgram"

-- | @Selector@ for @setExposureProgram:@
setExposureProgramSelector :: Selector '[Id NSString] ()
setExposureProgramSelector = mkSelector "setExposureProgram:"

-- | @Selector@ for @exposureTimeString@
exposureTimeStringSelector :: Selector '[] (Id NSString)
exposureTimeStringSelector = mkSelector "exposureTimeString"

-- | @Selector@ for @setExposureTimeString:@
setExposureTimeStringSelector :: Selector '[Id NSString] ()
setExposureTimeStringSelector = mkSelector "setExposureTimeString:"

-- | @Selector@ for @audioSampleRate@
audioSampleRateSelector :: Selector '[] (Id NSNumber)
audioSampleRateSelector = mkSelector "audioSampleRate"

-- | @Selector@ for @setAudioSampleRate:@
setAudioSampleRateSelector :: Selector '[Id NSNumber] ()
setAudioSampleRateSelector = mkSelector "setAudioSampleRate:"

-- | @Selector@ for @audioChannelCount@
audioChannelCountSelector :: Selector '[] (Id NSNumber)
audioChannelCountSelector = mkSelector "audioChannelCount"

-- | @Selector@ for @setAudioChannelCount:@
setAudioChannelCountSelector :: Selector '[Id NSNumber] ()
setAudioChannelCountSelector = mkSelector "setAudioChannelCount:"

-- | @Selector@ for @tempo@
tempoSelector :: Selector '[] (Id NSNumber)
tempoSelector = mkSelector "tempo"

-- | @Selector@ for @setTempo:@
setTempoSelector :: Selector '[Id NSNumber] ()
setTempoSelector = mkSelector "setTempo:"

-- | @Selector@ for @keySignature@
keySignatureSelector :: Selector '[] (Id NSString)
keySignatureSelector = mkSelector "keySignature"

-- | @Selector@ for @setKeySignature:@
setKeySignatureSelector :: Selector '[Id NSString] ()
setKeySignatureSelector = mkSelector "setKeySignature:"

-- | @Selector@ for @timeSignature@
timeSignatureSelector :: Selector '[] (Id NSString)
timeSignatureSelector = mkSelector "timeSignature"

-- | @Selector@ for @setTimeSignature:@
setTimeSignatureSelector :: Selector '[Id NSString] ()
setTimeSignatureSelector = mkSelector "setTimeSignature:"

-- | @Selector@ for @audioEncodingApplication@
audioEncodingApplicationSelector :: Selector '[] (Id NSString)
audioEncodingApplicationSelector = mkSelector "audioEncodingApplication"

-- | @Selector@ for @setAudioEncodingApplication:@
setAudioEncodingApplicationSelector :: Selector '[Id NSString] ()
setAudioEncodingApplicationSelector = mkSelector "setAudioEncodingApplication:"

-- | @Selector@ for @composer@
composerSelector :: Selector '[] (Id NSString)
composerSelector = mkSelector "composer"

-- | @Selector@ for @setComposer:@
setComposerSelector :: Selector '[Id NSString] ()
setComposerSelector = mkSelector "setComposer:"

-- | @Selector@ for @lyricist@
lyricistSelector :: Selector '[] (Id NSString)
lyricistSelector = mkSelector "lyricist"

-- | @Selector@ for @setLyricist:@
setLyricistSelector :: Selector '[Id NSString] ()
setLyricistSelector = mkSelector "setLyricist:"

-- | @Selector@ for @album@
albumSelector :: Selector '[] (Id NSString)
albumSelector = mkSelector "album"

-- | @Selector@ for @setAlbum:@
setAlbumSelector :: Selector '[Id NSString] ()
setAlbumSelector = mkSelector "setAlbum:"

-- | @Selector@ for @artist@
artistSelector :: Selector '[] (Id NSString)
artistSelector = mkSelector "artist"

-- | @Selector@ for @setArtist:@
setArtistSelector :: Selector '[Id NSString] ()
setArtistSelector = mkSelector "setArtist:"

-- | @Selector@ for @audioTrackNumber@
audioTrackNumberSelector :: Selector '[] (Id NSNumber)
audioTrackNumberSelector = mkSelector "audioTrackNumber"

-- | @Selector@ for @setAudioTrackNumber:@
setAudioTrackNumberSelector :: Selector '[Id NSNumber] ()
setAudioTrackNumberSelector = mkSelector "setAudioTrackNumber:"

-- | @Selector@ for @recordingDate@
recordingDateSelector :: Selector '[] (Id NSDate)
recordingDateSelector = mkSelector "recordingDate"

-- | @Selector@ for @setRecordingDate:@
setRecordingDateSelector :: Selector '[Id NSDate] ()
setRecordingDateSelector = mkSelector "setRecordingDate:"

-- | @Selector@ for @musicalGenre@
musicalGenreSelector :: Selector '[] (Id NSString)
musicalGenreSelector = mkSelector "musicalGenre"

-- | @Selector@ for @setMusicalGenre:@
setMusicalGenreSelector :: Selector '[Id NSString] ()
setMusicalGenreSelector = mkSelector "setMusicalGenre:"

-- | @Selector@ for @generalMIDISequence@
generalMIDISequenceSelector :: Selector '[] (Id NSNumber)
generalMIDISequenceSelector = mkSelector "generalMIDISequence"

-- | @Selector@ for @setGeneralMIDISequence:@
setGeneralMIDISequenceSelector :: Selector '[Id NSNumber] ()
setGeneralMIDISequenceSelector = mkSelector "setGeneralMIDISequence:"

-- | @Selector@ for @musicalInstrumentCategory@
musicalInstrumentCategorySelector :: Selector '[] (Id NSString)
musicalInstrumentCategorySelector = mkSelector "musicalInstrumentCategory"

-- | @Selector@ for @setMusicalInstrumentCategory:@
setMusicalInstrumentCategorySelector :: Selector '[Id NSString] ()
setMusicalInstrumentCategorySelector = mkSelector "setMusicalInstrumentCategory:"

-- | @Selector@ for @musicalInstrumentName@
musicalInstrumentNameSelector :: Selector '[] (Id NSString)
musicalInstrumentNameSelector = mkSelector "musicalInstrumentName"

-- | @Selector@ for @setMusicalInstrumentName:@
setMusicalInstrumentNameSelector :: Selector '[Id NSString] ()
setMusicalInstrumentNameSelector = mkSelector "setMusicalInstrumentName:"

-- | @Selector@ for @editors@
editorsSelector :: Selector '[] (Id NSArray)
editorsSelector = mkSelector "editors"

-- | @Selector@ for @setEditors:@
setEditorsSelector :: Selector '[Id NSArray] ()
setEditorsSelector = mkSelector "setEditors:"

-- | @Selector@ for @participants@
participantsSelector :: Selector '[] (Id NSArray)
participantsSelector = mkSelector "participants"

-- | @Selector@ for @setParticipants:@
setParticipantsSelector :: Selector '[Id NSArray] ()
setParticipantsSelector = mkSelector "setParticipants:"

-- | @Selector@ for @projects@
projectsSelector :: Selector '[] (Id NSArray)
projectsSelector = mkSelector "projects"

-- | @Selector@ for @setProjects:@
setProjectsSelector :: Selector '[Id NSArray] ()
setProjectsSelector = mkSelector "setProjects:"

-- | @Selector@ for @downloadedDate@
downloadedDateSelector :: Selector '[] (Id NSDate)
downloadedDateSelector = mkSelector "downloadedDate"

-- | @Selector@ for @setDownloadedDate:@
setDownloadedDateSelector :: Selector '[Id NSDate] ()
setDownloadedDateSelector = mkSelector "setDownloadedDate:"

-- | @Selector@ for @contentSources@
contentSourcesSelector :: Selector '[] (Id NSArray)
contentSourcesSelector = mkSelector "contentSources"

-- | @Selector@ for @setContentSources:@
setContentSourcesSelector :: Selector '[Id NSArray] ()
setContentSourcesSelector = mkSelector "setContentSources:"

-- | @Selector@ for @comment@
commentSelector :: Selector '[] (Id NSString)
commentSelector = mkSelector "comment"

-- | @Selector@ for @setComment:@
setCommentSelector :: Selector '[Id NSString] ()
setCommentSelector = mkSelector "setComment:"

-- | @Selector@ for @copyright@
copyrightSelector :: Selector '[] (Id NSString)
copyrightSelector = mkSelector "copyright"

-- | @Selector@ for @setCopyright:@
setCopyrightSelector :: Selector '[Id NSString] ()
setCopyrightSelector = mkSelector "setCopyright:"

-- | @Selector@ for @lastUsedDate@
lastUsedDateSelector :: Selector '[] (Id NSDate)
lastUsedDateSelector = mkSelector "lastUsedDate"

-- | @Selector@ for @setLastUsedDate:@
setLastUsedDateSelector :: Selector '[Id NSDate] ()
setLastUsedDateSelector = mkSelector "setLastUsedDate:"

-- | @Selector@ for @contentCreationDate@
contentCreationDateSelector :: Selector '[] (Id NSDate)
contentCreationDateSelector = mkSelector "contentCreationDate"

-- | @Selector@ for @setContentCreationDate:@
setContentCreationDateSelector :: Selector '[Id NSDate] ()
setContentCreationDateSelector = mkSelector "setContentCreationDate:"

-- | @Selector@ for @contentModificationDate@
contentModificationDateSelector :: Selector '[] (Id NSDate)
contentModificationDateSelector = mkSelector "contentModificationDate"

-- | @Selector@ for @setContentModificationDate:@
setContentModificationDateSelector :: Selector '[Id NSDate] ()
setContentModificationDateSelector = mkSelector "setContentModificationDate:"

-- | @Selector@ for @addedDate@
addedDateSelector :: Selector '[] (Id NSDate)
addedDateSelector = mkSelector "addedDate"

-- | @Selector@ for @setAddedDate:@
setAddedDateSelector :: Selector '[Id NSDate] ()
setAddedDateSelector = mkSelector "setAddedDate:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @contactKeywords@
contactKeywordsSelector :: Selector '[] (Id NSArray)
contactKeywordsSelector = mkSelector "contactKeywords"

-- | @Selector@ for @setContactKeywords:@
setContactKeywordsSelector :: Selector '[Id NSArray] ()
setContactKeywordsSelector = mkSelector "setContactKeywords:"

-- | @Selector@ for @codecs@
codecsSelector :: Selector '[] (Id NSArray)
codecsSelector = mkSelector "codecs"

-- | @Selector@ for @setCodecs:@
setCodecsSelector :: Selector '[Id NSArray] ()
setCodecsSelector = mkSelector "setCodecs:"

-- | @Selector@ for @mediaTypes@
mediaTypesSelector :: Selector '[] (Id NSArray)
mediaTypesSelector = mkSelector "mediaTypes"

-- | @Selector@ for @setMediaTypes:@
setMediaTypesSelector :: Selector '[Id NSArray] ()
setMediaTypesSelector = mkSelector "setMediaTypes:"

-- | @Selector@ for @streamable@
streamableSelector :: Selector '[] (Id NSNumber)
streamableSelector = mkSelector "streamable"

-- | @Selector@ for @setStreamable:@
setStreamableSelector :: Selector '[Id NSNumber] ()
setStreamableSelector = mkSelector "setStreamable:"

-- | @Selector@ for @totalBitRate@
totalBitRateSelector :: Selector '[] (Id NSNumber)
totalBitRateSelector = mkSelector "totalBitRate"

-- | @Selector@ for @setTotalBitRate:@
setTotalBitRateSelector :: Selector '[Id NSNumber] ()
setTotalBitRateSelector = mkSelector "setTotalBitRate:"

-- | @Selector@ for @videoBitRate@
videoBitRateSelector :: Selector '[] (Id NSNumber)
videoBitRateSelector = mkSelector "videoBitRate"

-- | @Selector@ for @setVideoBitRate:@
setVideoBitRateSelector :: Selector '[Id NSNumber] ()
setVideoBitRateSelector = mkSelector "setVideoBitRate:"

-- | @Selector@ for @audioBitRate@
audioBitRateSelector :: Selector '[] (Id NSNumber)
audioBitRateSelector = mkSelector "audioBitRate"

-- | @Selector@ for @setAudioBitRate:@
setAudioBitRateSelector :: Selector '[Id NSNumber] ()
setAudioBitRateSelector = mkSelector "setAudioBitRate:"

-- | @Selector@ for @deliveryType@
deliveryTypeSelector :: Selector '[] (Id NSNumber)
deliveryTypeSelector = mkSelector "deliveryType"

-- | @Selector@ for @setDeliveryType:@
setDeliveryTypeSelector :: Selector '[Id NSNumber] ()
setDeliveryTypeSelector = mkSelector "setDeliveryType:"

-- | @Selector@ for @organizations@
organizationsSelector :: Selector '[] (Id NSArray)
organizationsSelector = mkSelector "organizations"

-- | @Selector@ for @setOrganizations:@
setOrganizationsSelector :: Selector '[Id NSArray] ()
setOrganizationsSelector = mkSelector "setOrganizations:"

-- | @Selector@ for @role@
roleSelector :: Selector '[] (Id NSString)
roleSelector = mkSelector "role"

-- | @Selector@ for @setRole:@
setRoleSelector :: Selector '[Id NSString] ()
setRoleSelector = mkSelector "setRole:"

-- | @Selector@ for @languages@
languagesSelector :: Selector '[] (Id NSArray)
languagesSelector = mkSelector "languages"

-- | @Selector@ for @setLanguages:@
setLanguagesSelector :: Selector '[Id NSArray] ()
setLanguagesSelector = mkSelector "setLanguages:"

-- | @Selector@ for @rights@
rightsSelector :: Selector '[] (Id NSString)
rightsSelector = mkSelector "rights"

-- | @Selector@ for @setRights:@
setRightsSelector :: Selector '[Id NSString] ()
setRightsSelector = mkSelector "setRights:"

-- | @Selector@ for @publishers@
publishersSelector :: Selector '[] (Id NSArray)
publishersSelector = mkSelector "publishers"

-- | @Selector@ for @setPublishers:@
setPublishersSelector :: Selector '[Id NSArray] ()
setPublishersSelector = mkSelector "setPublishers:"

-- | @Selector@ for @contributors@
contributorsSelector :: Selector '[] (Id NSArray)
contributorsSelector = mkSelector "contributors"

-- | @Selector@ for @setContributors:@
setContributorsSelector :: Selector '[Id NSArray] ()
setContributorsSelector = mkSelector "setContributors:"

-- | @Selector@ for @coverage@
coverageSelector :: Selector '[] (Id NSArray)
coverageSelector = mkSelector "coverage"

-- | @Selector@ for @setCoverage:@
setCoverageSelector :: Selector '[Id NSArray] ()
setCoverageSelector = mkSelector "setCoverage:"

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] (Id NSNumber)
ratingSelector = mkSelector "rating"

-- | @Selector@ for @setRating:@
setRatingSelector :: Selector '[Id NSNumber] ()
setRatingSelector = mkSelector "setRating:"

-- | @Selector@ for @ratingDescription@
ratingDescriptionSelector :: Selector '[] (Id NSString)
ratingDescriptionSelector = mkSelector "ratingDescription"

-- | @Selector@ for @setRatingDescription:@
setRatingDescriptionSelector :: Selector '[Id NSString] ()
setRatingDescriptionSelector = mkSelector "setRatingDescription:"

-- | @Selector@ for @playCount@
playCountSelector :: Selector '[] (Id NSNumber)
playCountSelector = mkSelector "playCount"

-- | @Selector@ for @setPlayCount:@
setPlayCountSelector :: Selector '[Id NSNumber] ()
setPlayCountSelector = mkSelector "setPlayCount:"

-- | @Selector@ for @information@
informationSelector :: Selector '[] (Id NSString)
informationSelector = mkSelector "information"

-- | @Selector@ for @setInformation:@
setInformationSelector :: Selector '[Id NSString] ()
setInformationSelector = mkSelector "setInformation:"

-- | @Selector@ for @director@
directorSelector :: Selector '[] (Id NSString)
directorSelector = mkSelector "director"

-- | @Selector@ for @setDirector:@
setDirectorSelector :: Selector '[Id NSString] ()
setDirectorSelector = mkSelector "setDirector:"

-- | @Selector@ for @producer@
producerSelector :: Selector '[] (Id NSString)
producerSelector = mkSelector "producer"

-- | @Selector@ for @setProducer:@
setProducerSelector :: Selector '[Id NSString] ()
setProducerSelector = mkSelector "setProducer:"

-- | @Selector@ for @genre@
genreSelector :: Selector '[] (Id NSString)
genreSelector = mkSelector "genre"

-- | @Selector@ for @setGenre:@
setGenreSelector :: Selector '[Id NSString] ()
setGenreSelector = mkSelector "setGenre:"

-- | @Selector@ for @performers@
performersSelector :: Selector '[] (Id NSArray)
performersSelector = mkSelector "performers"

-- | @Selector@ for @setPerformers:@
setPerformersSelector :: Selector '[Id NSArray] ()
setPerformersSelector = mkSelector "setPerformers:"

-- | @Selector@ for @originalFormat@
originalFormatSelector :: Selector '[] (Id NSString)
originalFormatSelector = mkSelector "originalFormat"

-- | @Selector@ for @setOriginalFormat:@
setOriginalFormatSelector :: Selector '[Id NSString] ()
setOriginalFormatSelector = mkSelector "setOriginalFormat:"

-- | @Selector@ for @originalSource@
originalSourceSelector :: Selector '[] (Id NSString)
originalSourceSelector = mkSelector "originalSource"

-- | @Selector@ for @setOriginalSource:@
setOriginalSourceSelector :: Selector '[Id NSString] ()
setOriginalSourceSelector = mkSelector "setOriginalSource:"

-- | @Selector@ for @local@
localSelector :: Selector '[] (Id NSNumber)
localSelector = mkSelector "local"

-- | @Selector@ for @setLocal:@
setLocalSelector :: Selector '[Id NSNumber] ()
setLocalSelector = mkSelector "setLocal:"

-- | @Selector@ for @contentRating@
contentRatingSelector :: Selector '[] (Id NSNumber)
contentRatingSelector = mkSelector "contentRating"

-- | @Selector@ for @setContentRating:@
setContentRatingSelector :: Selector '[Id NSNumber] ()
setContentRatingSelector = mkSelector "setContentRating:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @accountIdentifier@
accountIdentifierSelector :: Selector '[] (Id NSString)
accountIdentifierSelector = mkSelector "accountIdentifier"

-- | @Selector@ for @setAccountIdentifier:@
setAccountIdentifierSelector :: Selector '[Id NSString] ()
setAccountIdentifierSelector = mkSelector "setAccountIdentifier:"

-- | @Selector@ for @accountHandles@
accountHandlesSelector :: Selector '[] (Id NSArray)
accountHandlesSelector = mkSelector "accountHandles"

-- | @Selector@ for @setAccountHandles:@
setAccountHandlesSelector :: Selector '[Id NSArray] ()
setAccountHandlesSelector = mkSelector "setAccountHandles:"

-- | @Selector@ for @HTMLContentData@
htmlContentDataSelector :: Selector '[] (Id NSData)
htmlContentDataSelector = mkSelector "HTMLContentData"

-- | @Selector@ for @setHTMLContentData:@
setHTMLContentDataSelector :: Selector '[Id NSData] ()
setHTMLContentDataSelector = mkSelector "setHTMLContentData:"

-- | @Selector@ for @textContent@
textContentSelector :: Selector '[] (Id NSString)
textContentSelector = mkSelector "textContent"

-- | @Selector@ for @setTextContent:@
setTextContentSelector :: Selector '[Id NSString] ()
setTextContentSelector = mkSelector "setTextContent:"

-- | @Selector@ for @authors@
authorsSelector :: Selector '[] (Id NSArray)
authorsSelector = mkSelector "authors"

-- | @Selector@ for @setAuthors:@
setAuthorsSelector :: Selector '[Id NSArray] ()
setAuthorsSelector = mkSelector "setAuthors:"

-- | @Selector@ for @primaryRecipients@
primaryRecipientsSelector :: Selector '[] (Id NSArray)
primaryRecipientsSelector = mkSelector "primaryRecipients"

-- | @Selector@ for @setPrimaryRecipients:@
setPrimaryRecipientsSelector :: Selector '[Id NSArray] ()
setPrimaryRecipientsSelector = mkSelector "setPrimaryRecipients:"

-- | @Selector@ for @additionalRecipients@
additionalRecipientsSelector :: Selector '[] (Id NSArray)
additionalRecipientsSelector = mkSelector "additionalRecipients"

-- | @Selector@ for @setAdditionalRecipients:@
setAdditionalRecipientsSelector :: Selector '[Id NSArray] ()
setAdditionalRecipientsSelector = mkSelector "setAdditionalRecipients:"

-- | @Selector@ for @hiddenAdditionalRecipients@
hiddenAdditionalRecipientsSelector :: Selector '[] (Id NSArray)
hiddenAdditionalRecipientsSelector = mkSelector "hiddenAdditionalRecipients"

-- | @Selector@ for @setHiddenAdditionalRecipients:@
setHiddenAdditionalRecipientsSelector :: Selector '[Id NSArray] ()
setHiddenAdditionalRecipientsSelector = mkSelector "setHiddenAdditionalRecipients:"

-- | @Selector@ for @emailHeaders@
emailHeadersSelector :: Selector '[] (Id NSDictionary)
emailHeadersSelector = mkSelector "emailHeaders"

-- | @Selector@ for @setEmailHeaders:@
setEmailHeadersSelector :: Selector '[Id NSDictionary] ()
setEmailHeadersSelector = mkSelector "setEmailHeaders:"

-- | @Selector@ for @mailboxIdentifiers@
mailboxIdentifiersSelector :: Selector '[] (Id NSArray)
mailboxIdentifiersSelector = mkSelector "mailboxIdentifiers"

-- | @Selector@ for @setMailboxIdentifiers:@
setMailboxIdentifiersSelector :: Selector '[Id NSArray] ()
setMailboxIdentifiersSelector = mkSelector "setMailboxIdentifiers:"

-- | @Selector@ for @authorNames@
authorNamesSelector :: Selector '[] (Id NSArray)
authorNamesSelector = mkSelector "authorNames"

-- | @Selector@ for @setAuthorNames:@
setAuthorNamesSelector :: Selector '[Id NSArray] ()
setAuthorNamesSelector = mkSelector "setAuthorNames:"

-- | @Selector@ for @recipientNames@
recipientNamesSelector :: Selector '[] (Id NSArray)
recipientNamesSelector = mkSelector "recipientNames"

-- | @Selector@ for @setRecipientNames:@
setRecipientNamesSelector :: Selector '[Id NSArray] ()
setRecipientNamesSelector = mkSelector "setRecipientNames:"

-- | @Selector@ for @authorEmailAddresses@
authorEmailAddressesSelector :: Selector '[] (Id NSArray)
authorEmailAddressesSelector = mkSelector "authorEmailAddresses"

-- | @Selector@ for @setAuthorEmailAddresses:@
setAuthorEmailAddressesSelector :: Selector '[Id NSArray] ()
setAuthorEmailAddressesSelector = mkSelector "setAuthorEmailAddresses:"

-- | @Selector@ for @recipientEmailAddresses@
recipientEmailAddressesSelector :: Selector '[] (Id NSArray)
recipientEmailAddressesSelector = mkSelector "recipientEmailAddresses"

-- | @Selector@ for @setRecipientEmailAddresses:@
setRecipientEmailAddressesSelector :: Selector '[Id NSArray] ()
setRecipientEmailAddressesSelector = mkSelector "setRecipientEmailAddresses:"

-- | @Selector@ for @authorAddresses@
authorAddressesSelector :: Selector '[] (Id NSArray)
authorAddressesSelector = mkSelector "authorAddresses"

-- | @Selector@ for @setAuthorAddresses:@
setAuthorAddressesSelector :: Selector '[Id NSArray] ()
setAuthorAddressesSelector = mkSelector "setAuthorAddresses:"

-- | @Selector@ for @recipientAddresses@
recipientAddressesSelector :: Selector '[] (Id NSArray)
recipientAddressesSelector = mkSelector "recipientAddresses"

-- | @Selector@ for @setRecipientAddresses:@
setRecipientAddressesSelector :: Selector '[Id NSArray] ()
setRecipientAddressesSelector = mkSelector "setRecipientAddresses:"

-- | @Selector@ for @phoneNumbers@
phoneNumbersSelector :: Selector '[] (Id NSArray)
phoneNumbersSelector = mkSelector "phoneNumbers"

-- | @Selector@ for @setPhoneNumbers:@
setPhoneNumbersSelector :: Selector '[Id NSArray] ()
setPhoneNumbersSelector = mkSelector "setPhoneNumbers:"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector '[] (Id NSArray)
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @setEmailAddresses:@
setEmailAddressesSelector :: Selector '[Id NSArray] ()
setEmailAddressesSelector = mkSelector "setEmailAddresses:"

-- | @Selector@ for @instantMessageAddresses@
instantMessageAddressesSelector :: Selector '[] (Id NSArray)
instantMessageAddressesSelector = mkSelector "instantMessageAddresses"

-- | @Selector@ for @setInstantMessageAddresses:@
setInstantMessageAddressesSelector :: Selector '[Id NSArray] ()
setInstantMessageAddressesSelector = mkSelector "setInstantMessageAddresses:"

-- | @Selector@ for @likelyJunk@
likelyJunkSelector :: Selector '[] (Id NSNumber)
likelyJunkSelector = mkSelector "likelyJunk"

-- | @Selector@ for @setLikelyJunk:@
setLikelyJunkSelector :: Selector '[Id NSNumber] ()
setLikelyJunkSelector = mkSelector "setLikelyJunk:"

-- | @Selector@ for @isPriority@
isPrioritySelector :: Selector '[] RawId
isPrioritySelector = mkSelector "isPriority"

-- | @Selector@ for @textContentSummary@
textContentSummarySelector :: Selector '[] RawId
textContentSummarySelector = mkSelector "textContentSummary"

-- | @Selector@ for @transcribedTextContent@
transcribedTextContentSelector :: Selector '[] RawId
transcribedTextContentSelector = mkSelector "transcribedTextContent"

-- | @Selector@ for @setTranscribedTextContent:@
setTranscribedTextContentSelector :: Selector '[RawId] ()
setTranscribedTextContentSelector = mkSelector "setTranscribedTextContent:"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector '[] (Id NSDate)
dueDateSelector = mkSelector "dueDate"

-- | @Selector@ for @setDueDate:@
setDueDateSelector :: Selector '[Id NSDate] ()
setDueDateSelector = mkSelector "setDueDate:"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector '[] (Id NSDate)
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @setCompletionDate:@
setCompletionDateSelector :: Selector '[Id NSDate] ()
setCompletionDateSelector = mkSelector "setCompletionDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector '[Id NSDate] ()
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @setEndDate:@
setEndDateSelector :: Selector '[Id NSDate] ()
setEndDateSelector = mkSelector "setEndDate:"

-- | @Selector@ for @importantDates@
importantDatesSelector :: Selector '[] (Id NSArray)
importantDatesSelector = mkSelector "importantDates"

-- | @Selector@ for @setImportantDates:@
setImportantDatesSelector :: Selector '[Id NSArray] ()
setImportantDatesSelector = mkSelector "setImportantDates:"

-- | @Selector@ for @allDay@
allDaySelector :: Selector '[] (Id NSNumber)
allDaySelector = mkSelector "allDay"

-- | @Selector@ for @setAllDay:@
setAllDaySelector :: Selector '[Id NSNumber] ()
setAllDaySelector = mkSelector "setAllDay:"

-- | @Selector@ for @subject@
subjectSelector :: Selector '[] (Id NSString)
subjectSelector = mkSelector "subject"

-- | @Selector@ for @setSubject:@
setSubjectSelector :: Selector '[Id NSString] ()
setSubjectSelector = mkSelector "setSubject:"

-- | @Selector@ for @theme@
themeSelector :: Selector '[] (Id NSString)
themeSelector = mkSelector "theme"

-- | @Selector@ for @setTheme:@
setThemeSelector :: Selector '[Id NSString] ()
setThemeSelector = mkSelector "setTheme:"

-- | @Selector@ for @contentDescription@
contentDescriptionSelector :: Selector '[] (Id NSString)
contentDescriptionSelector = mkSelector "contentDescription"

-- | @Selector@ for @setContentDescription:@
setContentDescriptionSelector :: Selector '[Id NSString] ()
setContentDescriptionSelector = mkSelector "setContentDescription:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @audiences@
audiencesSelector :: Selector '[] (Id NSArray)
audiencesSelector = mkSelector "audiences"

-- | @Selector@ for @setAudiences:@
setAudiencesSelector :: Selector '[Id NSArray] ()
setAudiencesSelector = mkSelector "setAudiences:"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector '[] (Id NSNumber)
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @setFileSize:@
setFileSizeSelector :: Selector '[Id NSNumber] ()
setFileSizeSelector = mkSelector "setFileSize:"

-- | @Selector@ for @pageCount@
pageCountSelector :: Selector '[] (Id NSNumber)
pageCountSelector = mkSelector "pageCount"

-- | @Selector@ for @setPageCount:@
setPageCountSelector :: Selector '[Id NSNumber] ()
setPageCountSelector = mkSelector "setPageCount:"

-- | @Selector@ for @pageWidth@
pageWidthSelector :: Selector '[] (Id NSNumber)
pageWidthSelector = mkSelector "pageWidth"

-- | @Selector@ for @setPageWidth:@
setPageWidthSelector :: Selector '[Id NSNumber] ()
setPageWidthSelector = mkSelector "setPageWidth:"

-- | @Selector@ for @pageHeight@
pageHeightSelector :: Selector '[] (Id NSNumber)
pageHeightSelector = mkSelector "pageHeight"

-- | @Selector@ for @setPageHeight:@
setPageHeightSelector :: Selector '[Id NSNumber] ()
setPageHeightSelector = mkSelector "setPageHeight:"

-- | @Selector@ for @securityMethod@
securityMethodSelector :: Selector '[] (Id NSString)
securityMethodSelector = mkSelector "securityMethod"

-- | @Selector@ for @setSecurityMethod:@
setSecurityMethodSelector :: Selector '[Id NSString] ()
setSecurityMethodSelector = mkSelector "setSecurityMethod:"

-- | @Selector@ for @creator@
creatorSelector :: Selector '[] (Id NSString)
creatorSelector = mkSelector "creator"

-- | @Selector@ for @setCreator:@
setCreatorSelector :: Selector '[Id NSString] ()
setCreatorSelector = mkSelector "setCreator:"

-- | @Selector@ for @encodingApplications@
encodingApplicationsSelector :: Selector '[] (Id NSArray)
encodingApplicationsSelector = mkSelector "encodingApplications"

-- | @Selector@ for @setEncodingApplications:@
setEncodingApplicationsSelector :: Selector '[Id NSArray] ()
setEncodingApplicationsSelector = mkSelector "setEncodingApplications:"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] (Id NSString)
kindSelector = mkSelector "kind"

-- | @Selector@ for @setKind:@
setKindSelector :: Selector '[Id NSString] ()
setKindSelector = mkSelector "setKind:"

-- | @Selector@ for @fontNames@
fontNamesSelector :: Selector '[] (Id NSArray)
fontNamesSelector = mkSelector "fontNames"

-- | @Selector@ for @setFontNames:@
setFontNamesSelector :: Selector '[Id NSArray] ()
setFontNamesSelector = mkSelector "setFontNames:"

-- | @Selector@ for @containerTitle@
containerTitleSelector :: Selector '[] (Id NSString)
containerTitleSelector = mkSelector "containerTitle"

-- | @Selector@ for @setContainerTitle:@
setContainerTitleSelector :: Selector '[Id NSString] ()
setContainerTitleSelector = mkSelector "setContainerTitle:"

-- | @Selector@ for @containerDisplayName@
containerDisplayNameSelector :: Selector '[] (Id NSString)
containerDisplayNameSelector = mkSelector "containerDisplayName"

-- | @Selector@ for @setContainerDisplayName:@
setContainerDisplayNameSelector :: Selector '[Id NSString] ()
setContainerDisplayNameSelector = mkSelector "setContainerDisplayName:"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

-- | @Selector@ for @setContainerIdentifier:@
setContainerIdentifierSelector :: Selector '[Id NSString] ()
setContainerIdentifierSelector = mkSelector "setContainerIdentifier:"

-- | @Selector@ for @containerOrder@
containerOrderSelector :: Selector '[] (Id NSNumber)
containerOrderSelector = mkSelector "containerOrder"

-- | @Selector@ for @setContainerOrder:@
setContainerOrderSelector :: Selector '[Id NSNumber] ()
setContainerOrderSelector = mkSelector "setContainerOrder:"

-- | @Selector@ for @supportsPhoneCall@
supportsPhoneCallSelector :: Selector '[] (Id NSNumber)
supportsPhoneCallSelector = mkSelector "supportsPhoneCall"

-- | @Selector@ for @setSupportsPhoneCall:@
setSupportsPhoneCallSelector :: Selector '[Id NSNumber] ()
setSupportsPhoneCallSelector = mkSelector "setSupportsPhoneCall:"

-- | @Selector@ for @supportsNavigation@
supportsNavigationSelector :: Selector '[] (Id NSNumber)
supportsNavigationSelector = mkSelector "supportsNavigation"

-- | @Selector@ for @setSupportsNavigation:@
setSupportsNavigationSelector :: Selector '[Id NSNumber] ()
setSupportsNavigationSelector = mkSelector "setSupportsNavigation:"

-- | @Selector@ for @actionIdentifiers@
actionIdentifiersSelector :: Selector '[] (Id NSArray)
actionIdentifiersSelector = mkSelector "actionIdentifiers"

-- | @Selector@ for @setActionIdentifiers:@
setActionIdentifiersSelector :: Selector '[Id NSArray] ()
setActionIdentifiersSelector = mkSelector "setActionIdentifiers:"

-- | @Selector@ for @sharedItemContentType@
sharedItemContentTypeSelector :: Selector '[] (Id UTType)
sharedItemContentTypeSelector = mkSelector "sharedItemContentType"

-- | @Selector@ for @setSharedItemContentType:@
setSharedItemContentTypeSelector :: Selector '[Id UTType] ()
setSharedItemContentTypeSelector = mkSelector "setSharedItemContentType:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector '[Id NSString] ()
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @alternateNames@
alternateNamesSelector :: Selector '[] (Id NSArray)
alternateNamesSelector = mkSelector "alternateNames"

-- | @Selector@ for @setAlternateNames:@
setAlternateNamesSelector :: Selector '[Id NSArray] ()
setAlternateNamesSelector = mkSelector "setAlternateNames:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id NSString] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @contentURL@
contentURLSelector :: Selector '[] (Id NSURL)
contentURLSelector = mkSelector "contentURL"

-- | @Selector@ for @setContentURL:@
setContentURLSelector :: Selector '[Id NSURL] ()
setContentURLSelector = mkSelector "setContentURL:"

-- | @Selector@ for @thumbnailURL@
thumbnailURLSelector :: Selector '[] (Id NSURL)
thumbnailURLSelector = mkSelector "thumbnailURL"

-- | @Selector@ for @setThumbnailURL:@
setThumbnailURLSelector :: Selector '[Id NSURL] ()
setThumbnailURLSelector = mkSelector "setThumbnailURL:"

-- | @Selector@ for @thumbnailData@
thumbnailDataSelector :: Selector '[] (Id NSData)
thumbnailDataSelector = mkSelector "thumbnailData"

-- | @Selector@ for @setThumbnailData:@
setThumbnailDataSelector :: Selector '[Id NSData] ()
setThumbnailDataSelector = mkSelector "setThumbnailData:"

-- | @Selector@ for @darkThumbnailURL@
darkThumbnailURLSelector :: Selector '[] (Id NSURL)
darkThumbnailURLSelector = mkSelector "darkThumbnailURL"

-- | @Selector@ for @setDarkThumbnailURL:@
setDarkThumbnailURLSelector :: Selector '[Id NSURL] ()
setDarkThumbnailURLSelector = mkSelector "setDarkThumbnailURL:"

-- | @Selector@ for @relatedUniqueIdentifier@
relatedUniqueIdentifierSelector :: Selector '[] (Id NSString)
relatedUniqueIdentifierSelector = mkSelector "relatedUniqueIdentifier"

-- | @Selector@ for @setRelatedUniqueIdentifier:@
setRelatedUniqueIdentifierSelector :: Selector '[Id NSString] ()
setRelatedUniqueIdentifierSelector = mkSelector "setRelatedUniqueIdentifier:"

-- | @Selector@ for @weakRelatedUniqueIdentifier@
weakRelatedUniqueIdentifierSelector :: Selector '[] RawId
weakRelatedUniqueIdentifierSelector = mkSelector "weakRelatedUniqueIdentifier"

-- | @Selector@ for @setWeakRelatedUniqueIdentifier:@
setWeakRelatedUniqueIdentifierSelector :: Selector '[RawId] ()
setWeakRelatedUniqueIdentifierSelector = mkSelector "setWeakRelatedUniqueIdentifier:"

-- | @Selector@ for @metadataModificationDate@
metadataModificationDateSelector :: Selector '[] (Id NSDate)
metadataModificationDateSelector = mkSelector "metadataModificationDate"

-- | @Selector@ for @setMetadataModificationDate:@
setMetadataModificationDateSelector :: Selector '[Id NSDate] ()
setMetadataModificationDateSelector = mkSelector "setMetadataModificationDate:"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id NSString)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @setContentType:@
setContentTypeSelector :: Selector '[Id NSString] ()
setContentTypeSelector = mkSelector "setContentType:"

-- | @Selector@ for @contentTypeTree@
contentTypeTreeSelector :: Selector '[] (Id NSArray)
contentTypeTreeSelector = mkSelector "contentTypeTree"

-- | @Selector@ for @setContentTypeTree:@
setContentTypeTreeSelector :: Selector '[Id NSArray] ()
setContentTypeTreeSelector = mkSelector "setContentTypeTree:"

-- | @Selector@ for @keywords@
keywordsSelector :: Selector '[] (Id NSArray)
keywordsSelector = mkSelector "keywords"

-- | @Selector@ for @setKeywords:@
setKeywordsSelector :: Selector '[Id NSArray] ()
setKeywordsSelector = mkSelector "setKeywords:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSString] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @userCreated@
userCreatedSelector :: Selector '[] RawId
userCreatedSelector = mkSelector "userCreated"

-- | @Selector@ for @setUserCreated:@
setUserCreatedSelector :: Selector '[RawId] ()
setUserCreatedSelector = mkSelector "setUserCreated:"

-- | @Selector@ for @userOwned@
userOwnedSelector :: Selector '[] RawId
userOwnedSelector = mkSelector "userOwned"

-- | @Selector@ for @setUserOwned:@
setUserOwnedSelector :: Selector '[RawId] ()
setUserOwnedSelector = mkSelector "setUserOwned:"

-- | @Selector@ for @userCurated@
userCuratedSelector :: Selector '[] RawId
userCuratedSelector = mkSelector "userCurated"

-- | @Selector@ for @setUserCurated:@
setUserCuratedSelector :: Selector '[RawId] ()
setUserCuratedSelector = mkSelector "setUserCurated:"

-- | @Selector@ for @rankingHint@
rankingHintSelector :: Selector '[] RawId
rankingHintSelector = mkSelector "rankingHint"

-- | @Selector@ for @setRankingHint:@
setRankingHintSelector :: Selector '[RawId] ()
setRankingHintSelector = mkSelector "setRankingHint:"

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector '[] RawId
domainIdentifierSelector = mkSelector "domainIdentifier"

-- | @Selector@ for @setDomainIdentifier:@
setDomainIdentifierSelector :: Selector '[RawId] ()
setDomainIdentifierSelector = mkSelector "setDomainIdentifier:"

