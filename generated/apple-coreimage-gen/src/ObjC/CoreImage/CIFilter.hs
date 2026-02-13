{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CIFilter are filter objects for Core Image that encapsulate the filter with its attributes
--
-- The CIFilter class produces a CIImage object as output. Typically, a filter takes one or more images as input. Some filters, however, generate an image based on other types of input parameters. The parameters of a CIFilter object are set and retrieved through the use of key-value pairs. You use the CIFilter object in conjunction with the CIImage, CIContext, CIVector, CIImageAccumulator, and CIColor objects to take advantage of the built-in Core Image filters when processing images. CIFilter objects are also used along with CIKernel, CISampler, and CIFilterShape objects to create custom filters.
--
-- Generated bindings for @CIFilter@.
module ObjC.CoreImage.CIFilter
  ( CIFilter
  , IsCIFilter(..)
  , name
  , setName
  , setDefaults
  , apply_arguments_options
  , apply
  , filterWithImageURL_options
  , filterWithImageData_options
  , filterWithCVPixelBuffer_properties_options
  , supportedRawCameraModels
  , filterWithName
  , filterWithName_keysAndValues
  , filterWithName_withInputParameters
  , filterNamesInCategory
  , filterNamesInCategories
  , registerFilterName_constructor_classAttributes
  , localizedNameForFilterName
  , localizedNameForCategory
  , localizedDescriptionForFilterName
  , localizedReferenceDocumentationForFilterName
  , outputImage
  , enabled
  , setEnabled
  , inputKeys
  , outputKeys
  , attributes
  , applySelector
  , apply_arguments_optionsSelector
  , attributesSelector
  , enabledSelector
  , filterNamesInCategoriesSelector
  , filterNamesInCategorySelector
  , filterWithCVPixelBuffer_properties_optionsSelector
  , filterWithImageData_optionsSelector
  , filterWithImageURL_optionsSelector
  , filterWithNameSelector
  , filterWithName_keysAndValuesSelector
  , filterWithName_withInputParametersSelector
  , inputKeysSelector
  , localizedDescriptionForFilterNameSelector
  , localizedNameForCategorySelector
  , localizedNameForFilterNameSelector
  , localizedReferenceDocumentationForFilterNameSelector
  , nameSelector
  , outputImageSelector
  , outputKeysSelector
  , registerFilterName_constructor_classAttributesSelector
  , setDefaultsSelector
  , setEnabledSelector
  , setNameSelector
  , supportedRawCameraModelsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsCIFilter ciFilter => ciFilter -> IO (Id NSString)
name ciFilter =
  sendMessage ciFilter nameSelector

-- | @- setName:@
setName :: (IsCIFilter ciFilter, IsNSString aString) => ciFilter -> aString -> IO ()
setName ciFilter aString =
  sendMessage ciFilter setNameSelector (toNSString aString)

-- | Sets all inputs to their default values (where default values are defined, other inputs are left as-is).
--
-- ObjC selector: @- setDefaults@
setDefaults :: IsCIFilter ciFilter => ciFilter -> IO ()
setDefaults ciFilter =
  sendMessage ciFilter setDefaultsSelector

-- | Used by CIFilter subclasses to apply the array of argument values 'args' to the kernel function 'k'. The supplied arguments must be type-compatible with the function signature of the kernel.
--
-- The key-value pairs defined by 'dict' (if non-nil) are used to control exactly how the kernel is evaluated. Valid keys include: kCIApplyOptionExtent: the size of the produced image. Value is a four element NSArray [X Y WIDTH HEIGHT]. kCIApplyOptionDefinition: the Domain of Definition of the produced image. Value is either a CIFilterShape object, or a four element NSArray defining a rectangle.
--
-- @k@ — CIKernel of the filter
--
-- @args@ — Array of arguments that are applied to the kernel
--
-- @dict@ — Array of additional options
--
-- ObjC selector: @- apply:arguments:options:@
apply_arguments_options :: (IsCIFilter ciFilter, IsCIKernel k, IsNSArray args, IsNSDictionary dict) => ciFilter -> k -> args -> dict -> IO (Id CIImage)
apply_arguments_options ciFilter k args dict =
  sendMessage ciFilter apply_arguments_optionsSelector (toCIKernel k) (toNSArray args) (toNSDictionary dict)

-- | Similar to above except that all argument values and option key-value are specified inline. The list of key-value pairs must be terminated by the 'nil' object.
--
-- ObjC selector: @- apply:@
apply :: (IsCIFilter ciFilter, IsCIKernel k) => ciFilter -> k -> IO (Id CIImage)
apply ciFilter k =
  sendMessage ciFilter applySelector (toCIKernel k)

-- | Returns a CIFilter that will in turn return a properly processed CIImage as "outputImage".
--
-- ObjC selector: @+ filterWithImageURL:options:@
filterWithImageURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id CIFilter)
filterWithImageURL_options url options =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterWithImageURL_optionsSelector (toNSURL url) (toNSDictionary options)

-- | Returns a CIFilter that will in turn return a properly processed CIImage as "outputImage".
--
-- Note that when using this initializer, you should pass in a source type identifier hint (kCGImageSourceTypeIdentifierHint) key/value pair in order to help the decoder determine the file type, as otherwise confusion and incorrect results are possible.
--
-- ObjC selector: @+ filterWithImageData:options:@
filterWithImageData_options :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> IO (Id CIFilter)
filterWithImageData_options data_ options =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterWithImageData_optionsSelector (toNSData data_) (toNSDictionary options)

-- | Returns a CIFilter that will in turn return a properly processed CIImage as "outputImage".
--
-- Note that when using this initializer, you should pass in a CVPixelBufferRef with one of the following Raw pixel format types    kCVPixelFormatType_14Bayer_GRBG, kCVPixelFormatType_14Bayer_RGGB, kCVPixelFormatType_14Bayer_BGGR, kCVPixelFormatType_14Bayer_GBRG as well as the root properties attachment from the CMSampleBufferRef.
--
-- ObjC selector: @+ filterWithCVPixelBuffer:properties:options:@
filterWithCVPixelBuffer_properties_options :: (IsNSDictionary properties, IsNSDictionary options) => Ptr () -> properties -> options -> IO (Id CIFilter)
filterWithCVPixelBuffer_properties_options pixelBuffer properties options =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterWithCVPixelBuffer_properties_optionsSelector pixelBuffer (toNSDictionary properties) (toNSDictionary options)

-- | Returns a NSArray containing the names of all supported RAW cameras.
--
-- ObjC selector: @+ supportedRawCameraModels@
supportedRawCameraModels :: IO (Id NSArray)
supportedRawCameraModels  =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' supportedRawCameraModelsSelector

-- | Creates a new filter of type 'name'. On OSX, all input values will be undefined. On iOS, all input values will be set to default values.
--
-- ObjC selector: @+ filterWithName:@
filterWithName :: IsNSString name => name -> IO (Id CIFilter)
filterWithName name =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterWithNameSelector (toNSString name)

-- | Creates a new filter of type 'name'. The filter's input parameters are set from the list of key-value pairs which must be nil-terminated. On OSX, any of the filter input parameters not specified in the list will be undefined. On iOS, any of the filter input parameters not specified in the list will be set to default values.
--
-- ObjC selector: @+ filterWithName:keysAndValues:@
filterWithName_keysAndValues :: IsNSString name => name -> RawId -> IO (Id CIFilter)
filterWithName_keysAndValues name key0 =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterWithName_keysAndValuesSelector (toNSString name) key0

-- | Creates a new filter of type 'name'. The filter's input parameters are set from the dictionary of key-value pairs. On OSX, any of the filter input parameters not specified in the dictionary will be undefined. On iOS, any of the filter input parameters not specified in the dictionary will be set to default values.
--
-- ObjC selector: @+ filterWithName:withInputParameters:@
filterWithName_withInputParameters :: (IsNSString name, IsNSDictionary params) => name -> params -> IO (Id CIFilter)
filterWithName_withInputParameters name params =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterWithName_withInputParametersSelector (toNSString name) (toNSDictionary params)

-- | Returns an array containing all published filter names in a category.
--
-- ObjC selector: @+ filterNamesInCategory:@
filterNamesInCategory :: IsNSString category => category -> IO (Id NSArray)
filterNamesInCategory category =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterNamesInCategorySelector (toNSString category)

-- | Returns an array containing all published filter names that belong to all listed categories.
--
-- ObjC selector: @+ filterNamesInCategories:@
filterNamesInCategories :: IsNSArray categories => categories -> IO (Id NSArray)
filterNamesInCategories categories =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' filterNamesInCategoriesSelector (toNSArray categories)

-- | Publishes a new filter called 'name'.
--
-- The constructor object 'anObject' should implement the filterWithName: method. That method will be invoked with the name of the filter to create. The class attributes must have a kCIAttributeFilterCategories key associated with a set of categories.
--
-- @attributes@ — Dictionary of the registration attributes of the filter. See below for attribute keys.
--
-- ObjC selector: @+ registerFilterName:constructor:classAttributes:@
registerFilterName_constructor_classAttributes :: (IsNSString name, IsNSDictionary attributes) => name -> RawId -> attributes -> IO ()
registerFilterName_constructor_classAttributes name anObject attributes =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' registerFilterName_constructor_classAttributesSelector (toNSString name) anObject (toNSDictionary attributes)

-- | Returns the localized name of a filter for display in the UI.
--
-- ObjC selector: @+ localizedNameForFilterName:@
localizedNameForFilterName :: IsNSString filterName => filterName -> IO (Id NSString)
localizedNameForFilterName filterName =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' localizedNameForFilterNameSelector (toNSString filterName)

-- | Returns the localized name of a category for display in the UI.
--
-- ObjC selector: @+ localizedNameForCategory:@
localizedNameForCategory :: IsNSString category => category -> IO (Id NSString)
localizedNameForCategory category =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' localizedNameForCategorySelector (toNSString category)

-- | Returns the localized description of a filter for display in the UI.
--
-- ObjC selector: @+ localizedDescriptionForFilterName:@
localizedDescriptionForFilterName :: IsNSString filterName => filterName -> IO (Id NSString)
localizedDescriptionForFilterName filterName =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' localizedDescriptionForFilterNameSelector (toNSString filterName)

-- | Returns the URL to the localized reference documentation describing the filter.
--
-- The URL can be a local file or a remote document on a webserver. It is possible, that this method returns nil (like filters that predate this feature). A client of this API has to handle this case gracefully.
--
-- ObjC selector: @+ localizedReferenceDocumentationForFilterName:@
localizedReferenceDocumentationForFilterName :: IsNSString filterName => filterName -> IO (Id NSURL)
localizedReferenceDocumentationForFilterName filterName =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMessage cls' localizedReferenceDocumentationForFilterNameSelector (toNSString filterName)

-- | @- outputImage@
outputImage :: IsCIFilter ciFilter => ciFilter -> IO RawId
outputImage ciFilter =
  sendMessage ciFilter outputImageSelector

-- | @- enabled@
enabled :: IsCIFilter ciFilter => ciFilter -> IO Bool
enabled ciFilter =
  sendMessage ciFilter enabledSelector

-- | @- setEnabled:@
setEnabled :: IsCIFilter ciFilter => ciFilter -> Bool -> IO ()
setEnabled ciFilter value =
  sendMessage ciFilter setEnabledSelector value

-- | Returns an array containing the names of all inputs in the filter.
--
-- ObjC selector: @- inputKeys@
inputKeys :: IsCIFilter ciFilter => ciFilter -> IO (Id NSArray)
inputKeys ciFilter =
  sendMessage ciFilter inputKeysSelector

-- | Returns an array containing the names of all outputs in the filter.
--
-- ObjC selector: @- outputKeys@
outputKeys :: IsCIFilter ciFilter => ciFilter -> IO (Id NSArray)
outputKeys ciFilter =
  sendMessage ciFilter outputKeysSelector

-- | Returns a dictionary containing key/value pairs describing the filter. (see description of keys below)
--
-- ObjC selector: @- attributes@
attributes :: IsCIFilter ciFilter => ciFilter -> IO (Id NSDictionary)
attributes ciFilter =
  sendMessage ciFilter attributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @setDefaults@
setDefaultsSelector :: Selector '[] ()
setDefaultsSelector = mkSelector "setDefaults"

-- | @Selector@ for @apply:arguments:options:@
apply_arguments_optionsSelector :: Selector '[Id CIKernel, Id NSArray, Id NSDictionary] (Id CIImage)
apply_arguments_optionsSelector = mkSelector "apply:arguments:options:"

-- | @Selector@ for @apply:@
applySelector :: Selector '[Id CIKernel] (Id CIImage)
applySelector = mkSelector "apply:"

-- | @Selector@ for @filterWithImageURL:options:@
filterWithImageURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id CIFilter)
filterWithImageURL_optionsSelector = mkSelector "filterWithImageURL:options:"

-- | @Selector@ for @filterWithImageData:options:@
filterWithImageData_optionsSelector :: Selector '[Id NSData, Id NSDictionary] (Id CIFilter)
filterWithImageData_optionsSelector = mkSelector "filterWithImageData:options:"

-- | @Selector@ for @filterWithCVPixelBuffer:properties:options:@
filterWithCVPixelBuffer_properties_optionsSelector :: Selector '[Ptr (), Id NSDictionary, Id NSDictionary] (Id CIFilter)
filterWithCVPixelBuffer_properties_optionsSelector = mkSelector "filterWithCVPixelBuffer:properties:options:"

-- | @Selector@ for @supportedRawCameraModels@
supportedRawCameraModelsSelector :: Selector '[] (Id NSArray)
supportedRawCameraModelsSelector = mkSelector "supportedRawCameraModels"

-- | @Selector@ for @filterWithName:@
filterWithNameSelector :: Selector '[Id NSString] (Id CIFilter)
filterWithNameSelector = mkSelector "filterWithName:"

-- | @Selector@ for @filterWithName:keysAndValues:@
filterWithName_keysAndValuesSelector :: Selector '[Id NSString, RawId] (Id CIFilter)
filterWithName_keysAndValuesSelector = mkSelector "filterWithName:keysAndValues:"

-- | @Selector@ for @filterWithName:withInputParameters:@
filterWithName_withInputParametersSelector :: Selector '[Id NSString, Id NSDictionary] (Id CIFilter)
filterWithName_withInputParametersSelector = mkSelector "filterWithName:withInputParameters:"

-- | @Selector@ for @filterNamesInCategory:@
filterNamesInCategorySelector :: Selector '[Id NSString] (Id NSArray)
filterNamesInCategorySelector = mkSelector "filterNamesInCategory:"

-- | @Selector@ for @filterNamesInCategories:@
filterNamesInCategoriesSelector :: Selector '[Id NSArray] (Id NSArray)
filterNamesInCategoriesSelector = mkSelector "filterNamesInCategories:"

-- | @Selector@ for @registerFilterName:constructor:classAttributes:@
registerFilterName_constructor_classAttributesSelector :: Selector '[Id NSString, RawId, Id NSDictionary] ()
registerFilterName_constructor_classAttributesSelector = mkSelector "registerFilterName:constructor:classAttributes:"

-- | @Selector@ for @localizedNameForFilterName:@
localizedNameForFilterNameSelector :: Selector '[Id NSString] (Id NSString)
localizedNameForFilterNameSelector = mkSelector "localizedNameForFilterName:"

-- | @Selector@ for @localizedNameForCategory:@
localizedNameForCategorySelector :: Selector '[Id NSString] (Id NSString)
localizedNameForCategorySelector = mkSelector "localizedNameForCategory:"

-- | @Selector@ for @localizedDescriptionForFilterName:@
localizedDescriptionForFilterNameSelector :: Selector '[Id NSString] (Id NSString)
localizedDescriptionForFilterNameSelector = mkSelector "localizedDescriptionForFilterName:"

-- | @Selector@ for @localizedReferenceDocumentationForFilterName:@
localizedReferenceDocumentationForFilterNameSelector :: Selector '[Id NSString] (Id NSURL)
localizedReferenceDocumentationForFilterNameSelector = mkSelector "localizedReferenceDocumentationForFilterName:"

-- | @Selector@ for @outputImage@
outputImageSelector :: Selector '[] RawId
outputImageSelector = mkSelector "outputImage"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @inputKeys@
inputKeysSelector :: Selector '[] (Id NSArray)
inputKeysSelector = mkSelector "inputKeys"

-- | @Selector@ for @outputKeys@
outputKeysSelector :: Selector '[] (Id NSArray)
outputKeysSelector = mkSelector "outputKeys"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

