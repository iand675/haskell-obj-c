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
  , enabled
  , setEnabled
  , inputKeys
  , outputKeys
  , attributes
  , nameSelector
  , setNameSelector
  , setDefaultsSelector
  , apply_arguments_optionsSelector
  , applySelector
  , filterWithImageURL_optionsSelector
  , filterWithImageData_optionsSelector
  , filterWithCVPixelBuffer_properties_optionsSelector
  , supportedRawCameraModelsSelector
  , filterWithNameSelector
  , filterWithName_keysAndValuesSelector
  , filterWithName_withInputParametersSelector
  , filterNamesInCategorySelector
  , filterNamesInCategoriesSelector
  , registerFilterName_constructor_classAttributesSelector
  , localizedNameForFilterNameSelector
  , localizedNameForCategorySelector
  , localizedDescriptionForFilterNameSelector
  , localizedReferenceDocumentationForFilterNameSelector
  , enabledSelector
  , setEnabledSelector
  , inputKeysSelector
  , outputKeysSelector
  , attributesSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsCIFilter ciFilter => ciFilter -> IO (Id NSString)
name ciFilter  =
  sendMsg ciFilter (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsCIFilter ciFilter, IsNSString aString) => ciFilter -> aString -> IO ()
setName ciFilter  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg ciFilter (mkSelector "setName:") retVoid [argPtr (castPtr raw_aString :: Ptr ())]

-- | Sets all inputs to their default values (where default values are defined, other inputs are left as-is).
--
-- ObjC selector: @- setDefaults@
setDefaults :: IsCIFilter ciFilter => ciFilter -> IO ()
setDefaults ciFilter  =
  sendMsg ciFilter (mkSelector "setDefaults") retVoid []

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
apply_arguments_options ciFilter  k args dict =
withObjCPtr k $ \raw_k ->
  withObjCPtr args $ \raw_args ->
    withObjCPtr dict $ \raw_dict ->
        sendMsg ciFilter (mkSelector "apply:arguments:options:") (retPtr retVoid) [argPtr (castPtr raw_k :: Ptr ()), argPtr (castPtr raw_args :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | Similar to above except that all argument values and option key-value are specified inline. The list of key-value pairs must be terminated by the 'nil' object.
--
-- ObjC selector: @- apply:@
apply :: (IsCIFilter ciFilter, IsCIKernel k) => ciFilter -> k -> IO (Id CIImage)
apply ciFilter  k =
withObjCPtr k $ \raw_k ->
    sendMsg ciFilter (mkSelector "apply:") (retPtr retVoid) [argPtr (castPtr raw_k :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a CIFilter that will in turn return a properly processed CIImage as "outputImage".
--
-- ObjC selector: @+ filterWithImageURL:options:@
filterWithImageURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id CIFilter)
filterWithImageURL_options url options =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "filterWithImageURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a CIFilter that will in turn return a properly processed CIImage as "outputImage".
--
-- Note that when using this initializer, you should pass in a source type identifier hint (kCGImageSourceTypeIdentifierHint) key/value pair in order to help the decoder determine the file type, as otherwise confusion and incorrect results are possible.
--
-- ObjC selector: @+ filterWithImageData:options:@
filterWithImageData_options :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> IO (Id CIFilter)
filterWithImageData_options data_ options =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "filterWithImageData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a CIFilter that will in turn return a properly processed CIImage as "outputImage".
--
-- Note that when using this initializer, you should pass in a CVPixelBufferRef with one of the following Raw pixel format types    kCVPixelFormatType_14Bayer_GRBG, kCVPixelFormatType_14Bayer_RGGB, kCVPixelFormatType_14Bayer_BGGR, kCVPixelFormatType_14Bayer_GBRG as well as the root properties attachment from the CMSampleBufferRef.
--
-- ObjC selector: @+ filterWithCVPixelBuffer:properties:options:@
filterWithCVPixelBuffer_properties_options :: (IsNSDictionary properties, IsNSDictionary options) => Ptr () -> properties -> options -> IO (Id CIFilter)
filterWithCVPixelBuffer_properties_options pixelBuffer properties options =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr properties $ \raw_properties ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "filterWithCVPixelBuffer:properties:options:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_properties :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a NSArray containing the names of all supported RAW cameras.
--
-- ObjC selector: @+ supportedRawCameraModels@
supportedRawCameraModels :: IO (Id NSArray)
supportedRawCameraModels  =
  do
    cls' <- getRequiredClass "CIFilter"
    sendClassMsg cls' (mkSelector "supportedRawCameraModels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates a new filter of type 'name'. On OSX, all input values will be undefined. On iOS, all input values will be set to default values.
--
-- ObjC selector: @+ filterWithName:@
filterWithName :: IsNSString name => name -> IO (Id CIFilter)
filterWithName name =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "filterWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a new filter of type 'name'. The filter's input parameters are set from the list of key-value pairs which must be nil-terminated. On OSX, any of the filter input parameters not specified in the list will be undefined. On iOS, any of the filter input parameters not specified in the list will be set to default values.
--
-- ObjC selector: @+ filterWithName:keysAndValues:@
filterWithName_keysAndValues :: IsNSString name => name -> RawId -> IO (Id CIFilter)
filterWithName_keysAndValues name key0 =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "filterWithName:keysAndValues:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId key0) :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a new filter of type 'name'. The filter's input parameters are set from the dictionary of key-value pairs. On OSX, any of the filter input parameters not specified in the dictionary will be undefined. On iOS, any of the filter input parameters not specified in the dictionary will be set to default values.
--
-- ObjC selector: @+ filterWithName:withInputParameters:@
filterWithName_withInputParameters :: (IsNSString name, IsNSDictionary params) => name -> params -> IO (Id CIFilter)
filterWithName_withInputParameters name params =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr name $ \raw_name ->
      withObjCPtr params $ \raw_params ->
        sendClassMsg cls' (mkSelector "filterWithName:withInputParameters:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an array containing all published filter names in a category.
--
-- ObjC selector: @+ filterNamesInCategory:@
filterNamesInCategory :: IsNSString category => category -> IO (Id NSArray)
filterNamesInCategory category =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr category $ \raw_category ->
      sendClassMsg cls' (mkSelector "filterNamesInCategory:") (retPtr retVoid) [argPtr (castPtr raw_category :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an array containing all published filter names that belong to all listed categories.
--
-- ObjC selector: @+ filterNamesInCategories:@
filterNamesInCategories :: IsNSArray categories => categories -> IO (Id NSArray)
filterNamesInCategories categories =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr categories $ \raw_categories ->
      sendClassMsg cls' (mkSelector "filterNamesInCategories:") (retPtr retVoid) [argPtr (castPtr raw_categories :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr name $ \raw_name ->
      withObjCPtr attributes $ \raw_attributes ->
        sendClassMsg cls' (mkSelector "registerFilterName:constructor:classAttributes:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | Returns the localized name of a filter for display in the UI.
--
-- ObjC selector: @+ localizedNameForFilterName:@
localizedNameForFilterName :: IsNSString filterName => filterName -> IO (Id NSString)
localizedNameForFilterName filterName =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr filterName $ \raw_filterName ->
      sendClassMsg cls' (mkSelector "localizedNameForFilterName:") (retPtr retVoid) [argPtr (castPtr raw_filterName :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the localized name of a category for display in the UI.
--
-- ObjC selector: @+ localizedNameForCategory:@
localizedNameForCategory :: IsNSString category => category -> IO (Id NSString)
localizedNameForCategory category =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr category $ \raw_category ->
      sendClassMsg cls' (mkSelector "localizedNameForCategory:") (retPtr retVoid) [argPtr (castPtr raw_category :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the localized description of a filter for display in the UI.
--
-- ObjC selector: @+ localizedDescriptionForFilterName:@
localizedDescriptionForFilterName :: IsNSString filterName => filterName -> IO (Id NSString)
localizedDescriptionForFilterName filterName =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr filterName $ \raw_filterName ->
      sendClassMsg cls' (mkSelector "localizedDescriptionForFilterName:") (retPtr retVoid) [argPtr (castPtr raw_filterName :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the URL to the localized reference documentation describing the filter.
--
-- The URL can be a local file or a remote document on a webserver. It is possible, that this method returns nil (like filters that predate this feature). A client of this API has to handle this case gracefully.
--
-- ObjC selector: @+ localizedReferenceDocumentationForFilterName:@
localizedReferenceDocumentationForFilterName :: IsNSString filterName => filterName -> IO (Id NSURL)
localizedReferenceDocumentationForFilterName filterName =
  do
    cls' <- getRequiredClass "CIFilter"
    withObjCPtr filterName $ \raw_filterName ->
      sendClassMsg cls' (mkSelector "localizedReferenceDocumentationForFilterName:") (retPtr retVoid) [argPtr (castPtr raw_filterName :: Ptr ())] >>= retainedObject . castPtr

-- | @- enabled@
enabled :: IsCIFilter ciFilter => ciFilter -> IO Bool
enabled ciFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFilter (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsCIFilter ciFilter => ciFilter -> Bool -> IO ()
setEnabled ciFilter  value =
  sendMsg ciFilter (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Returns an array containing the names of all inputs in the filter.
--
-- ObjC selector: @- inputKeys@
inputKeys :: IsCIFilter ciFilter => ciFilter -> IO (Id NSArray)
inputKeys ciFilter  =
  sendMsg ciFilter (mkSelector "inputKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an array containing the names of all outputs in the filter.
--
-- ObjC selector: @- outputKeys@
outputKeys :: IsCIFilter ciFilter => ciFilter -> IO (Id NSArray)
outputKeys ciFilter  =
  sendMsg ciFilter (mkSelector "outputKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a dictionary containing key/value pairs describing the filter. (see description of keys below)
--
-- ObjC selector: @- attributes@
attributes :: IsCIFilter ciFilter => ciFilter -> IO (Id NSDictionary)
attributes ciFilter  =
  sendMsg ciFilter (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @setDefaults@
setDefaultsSelector :: Selector
setDefaultsSelector = mkSelector "setDefaults"

-- | @Selector@ for @apply:arguments:options:@
apply_arguments_optionsSelector :: Selector
apply_arguments_optionsSelector = mkSelector "apply:arguments:options:"

-- | @Selector@ for @apply:@
applySelector :: Selector
applySelector = mkSelector "apply:"

-- | @Selector@ for @filterWithImageURL:options:@
filterWithImageURL_optionsSelector :: Selector
filterWithImageURL_optionsSelector = mkSelector "filterWithImageURL:options:"

-- | @Selector@ for @filterWithImageData:options:@
filterWithImageData_optionsSelector :: Selector
filterWithImageData_optionsSelector = mkSelector "filterWithImageData:options:"

-- | @Selector@ for @filterWithCVPixelBuffer:properties:options:@
filterWithCVPixelBuffer_properties_optionsSelector :: Selector
filterWithCVPixelBuffer_properties_optionsSelector = mkSelector "filterWithCVPixelBuffer:properties:options:"

-- | @Selector@ for @supportedRawCameraModels@
supportedRawCameraModelsSelector :: Selector
supportedRawCameraModelsSelector = mkSelector "supportedRawCameraModels"

-- | @Selector@ for @filterWithName:@
filterWithNameSelector :: Selector
filterWithNameSelector = mkSelector "filterWithName:"

-- | @Selector@ for @filterWithName:keysAndValues:@
filterWithName_keysAndValuesSelector :: Selector
filterWithName_keysAndValuesSelector = mkSelector "filterWithName:keysAndValues:"

-- | @Selector@ for @filterWithName:withInputParameters:@
filterWithName_withInputParametersSelector :: Selector
filterWithName_withInputParametersSelector = mkSelector "filterWithName:withInputParameters:"

-- | @Selector@ for @filterNamesInCategory:@
filterNamesInCategorySelector :: Selector
filterNamesInCategorySelector = mkSelector "filterNamesInCategory:"

-- | @Selector@ for @filterNamesInCategories:@
filterNamesInCategoriesSelector :: Selector
filterNamesInCategoriesSelector = mkSelector "filterNamesInCategories:"

-- | @Selector@ for @registerFilterName:constructor:classAttributes:@
registerFilterName_constructor_classAttributesSelector :: Selector
registerFilterName_constructor_classAttributesSelector = mkSelector "registerFilterName:constructor:classAttributes:"

-- | @Selector@ for @localizedNameForFilterName:@
localizedNameForFilterNameSelector :: Selector
localizedNameForFilterNameSelector = mkSelector "localizedNameForFilterName:"

-- | @Selector@ for @localizedNameForCategory:@
localizedNameForCategorySelector :: Selector
localizedNameForCategorySelector = mkSelector "localizedNameForCategory:"

-- | @Selector@ for @localizedDescriptionForFilterName:@
localizedDescriptionForFilterNameSelector :: Selector
localizedDescriptionForFilterNameSelector = mkSelector "localizedDescriptionForFilterName:"

-- | @Selector@ for @localizedReferenceDocumentationForFilterName:@
localizedReferenceDocumentationForFilterNameSelector :: Selector
localizedReferenceDocumentationForFilterNameSelector = mkSelector "localizedReferenceDocumentationForFilterName:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @inputKeys@
inputKeysSelector :: Selector
inputKeysSelector = mkSelector "inputKeys"

-- | @Selector@ for @outputKeys@
outputKeysSelector :: Selector
outputKeysSelector = mkSelector "outputKeys"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

