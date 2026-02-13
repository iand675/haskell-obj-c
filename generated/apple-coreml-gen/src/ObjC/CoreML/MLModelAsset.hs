{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A compiled model asset.
--
-- @MLModelAsset@ is an abstraction of a compiled model, which can be:
--
-- - @.mlmodelc@ bundle on the file system  - In-memory model specification
--
-- It provides the unified interface to query the model description and to instantiate @MLModel@.
--
-- ```swift // Creates an object. let modelAsset = MLModelAsset(url: modelURL)
--
-- // Query the model description let description = try await modelAsset.modelDescription
--
-- // Query the list of functions in the model asset. let functionNames = try await modelAsset.functionNames
--
-- // Query the model description of a specific function. let descriptionOfMyFunction = try await modelAsset.modelDescription(of: "MyFunction")
--
-- // Instantiate @MLModel@ for "MyFunction". let modelConfiguration = MLModelConfiguration() modelConfiguration.functionName = "MyFunction" let model = try await MLModel.load(asset: modelAsset, configuration: modelConfiguration) ```
--
-- Generated bindings for @MLModelAsset@.
module ObjC.CoreML.MLModelAsset
  ( MLModelAsset
  , IsMLModelAsset(..)
  , modelAssetWithSpecificationData_error
  , modelAssetWithSpecificationData_blobMapping_error
  , modelAssetWithURL_error
  , modelDescriptionWithCompletionHandler
  , modelDescriptionOfFunctionNamed_completionHandler
  , init_
  , new
  , initSelector
  , modelAssetWithSpecificationData_blobMapping_errorSelector
  , modelAssetWithSpecificationData_errorSelector
  , modelAssetWithURL_errorSelector
  , modelDescriptionOfFunctionNamed_completionHandlerSelector
  , modelDescriptionWithCompletionHandlerSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Construct a model asset from the contents of specification data.
--
-- - Parameters:   - specificationData: Contents of .mlmodel as a data blob.   - error: When the model asset creation fails error is populated with the reason for failure.
--
-- ObjC selector: @+ modelAssetWithSpecificationData:error:@
modelAssetWithSpecificationData_error :: (IsNSData specificationData, IsNSError error_) => specificationData -> error_ -> IO (Id MLModelAsset)
modelAssetWithSpecificationData_error specificationData error_ =
  do
    cls' <- getRequiredClass "MLModelAsset"
    sendClassMessage cls' modelAssetWithSpecificationData_errorSelector (toNSData specificationData) (toNSError error_)

-- | Construct a model asset from an ML Program specification by replacing blob file references with corresponding in-memory blobs.
--
-- An ML Program may use @BlobFileValue@ syntax, which stores the blob data in external files and refers them by URL. This factory method enables in-memory workflow for such models by using the specified in-memory blob data in place of the external files.
--
-- The format of in-memory blobs must be the same as the external files. The dictionary must contain all the reference URLs used in the specification.
--
-- - Parameters:   - specification: Contents of .mlmodel as a data blob.   - blobMapping: A dictionary with blob URL as the key and blob data as the value.   - error: When the model asset creation fails error is populated with the reason for failure.
--
-- ObjC selector: @+ modelAssetWithSpecificationData:blobMapping:error:@
modelAssetWithSpecificationData_blobMapping_error :: (IsNSData specificationData, IsNSDictionary blobMapping, IsNSError error_) => specificationData -> blobMapping -> error_ -> IO (Id MLModelAsset)
modelAssetWithSpecificationData_blobMapping_error specificationData blobMapping error_ =
  do
    cls' <- getRequiredClass "MLModelAsset"
    sendClassMessage cls' modelAssetWithSpecificationData_blobMapping_errorSelector (toNSData specificationData) (toNSDictionary blobMapping) (toNSError error_)

-- | Constructs a ModelAsset from a compiled model URL.
--
-- - Parameters:   - compiledModelURL: Location on the disk where the model asset is present.   - error: Errors if the model asset is not loadable.
--
-- - Returns: a model asset or nil if there is an error.
--
-- ObjC selector: @+ modelAssetWithURL:error:@
modelAssetWithURL_error :: (IsNSURL compiledModelURL, IsNSError error_) => compiledModelURL -> error_ -> IO (Id MLModelAsset)
modelAssetWithURL_error compiledModelURL error_ =
  do
    cls' <- getRequiredClass "MLModelAsset"
    sendClassMessage cls' modelAssetWithURL_errorSelector (toNSURL compiledModelURL) (toNSError error_)

-- | The default model descripton.
--
-- Use this method to get the description of the model such as the feature descriptions, the model author, and other metadata.
--
-- For the multi-function model asset, this method vends the description for the default function. Use @modelDescription(for:)@ to get the model description of other functions.
--
-- ```swift let modelAsset = try MLModelAsset(url: modelURL) let modelDescription = try await modelAsset.modelDescription() print(modelDescription) ```
--
-- ObjC selector: @- modelDescriptionWithCompletionHandler:@
modelDescriptionWithCompletionHandler :: IsMLModelAsset mlModelAsset => mlModelAsset -> Ptr () -> IO ()
modelDescriptionWithCompletionHandler mlModelAsset handler =
  sendMessage mlModelAsset modelDescriptionWithCompletionHandlerSelector handler

-- | The model descripton for a specified function.
--
-- Use this method to get the description of the model such as the feature descriptions, the model author, and other metadata.
--
-- ```swift let modelAsset = try MLModelAsset(url: modelURL) let modelDescription = try await modelAsset.modelDescription(of: "my_function") print(modelDescription) ```
--
-- ObjC selector: @- modelDescriptionOfFunctionNamed:completionHandler:@
modelDescriptionOfFunctionNamed_completionHandler :: (IsMLModelAsset mlModelAsset, IsNSString functionName) => mlModelAsset -> functionName -> Ptr () -> IO ()
modelDescriptionOfFunctionNamed_completionHandler mlModelAsset functionName handler =
  sendMessage mlModelAsset modelDescriptionOfFunctionNamed_completionHandlerSelector (toNSString functionName) handler

-- | @- init@
init_ :: IsMLModelAsset mlModelAsset => mlModelAsset -> IO (Id MLModelAsset)
init_ mlModelAsset =
  sendOwnedMessage mlModelAsset initSelector

-- | @+ new@
new :: IO (Id MLModelAsset)
new  =
  do
    cls' <- getRequiredClass "MLModelAsset"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modelAssetWithSpecificationData:error:@
modelAssetWithSpecificationData_errorSelector :: Selector '[Id NSData, Id NSError] (Id MLModelAsset)
modelAssetWithSpecificationData_errorSelector = mkSelector "modelAssetWithSpecificationData:error:"

-- | @Selector@ for @modelAssetWithSpecificationData:blobMapping:error:@
modelAssetWithSpecificationData_blobMapping_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSError] (Id MLModelAsset)
modelAssetWithSpecificationData_blobMapping_errorSelector = mkSelector "modelAssetWithSpecificationData:blobMapping:error:"

-- | @Selector@ for @modelAssetWithURL:error:@
modelAssetWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id MLModelAsset)
modelAssetWithURL_errorSelector = mkSelector "modelAssetWithURL:error:"

-- | @Selector@ for @modelDescriptionWithCompletionHandler:@
modelDescriptionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
modelDescriptionWithCompletionHandlerSelector = mkSelector "modelDescriptionWithCompletionHandler:"

-- | @Selector@ for @modelDescriptionOfFunctionNamed:completionHandler:@
modelDescriptionOfFunctionNamed_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
modelDescriptionOfFunctionNamed_completionHandlerSelector = mkSelector "modelDescriptionOfFunctionNamed:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelAsset)
newSelector = mkSelector "new"

