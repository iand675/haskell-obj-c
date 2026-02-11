{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNSceneSource
--
-- SCNSceneSource objects, abstract the data-reading task. A scene source can read scene data from a URL or a NSData object. After creating a SCNSceneSource object for the appropriate source, you can obtain scenes using SCNSceneSource methods.
--
-- Generated bindings for @SCNSceneSource@.
module ObjC.SceneKit.SCNSceneSource
  ( SCNSceneSource
  , IsSCNSceneSource(..)
  , sceneSourceWithURL_options
  , sceneSourceWithData_options
  , initWithURL_options
  , initWithData_options
  , sceneWithOptions_statusHandler
  , sceneWithOptions_error
  , propertyForKey
  , entryWithIdentifier_withClass
  , identifiersOfEntriesWithClass
  , entriesPassingTest
  , url
  , data_
  , sceneSourceWithURL_optionsSelector
  , sceneSourceWithData_optionsSelector
  , initWithURL_optionsSelector
  , initWithData_optionsSelector
  , sceneWithOptions_statusHandlerSelector
  , sceneWithOptions_errorSelector
  , propertyForKeySelector
  , entryWithIdentifier_withClassSelector
  , identifiersOfEntriesWithClassSelector
  , entriesPassingTestSelector
  , urlSelector
  , dataSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sceneSourceWithURL:options:
--
-- Creates and initialize a SCNSceneSource instance.
--
-- @url@ — The URL to read scenes from.
--
-- @options@ — An optional dictionary for future extensions.
--
-- ObjC selector: @+ sceneSourceWithURL:options:@
sceneSourceWithURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id SCNSceneSource)
sceneSourceWithURL_options url options =
  do
    cls' <- getRequiredClass "SCNSceneSource"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "sceneSourceWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | sceneSourceWithData:options:
--
-- Creates and initialize a SCNSceneSource instance.
--
-- @data@ — The scene data.
--
-- @options@ — An optional dictionary for future extensions.
--
-- ObjC selector: @+ sceneSourceWithData:options:@
sceneSourceWithData_options :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> IO (Id SCNSceneSource)
sceneSourceWithData_options data_ options =
  do
    cls' <- getRequiredClass "SCNSceneSource"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "sceneSourceWithData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | initWithURL:options:
--
-- Initialize a SCNSceneSource instance from a URL.
--
-- @url@ — The URL to read scenes from.
--
-- @options@ — An optional dictionary for future extensions.
--
-- ObjC selector: @- initWithURL:options:@
initWithURL_options :: (IsSCNSceneSource scnSceneSource, IsNSURL url, IsNSDictionary options) => scnSceneSource -> url -> options -> IO (Id SCNSceneSource)
initWithURL_options scnSceneSource  url options =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
      sendMsg scnSceneSource (mkSelector "initWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | initWithData:options:
--
-- Initialize a SCNSceneSource instance from a NSData object.
--
-- @data@ — The data to read scenes from.
--
-- @options@ — An optional dictionary for future extensions.
--
-- ObjC selector: @- initWithData:options:@
initWithData_options :: (IsSCNSceneSource scnSceneSource, IsNSData data_, IsNSDictionary options) => scnSceneSource -> data_ -> options -> IO (Id SCNSceneSource)
initWithData_options scnSceneSource  data_ options =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
      sendMsg scnSceneSource (mkSelector "initWithData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | sceneWithOptions:statusHandler:
--
-- Creates and initializes the scene described in the 3D file with the specified options and lets you monitor the progress.
--
-- @options@ — A dictionary of options. The valid keys are described in the "Scene loading options" section.
--
-- @statusHandler@ — This block will be called repeatedly while the scene is being loaded.                      - The first argument, overallProgress, is a floating-point number between 0 and 1. 0 means the loading process has just started and 1 that it is complete.					  - The second argument, status, tells you what the source is currently doing. It takes one of the values in the SCNSceneSourceStatus enum. New values might be added to this enum in the future.					  - If status == SCNSceneStatusError, then error will contain more information about the failure, and the method will return nil after having called the block. Otherwise error will be nil.					  - Set *stop to YES if you want the source to abort the loading operation.
--
-- ObjC selector: @- sceneWithOptions:statusHandler:@
sceneWithOptions_statusHandler :: (IsSCNSceneSource scnSceneSource, IsNSDictionary options) => scnSceneSource -> options -> Ptr () -> IO (Id SCNScene)
sceneWithOptions_statusHandler scnSceneSource  options statusHandler =
withObjCPtr options $ \raw_options ->
    sendMsg scnSceneSource (mkSelector "sceneWithOptions:statusHandler:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr statusHandler :: Ptr ())] >>= retainedObject . castPtr

-- | sceneWithOptions:error:
--
-- Creates and initializes the scene described in the 3D file with the specified options.
--
-- @options@ — A dictionary of options. The valid keys are described in the "Scene loading options" section.
--
-- @error@ — If this method returns nil, an error providing more information is returned by reference.
--
-- This simpler version is equivalent to providing a block to sceneWithOptions:statusHandler: and checking the "error" parameter of the block if the status is SCNSceneStatusError.
--
-- ObjC selector: @- sceneWithOptions:error:@
sceneWithOptions_error :: (IsSCNSceneSource scnSceneSource, IsNSDictionary options, IsNSError error_) => scnSceneSource -> options -> error_ -> IO (Id SCNScene)
sceneWithOptions_error scnSceneSource  options error_ =
withObjCPtr options $ \raw_options ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg scnSceneSource (mkSelector "sceneWithOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | propertyForKey:
--
-- @key@ — The key for which to return the corresponding property.
--
-- Returns the property as defined in the 3D file for the given key. See keys above.
--
-- ObjC selector: @- propertyForKey:@
propertyForKey :: (IsSCNSceneSource scnSceneSource, IsNSString key) => scnSceneSource -> key -> IO RawId
propertyForKey scnSceneSource  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg scnSceneSource (mkSelector "propertyForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | entryWithIdentifier:classType:
--
-- Returns the instance of "entryClass" found in the receiver's library with the id "uid".
--
-- @uid@ — The id of the entry to find as specified in the source file
--
-- @entryClass@ — Specifies the type of the object to be returned. It can be one of the following classes: SCNMaterial, SCNGeometry, SCNScene, SCNNode, CAAnimation, SCNLight, SCNCamera, SCNSkinner, SCNMorpher, NSImage
--
-- Returns NULL if the receiver's library doesn't contains such an uid for the specified type.
--
-- ObjC selector: @- entryWithIdentifier:withClass:@
entryWithIdentifier_withClass :: (IsSCNSceneSource scnSceneSource, IsNSString uid) => scnSceneSource -> uid -> Class -> IO RawId
entryWithIdentifier_withClass scnSceneSource  uid entryClass =
withObjCPtr uid $ \raw_uid ->
    fmap (RawId . castPtr) $ sendMsg scnSceneSource (mkSelector "entryWithIdentifier:withClass:") (retPtr retVoid) [argPtr (castPtr raw_uid :: Ptr ()), argPtr (unClass entryClass)]

-- | identifiersOfEntriesWithClass:
--
-- Returns the IDs found in the receiver's library for the class "entryClass".
--
-- @entryClass@ — Specifies the type of the object referenced by the returned IDs. It can be one of the following classes: SCNMaterial, SCNScene, SCNGeometry, SCNNode, CAAnimation, SCNLight, SCNCamera, SCNSkinner, SCNMorpher, NSImage
--
-- ObjC selector: @- identifiersOfEntriesWithClass:@
identifiersOfEntriesWithClass :: IsSCNSceneSource scnSceneSource => scnSceneSource -> Class -> IO (Id NSArray)
identifiersOfEntriesWithClass scnSceneSource  entryClass =
  sendMsg scnSceneSource (mkSelector "identifiersOfEntriesWithClass:") (retPtr retVoid) [argPtr (unClass entryClass)] >>= retainedObject . castPtr

-- | entriesPassingTest:
--
-- Returns the entries in the receiver's library that pass a test in a given Block.
--
-- @predicate@ — The block to apply to entries in the library. The block takes three arguments: "entry" is an entry in the library, "identifier" is the ID of this entry and "stop" is a reference to a Boolean value. The block can set the value to YES to stop further processing of the library. The stop argument is an out-only argument. You should only ever set this Boolean to YES within the Block. The Block returns a Boolean value that indicates whether "entry" passed the test.
--
-- The entry is an instance of one of following classes: SCNMaterial, SCNScene, SCNGeometry, SCNNode, CAAnimation, SCNLight, SCNCamera, SCNSkinner, SCNMorpher, NSImage.
--
-- ObjC selector: @- entriesPassingTest:@
entriesPassingTest :: IsSCNSceneSource scnSceneSource => scnSceneSource -> Ptr () -> IO (Id NSArray)
entriesPassingTest scnSceneSource  predicate =
  sendMsg scnSceneSource (mkSelector "entriesPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | url
--
-- The receiver's URL (if any).
--
-- ObjC selector: @- url@
url :: IsSCNSceneSource scnSceneSource => scnSceneSource -> IO (Id NSURL)
url scnSceneSource  =
  sendMsg scnSceneSource (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | data
--
-- The receiver's data (if any).
--
-- ObjC selector: @- data@
data_ :: IsSCNSceneSource scnSceneSource => scnSceneSource -> IO (Id NSData)
data_ scnSceneSource  =
  sendMsg scnSceneSource (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneSourceWithURL:options:@
sceneSourceWithURL_optionsSelector :: Selector
sceneSourceWithURL_optionsSelector = mkSelector "sceneSourceWithURL:options:"

-- | @Selector@ for @sceneSourceWithData:options:@
sceneSourceWithData_optionsSelector :: Selector
sceneSourceWithData_optionsSelector = mkSelector "sceneSourceWithData:options:"

-- | @Selector@ for @initWithURL:options:@
initWithURL_optionsSelector :: Selector
initWithURL_optionsSelector = mkSelector "initWithURL:options:"

-- | @Selector@ for @initWithData:options:@
initWithData_optionsSelector :: Selector
initWithData_optionsSelector = mkSelector "initWithData:options:"

-- | @Selector@ for @sceneWithOptions:statusHandler:@
sceneWithOptions_statusHandlerSelector :: Selector
sceneWithOptions_statusHandlerSelector = mkSelector "sceneWithOptions:statusHandler:"

-- | @Selector@ for @sceneWithOptions:error:@
sceneWithOptions_errorSelector :: Selector
sceneWithOptions_errorSelector = mkSelector "sceneWithOptions:error:"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @entryWithIdentifier:withClass:@
entryWithIdentifier_withClassSelector :: Selector
entryWithIdentifier_withClassSelector = mkSelector "entryWithIdentifier:withClass:"

-- | @Selector@ for @identifiersOfEntriesWithClass:@
identifiersOfEntriesWithClassSelector :: Selector
identifiersOfEntriesWithClassSelector = mkSelector "identifiersOfEntriesWithClass:"

-- | @Selector@ for @entriesPassingTest:@
entriesPassingTestSelector :: Selector
entriesPassingTestSelector = mkSelector "entriesPassingTest:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

