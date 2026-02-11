{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main class for setting up and controlling a model update. It provides some utility class methods for performing an update synchronously as well as class constructors for configuring an update and give developers control for the execution of that update.
--
-- Generated bindings for @MLUpdateTask@.
module ObjC.CoreML.MLUpdateTask
  ( MLUpdateTask
  , IsMLUpdateTask(..)
  , updateTaskForModelAtURL_trainingData_configuration_completionHandler_error
  , updateTaskForModelAtURL_trainingData_completionHandler_error
  , updateTaskForModelAtURL_trainingData_configuration_progressHandlers_error
  , updateTaskForModelAtURL_trainingData_progressHandlers_error
  , resumeWithParameters
  , init_
  , new
  , updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector
  , updateTaskForModelAtURL_trainingData_completionHandler_errorSelector
  , updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector
  , updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector
  , resumeWithParametersSelector
  , initSelector
  , newSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:@
updateTaskForModelAtURL_trainingData_configuration_completionHandler_error :: (IsNSURL modelURL, IsMLModelConfiguration configuration, IsNSError error_) => modelURL -> RawId -> configuration -> Ptr () -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_configuration_completionHandler_error modelURL trainingData configuration completionHandler error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    withObjCPtr modelURL $ \raw_modelURL ->
      withObjCPtr configuration $ \raw_configuration ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:") (retPtr retVoid) [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr (unRawId trainingData) :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ updateTaskForModelAtURL:trainingData:completionHandler:error:@
updateTaskForModelAtURL_trainingData_completionHandler_error :: (IsNSURL modelURL, IsNSError error_) => modelURL -> RawId -> Ptr () -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_completionHandler_error modelURL trainingData completionHandler error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    withObjCPtr modelURL $ \raw_modelURL ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "updateTaskForModelAtURL:trainingData:completionHandler:error:") (retPtr retVoid) [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr (unRawId trainingData) :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_error :: (IsNSURL modelURL, IsMLModelConfiguration configuration, IsMLUpdateProgressHandlers progressHandlers, IsNSError error_) => modelURL -> RawId -> configuration -> progressHandlers -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_error modelURL trainingData configuration progressHandlers error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    withObjCPtr modelURL $ \raw_modelURL ->
      withObjCPtr configuration $ \raw_configuration ->
        withObjCPtr progressHandlers $ \raw_progressHandlers ->
          withObjCPtr error_ $ \raw_error_ ->
            sendClassMsg cls' (mkSelector "updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:") (retPtr retVoid) [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr (unRawId trainingData) :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_progressHandlers :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ updateTaskForModelAtURL:trainingData:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_progressHandlers_error :: (IsNSURL modelURL, IsMLUpdateProgressHandlers progressHandlers, IsNSError error_) => modelURL -> RawId -> progressHandlers -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_progressHandlers_error modelURL trainingData progressHandlers error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    withObjCPtr modelURL $ \raw_modelURL ->
      withObjCPtr progressHandlers $ \raw_progressHandlers ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "updateTaskForModelAtURL:trainingData:progressHandlers:error:") (retPtr retVoid) [argPtr (castPtr raw_modelURL :: Ptr ()), argPtr (castPtr (unRawId trainingData) :: Ptr ()), argPtr (castPtr raw_progressHandlers :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- resumeWithParameters:@
resumeWithParameters :: (IsMLUpdateTask mlUpdateTask, IsNSDictionary updateParameters) => mlUpdateTask -> updateParameters -> IO ()
resumeWithParameters mlUpdateTask  updateParameters =
withObjCPtr updateParameters $ \raw_updateParameters ->
    sendMsg mlUpdateTask (mkSelector "resumeWithParameters:") retVoid [argPtr (castPtr raw_updateParameters :: Ptr ())]

-- | @- init@
init_ :: IsMLUpdateTask mlUpdateTask => mlUpdateTask -> IO (Id MLUpdateTask)
init_ mlUpdateTask  =
  sendMsg mlUpdateTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:@
updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector :: Selector
updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:"

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:completionHandler:error:@
updateTaskForModelAtURL_trainingData_completionHandler_errorSelector :: Selector
updateTaskForModelAtURL_trainingData_completionHandler_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:completionHandler:error:"

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector :: Selector
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:"

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector :: Selector
updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:progressHandlers:error:"

-- | @Selector@ for @resumeWithParameters:@
resumeWithParametersSelector :: Selector
resumeWithParametersSelector = mkSelector "resumeWithParameters:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

