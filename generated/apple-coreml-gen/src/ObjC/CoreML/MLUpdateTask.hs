{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector
  , resumeWithParametersSelector
  , updateTaskForModelAtURL_trainingData_completionHandler_errorSelector
  , updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector
  , updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector
  , updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:@
updateTaskForModelAtURL_trainingData_configuration_completionHandler_error :: (IsNSURL modelURL, IsMLModelConfiguration configuration, IsNSError error_) => modelURL -> RawId -> configuration -> Ptr () -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_configuration_completionHandler_error modelURL trainingData configuration completionHandler error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    sendClassMessage cls' updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector (toNSURL modelURL) trainingData (toMLModelConfiguration configuration) completionHandler (toNSError error_)

-- | @+ updateTaskForModelAtURL:trainingData:completionHandler:error:@
updateTaskForModelAtURL_trainingData_completionHandler_error :: (IsNSURL modelURL, IsNSError error_) => modelURL -> RawId -> Ptr () -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_completionHandler_error modelURL trainingData completionHandler error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    sendClassMessage cls' updateTaskForModelAtURL_trainingData_completionHandler_errorSelector (toNSURL modelURL) trainingData completionHandler (toNSError error_)

-- | @+ updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_error :: (IsNSURL modelURL, IsMLModelConfiguration configuration, IsMLUpdateProgressHandlers progressHandlers, IsNSError error_) => modelURL -> RawId -> configuration -> progressHandlers -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_error modelURL trainingData configuration progressHandlers error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    sendClassMessage cls' updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector (toNSURL modelURL) trainingData (toMLModelConfiguration configuration) (toMLUpdateProgressHandlers progressHandlers) (toNSError error_)

-- | @+ updateTaskForModelAtURL:trainingData:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_progressHandlers_error :: (IsNSURL modelURL, IsMLUpdateProgressHandlers progressHandlers, IsNSError error_) => modelURL -> RawId -> progressHandlers -> error_ -> IO (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_progressHandlers_error modelURL trainingData progressHandlers error_ =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    sendClassMessage cls' updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector (toNSURL modelURL) trainingData (toMLUpdateProgressHandlers progressHandlers) (toNSError error_)

-- | @- resumeWithParameters:@
resumeWithParameters :: (IsMLUpdateTask mlUpdateTask, IsNSDictionary updateParameters) => mlUpdateTask -> updateParameters -> IO ()
resumeWithParameters mlUpdateTask updateParameters =
  sendMessage mlUpdateTask resumeWithParametersSelector (toNSDictionary updateParameters)

-- | @- init@
init_ :: IsMLUpdateTask mlUpdateTask => mlUpdateTask -> IO (Id MLUpdateTask)
init_ mlUpdateTask =
  sendOwnedMessage mlUpdateTask initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLUpdateTask"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:@
updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector :: Selector '[Id NSURL, RawId, Id MLModelConfiguration, Ptr (), Id NSError] (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_configuration_completionHandler_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:configuration:completionHandler:error:"

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:completionHandler:error:@
updateTaskForModelAtURL_trainingData_completionHandler_errorSelector :: Selector '[Id NSURL, RawId, Ptr (), Id NSError] (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_completionHandler_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:completionHandler:error:"

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector :: Selector '[Id NSURL, RawId, Id MLModelConfiguration, Id MLUpdateProgressHandlers, Id NSError] (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_configuration_progressHandlers_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:configuration:progressHandlers:error:"

-- | @Selector@ for @updateTaskForModelAtURL:trainingData:progressHandlers:error:@
updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector :: Selector '[Id NSURL, RawId, Id MLUpdateProgressHandlers, Id NSError] (Id MLUpdateTask)
updateTaskForModelAtURL_trainingData_progressHandlers_errorSelector = mkSelector "updateTaskForModelAtURL:trainingData:progressHandlers:error:"

-- | @Selector@ for @resumeWithParameters:@
resumeWithParametersSelector :: Selector '[Id NSDictionary] ()
resumeWithParametersSelector = mkSelector "resumeWithParameters:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLUpdateTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

